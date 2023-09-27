# booststrap regression
# https://www.youtube.com/watch?v=sjCxIHVGkdE&ab_channel=yuzaRDataScience
# https://markjrieke.github.io/workboots/
# https://patrick-rockenschaub.com/post/2022-01-26-tidymodels-optimism-bootstrap/

# https://emilhvitfeldt.github.io/ISLR-tidymodels-labs/05-resampling-methods.html
library(tidyverse)
library(tidymodels)
### carregar os dados
my_df_complete  <-  readRDS(here::here("data","final.RDS")) 
my_df_complete %>% glimpse()

my_df_complete <- 
  my_df_complete %>% 
  mutate(cat_pop = ifelse(populacao < 30000, "menor 30k" , "mais 30k")) %>% 
  mutate(idade_cat = case_when(
    anomes == 0 ~ "0",
    anomes >= 202206 ~ "até 1 ano",
    anomes >= 202006 ~ "até 3 anos",
    anomes >= 201806 ~ "até 5 anos",
    TRUE ~           "mais que 5 anos"
  ))

my_df_complete %>% glimpse()
best_model <- c("prop_vab_ag",
                "bovino","anomes","qtd_ag","total_auto_pc",
                "vab_serv","latitude")
my_df <- 
  my_df_complete %>% 
  filter(credito > 0) %>% 
  transmute(
    credito = log1p(credito),
    # regiao_metropolitana = ifelse(regiao_metropolitana == "NA",0,1),
    idade_cat,
    pib_prc_corr,
    vab_serv,
    prop_vab_ag,
    prop_vab_serv,
    latitude,
    longitude,
    index,
    anomes,
    qtd_ag,
    total_auto_pc,
    cat_pop,
    populacao,
    empresas,
    bovino
  )
my_df %>% glimpse()
minimo <- abs(my_df$index %>% min())
my_df <- my_df %>% 
  mutate(index = index + minimo) 
### initial split------------------
set.seed(458)
my_df_split <- initial_split(my_df, prop = 0.75,
                             strata = cat_pop)

## in case you want use bootstrap bootstrap
boots_folds <- bootstraps(training(my_df_split), times = 20, apparent = TRUE,
                          strata = cat_pop)
boots_folds
## recipe-------------------
boots_recipe <-
  recipe(formula = credito ~ ., data = training(my_df_split)) %>%
  step_log(all_numeric_predictors(),-latitude, -longitude,offset = 1) %>% 
  step_range(all_numeric_predictors()) %>%
  step_poly(anomes,degree = 3) %>% 
  step_poly(populacao,degree = 3) %>% 
  step_poly(pib_prc_corr, degree = 3) %>% 
  step_zv(all_numeric_predictors()) %>% 
  #  step_normalize(all_numeric_predictors()) %>% 
  step_corr(all_numeric_predictors(),threshold = 0.9) %>% 
  # step_novel(all_nominal_predictors()) %>% 
  # step_unknown(all_nominal_predictors()) %>% 
  # step_other(all_nominal_predictors(), threshold = 0.3) %>%
  step_dummy(all_nominal_predictors(),one_hot = FALSE) %>%
  step_zv(all_numeric_predictors()) 

boots_recipe %>% prep() %>% 
  bake(my_df_split) %>% glimpse()

col_names <- 
  boots_recipe %>% prep() %>% 
  bake(my_df_split) %>% names()

boots_recipe %>% prep() %>% 
  bake(my_df_split) %>% ncol()

#boots_recipe %>% prep() %>% bake(my_df_split) %>% View()

##spec-------------------

# best = penalty 0.0320 mixture = 0.156
boots_spec <-
  linear_reg(
    penalty = 0.03,
    mixture = 0.15
  ) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

##workflow----------------------
formula <-  credito ~.
boots_spec <-
  linear_reg() %>% 
  set_mode("regression") %>% 
  set_engine("lm")

my_model <- 
  boots_folds %>% 
  mutate(
    processed = map(splits, ~juice(prep(boots_recipe,training(.)))),
    model = map(processed, ~fit(linear_reg(),formula,data =.)),
    coefs = map(model,~tidy(.)),
    .pred = map2(model,processed, ~predict(.x,new_data = .y)),
    .mape = map2_dbl(processed,.pred,~mape_vec(.x$credito,.y$.pred)),
    .rmse = map2_dbl(processed,.pred,~rmse_vec(.x$credito,.y$.pred)),
    glance = map(model,broom::glance),
    .rsq = glance %>% map_dbl("r.squared"),
    .adj_rsq = glance %>% map_dbl("adj.r.squared")
  )

my_model %>% select(
  .rsq, .adj_rsq,.mape,.rmse
) %>% 
  pivot_longer(everything()) %>% 
  group_by(name) %>% 
  summarise(
    mediana = median(value),
    media = mean(value),
    conf025 = quantile(value,0.025),
    conf975 = quantile(value, 0.975)
  )

## coefs ---------------------------------------
my_model %>% 
  select(coefs) %>% 
  unnest(coefs) %>% 
  distinct(term)

my_model %>% 
  select(coefs) %>% 
  unnest(coefs) %>% 
  select(term,estimate) %>% 
  filter(term %in% c("pib_pc","vab_agro",
                     "vab_ind",
                     "area_plantada_temp")) %>% 
  ggplot(aes(x = estimate)) +
  geom_histogram()+
  facet_wrap(~term, scales = "free")

boots_medias <- 
  my_model %>% 
  select(coefs) %>% 
  unnest(coefs) %>% 
  group_by(term) %>% 
  summarise(
    med_est = median(estimate),
    p.value = median(p.value)
  )
boots_medias %>% View()
library(ggrepel)
boots_medias %>% 
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x=med_est, y = term, label = p.value))+
  geom_vline(
    xintercept = 0,lty=2,size=1.5,alpha=0.7,color="gray50"
  )+
  geom_point(
    alpha = 0.8,size=2.5,show.legend = FALSE
  )+
  theme_light(base_family = "IBMPlexSans") +
  labs(x = "coeficiente")

### testing data ----------------------------
add_cols <- function(df,cols) {
  add <- cols[!cols %in% names(df)]
  if(length(add)!= 0) df[add] <- 0
  return(df)
}

my_model_testing <- 
  my_model %>% 
  mutate(
    testing_processed = map(splits, ~juice(prep(boots_recipe,testing(.)))),
    col_names = map(processed,names),
    testing_processed = map2(testing_processed,col_names,
                             ~add_cols(.x,.y)),
    .pred_test = map2(model, testing_processed, ~predict(.x,new_data=.y)),
    .rmse_test = map2_dbl(testing_processed,.pred_test, ~rmse_vec(.x$credito,.y$.pred)),
    .rsq_test = map2_dbl(testing_processed,.pred_test, ~rsq_vec(.x$credito,.y$.pred))
  )
my_model

my_model_testing %>% select(
  .rsq_test, .rmse_test
) %>% 
  pivot_longer(everything()) %>% 
  group_by(name) %>% 
  summarise(
    mediana = median(value),
    media = mean(value),
    conf025 = quantile(value,0.025),
    conf975 = quantile(value, 0.975)
  )

## forecasting -------------------------
my_model_forecast <- 
  tibble(
    my_model_testing,
    data = list(my_df_complete)
  )

my_model_forecast %>% names()
my_model_forecast <- 
  my_model_forecast %>% 
  mutate(
    data_processed = map(data, ~juice(prep(boots_recipe,.))),
    #col_names = map(processed,names),
    data_processed = map2(data_processed,col_names,
                          ~add_cols(.x,.y)),
    .pred_forecast = map2(model, data_processed, ~predict(.x,new_data=.y))
  )
my_model_forecast <- 
  my_model_forecast %>% 
  mutate(
    .pred_forecast = map(.pred_forecast, ~cbind(.x,my_df_complete$cod_ibge))
  )
my_df_forecast <- 
  my_model_forecast %>% 
  select(.pred_forecast) %>% 
  unnest(.pred_forecast)
names(my_df_forecast) <- c(".pred","ibge")
my_forecast <- 
  my_df_forecast %>% 
  group_by(ibge) %>% 
  summarise(
    .pred_mean = mean(.pred),
    .pred_median = median(.pred),
    .pred_0.25 = quantile(.pred,0.025),
    .pred_0.975 = quantile(.pred,0.975)
    
  )
my_forecast %>% saveRDS("bootstrap.RDS")

my_forecast %>% 
  mutate(.pred_mean=expm1(.pred_mean))

my_forecast %>% 
  ggplot(aes(x = .pred_mean)) +
  geom_histogram()
exp(22)/1000000

my_forecast <-  readRDS(
  here::here("predict",
  "bootstrap_prediction.RDS")
)
