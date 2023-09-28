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
  filter(deposito > 0) %>% 
  transmute(
    deposito = log1p(deposito),
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

my_recipe <-
  recipe(formula = deposito ~ ., data = training(my_df_split)) %>%
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

my_recipe %>% prep() %>% bake(
  my_df_split) %>% glimpse()

##spec-------------------
my_spec <-
  linear_reg(
    penalty = tune(),
    mixture = tune()
  ) %>%
  set_mode("regression") %>%
  set_engine("glmnet")


##workflow----------------------
my_workflow <-
  workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_spec)
my_workflow

## tune grid------------------
set.seed(7785)

my_grid <- grid_latin_hypercube(
  penalty(),
  mixture(),
  size = 20)

my_grid
doParallel::registerDoParallel()
my_boots <- bootstraps(training(my_df_split), times = 15)
my_tune <-
  tune_grid(my_workflow,
            my_boots,
            grid = my_grid,
            metrics = 
              metric_set(
                rsq,
                mape, 
                rmse#, # traditional
                # huber_loss, # menos sensivel a outliers ao rmse
                # ccc, # Concordance correlation coefficient sensível a outleirs
                # iic,  # mede a correlação ideal
                # rpiq,rpd # medem consistency/correlation não acurácia
              )
  )
my_tune
autoplot(my_tune)
### show best ------------------------
show_best(my_tune,metric = "mape")
# best = penalty 0.0320 mixture = 0.156
show_best(my_tune,metric = "rmse")
show_best(my_tune,metric = "rsq")
## finalize workflow---------------
final_my <- my_workflow %>%
  finalize_workflow(select_best(my_tune,metric = "rsq"))

## last fit----------------------
my_df_fit <- last_fit(final_my, my_df_split)
my_df_fit_v <- fit(final_my, testing(my_df_split))
#saveRDS(my_df_fit,here::here("artifacts","wkflw-my.RDS"))

collect_metrics(my_df_fit,
                metrics = 
                  metric_set(rsq,
                             mape, 
                             rmse))

predict(
  my_df_fit$.workflow[[1]],
  slice_tail(testing(my_df_split),n=20))
predict(
  my_df_fit$.workflow[[1]],
  slice_head(testing(my_df_split),n=20))

### fforecast ----------------
data_to_forecast <- 
  my_df_complete %>% 
  mutate(
    qtd_ag = ifelse(qtd_ag == 0,1,qtd_ag),
    index = ifelse(index == 0,max(my_df$index),index),
    idade_cat = ifelse(idade_cat=="0", "até 1 ano",idade_cat),
    anomes = ifelse(anomes == 0,max(my_df$anomes),anomes)
  )

data_to_forecast %>% View()
my_df %>% glimpse()
predict(
  my_df_fit$.workflow[[1]],
  data_to_forecast) %>% 
  cbind(data_to_forecast) %>% 
  tibble() %>% 
  mutate(.pred = expm1(.pred)) %>% 
  saveRDS("dep_simplest_forecast.RDS")

predict(
  my_df_fit$.workflow[[1]],
  my_df_complete) %>% 
  cbind(my_df_complete) %>%    
  tibble() %>%
  mutate(
    .pred = expm1(.pred)) %>% 
  saveRDS("dep_simplest_predict.RDS")

## understand the model--------------------

library(vip)
my_df_fit %>% 
  extract_fit_parsnip() %>% 
  vip(15)
# imp_spec <- my_spec %>%
#   finalize_model(select_best(my_tune)) %>%
#   set_engine("ranger", importance = "permutation")
# 
# vip_obj <- 
# workflow() %>%
#   add_recipe(my_recipe) %>%
#   add_model(imp_spec) %>%
#   fit(training(my_df_split)) %>%
#   extract_fit_parsnip() 
# vip_obj %>% 
#   vip(aesthetics = list(alpha = 0.8, fill = "midnightblue"))

library(DALEXtra)
final_fitted <- my_df_fit %>% extract_workflow() 
predict(final_fitted, my_df[10:12, ])

my_explainer <- explain_tidymodels(
  final_fitted,
  data = dplyr::select(testing(my_df_split),
                       -deposito),
  y = dplyr::select(training(my_df_split),
                    deposito),
  verbose = FALSE
)
my_explainer
pdp_time <- model_profile(
  my_explainer,
  variables = "loan_amnt",
  N = NULL,
  groups = "term"
)


as_tibble(pdp_time$agr_profiles) %>%
  mutate(`_label_` = str_remove(`_label_`, "workflow_")) %>%
  ggplot(aes(`_x_`, `_yhat_`, color = `_label_`)) +
  geom_line(size = 1.2, alpha = 0.8) +
  labs(
    x = "Time to complete track",
    y = "Predicted probability of shortcut",
    color = NULL,
    title = "Partial dependence plot for Mario Kart world records",
    subtitle = "Predictions from a decision my model"
  )

# 
# 
# ## save the model------------------
# library(vetiver)
# v <- my_df_fit %>%
#   extract_workflow() %>%
#   vetiver_model(model_name = "my-v1")
# v
# library(pins)
# board <- board_temp(versioned = TRUE)
# board %>% vetiver_pin_write(v)
# vetiver_write_plumber(board, "credit-risk-my", rsconnect = FALSE)
# vetiver_write_docker(v)
# 