# https://parsnip.tidymodels.org/reference/pls.html
# partial least squares


library(tidymodels)
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

# Crie uma receita para o pré-processamento dos dados

### initial split------------------
set.seed(458)
my_df_split <- initial_split(my_df, prop = 0.75,
                             strata = cat_pop)

my_recipe <-
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

my_spec <-
  linear_reg(
    penalty = tune(),
    mixture = tune()
  ) %>%
  set_mode("pls") %>%
  set_engine("pls")


modelo_pls <- 
  linear_reg(mode = "pls", penalty = 0.1) %>%
  set_engine("pls") %>%
  fit(y ~ ., data = juice(preprocess(my_recipe)))

dados_teste <- rsample::validation_split(dados, prop = 0.2)
previsoes <- predict(modelo_pls, new_data = juice(preprocess(receita, dados_teste)))


rmse <- rmse(previsoes, truth = dados_teste$y)


