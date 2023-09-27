# https://github.com/spsanderson/tidyAML
library(tidyAML)
library(recipes)
library(dplyr)
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
minimo <- abs(my_df$index %>% min())
my_df <- my_df %>% 
  mutate(index = index + minimo) 
### splits ---------------
set.seed(458)
my_df_split <- initial_split(my_df, prop = 0.75,
                             strata = cat_pop)
### recipe ----------
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

### fast regression ---------

frt_tbl <- fast_regression(
  .data = my_df, 
  .rec_obj = my_recipe, 
  .parsnip_eng = c("lm","glm"),
  .parsnip_fns = "linear_reg",
  .split_type = "initial_split",
  .split_args = list(
    prop = 0.75
  )
)

glimpse(frt_tbl)

frt_tbl$pred_wflw
frt_tbl$fitted_wflw
