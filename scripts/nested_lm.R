# https://cran.r-project.org/web/packages/nestedmodels/vignettes/nestedmodels.html
library(tidyverse)
library(tidymodels)
library(nestedmodels)
### carregar os dados
my_df_complete  <-  readRDS(here::here("data","final.RDS")) %>% 
  mutate(cat_pop = case_when(
    pop_est > 500000 ~"acima 500k",
    pop_est > 75000 ~"acima 75k",
    TRUE ~ "abaixo de 75k"
  )) 

my_df <- 
  my_df_complete %>% 
  filter(credito > 0) %>% 
  select(-cod_ibge, -uf,-municipio)

# my_df <- 
#  my_df %>% 
#    select(-mesorregiao,-microrregiao)

### initial split------------------
set.seed(458)
my_df_split <- initial_split(my_df, prop = 0.8,
                             strata =  cat_pop)
my_df_split
## in case you want use bootstrap bootstrap
k_fold <- results <- nested_cv(training(my_df_split), 
                               outside = vfold_cv(repeats = 5), 
                               inside = bootstraps(times = 25))

## recipe-------------------
lm_recipe <-
  recipe(formula = credito ~ ., data = training(my_df_split)) %>%
  step_nzv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_corr(all_numeric_predictors(),threshold = 0.9) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_unknown(all_nominal_predictors()) %>% 
  step_other(all_nominal_predictors(), threshold = 0.1) %>%
  step_dummy(all_nominal_predictors(),
             -cat_pop,
             one_hot = FALSE
  ) %>% 
  step_nest(cat_pop)

lm_recipe %>% prep() %>% 
  bake(my_df_split) %>% glimpse()

lm_recipe %>% prep() %>% 
  bake(my_df_split) %>% ncol()

#lm_recipe %>% prep() %>% bake(my_df_split) %>% View()

##spec-------------------
lm_spec <-
  linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm") %>% 
  nested()
lm_spec
##workflow----------------------
lm_workflow <-
  workflow() %>%
  add_recipe(lm_recipe) %>%
  add_model(lm_spec)
lm_workflow

## fit ---------------
final_lm <- lm_workflow %>%
  fit(training(my_df_split))
final_lm
predict(final_lm, testing(my_df_split))
augment(final_lm, testing(my_df_split))
tidy(final_lm)
