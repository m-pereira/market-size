# https://agua.tidymodels.org/articles/auto_ml.html
# automl
library(tidymodels)
library(agua)
library(ggplot2)
theme_set(theme_bw())
h2o_start()

data(concrete)
set.seed(4595)
concrete_split <- initial_split(concrete, strata = compressive_strength)
concrete_train <- training(concrete_split)
concrete_test <- testing(concrete_split)

# run for a maximum of 120 seconds
auto_spec <-
  auto_ml() %>%
  set_engine("h2o", max_runtime_secs = 120, seed = 1) %>%
  set_mode("regression")

normalized_rec <-
  recipe(compressive_strength ~ ., data = concrete_train) %>%
  step_normalize(all_predictors())

auto_wflow <-
  workflow() %>%
  add_model(auto_spec) %>%
  add_recipe(normalized_rec)

auto_fit <- fit(auto_wflow, data = concrete_train)

extract_fit_parsnip(auto_fit)

predict(auto_fit, new_data = concrete_test)

rank_results(auto_fit) %>%
  filter(.metric == "mape") %>%
  arrange(rank)

collect_metrics(auto_fit, summarize = FALSE)

tidy(auto_fit) %>%
  mutate(
    .predictions = map(.model, predict, new_data = head(concrete_test))
  )

auto_fit %>%
  extract_fit_parsnip() %>%
  member_weights() %>%
  unnest(importance) %>%
  filter(type == "scaled_importance") %>%
  ggplot() +
  geom_boxplot(aes(value, algorithm)) +
  scale_x_sqrt() +
  labs(y = NULL, x = "scaled importance", title = "Member importance in stacked ensembles")


# not run 
auto_spec_refit <-
  auto_ml() %>%
  set_engine("h2o", 
             max_runtime_secs = 300, 
             save_data = TRUE,
             keep_cross_validation_predictions = TRUE) %>%
  set_mode("regression")

auto_wflow_refit <-
  workflow() %>%
  add_model(auto_spec_refit) %>%
  add_recipe(normalized_rec)

first_auto <- fit(auto_wflow_refit, data = concrete_train)
# fit another 60 seconds 
second_auto <- refit(first_auto, max_runtime_secs = 60)


