# https://parsnip.tidymodels.org/reference/pls.html
# partial least squares


library(tidymodels)
library(parsnip)
library(recipes)
library(rsample)
library(yardstick)

dados <- read.csv("seuarquivo.csv")  # Substitua pelo caminho para o seu arquivo de dados

# Crie uma receita para o pré-processamento dos dados
receita <- recipe(y ~ ., data = dados) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

modelo_pls <- linear_reg(mode = "pls", penalty = 0.1) %>%
  set_engine("pls") %>%
  fit(y ~ ., data = juice(preprocess(receita)))

dados_teste <- rsample::validation_split(dados, prop = 0.2)
previsoes <- predict(modelo_pls, new_data = juice(preprocess(receita, dados_teste)))
rmse <- rmse(previsoes, truth = dados_teste$y)


