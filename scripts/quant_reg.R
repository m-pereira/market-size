# Carregue os pacotes necessários
library(tidymodels)

# Crie um tibble (ou dataframe) com seus dados
dados <- data.frame(x = rnorm(100), y = rnorm(100))

# Crie um objeto de especificação do modelo
modelo_quantil <- quant_reg() %>%
  set_engine("quantreg")

# Crie um objeto de receita para pré-processar os dados, se necessário
receita <- recipe(y ~ x, data = dados) %>%
  step_center(x) %>%
  step_scale(x)

# Crie um modelo de regressão quantílica
modelo <- workflow() %>%
  add_recipe(receita) %>%
  add_model(modelo_quantil)

# Ajuste o modelo aos seus dados
modelo_fit <- fit(modelo, data = dados)

# Realize previsões para um valor específico do quantil (por exemplo, o 0,5 representa a mediana)
previsoes <- predict(modelo_fit, new_data = data.frame(x = 0), type = "quantile", quantiles = 0.5)

# Visualize as previsões
print(previsoes)