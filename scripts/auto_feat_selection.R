## auto feature selection
# Carregue os pacotes necessários
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

my_df <- 
  my_df_complete %>% 
  filter(credito > 0) %>% 
  select(-cod_ibge, -uf,-municipio,-nome_mun,
         -min_idade_ag, -deposito) %>% 
  select(
    #regiao_metropolitana,
    credito,
    pib_prc_corr,
    pib_pc,
    vab_agro,
    prop_vab_ag,
    prop_vab_ind,
    valor_area_temp,
    bovino,
    total_frota,
    index,
    anomes,
    qtd_ag,
    total_auto_pc,
    populacao,
    vab_ind,
    vab_serv,
    valor_area_perm,
    area_urbanizada
  )
dados <- my_df
minimo <- abs(dados$index %>% min())
dados <- 
dados %>% 
  mutate(index = index + minimo) 
dados %>% glimpse()
dados <- 
dados %>% 
  mutate_all(~.+2) %>% 
  mutate_all(~log(.))
dados <- 
dados %>% bind_cols((my_df_complete %>%  
                       filter(credito > 0) %>% 
                       select(latitude,longitude)))
# Carregue seus dados (substitua 'dados' pelos seus dados)
# dados <- data.frame(
#   y = c(1.1, 2.0, 3.1, 3.8, 5.2),
#   x1 = c(2.0, 3.0, 4.0, 5.0, 6.0),
#   x2 = c(1.0, 1.5, 2.0, 2.5, 3.0)
# )

# Crie todas as combinações possíveis de variáveis independentes
variaveis <- colnames(dados)[-1]  # Exclua a variável de resposta
combinacoes <- combn(variaveis, 7)  # Todas as combinações de 1 variável
combinacoes %>% length()
for (i in 2:length(variaveis)) {
  combinacoes <- cbind(combinacoes, combn(variaveis, i))  # Combinações de i variáveis
}
combinacoes <- lapply(1:ncol(combinacoes), function(i) combinacoes[, i])

# Inicialize variáveis para acompanhar os melhores modelos
melhor_modelo <- NULL
melhor_avaliacao <- Inf

# Ajuste modelos lineares para todas as combinações e avalie
for (combo in combinacoes) {
  formula <- as.formula(paste("credito ~", paste(combo, collapse = "+")))
  modelo <- lm(formula, data = dados)
  
  # Avalie o modelo (por exemplo, usando o RMSE)
  previsoes <- predict(modelo, newdata = dados)
  rmse <- sqrt(mean((dados$credito - previsoes)^2))
  
  # Se a avaliação for melhor do que o melhor modelo anterior, atualize
  if (rmse < melhor_avaliacao) {
    melhor_modelo <- modelo
    melhor_avaliacao <- rmse
  }
}

# Visualize o melhor modelo e seus coeficientes
summary(melhor_modelo)



