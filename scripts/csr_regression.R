# complete subset regression
install.packages("leaps")
library(leaps)


csr_resultados <- regsubsets(y ~ ., data = dados, nvmax = ncol(dados) - 1, method = "exhaustive")


summary(csr_resultados)
melhor_modelo <- lm(y ~ variavel1 + variavel2, data = dados)  # Substitua as variáveis pelo seu modelo selecionado

