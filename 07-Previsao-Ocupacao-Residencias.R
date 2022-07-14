# Problema de Negocio - Analisando dados das casas de Boston, nos EUA e fazendo previsoes.

# The Boston Housing Dataset
# http://www.cs.toronto.edu/~delve/data/boston/bostonDetail.html

# O modelo deve prever a MEDV (Valor da Mediana de ocupação das casas) utilizando um modelo de rede neural!

# Carregando o pacote MASS
library(MASS)

# Importando os dados do dataset Boston
set.seed(101)
dados <- Boston
head(dados)

# Resumo dos dados
str(dados)
summary(dados)
any(is.na(dados))

# Carregando o pacote para Redes Neurais
#install.packages("neuralnet")
library(neuralnet)


# Pre-Processamento dos Dados

#Normalizando os dados
maxs <- apply(dados, 2, max)
maxs

mins <- apply(dados, 2, min)
mins

dados_normalizados <- as.data.frame(scale(dados, center = mins, scale = maxs - mins))
head(dados_normalizados)

# Criando os subset de Traino e Teste
library('caTools')

split <- sample.split(dados_normalizados$medv, SplitRatio = .7)
split

treino <- subset(dados_normalizados, split == TRUE)
teste <- subset(dados_normalizados, split == FALSE)

# Obtendo o nome das colunas
colNames <- names(treino)
colNames


# Agregando
formula <- as.formula(paste('medv ~', paste(colNames[!colNames %in%'medv'], collapse = ' + ')))
formula

?neuralnet
rede_neural <- neuralnet(formula, data = treino, hidden = c(5,3), linear.output = TRUE)

summary(rede_neural)

# Plotagem da Rede
plot(rede_neural)


# Realizando Previsões
dim(teste)
View(teste)
teste[1:13]

rede_neural_prev <- compute(rede_neural, teste[1:13])
rede_neural_prev


str(rede_neural_prev)

# Revertendo a normalização dos dados previstos resultantes do Teste
previsao <- rede_neural_prev$net.result * (max(dados$medv) - min(dados$medv)) + min(dados$medv)
teste_convert <- (teste$medv) * (max(dados$medv)- min(dados$medv) + min(dados$medv))
teste_convert


# Calculando o MSE
MSE <- sum((teste_convert - previsao)^2)/nrow(teste)
MSE

# Obtendo Erro Previsto
erro.df <- data.frame(teste_convert, previsao)
head(erro.df)

# Plot dos Erros
library(ggplot2)
ggplot(erro.df, aes(x= teste_convert, y=previsao)) +
  geom_point() +
  stat_smooth()

