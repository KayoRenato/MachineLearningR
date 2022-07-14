# Problema de Negocio: Prever as notas dos alunos com base em diversas métricas
# https://archive.ics.uci.edu/ml/datasets/Student+Performance
# Dataset com dados de estudantes
# Vamos prever a nota final (grade) dos alunos

# Carregando o dataset
?read.csv2
df <- read.csv2('estudantes.csv', stringsAsFactors = T)

# Explorando os dados
head(df)
summary(df)
str(df)
any(is.na(df))

#install.packages("ggplot2")
#install.packages("ggthemes")
#install.packages("dplyr")
library(ggplot2)
library(ggthemes)
library(dplyr)

View(df)

# Selecionar colunas numericas
colNumeric <- sapply(df, is.numeric)
colNumeric


# Filtrar colunas numericas para correlação
dim(df)
dim(df[,colNumeric])

data_cor <- cor(df[, colNumeric])
data_cor


#Visualização de Correlação entre as variáveis númericas
library('corrplot')
library('corrgram')

?corrplot
corrplot(data_cor)
corrplot(data_cor, method = 'number', type = 'lower')

?corrgram
corrgram(df)
str(df)

# Criação de Histograma para entendimento da Variável 'G3'

ggplot(df, aes(x = G3)) +
  geom_histogram(bins = 20, 
                 alpha = .5,
                 fill = 'blue') +
  theme_minimal()


# Treinando e Interpretando o Modelo 
library('caTools')


#Criação amostras aleatórias da 'população'

set.seed(101)
?sample.split # Criar amostras aleatórias informando a proporção
amostra <- sample.split(df$age,  SplitRatio = .7) #usando df$age como um indice
amostra


#  Criando o dados de treino (70%)
treino <- subset(df, amostra == TRUE)

#  Criando o dados de teste (30%)
teste <- subset(df, amostra == FALSE)

# Gerando os modelos
md_1 <- lm(G3 ~ ., treino) # Regressão Linear Multipla (Com todas as variaveis do df)
md_2 <- lm(G3 ~ G2 + G1, treino) # Regressão Linear Multipla (Com todas as variaveis G1 e G2)
md_3 <- lm(G3 ~ absences, treino) # Regressão Linear Simples (Com a variavel absences)
md_4 <- lm(G3 ~ Medu, treino) # Regressão Linear Simples (Com a variavel Medu)

# Interpretando os modelos
summary(md_1) #r^2 - 0.8616
summary(md_2) #r^2 - 0.8211
summary(md_3) #r^2 - 0.000267
summary(md_4) #r^2 - 0.06442


# Visualização dos Modelos e Previsões

res <- residuals(md_1)
res

res <- as.data.frame(res)
res
sum(res)

# Histograma dos Resíduos
ggplot(res, aes(res)) +
  geom_histogram(fill = 'blue',
                 alpha = .5,
                 binwidth = 1)

# Plot Modelo
plot(md_1)

# Fazendo Predições
prevendo_G3 <- predict(md_1, teste)

View(teste)
# Confrontando os valores Previstos com os Observado
conf <- cbind(Previsto = prevendo_G3, Real = teste$G3)
View(conf)

Result <- as.data.frame(conf)
Result
min(Result) # Não existe Nota Negativa (O modelo apresenta erro)

# Tratando valores Negativos
tratar_valor_negativo <- function(x){
  return(ifelse(x< 0, 0, x))
}

Result$Previsto <- sapply(Result$Previsto, tratar_valor_negativo)
Result$Previsto
min(Result$Previsto)


# Calculando o Erro Médio (MSE)
mse <- mean((Result$Real - Result$Previsto)^2)
mse

# Calculando o RSME
rsme <- sqrt(mse)
rsme

# Calculando o R^2
SSE <- sum((Result$Previsto - Result$Real)^2)
SSE

SST <- sum((mean(df$G3) -  Result$Real)^2)
SST

R2 <- 1 - (SSE/SST)
R2
