## Exercício 1 - Massa de dados aleatória

# Criando a massa de dados (apesar de aleatória, y possui 
# uma relação com os dados de x)
x <- seq(0, 100)
y <- 2 * x + 35

# Imprimindo as variáveis
x
y

# Gerando uma distribuição normal
?rnorm
y1 <- y + rnorm(101, 0, 50)
y1
hist(y1)

# Crie um plot do relacionamento de x e y1


# Crie um modelo de regressão para as duas variáveis x e y1


# Capture os coeficentes


# Fórmula de Regressão
y2 <- a + b*x

# Visualize a linha de regressão


# Simulando outras possíveis linhas de regressão
y3 <- (y2[51]-50*(b-1))+(b-1)*x
y4 <- (y2[51]-50*(b+1))+(b+1)*x
y5 <- (y2[51]-50*(b+2))+(b+2)*x
lines(x,y3,lty=3)
lines(x,y4,lty=3)
lines(x,y5,lty=3)


## Exercício 2 - Pesquisa sobre idade e tempo de reação

# Criando os dados
Idade <- c(9,13,14,21,15,18,20,8,14,23,16,21,10,12,20,
           9,13,5,15,21)

Tempo <- c(17.87,13.75,12.72,6.98,11.01,10.48,10.19,19.11,
           12.72,0.45,10.67,1.59,14.91,14.14,9.40,16.23,
           12.74,20.64,12.34,6.44)

# Crie um Gráfico de Dispersão (ScatterPlot)


# Crie um modelo de regressão


# Calcule a reta de regressão


# Crie o gráfico da reta



# Exercício 3 - Relação entre altura e peso

# Criando os dados
alturas = c(176, 154, 138, 196, 132, 176, 181, 169, 150, 175)
pesos = c(82, 49, 53, 112, 47, 69, 77, 71, 62, 78)

plot(alturas, pesos, pch = 16, cex = 1.3, col = "blue", 
     main = "Altura x Peso", 
     ylab = "Peso Corporal (kg)", 
     xlab = "Altura (cm)")

# Crie o modelo de regressão


# Visualizando o modelo


# Gere a linha de regressão


# Faça as previsões de pesos com base na nova lista de alturas
alturas2 = data.frame(c(179, 152, 134, 197, 131, 178, 185, 162, 155, 172))


# Plot
plot(alturas, pesos, pch = 16, cex = 1.3, 
     col = "blue", 
     main = "Altura x Peso", 
     ylab = "Peso (kg)", 
     xlab = "Altura (cm)")

# Construindo a linha de regressão
abline(lm(pesos ~ alturas)) 

# Obtendo o tamanho de uma das amostras de dados
num <- length(alturas)
num

# Gerando um gráfico com os valores residuais
for (k in 1: num)  
  lines(c(alturas[k], alturas[k]), 
        c(pesos[k], pesos[k]))

# Gerando gráficos com a distribuição dos resíduos
par(mfrow = c(2,2))
plot(modelo)


# Lista de Exercícios Parte 2 - Capítulo 11

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("C:/FCD/BigDataRAzure/Cap12")
getwd()


# Regressão Linear
# Definição do Problema: Prever as notas dos alunos com base em diversas métricas
# https://archive.ics.uci.edu/ml/datasets/Student+Performance
# Dataset com dados de estudantes
# Vamos prever a nota final (grade) dos alunos

# Carregando o dataset
df <- read.csv2('estudantes.csv')

# Explorando os dados
head(df)
summary(df)
str(df)
any(is.na(df))

# install.packages("ggplot2")
# install.packages("ggthemes")
# install.packages("dplyr")
library(ggplot2)
library(ggthemes)
library(dplyr)


# Lista de Exercícios Parte 3 - Capítulo 11

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("C:/FCD/BigDataRAzure/Cap12")
getwd()


# Definindo o Problema: Analisando dados das casas de Boston, nos EUA e fazendo previsoes.

# The Boston Housing Dataset
# http://www.cs.toronto.edu/~delve/data/boston/bostonDetail.html

# Seu modelo deve prever a MEDV (Valor da Mediana de ocupação das casas). Utilize um modelo de rede neural!

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
install.packages("neuralnet")
library(neuralnet)

# Lista de Exercícios Parte 4 - Capítulo 11

# Definindo o Problema: OCR - Optical Character Recognition
# Seu modelo deve prever o caracter a partir do dataset fornecido. Use um modelo SVM

## Explorando e preparando os dados
letters <- read.csv("letterdata.csv")
str(letters)

# Criando dados de treino e dados de teste
letters_treino <- letters[1:16000, ]
letters_teste  <- letters[16001:20000, ]

## Treinando o Modelo
library(kernlab)

# Criando o modelo com o kernel vanilladot
letter_classifier <- ksvm(..........)




