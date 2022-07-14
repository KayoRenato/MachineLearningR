# Problema de Negocio - Prever o caracter a partir do dataset fornecido usando um modelo SVM

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




