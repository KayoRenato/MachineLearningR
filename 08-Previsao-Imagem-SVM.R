# Problema de Negocio - Prever o caracter a partir do dataset fornecido usando um modelo SVM

## Explorando e preparando os dados
letters <- read.csv("letterdata.csv", stringsAsFactors = T)
str(letters)
head(letters)

# Criando dados de treino e dados de teste
letters_treino <- letters[1:16000, ]
letters_teste  <- letters[16001:20000, ]

# Treinando o Modelo
library(kernlab)

# Criando o modelo com o kernel vanilladot
?ksvm
letter_classifier <- ksvm(letter ~ ., data = letters_treino, kernel = "vanilladot")

# Visualizar o resultado do modelo
letter_classifier


# Avaliando a performance do modelo
letter_predictons <- predict(letter_classifier, letters_teste)
letter_predictons
table(letter_predictons, letters_teste$letter)

# Criando um vetor de TRUE/FALSE para indicar previsoes Corretas / Incorretas
Agreement <- letter_predictons == letters_teste$letter
table(Agreement)
round(prop.table(table(Agreement)), 2)


# Otimizando o Modelo 
set.seed(12345)

# Recriando o modelo com um novo tipo de Kernel
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_treino, kernel = "rbfdot")

# Visualizar o resultado do modelo
letter_classifier_rbf

# Avaliando a performance do modelo
letter_predictons <- predict(letter_classifier_rbf, letters_teste)
letter_predictons
table(letter_predictons, letters_teste$letter)

# Criando um vetor de TRUE/FALSE para indicar previsoes Corretas / Incorretas
Agreement <- letter_predictons == letters_teste$letter
table(Agreement)
round(prop.table(table(Agreement)), 2)

# O modelo performou melhor como Kernel 'rbfdot'(Radial Basis kernel "Gaussian")