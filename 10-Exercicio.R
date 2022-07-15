# Existem diversos pacotes para árvores de decisão em R. Usaremos aqui o rpart.
#install.packages('rpart')
library(rpart)

# Vamos utilizar um dataset que é disponibilizado junto com o pacote rpart
str(kyphosis)
head(kyphosis)
View(kyphosis)
?kyphosis

# Exercício 1 - Depois de explorar o dataset, crie um modelo de árvore de decisão
?rpart
arvore <- rpart(Kyphosis ~ . , method = 'class', data = kyphosis)
class(arvore)
arvore

# Para examinar o resultado de uma árvore de decisao, existem diversas funcões, mas você pode usar printcp()
printcp(arvore)

# Visualizando a ávore (execute uma função para o plot e outra para o texto no plot)
# Utilize o zoom para visualizar melhor o gráfico
plot(arvore, uniform = TRUE, main = "Arvore de Decisao em R")
text(arvore, use.n = TRUE, all = TRUE)

# Este outro pacote faz a visualizaco ficar mais legivel
install.packages('rpart.plot')
library(rpart.plot)
prp(arvore)


# Usaremos o dataset iris neste exemplo
# O dataset iris possui observações de 3 espécies de flores (Iris setosa, Iris virginica e Iris versicolor)
# Para cada flor, 4 medidas sao usadas: 
# comprimento (length) e largura (width) do caule (sepal) e comprimento e largura da petala (petal)
library(datasets)
head(iris)
View(iris)

# Análise exploratória de dados com ggplot2
library(ggplot2)

# Veja que os dados claramente possui grupos com característcas similares
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point(size = 3)

# Agora usarmeos o K-Means para tentar agrupar os dados em clusters
set.seed(101)
help(kmeans)

# Exercício 1 - Usar a função kmeans(), para criar um modelo de clustering (aprendizagem não supervisionada). 
# Use a documentação, para fazer sua pesquisa.
# Neste caso, ja sabemos quantos grupos (clusters) existem em nossos dados (3)
# O dataset iris possui 5 colunas, mas estamos usando as 4 primeiras
irisCluster <- kmeans(iris[, 1:4], 3, nstart = 20)
irisCluster$

# Obtendo informação sobre os clusters
# Foram criados 3 clusters: cluster 1, 2 e 3
# Perceba que apesar o algoritmo ter feito a divisão dos dados em clusters, houve problema em dividir alguns dos dados, 
# que apesar de terem caracteristicas diferentes, ficaram no mesmo cluster
table(irisCluster$cluster, iris$Species)
irisCluster

# Visualizando os clusters
#install.packages("cluster")
library(cluster)
help(clusplot)

# Plot
clusplot(iris, irisCluster$cluster, color = TRUE, shade = TRUE, labels = 0, lines = 0 )





