options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx12288m")) 

#version 1.5
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myBasic.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPreprocessing.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myClustering.R")

library(cluster)
library(purrr)

#Carrega base de dados de viagens
load("~/mobility-2014-05-02.RData")

#Avalia entropia
eval <- cluster_evaluation(rep(1, nrow(data)), data$cod)
print(eval$entropy)

#Parte 1 - Avalia, limpa e normaliza atributos
summary(data)

#Retira coluna linha - muitos valores com "" e valor único (ordem)
data <- dplyr::select (data, -c (linha, ordem, date))

#Limpa valores infinitos e NA
is.na(data) <- sapply(data, is.infinite)
data[is.na(data)] <- 0
data <- na.omit(data)

#Normalizacao
norm <- zscore()
norm <- fit(norm, data)
data <- transform(norm, data)

#Parte 2 - Faz agrupamentos
#Testa valores de K
#set.seed(123)
#Funcao para computar total somas quadradas de within-cluster
#wss <- function(k) {
#  kmeans(data, k, nstart = 10)$tot.withinss
#}
#Calcula e plota wss para k=1 ate k=15
#k.values <- 1:15
#Extrai wss para agrupamentos de 2-15
#wss_values <- map_dbl(k.values, wss)
#plot(k.values, wss_values,type="b", pch = 19, frame = FALSE, xlab="Número de agrupamentos K", ylab="Total somas quadradas within-clusters")

#Funcao geral para teste dos metodo de agrupamento
test_clustering <- function(model, data, attribute, opt=FALSE) {
  print(class(model)[1])
  if (opt) 
    model <- optimize(model, data)    
  clu <- fit(model, data)
  print(table(clu))
  eval <- cluster_evaluation(clu, attribute)
  print(eval$entropy)
}

#K-means. K = 3
test_clustering(cluster_kmeans(k=3), data[,2:7], data[1])

#Valor otimo de K-means
test_clustering(cluster_kmeans(NULL), data[,2:7], data[1], TRUE)

# kmedoid; k = 3
test_clustering(cluster_pam(k=3), data[,2:7], data[1])

#Valor otimo de kmedoid
test_clustering(cluster_pam(NULL), data[,2:7], data[1], TRUE)

# dbscan
test_clustering(cluster_dbscan(eps = 0.4, MinPts = 3), data[,2:7], data[1])

#Valor otimo de dbscan
test_clustering(cluster_dbscan(eps = NULL, MinPts = 3), data[,2:7], data[1], TRUE)