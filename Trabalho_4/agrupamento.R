options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx12288m")) 

#version 1.5
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myBasic.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPreprocessing.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myClustering.R")

library(factoextra)
library(cluster)
library(fpc)
library(purrr)

#Carrega base de dados de viagens
load("~/mobility-2014-05-02.RData")

#Avalia entropia
eval <- cluster_evaluation(rep(1, nrow(data)), data$cod)
print(eval$entropy)

#Parte 1 - Avalia, limpa e normaliza atributos
summary(data)

#Retira coluna linha - muitos valores com "" e valor único (ordem)
data <- dplyr::select (data, -c (linha, ordem, date, cod))

#Limpa valores infinitos e NA
is.na(data) <- sapply(data, is.infinite)
data <- na.omit(data)

#Avalia e exclui outliers
boxplot(data)
#Detecta outliers
detect_outlier <- function(x) {
  Quantile1 <- quantile(x, probs=.25) #Calcula primeiro quartil
  Quantile3 <- quantile(x, probs=.75) #Calcula terceiro quartil
  IQR <- Quantile3-Quantile1 #Calcula distancia interquartil
  x > Quantile3 + (IQR*1.5) | x < Quantile1 - (IQR*1.5)
}
#Remove outliers
remove_outlier <- function(dataframe, columns=names(dataframe)) {
  for (col in columns) {
    dataframe <- dataframe[!detect_outlier(dataframe[[col]]), ]
  }
  return (dataframe)
}
data <- remove_outlier(data, c('lat', 'long', 'velocidade', 'dist', 'hour','minute'))
boxplot(data)

#Normalizacao
data <- scale(data)

#Seleciona amostra
#data <- data %>% group_by(cod) %>% sample_frac(size=.30)
#data = data[sample(nrow(data), size=0.1*nrow(data)), ]

#Parte 2 - Faz agrupamentos
#Testa valores de K - K-means
set.seed(123)
#Funcao para computar total somas quadradas de within-cluster
wss <- function(k) {
  kmeans(data, k, nstart = 10)$tot.withinss
}
#Calcula e plota wss para k=1 ate k=10
k.values <- 1:10
#Extrai wss para agrupamentos de 2-10
wss_values <- map_dbl(k.values, wss)
plot(k.values, wss_values,type="b", pch = 19, frame = FALSE, xlab="Número de agrupamentos K", ylab="Total somas quadradas within-clusters")

#K-means. K = 4
km <- kmeans(data,4)
#Visualiza Agrupamento K-means
fviz_cluster(km, data=data, geom="point", stand=FALSE, main="kMEANS — CLUSTERING", ellipse.type="norm")

#Testa valores de K
fviz_nbclust(data, clara,  method="wss")#Executar com 1%
#Kmedoid. K = 3
claraa <- clara(data,3)
# Visualize Agrupamento CLARA Clustering
fviz_cluster(claraa, data=data, geom="point", stand=FALSE, main="CLARA — CLUSTERING", ellipse.type="norm")

#Testa valores de eps
dbscan::kNNdistplot(data, k=10)
abline(h = 0.7, lty = 2)
#DBSCAN. MinPts = 10
db <- fpc::dbscan(data,eps = 0.7, MinPts=100)
#Visualiza Agrupamento DBSCAN
fviz_cluster(db, data=data, geom="point", stand=FALSE, show.clust.cent=FALSE, main="DBSCAN — CLUSTERING", ellipse.type="norm")
