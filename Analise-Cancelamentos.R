# version 1.2
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myBasic.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphic.R")
library(GGally)

#Carrega base de dados de viagens
load("~/Trabalho_1/bfd.rda")

#write.csv(bfd, "bfd.csv", sep = ",")

#Definicoes de cores
colors <- brewer.pal(11, 'Paired')
font <- theme(text = element_text(size=16))

#Seleciona as colunas de interesse.
#Retira colunas identificadoras (id voo, linha aerea, aeroporto, jutificativa etc)
#Retira colunas de expectativa/real de saida/chegada nao foram consideradas, uma vez que ha uma coluna derivada (atraso)
#Retira colunas pressao e cloudiness por terem sempre o mesmo valor (1015 e NA, respectivamente)
bfd <- dplyr::select(bfd, situation_type, departure_delay, depart_temperature, depart_dew_point, depart_humidity, 
                     depart_visibility, depart_ceiling, depart_wind_speed, depart_wind_direction, arrival_delay, 
                     arrival_temperature, arrival_dew_point, arrival_humidity, arrival_visibility, arrival_ceiling, 
                     arrival_wind_speed, arrival_wind_direction)

colunas <- colnames(bfd)[-1]

#Limpa valores infinitos e NA. Estava dando o seguinte erro: Error in hclust(as.dist(1 - corr), method = hclust.method): NA/NaN/Inf in foreign function call (arg 10)
is.na(bfd) <- sapply(bfd, is.infinite)
bfd[is.na(bfd)] <- 0

bfd <- bfd[!is.na(bfd$depart_ceiling),]
bfd <- bfd[!is.na(bfd$depart_visibility),]
bfd <- bfd[!is.na(bfd$depart_wind_direction),]

#Limpa valores com erro de temperatura
lo    <- 0
up    <- 100
bfd <- subset(bfd, bfd$depart_humidity >= lo & bfd$depart_humidity <= up)

#Plota distribuicao de densidade por situacao
for (col in colunas) {
  grf <- plot.density.class(bfd %>% dplyr::select(situation_type, col), class_label="situation_type", label_x = col, color=colors[c(2:3)])
  plot(grf)
}

#Analisa correlacao das colunas
plot.correlation(bfd %>% dplyr::select(colunas))

#Nova selecao apos analises graficas
bfd <- select(bfd, situation_type, depart_temperature, depart_humidity, depart_visibility, 
              depart_ceiling, depart_wind_direction, arrival_temperature, arrival_dew_point, 
              arrival_humidity, arrival_visibility, arrival_ceiling, arrival_wind_direction)
colunas <- colnames(bfd)[-1]

#Plota box-plot por situacao
for (col in colunas) {
  grf <- plot.boxplot.class(bfd %>% dplyr::select(situation_type, col), class_label="situation_type", label_x = col, color=colors[c(2:3)]) 
  plot(grf)
}

#Nova selecao apos analises graficas
bfd <- select(bfd, situation_type, arrival_dew_point, arrival_wind_direction, depart_wind_direction, 
              depart_temperature, arrival_temperature)
colunas <- colnames(bfd)[-1]

grf <- ggparcoord(data = bfd, columns = c(2:ncol(bfd)), groupColumn = "situation_type") + theme_bw(base_size = 10) + scale_color_manual(values=colors[c(2:3)]) + facet_wrap(~ situation_type)
plot(grf)

#Plota matriz de dispersao
plot.size(12, 12)
grf <- plot.pair(data=bfd, cnames=colunas, title="bfd", clabel='situation_type', colors=colors[c(2:3)])
grf

#Plota matriz de dispersao avancada
grf <- plot.pair.adv(data=bfd, cnames=colunas, title="bfd", clabel='situation_type', colors=colors[c(2:3)])
grf

#Plota Chernoff faces
set.seed(1)
sample_rows = sample(1:nrow(bfd), 25)

isample = bfd[sample_rows,]
labels = as.character(rownames(isample))
isample$situation_type <- NULL

loadlibrary("aplpack")

plot.size(12, 12)
faces(isample, labels = labels, print.info=F, cex=1)
