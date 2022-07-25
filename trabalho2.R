
library(ggcorrplot)
library(GGally)
library(dplyr)
library(naniar)
library(tidyverse)
library(ggplot2)
library(BBmisc)

#Trabalho 2 - Pré-processamento

#Carregar dados do arquivo
load("~/data/flight/bfd.rda")

#Substitui valores N/A, Not Informed nas colunas, para NA
#Essa limpeza e necessaria para possibilitar a verificacao de atributos vazios
bfd <- bfd %>% dplyr::na_if("N/A")
bfd <- bfd %>% dplyr::na_if("Not Informed")

#Criar data frame  voos e receber dados do arquivo
voos <- data.frame(bfd) 

#limpar bfd da memória
rm(bfd)

#Selecionar atributos que não apresentem valores constantes , para analise da chegada dos voos
voos <- select(voos,airline_icao , linetype_code ,origin_icao,departure_delay,
               ds_depart_day_period,destination_icao,real_arrival_date,arrival_delay,arrival_temperature ,
               arrival_dew_point,arrival_humidity,arrival_visibility,arrival_cloudiness,
               arrival_ceiling,ds_arrival_wind_speed,ds_arrival_wind_direction,ds_arrival_day_period,situation_type,justification_code)

#Separação em conjuntos de voos realiazados e cancelados
voos_cancelados <- subset(voos,situation_type == "CANCELADO")
voos_realizados <- subset(voos,situation_type == "REALIZADO")     

#Visualizar estrutura
str(voos_realizados)

#visualizar sumario dos dados
naniar::miss_var_summary(voos_realizados)

#Eliminar atributos com mais de 30% dos dados faltantes:justification_code 74.9% | arrival_ceiling 38.7%  arrival_cloudiness 35.8%
voos_realizados <- select(voos_realizados,airline_icao , linetype_code ,origin_icao,departure_delay,
                          ds_depart_day_period,destination_icao,real_arrival_date,arrival_delay,arrival_temperature ,
                          arrival_dew_point,arrival_humidity,arrival_visibility,ds_arrival_wind_speed,ds_arrival_wind_direction,ds_arrival_day_period,situation_type)

#visualizar sumario dos dados
summary(voos_realizados)

#Retirando registros NA
voos_realizados<- voos_realizados %>% na.omit() 

#visualizar sumario dos dados
summary(voos_realizados)

#Eliminando registros com umidade acima de 100% 
voos_realizados <- dplyr::filter(voos_realizados,arrival_humidity <= 100)

#adicionando coluna categorica com dados de dhorario da chegada
voos_realizados$chegada <- horario_chegada

#Retirando registros NA
voos_realizados<- voos_realizados %>% na.omit() 

#Retira outliers
Q1 <- quantile(voos_realizados$arrival_temperature, .25)
Q3 <- quantile(voos_realizados$arrival_temperature, .75)
iqr <- IQR(voos_realizados$arrival_temperature)
voos_realizados <- subset(voos_realizados, voos_realizados$arrival_temperature > (Q1 - 1.5*iqr) & voos_realizados$arrival_temperature < (Q3 + 1.5*iqr))


Q1 <- quantile(voos_realizados$arrival_humidity, .25)
Q3 <- quantile(voos_realizados$arrival_humidity, .75)
iqr <- IQR(voos_realizados$arrival_humidity)
voos_realizados <- subset(voos_realizados, voos_realizados$arrival_humidity > (Q1 - 1.5*iqr) & voos_realizados$arrival_humidity < (Q3 + 1.5*iqr))


Q1 <- quantile(voos_realizados$arrival_dew_point, .25)
Q3 <- quantile(voos_realizados$arrival_dew_point, .75)
iqr <- IQR(voos_realizados$arrival_dew_point)
voos_realizados <- subset(voos_realizados, voos_realizados$arrival_dew_point > (Q1 - 1.5*iqr) & voos_realizados$arrival_dew_point < (Q3 + 1.5*iqr))

#Problemas ao retirar outlier visibility
#Q1 <- quantile(voos_realizados$arrival_visibility, .25)
#Q3 <- quantile(voos_realizados$arrival_visibility, .75)
#iqr <- IQR(voos_realizados$arrival_visibility)
#voos_realizados <- subset(voos_realizados, voos_realizados$arrival_visibility > (Q1 - 1.5*iqr) & voos_realizados$arrival_visibility < (Q3 + 1.5*iqr))

Q1 <- quantile(voos_realizados$arrival_delay, .25)
Q3 <- quantile(voos_realizados$arrival_delay, .75)
iqr <- IQR(voos_realizados$arrival_delay)
voos_realizados <- subset(voos_realizados, voos_realizados$arrival_delay > (Q1 - 1.5*iqr) & voos_realizados$arrival_delay < (Q3 + 1.5*iqr))

Q1 <- quantile(voos_realizados$departure_delay, .25)
Q3 <- quantile(voos_realizados$departure_delay, .75)
iqr <- IQR(voos_realizados$departure_delay)
voos_realizados <- subset(voos_realizados, voos_realizados$departure_delay > (Q1 - 1.5*iqr) & voos_realizados$departure_delay < (Q3 + 1.5*iqr))

#visualizar sumario dos dados
summary(voos_realizados)

#Criar fator para chegada do vôo
horario_chegada=cut(voos_realizados$arrival_delay,breaks=c(-40,-5,5,40), ordered=TRUE)
levels(horario_chegada) <- c("antecipado", "no horário", "atrasado")

#adicionando coluna categorica com dados de dhorario da chegada
voos_realizados$chegada <- horario_chegada


#Voos chegada somente dados numericos
voos_realizados_numeric <- select(voos_realizados,departure_delay,arrival_delay,arrival_temperature ,
                                  arrival_dew_point,arrival_humidity,arrival_visibility)

#Dados normalizados
voos_realizados_norm_numeric  <- normalize(  voos_realizados_numeric,  method = "range",  range = c(0, 1),  margin = 1L,  on.constant = "quiet")
voos_realizados_norm <- normalize(  voos_realizados,  method = "range",  range = c(0, 1),  margin = 1L,  on.constant = "quiet")


#Slice no conjunto para gerar grafico de correlacao, limite para gerar grafico ~ 65000 registros
voos_realizados_norm_numeric_reduzido <- voos_realizados_norm_numeric %>% slice_sample(n=65000)

# Criando matriz de correlação
matriz_corr_norm<- round(cor(voos_realizados_norm_numeric_reduzido),1)

#Criando gráfico a partir da matriz de correlação
ggcorrplot(matriz_corr_norm, method = "circle", lab = TRUE, lab_size = 2.5,
           hc.order = TRUE, type = "lower",legend.title = "Grau de correlação",
           outline.color = "white", aes(x=horario_chegada)) 

#Grafico de correlação, dispersão e histograma
ggpairs(voos_realizados_norm_numeric)

ggplot(data=voos_realizados_norm) + geom_bar(mapping = aes(x=horario_chegada)) + facet_wrap(~ linetype_code, nrow = 2)

#grafico de dispersao relacionando ponto de orvalho e visibilidade para cada tipo de voo
ggplot(voos_realizados_norm, aes(y = arrival_visibility, x = arrival_dew_point ,shape = horario_chegada, color = horario_chegada)) + geom_point(size = 3, alpha = .7) +
  xlab("Ponto de orvalho") + ylab("Temperatura") + facet_wrap(~ linetype_code, nrow = 2)



