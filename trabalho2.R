library(dplyr)
library(tidyverse)
library(ggplot2)
install.packages("ggcorrplot")
library(ggcorrplot)
library(GGally)


voos <- data.frame(bfd) 
summary(voos)              

#Selecionar atributos que não apresentem valores constantes e os que sejam derivados
voos <- select(voos,airline_icao , linetype_code ,origin_icao,departure_delay,
               depart_temperature, depart_dew_point, depart_humidity,depart_visibility,
               depart_ceiling,ds_depart_wind_speed,ds_depart_wind_direction,
               ds_depart_day_period,destination_icao,arrival_delay,arrival_temperature ,
               arrival_dew_point,arrival_humidity,arrival_visibility,arrival_cloudiness,
               arrival_ceiling,ds_arrival_wind_speed,ds_arrival_wind_direction,ds_arrival_day_period,situation_type)

#Separação em conjuntos de voos realiazados e cancelados
voos_cancelados <- subset(voos,situation_type == "CANCELADO")
voos_realizados <- subset(voos,situation_type == "REALIZADO")     

#Voos realizados para análise da chegada
voos_realizados_chegada <- select(voos_realizados,airline_icao , linetype_code ,origin_icao,departure_delay,
          destination_icao,arrival_delay,arrival_temperature ,
          arrival_dew_point,arrival_humidity,arrival_visibility,arrival_cloudiness,
          arrival_ceiling,ds_arrival_wind_speed,ds_arrival_wind_direction,ds_arrival_day_period,situation_type)

#Visualizar estrutura
str(voos_realizados_chegada)

#Voos chegada somente dados numericos
voos_realizados_chegada_numeric <- select(voos_realizados,departure_delay,arrival_delay,arrival_temperature ,
                                  arrival_dew_point,arrival_humidity,arrival_visibility,
                                  arrival_ceiling)

#Avaliar se o atraso do vôo na partida impacta no atraso da chegada
voos_realizados_chegada_numeric %>% ggplot(aes(x=departure_delay,y=arrival_delay)) + geom_point()

#Verificação de correlação entre atrasos na saida e chegada correlação positiva = 0.9998964 
cor(voos_realizados_chegada_numeric$departure_delay,voos_realizados_chegada_numeric$arrival_delay)

#Analise de correlação entre todas as variaveis
ggcorrplot(voos_realizados_chegada_numeric, method = "circle", hc.order = TRUE,type = "lower", lab = TRUE, lab_size = 2.5)

# identificar quantidade de valores não informados por cada coluna
colSums(is.na(voos_realizados_chegada_numeric))

#Slice no conjunto para gerar grafico de correlacao, limite para gerar grafico ~ 65000 registros
voos_chegada_numeric_reduzido <- voos_realizados_chegada_numeric %>% slice_sample(n=65000)

#Analise de correlação entre todas as variaveis
ggcorrplot(voos_chegada_numeric_reduzido, method = "circle", lab = TRUE, lab_size = 2.5)

#Retirando NA
voos_chegada_numeric_reduzido_limpo <- voos_chegada_numeric_reduzido %>% na.omit() 

# Criando matriz de correlação
matriz_corr_cheg_limpo <- round(cor(voos_chegada_numeric_reduzido_limpo),1)


#Criando gráfico a partir da matriz de correlação
ggcorrplot(matriz_corr_cheg_limpo, method = "circle", lab = TRUE, lab_size = 2.5,
           hc.order = TRUE, type = "lower",legend.title = "Grau de correlação",
           outline.color = "white", col=brewer.pal(n=8, name="RdYlBu"))

#Grafico de correlação, dispersão e histograma
ggpairs(voos_chegada_numeric_reduzido_limpo)

#Criar fator para chegada do vôo
horario_chegada=cut(voos_realizados_chegada$arrival_delay,breaks=c(-40,-5,5,40), ordered=TRUE)
levels(horario_chegada) <- c("antecipado", "no horário", "atrasado")

#grafico de dispersao relacionando ponto de orvalho e temperatura ambiente para cada tipo de voo
ggplot(voos_realizados_chegada, aes(y = arrival_temperature, x = arrival_dew_point ,shape = horario_chegada, color = horario_chegada)) + geom_point(size = 3, alpha = .7) +
  xlab("Ponto de orvalho") + ylab("Temperatura") + facet_wrap(~ linetype_code, nrow = 2)

