#Analise exploratoria sobre dados de chegada do voos
#Carregando base de dados 
load('/home/data/flight/bfd.rda')

#Particionando os dados de voos em realizados e cancelados
voos_realizados <- subset(bfd,situation_type == "REALIZADO")
voos_cancelados <- subset(bfd,situation_type == "CANCELADO")

#Analise da estrutura dos dados pra identificar categoricos e numericos
str(voos_realizados)
#Sumario com informacao estatistica dos voos
summary(voos_realizados)

#Criacao de subconjuntos 
voos_realizados_categoria<- select(voos_realizados_categoria,linetype_code, arrival_delay, arrival_temperature, arrival_dew_point, arrival_humidity, arrival_visibility, arrival_ceiling, arrival_wind_speed,arrival_wind_direction,ds_arrival_day_period)
voos_realizados_companhia <- select(voos_realizados,airline_icao, arrival_delay, arrival_temperature, arrival_dew_point, arrival_humidity, arrival_visibility, arrival_ceiling,arrival_wind_speed,arrival_wind_direction)

#Retirada de outliers e de valores NA

Q1 <- quantile(voos_realizados_categoria$arrival_humidity, .25)
Q3 <- quantile(voos_realizados_categoria$arrival_humidity, .75)
iqr <- IQR(voos_realizados_categoria$arrival_humidity)
voos_realizados_categoria <- subset(voos_realizados_categoria, voos_realizados_categoria$arrival_humidity > (Q1 - 1.5*IQR) & voos_realizados_categoria$arrival_humidity < (Q3 + 1.5*IQR))
voos_realizados_categoria <- subset(voos_realizados_categoria, voos_realizados_categoria$arrival_humidity > (Q1 - 1.5*iqr) & voos_realizados_categoria$arrival_humidity < (Q3 + 1.5*iqr))
Q1 <- quantile(voos_realizados_categoria$arrival_temperature, .25)
Q3 <- quantile(voos_realizados_categoria$arrival_temperature, .75)
iqr <- IQR(voos_realizados_categoria$arrival_temperature)
voos_realizados_categoria <- subset(voos_realizados_categoria, voos_realizados_categoria$arrival_temperature > (Q1 - 1.5*iqr) & voos_realizados_categoria$arrival_temperature < (Q3 + 1.5*iqr))
Q1 <- quantile(voos_realizados_categoria$arrival_dew_point, .25)
Q3 <- quantile(voos_realizados_categoria$arrival_dew_point, .75)
iqr <- IQR(voos_realizados_categoria$arrival_dew_point)
voos_realizados_categoria <- subset(voos_realizados_categoria, voos_realizados_categoria$arrival_dew_point > (Q1 - 1.5*iqr) & voos_realizados_categoria$arrival_dew_point < (Q3 + 1.5*iqr))
Q1 <- quantile(voos_realizados_categoria$arrival_visibility, .25)
Q1 <- quantile(voos_realizados_categoria$arrival_ceiling, .25)
Q1 <- quantile(voos_realizados_categoria$arrival_wind_speed, .25)
Q3 <- quantile(voos_realizados_categoria$arrival_wind_speed, .75)
iqr <- IQR(voos_realizados_categoria$arrival_wind_speed)
voos_realizados_categoria <- subset(voos_realizados_categoria, voos_realizados_categoria$arrival_wind_speed > (Q1 - 1.5*iqr) & voos_realizados_categoria$arrival_wind_speed < (Q3 + 1.5*iqr))
Q1 <- quantile(voos_realizados_categoria$arrival_wind_direction, .25)
Q1 <- quantile(voos_realizados_categoria$arrival_visibility, .25,na.rm= TRUE)
Q3 <- quantile(voos_realizados_categoria$arrival_visibility, .75,na.rm= TRUE)
iqr <- IQR(voos_realizados_categoria$arrival_visibility)
iqr <- IQR(voos_realizados_categoria$arrival_visibility,na.rm = TRUE)
Q1 <- quantile(voos_realizados_categoria$arrival_ceiling, .25,na.rm= TRUE)
Q3 <- quantile(voos_realizados_categoria$arrival_ceiling, .75,na.rm= TRUE)
iqr <- IQR(voos_realizados_categoria$arrival_ceiling,na.rm = TRUE)
voos_realizados_categoria <- subset(voos_realizados_categoria, voos_realizados_categoria$arrival_ceiling > (Q1 - 1.5*iqr) & voos_realizados_categoria$arrival_ceiling < (Q3 + 1.5*iqr))
Q1 <- quantile(voos_realizados_categoria$arrival_wind_direction, .25,na.rm= TRUE)
Q3 <- quantile(voos_realizados_categoria$arrival_wind_direction, .75,na.rm= TRUE)
iqr <- IQR(voos_realizados_categoria$arrival_wind_direction,na.rm = TRUE)
voos_realizados_categoria <- subset(voos_realizados_categoria, voos_realizados_categoria$arrival_wind_direction > (Q1 - 1.5*iqr) & voos_realizados_categoria$arrival_wind_direction < (Q3 + 1.5*iqr))

#criacao de boxplot sobre dados de atraso, separadamente para cada periodo do dia, usando variavel categorica ds_arrival_day_period dentro de um facet_wrap
ggplot(data=voos_realizados_categoria)+geom_boxplot(mapping = aes(y=arrival_delay))+facet_wrap(~ ds_arrival_day_period,nrow = 2)

#criacao de boxplot sobre dados de atraso na chegada para cada tipo de voo
ggplot(data=voos_realizados_categoria)+geom_boxplot(mapping = aes(y=arrival_delay))+facet_wrap(~ linetype_code,nrow = 2)

#grafico de dispersao relacionando ponto de orvalho e temperatura ambiente para cada tipo de voo
ggplot(voos_realizados, aes(y = arrival_temperature, x = arrival_dew_point ,shape = chegada, color = chegada)) + geom_point(size = 3, alpha = .7) +
xlab("Ponto de orvalho") + ylab("Temperatura") + facet_wrap(~ linetype_code, nrow = 2)

#Criacao de Chernoff faces para analise de companhias aereas, avaliando 300 registros
faces(voos_realizados_categoria[ 1:300,2:9], nr=10, scale = TRUE,main = "Companhias_Aéreas", labels=voos_realizados_companhia$airline_icao)



