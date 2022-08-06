options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m")) 

#version 1.5
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myBasic.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPreprocessing.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphic.R")

library(naniar) #Para verificar valores faltantes
library(rpart) #Decision Tree
library(rpart.plot) #Decision Tree
loadlibrary("arules") #Padroes frequentes, apriori
loadlibrary("arulesViz") #Padroes frequentes, apriori
loadlibrary("arulesSequences") #Padroes frequentes, apriori

#Carrega base de dados de viagens
load("~/bfd.rda")

#Definicoes de cores
colors <- brewer.pal(11, 'Paired')
font <- theme(text = element_text(size=16))

#Parte 1: Avaliacao atributos
#Verifica situacao atributos
summary(bfd)

#Remove colunas com valores constantes
bfd <- dplyr::select (bfd, -c (depart_cloudiness, depart_visibility, depart_pressure, arrival_pressure, arrival_visibility, origin_name, origin_country, destination_name, destination_country, airline_name, justification_description))

#Remove flight_id, muito especifico
bfd <- dplyr::select (bfd, -c (flight_id))

#Exclui atributos real_depart_date, real_depart_hour, real_arrival_date e real_arrival_hour, porque podem ser calculados somando a data/hora esperadas com o tempo de atraso (correlacao)
bfd <- dplyr::select (bfd, -c (real_depart_date, real_depart_hour, real_arrival_date, real_arrival_hour))

#Exclui atributos expected_arrival_date e expected_arrival_hour. Expectativa de chegada pode ser pertubada por atrasos na saida ou voo.
bfd <- dplyr::select (bfd, -c (expected_arrival_date, expected_arrival_hour))

#Exclui atributos categóricos com numérico correspondente
bfd <- dplyr::select (bfd, -c (ds_depart_wind_speed, ds_depart_wind_direction, ds_depart_day_period, ds_arrival_wind_speed, ds_arrival_wind_direction, ds_arrival_day_period))

#Escopo de analise sera apenas atrasos. Retira todos os casos de antecipacao
bfd <- subset(bfd, bfd$arrival_delay > -5)

#Substitui valores N/A, Not Informed nas colunas, para NA
#Essa limpeza e necessaria para possibilitar a verificacao de atributos vazios
bfd <- bfd %>% dplyr::na_if("N/A")
bfd <- bfd %>% dplyr::na_if("Not Informed")

#Verifica atributos com valores faltantes
naniar::miss_var_summary(bfd) #sumario
naniar::gg_miss_var(bfd) #formato visual

#Remove atributos com mais de 30% de valores vazios
bfd <- dplyr::select (bfd, -c (justification_code, arrival_ceiling, depart_ceiling, arrival_cloudiness))

#Parte 2: Transformações

#Substitui real_duration e expected_duration por flight_delay
bfd$flight_delay = bfd$real_duration - bfd$expected_duration
bfd <- dplyr::select (bfd, -c (real_duration, expected_duration))

bfd <- mutate(bfd, delay = ifelse(arrival_delay <= 0, "on_time", ifelse(flight_delay <= 0 & departure_delay > 0, "delay_on_departure", ifelse(flight_delay > 0 & departure_delay <= 0, "delay_on_flight", "delay_on_flight_departure"))))

bfd <- dplyr::select (bfd, -c (arrival_delay, departure_delay, flight_delay))

#Transforma atributos Factor para num
bfd$expected_depart_date <- as.numeric(bfd$expected_depart_date)
bfd$expected_depart_hour <- as.numeric(bfd$expected_depart_hour)

#Mapeamento categórico - linetype_code
bfd <- subset(bfd, !is.na(bfd$linetype_code)) #Remove linhas de linetype_code com NA
cm <- categ_mapping("linetype_code")
bfd <- transform(cm, bfd)

#Mapeamento categórico - situation_type
cm <- categ_mapping("situation_type")
bfd <- transform(cm, bfd)
bfd <- dplyr::select (bfd, -c (linetype_code, situation_type, situation_typeCANCELADO)) #Mantem apenas situation_typeREALIZADO

#Parte 3: Limpezas

#Verifica novamente valores faltantes com relação entre atributos
naniar::gg_miss_upset(bfd) #Não tem relação entre valores vazios

#Limpa valores com erro de temperatura
lo    <- 0
up    <- 100
bfd.clean <- subset(bfd, bfd$depart_humidity >= lo & bfd$depart_humidity <= up)

#Limpa valores infinitos e NA
is.na(bfd.clean) <- sapply(bfd.clean, is.infinite)
bfd.clean[is.na(bfd.clean)] <- 0
bfd.clean <- na.omit(bfd.clean)

#Remove outliers
out_obj <- outliers() #classe analise de outliers
out_obj <- fit(out_obj, bfd.clean) #calculando fronteiras
bfd.clean <- transform(out_obj, bfd.clean) #retorna dados limpos

#Parte 5: Redução de dimensionalidade

#Verifica correlação
#Separa atributos numericos e categoricos para analise de correlacao
bfd.clean.num <- dplyr::select_if(bfd.clean, is.numeric)

#Remove linetype_code e situation_type (da erro NA/NaN/Inf in foreign function call (arg 10))
bfd.clean.num <- dplyr::select (bfd.clean.num, -c (linetype_codeC,linetype_codeE, linetype_codeG, linetype_codeH, linetype_codeI, linetype_codeL, linetype_codeN, linetype_codeR, situation_typeREALIZADO))

#Avaliação a partir da categoria departure_delay
colunas <- colnames(bfd.clean.num) #Seleciona colunas numericas
bfd.clean.num <- data.frame (bfd.clean.num,  dplyr::select (bfd.clean, delay)) #Adiciona coluna atraso

#Analisa correlacao das colunas
plot.correlation(bfd.clean.num %>% dplyr::select(colunas))

#Feature selection
select_features <- function(myfeature, data) {
  myfeature <- fit(myfeature, data)
  print(myfeature$features)
  data <- transform(myfeature, data)
  print(head(data))
}

#Feature Selecion
bfd.clean.num <- dplyr::select_if(bfd.clean, is.numeric)
bfd.clean.num <- data.frame (bfd.clean.num,  dplyr::select (bfd.clean, delay)) #Adiciona coluna atraso
#Decision Tree
set.seed(1234)
ind <- sample(2, nrow(bfd.clean.num), replace = T, prob = c(0.5, 0.5))
train <- bfd.clean.num[ind == 1,]
test <- bfd.clean.num[ind == 2,]
tree <- rpart(delay ~., data = train)
rpart.plot(tree, box.palette = "green")

#Lasso
select_features(feature_selection_lasso("delay"), bfd.clean.num)

#FSS
select_features(feature_selection_fss("delay"), bfd.clean.num)

#IG
select_features(feature_selection_ig("delay"), bfd.clean.num)

#Seleciona atributos de delay
bfd.clean.num <- dplyr::select (bfd.clean.num, expected_depart_date, expected_depart_hour, depart_temperature, depart_wind_direction, arrival_dew_point)
