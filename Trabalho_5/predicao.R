#options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))
options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx12000m"))

#version 1.5
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myBasic.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPreprocessing.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myClassification.R")

library(naniar) #Para verificar valores faltantes
library(rpart) #Decision Tree
library(rpart.plot) #Decision Tree

#Carrega base de dados de viagens
load("~/bfd.rda")

#Definicoes de cores
#colors <- brewer.pal(11, 'Paired')
#font <- theme(text = element_text(size=16))

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

#Substitui valores N/A, Not Informed nas colunas, para NA
#Essa limpeza e necessaria para possibilitar a verificacao de atributos vazios
bfd <- bfd %>% dplyr::na_if("N/A")
bfd <- bfd %>% dplyr::na_if("Not Informed")

#Verifica atributos com valores faltantes
naniar::miss_var_summary(bfd) #sumario
naniar::gg_miss_var(bfd) #formato visual

#Remove atributos com mais de 30% de valores vazios
bfd <- dplyr::select (bfd, -c (justification_code, arrival_ceiling, depart_ceiling, arrival_cloudiness))

#Parte 2: Limpezas

#Verifica novamente valores faltantes com relação entre atributos
naniar::gg_miss_upset(bfd) #Não tem relação entre valores vazios

#Limpa valores com erro de temperatura
lo    <- 0
up    <- 100
bfd.clean <- subset(bfd, bfd$depart_humidity >= lo & bfd$depart_humidity <= up)

#Limpa valores infinitos e NA
is.na(bfd.clean) <- sapply(bfd.clean, is.infinite)
bfd.clean <- na.omit(bfd.clean)

#Remove outliers
out_obj <- outliers() #classe analise de outliers
out_obj <- fit(out_obj, bfd.clean) #calculando fronteiras
bfd.clean <- transform(out_obj, bfd.clean) #retorna dados limpos

#Parte 3: Transformações

#Substitui real_duration e expected_duration por flight_delay
bfd.clean$flight_delay = bfd.clean$real_duration - bfd.clean$expected_duration
bfd.clean <- dplyr::select (bfd.clean, -c (real_duration, expected_duration))

#Determina categorias para atraso - conforme quadrantes
lev <- cut(bfd.clean$arrival_delay, breaks=c(min(bfd.clean$arrival_delay)-1, -5, 5, max(bfd.clean$arrival_delay)+1), ordered=TRUE)
levels(lev) <- c("early", "on_time", "delayed")
bfd.clean$arrival_delay <- lev

#Retira departure_delay e flight_delay. Estes atributos estão tendendo a decisão para eles, o que interesse sao as condicoes para o atraso e nao o momento
bfd.clean <- dplyr::select (bfd.clean, -c (departure_delay, flight_delay))

#Transforma atributos Factor para num
bfd.clean$expected_depart_date <- as.numeric(bfd.clean$expected_depart_date)
bfd.clean$expected_depart_hour <- as.numeric(bfd.clean$expected_depart_hour)

#Mapeamento categórico - linetype_code
bfd.clean <- subset(bfd.clean, !is.na(bfd.clean$linetype_code)) #Remove linhas de linetype_code com NA
#cm <- categ_mapping("linetype_code")
#bfd.clean <- transform(cm, bfd.clean)

#Mapeamento categórico - situation_type
#cm <- categ_mapping("situation_type")
#bfd.clean <- transform(cm, bfd.clean)
bfd.clean <- dplyr::select (bfd.clean, -c (linetype_code, situation_type)) #situation_typeCANCELADOMantem apenas situation_typeREALIZADO

#Retira atributos categoricos que nao foi possivel transformar em numerico
bfd.clean <- dplyr::select (bfd.clean, -c (airline_icao, origin_icao, destination_icao))

#Normalizacao
norm <- zscore()
norm <- fit(norm, bfd.clean)
bfd.clean <- transform(norm, bfd.clean)

#Extrai os levels do do dataset
slevels <- levels(bfd.clean$arrival_delay)
slevels

#Seleciona amostra
#data <- data %>% group_by(cod) %>% sample_frac(size=.30)
bfd.clean = bfd.clean[sample(nrow(bfd.clean), size=0.01*nrow(bfd.clean)), ]

#Parte 4: Amostra (separa em treinamento e teste)
# preparing dataset for random sampling
set.seed(1)
sr <- sample_random()
sr <- train_test(sr, bfd.clean)
bfd_train = sr$train
bfd_test = sr$test

tbl <- rbind(table(bfd.clean[,"arrival_delay"]), 
             table(bfd_train[,"arrival_delay"]), 
             table(bfd_test[,"arrival_delay"]))
rownames(tbl) <- c("dataset", "training", "test")
head(tbl)

#Parte 5: Função geral para os metodo de classificacao
train_test <- function(model, bfd_train, bfd_test) {
  print(class(model)[1])
  
  model <- fit(model, bfd_train)
  train_prediction <- predict(model, bfd_train)
  
  bfd_train_predictand = RSNNS::decodeClassLabels(bfd_train[,"arrival_delay"])
  train_eval <- evaluation.classification(bfd_train_predictand, train_prediction)
  print(train_eval$metrics)
  plot(roc_curve(train_eval))
  
  test_prediction <- predict(model, bfd_test)
  
  bfd_test_predictand = RSNNS::decodeClassLabels(bfd_test[,"arrival_delay"])
  test_eval <- evaluation.classification(bfd_test_predictand, test_prediction)
  print(test_eval$metrics)
  plot(roc_curve(test_eval))
}

#Parte 6: Aplicação dos métodos

#Classe majoritaria
train_test(classification_majority("arrival_delay", slevels), bfd_train, bfd_test)

#Arvore de decisao
train_test(classification_dtree("arrival_delay", slevels), bfd_train, bfd_test)

#Naive Bayes
train_test(classification_nb("arrival_delay", slevels), bfd_train, bfd_test)

#Random Forest
train_test(classification_rf("arrival_delay", slevels, mtry=3, ntree=5), bfd_train, bfd_test)

#SVM
train_test(classification_svm("arrival_delay", slevels), bfd_train, bfd_test)

#KNN
train_test(classification_knn("arrival_delay", slevels, k=1), bfd_train, bfd_test)

#Redes Neurais
train_test(classification_mlp("arrival_delay", slevels, size=3,decay=0.03), bfd_train, bfd_test)

#Redes Neurais Convulacionais
train_test(classification_cnn("arrival_delay", slevels, neurons=16,epochs=150), bfd_train, bfd_test)

