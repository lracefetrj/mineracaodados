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
load("~/data/flight/bfd.RData")

bfd <- data.frame(data)
#Definicoes de cores
colors <- brewer.pal(11, 'Paired')
font <- theme(text = element_text(size=16))

#Parte 1: Seleção atributos

#Remove colunas com valores constantes
bfd <- dplyr::select (bfd, -c (depart_cloudiness, depart_visibility, depart_pressure, arrival_pressure, arrival_visibility, origin_name, origin_country, destination_name, destination_country, airline_name, justification_description))

#Remove flight_id, muito especifico
bfd <- dplyr::select (bfd, -c (flight_id))

#Exclui atributos real_depart_date, real_depart_hour, real_arrival_date e real_arrival_hour, porque podem ser calculados somando a data/hora esperadas com o tempo de atraso (correlacao)
bfd <- dplyr::select (bfd, -c (real_depart_date, real_depart_hour, real_arrival_date, real_arrival_hour))

#Exclui atributos expected_arrival_date e expected_arrival_hour. Expectativa de chegada pode ser pertubada por atrasos na saida ou voo.
bfd <- dplyr::select (bfd, -c (expected_arrival_date, expected_arrival_hour))

#Retira situation_type. A intencao e verificar os atrasos, se houve cancelamento nao e relevante
#bfd <- dplyr::select (bfd, -c (situation_type))

#Exclui atributos numérico com categóricos correspondentes
bfd <- dplyr::select (bfd, -c (depart_wind_speed, depart_wind_direction, expected_depart_hour, arrival_wind_speed, arrival_wind_direction))

#Retira ponto de orvalho, que tem relacao com umidade e temperatura
bfd <- dplyr::select (bfd, -c (depart_dew_point, arrival_dew_point))

#Retira departure_delay e flight_delay. Estes atributos estão tendendo a decisão para eles, o que interesse sao as condicoes para o atraso e nao o momento. Os atributos real_duration e expected_duration foram utilizados para gerar o flight_delay
bfd <- dplyr::select (bfd, -c (departure_delay, real_duration, expected_duration))

#Remove atributos com mais de 30% de valores vazios
bfd <- dplyr::select (bfd, -c (justification_code, arrival_ceiling, depart_ceiling, arrival_cloudiness))

#Parte 2: Limpezas

#Substitui valores N/A, Not Informed nas colunas, para NA
#Essa limpeza e necessaria para possibilitar a verificacao de atributos vazios
bfd.clean <- bfd %>% dplyr::na_if("N/A")
bfd.clean <- bfd.clean %>% dplyr::na_if("Not Informed")

#Limpa valores com erro de temperatura
lo    <- 0
up    <- 100
bfd.clean <- subset(bfd.clean, bfd$depart_humidity >= lo & bfd$depart_humidity <= up)
bfd.clean <- subset(bfd.clean, bfd.clean$arrival_humidity >= lo & bfd.clean$arrival_humidity <= up)

#Limpa valores infinitos e NA
#is.na(bfd.clean) <- sapply(bfd.clean, is.infinite)
bfd.clean[is.na(bfd.clean)] <- 0
bfd.clean <- na.omit(bfd.clean)

#Remove outliers
out_obj <- outliers() #classe analise de outliers
out_obj <- fit(out_obj, bfd.clean) #calculando fronteiras
bfd.clean <- transform(out_obj, bfd.clean) #retorna dados limpos

#Parte 3: Transformações

#Determina categorias para atraso - conforme quadrantes
lev <- cut(bfd.clean$arrival_delay, breaks=c(min(bfd.clean$arrival_delay)-1, -5, 5, max(bfd.clean$arrival_delay)+1), ordered=TRUE)
levels(lev) <- c("early", "on_time", "delayed")
bfd.clean$arrival_delay <- lev

#Determina niveis de temperatura
lev <- cut(bfd.clean$depart_temperature, breaks=c(min(bfd.clean$depart_temperature)-1, 15, 32, max(bfd.clean$depart_temperature)+1), ordered=TRUE)
levels(lev) <- c("cold", "normal", "hot")
bfd.clean$depart_temperature <- lev

lev <- cut(bfd.clean$arrival_temperature, breaks=c(min(bfd.clean$arrival_temperature)-1, 15, 32, max(bfd.clean$arrival_temperature)+1), ordered=TRUE)
levels(lev) <- c("cold", "normal", "hot")
bfd.clean$arrival_temperature <- lev

#Determina niveis de umidade
lev <- cut(bfd.clean$depart_humidity, breaks=c(min(bfd.clean$depart_humidity)-1, 40, 70, max(bfd.clean$depart_humidity)+1), ordered=TRUE)
levels(lev) <- c("low", "normal", "high")
bfd.clean$depart_humidity <- lev

lev <- cut(bfd.clean$arrival_humidity, breaks=c(min(bfd.clean$arrival_humidity)-1, 40, 70, max(bfd.clean$arrival_humidity)+1), ordered=TRUE)
levels(lev) <- c("low", "normal", "high")
bfd.clean$arrival_humidity <- lev

#Parte 4: APriori

#Converte para transacao
bfdTrans <- as(bfd.clean, "transactions")

#Apriori
rules <- apriori(bfdTrans, parameter=list(supp = 0.1, conf = 0.1, minlen=2, maxlen= 10, target = "rules"), appearance=list(rhs = c("arrival_delay=delayed"), default="lhs"), control=NULL)
inspect(rules)

rules_a <- as(rules, "data.frame")
head(rules_a)

#Parte 5: Analise das regras
imrules <- interestMeasure(rules, transactions = bfdTrans)
head(imrules)

#Remove regras redundantes
nrules <- rules[!is.redundant(rules)]
arules::inspect(nrules)

#Ver transacao que suporta regra 1
st <- supportingTransactions(nrules[1], bfdTrans)
trans <- unique(st@data@i)
length(trans)
print(c(length(trans)/length(bfdTrans), nrules[1]@quality$support))

#Ver transacao que suporta regra 1 e 2
st <- supportingTransactions(nrules[1:2], bfdTrans)
trans <- unique(st@data@i)
length(trans)
print(c(length(trans)/length(bfdTrans), nrules[1:2]@quality$support))

#Parte 4: Visualizacao
options(repr.plot.width=7, repr.plot.height=4)
plot(rules)

options(repr.plot.width=7, repr.plot.height=4)
plot(rules, method="paracoord", control=list(reorder=TRUE))

install.packages("rJava")
library('rJava')

install.packages('rCBA')
library('rCBA')

#FP_growth
rules = rCBA::fpgrowth(bfdTrans, support=0.5, confidence=0.2, maxLength=2, consequent="arrival_delay",
                       parallel=FALSE)

#Analise das regras
imrules <- interestMeasure(rules, transactions = bfd.clean)
head(imrules)

#Remove regras redundantes
#nrules <- rules[!is.redundant(rules)]
arules::inspect(nrules)

#Visualizacao
options(repr.plot.width=7, repr.plot.height=4)
plot(rules)

options(repr.plot.width=7, repr.plot.height=4)
plot(rules, method="paracoord", control=list(reorder=TRUE))

#prunedRules <- rCBA::pruning(bfd.clean, rules, method="m2cba", parallel=FALSE)
#predictions <- rCBA::classification(bfd.clean, prunedRules)
#table(predictions)