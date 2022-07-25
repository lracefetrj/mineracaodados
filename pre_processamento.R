#Evitar erro de java heap space
#options(java.parameters = "-Xmx8000m")
options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m")) 
library(xlsx)

#lib <- require(factoextra)
#if (!lib)
#  install.packages("factoextra")

#lib <- require(naniar)
#if (!lib)
#  install.packages("naniar")

#lib <- require(rpart.plot)
#if (!lib)
#  install.packages("rpart.plot")

#lib <- require(party)
#if (!lib)
#  install.packages("party")

#lib <- require(Boruta)
#if (!lib)
#  install.packages("Boruta")

#lib <- require(Boruta)
#if (!lib)
#  install.packages("mlbench")

#lib <- require(Boruta)
#if (!lib)
#  install.packages("caret")

#version 1.5
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myBasic.R")
source("~/Trabalho_2/myPreprocessing.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphic.R")

library(naniar) #Para verificar valores faltantes
library(stats) #Para PCA
library(factoextra) #Para criar alguns gráficos (PCA)
library(rpart) #Decision Tree
library(rpart.plot) #Decision Tree
#library(Boruta) #Importancia de atributo
#library(ElemStatLearn) #Importancia de atributo Randon Forest
#library(FSelector) #Importancia de atributo Randon Forest
library(mlbench) #Importancia de atributo Carrot
library(caret) #Importancia de atributo Carrot

#Carrega base de dados de viagens
load("~/Trabalho_2/bfd.rda")

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

#Exclui atributos categóricos com numérico correspondente
bfd <- dplyr::select (bfd, -c (ds_depart_wind_speed, ds_depart_wind_direction, ds_depart_day_period, ds_arrival_wind_speed, ds_arrival_wind_direction, ds_arrival_day_period))

#Substitui valores N/A, Not Informed nas colunas, para NA
#Essa limpeza e necessaria para possibilitar a verificacao de atributos vazios
bfd <- bfd %>% dplyr::na_if("N/A")
bfd <- bfd %>% dplyr::na_if("Not Informed")

#Verifica atributos com valores faltantes
naniar::miss_var_summary(bfd) #sumario
#naniar::gg_miss_var(bfd) #formato visual

#Remove atributos com mais de 30% de valores vazios
bfd <- dplyr::select (bfd, -c (justification_code, arrival_ceiling, depart_ceiling, arrival_cloudiness))

#Remove linhas de linetype_code com NA
bfd <- subset(bfd, !is.na(bfd$linetype_code))

#Parte 2: Transformações

#Determina categorias para atraso - conforme quadrantes
lev <- cut(bfd$departure_delay, breaks=c(min(bfd$departure_delay)-1, -5, 5, max(bfd$departure_delay)+1), ordered=TRUE)
levels(lev) <- c("early", "on_time", "delayed")
bfd$departure_delay <- lev

lev <- cut(bfd$arrival_delay, breaks=c(min(bfd$arrival_delay)-1, -5, 5, max(bfd$arrival_delay)+1), ordered=TRUE)
levels(lev) <- c("early", "on_time", "delayed")
bfd$arrival_delay <- lev

#Transforma atributos Factor para num
bfd$expected_depart_date <- as.numeric(bfd$expected_depart_date)
bfd$expected_depart_hour <- as.numeric(bfd$expected_depart_hour)
bfd$real_depart_date <- as.numeric(bfd$real_depart_date)
bfd$real_depart_hour <- as.numeric(bfd$real_depart_hour)

bfd$expected_arrival_date <- as.numeric(bfd$expected_arrival_date)
bfd$expected_arrival_hour <- as.numeric(bfd$expected_arrival_hour)
bfd$real_arrival_date <- as.numeric(bfd$real_arrival_date)
bfd$real_arrival_hour <- as.numeric(bfd$real_arrival_hour)

#Parte 3: Limpezas de registros - parte 1

#Verifica novamente valores faltantes com relação entre atributos
naniar::gg_miss_upset(bfd) #Não tem relação entre valores vazios
#Limpa quando depart_wind_direction e arrival_wind_direction sao NA - nao vale a pena
#bfd.clean <- subset(bfd, !is.na(bfd$depart_wind_direction) & !is.na(bfd$arrival_wind_direction)

#Limpa valores com erro de temperatura
lo    <- 0
up    <- 100
bfd.clean <- subset(bfd, bfd$depart_humidity >= lo & bfd$depart_humidity <= up)

#Limpa valores infinitos e NA
is.na(bfd.clean) <- sapply(bfd.clean, is.infinite)
bfd.clean[is.na(bfd.clean)] <- 0 #TODO: substituir por metodo que achar adequado: média, determinação a partir de outros valores
bfd.clean <- na.omit(bfd.clean)

#Remove outliers
out_obj <- outliers() #classe analise de outliers
out_obj <- fit(out_obj, bfd.clean) #calculando fronteiras
bfd.clean <- transform(out_obj, bfd.clean) #retorna dados limpos

#Separa atributos numericos e categoricos para analise de correlacao
bfd_num <- dplyr::select_if(bfd.clean, is.numeric)

#Parte 3: Verifica correlação

#Avaliação a partir da categoria departure_delay
colunas <- colnames(bfd_num) #Seleciona colunas numericas
bfd_num.dd <- data.frame (bfd_num,  dplyr::select (bfd.clean, departure_delay)) #Adiciona coluna atraso

#Analisa correlacao das colunas
#plot.correlation(bfd_num.dd %>% dplyr::select(colunas))

#Avaliação a partir da categoria arrival_delay
colunas <- colnames(bfd_num) #Seleciona colunas numericas
bfd_num.ad <- data.frame (bfd_num,  dplyr::select (bfd.clean, arrival_delay)) #Adiciona coluna atraso

#Analisa correlacao das colunas
#plot.correlation(bfd_num.ad %>% dplyr::select(colunas))

#Remove colunas muito correlatas
bfd.clean <- dplyr::select (bfd.clean, -c (expected_depart_date, expected_arrival_date, expected_duration, expected_depart_hour, expected_arrival_hour, real_depart_date))

#Parte 4: Mapeamento categórico

#Mapeamento categórico - linetype_code
cm <- categ_mapping("linetype_code")
bfd.clean <- transform(cm, bfd.clean)

#Mapeamento categórico - situation_type
cm <- categ_mapping("situation_type")
bfd.clean <- transform(cm, bfd.clean)

bfd.clean <- dplyr::select (bfd.clean, -c (situation_type, situation_typeCANCELADO)) #Mantem apenas situation_typeREALIZADO

#Refaz bfd.clean.dd e bfd.clean.ad
bfd_num <- dplyr::select_if(bfd.clean, is.numeric)
bfd_num.dd <- data.frame (bfd_num,  dplyr::select (bfd.clean, departure_delay)) #Adiciona coluna atraso
bfd_num.ad <- data.frame (bfd_num,  dplyr::select (bfd.clean, arrival_delay)) #Adiciona coluna atraso

#Parte 5: Redução de dimensionalidade
#Aplica PCA - departure_delay
#dados_pca <- dt_pca("departure_delay")
#dados_pca <- fit(dados_pca, bfd_num.dd)
#bfd.pca <- transform(dados_pca, bfd_num.dd)

#print(head(bfd.pca)) #Propriedades PCA
#print(head(dados_pca$pca.transf)) #Propriedades PCA

#grf <- plot.scatter(bfd.pca %>% dplyr::select(x=PC1, value=PC2, variable=departure_delay), colors=colors[c(2,4,6)])
#plot(grf)

#Aplica PCA - arrival_delay
#dados_pca <- dt_pca("arrival_delay")
#dados_pca <- fit(dados_pca, bfd_num.ad)
#bfd.pca <- transform(dados_pca, bfd_num.ad)

#print(head(bfd.pca)) #Propriedades PCA
#print(head(dados_pca$pca.transf)) #Propriedades PCA

#grf <- plot.scatter(bfd.pca %>% dplyr::select(x=PC1, value=PC2, variable=arrival_delay), colors=colors[c(2,4,6)])
#plot(grf)

#Feature selection
select_features <- function(myfeature, data) {
  myfeature <- fit(myfeature, data)
  print(myfeature$features)
  data <- transform(myfeature, data)
  print(head(data))
}

#Feature Selecion - departure_delay

#Decision Tree - departure_delay
#set.seed(1234)
#ind <- sample(2, nrow(bfd_num.dd), replace = T, prob = c(0.5, 0.5))
#train <- bfd_num.dd[ind == 1,]
#test <- bfd_num.dd[ind == 2,]
#tree <- rpart(departure_delay ~., data = train)
#rpart.plot(tree)

#Lasso
#select_features(feature_selection_lasso("departure_delay"), bfd_num.dd)

#FSS
#select_features(feature_selection_fss("departure_delay"), bfd_num.dd)

#IG - funciona no regulus
#select_features(feature_selection_ig("departure_delay"), bfd_num.dd)

#Relief - funciona no regulus
#select_features(feature_selection_relief("departure_delay"), bfd_num.dd)

#Boruta
#boruta_output <- Boruta(departure_delay ~ ., data=bfd_num.dd, doTrace=2)
#boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])
#print(boruta_signif)
#plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")

#Random Dorest importance
#att.scores <- random.forest.importance(departure_delay ~ ., bfd_num.dd)
#print(att.scores)

#Carrot
#Prepara esquema de treinamento
#control <- trainControl(method="repeatedcv", number=10, repeats=3)
#Treina o modelo
#model <- train(departure_delay~., data=bfd_num.dd, method="lvq", preProcess="scale", trControl=control)
#Estima a importancia
#importance <- varImp(model, scale=FALSE)
#Plota a importancia
#print(importance)
#plot(importance)

#Feature Selecion - arrival_delay

#Decision Tree - arrival_delay
#set.seed(1234)
#ind <- sample(2, nrow(bfd_num.ad), replace = T, prob = c(0.5, 0.5))
#train <- bfd_num.ad[ind == 1,]
#test <- bfd_num.ad[ind == 2,]
#tree <- rpart(arrival_delay ~., data = train)
#rpart.plot(tree)

#Lasso
select_features(feature_selection_lasso("arrival_delay"), bfd_num.ad)

#FSS
select_features(feature_selection_fss("arrival_delay"), bfd_num.ad)

#IG - funciona no regulus
select_features(feature_selection_ig("arrival_delay"), bfd_num.ad)

#Relief - funciona no regulus
#select_features(feature_selection_relief("arrival_delay"), bfd_num.ad)

#Boruta
#boruta_output <- Boruta(arrival_delay ~ ., data=bfd_num.ad, doTrace=2)
#boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])
#print(boruta_signif)
#plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")

#Random Dorest importance
#att.scores <- random.forest.importance(arrival_delay ~ ., bfd_num.ad)
#print(att.scores)

#Carrot
#Prepara esquema de treinamento
#control <- trainControl(method="repeatedcv", number=10, repeats=3)
#Treina o modelo
#model <- train(arrival_delay~., data=bfd_num.ad, method="lvq", preProcess="scale", trControl=control)
#Estima a importancia
#importance <- varImp(model, scale=FALSE)
#Plota a importancia
#print(importance)
#plot(importance)

#Seleciona atributos de departure_delay
bfd_num.ad <- dplyr::select (bfd_num.ad, real_depart_hour, depart_temperature, depart_humidity, real_arrival_hour, real_duration, arrival_dew_point)

#Seleciona atributos de arrival_delay
bfd_num.ad <- dplyr::select (bfd_num.ad, arrival_delay, real_depart_hour, depart_temperature, depart_humidity, real_arrival_hour, real_duration, arrival_dew_point)

#Balanceamento situation_type. Subsampling.
#teste_balance <- function(obj, data)  {
#  print(class(obj)[1])
#  data <- transform(obj, data)
#  print(table(data$Species))
#  return(data)
#}

#bfd_num.st.sub <- teste_balance(balance_subsampling("situation_type"), bfd_num.st)

#TODO. Normalizacao? https://nbviewer.org/github/eogasawara/mylibrary/blob/master/Normalization.ipynb
#TODO. Atenuacao? https://nbviewer.org/github/eogasawara/mylibrary/blob/master/Smoothing.ipynb
#TODO. Amostrar? https://nbviewer.org/github/eogasawara/mylibrary/blob/master/Sample.ipynb

#Parte 6:Graficos

#ANTES DA LIMPEZA

#Selecao dos atributos - departure_delay
#bfd.sel <- dplyr::select (bfd, departure_delay, real_depart_hour, depart_temperature, depart_dew_point, real_arrival_date, real_arrival_hour, real_duration)

#Plota boxplot - departure_delay
#plot.boxplot(bfd_num.dd)

#Plota distribuicao de densidade, antes da limpeza - departure_delay
#colunas <- colnames(dplyr::select_if(bfd.sel, is.numeric))
#for (col in colunas) {
#  grf <- plot.density.class(bfd.sel %>% dplyr::select(departure_delay, col), class_label="departure_delay", label_x = col, color=colors[c(2,4,6)])
#  plot(grf)
#}

#Plota matriz de dispersao avancada, antes da limpeza - departure_delay
#grf <- plot.pair.adv(data=bfd.sel, cnames=colunas, title="bfd antes limpeza", clabel='departure_delay', colors=colors[c(2,4,6)])
#grf

#Selecao dos atributos - arrival_delay
#bfd.sel <- dplyr::select (bfd, arrival_delay, real_depart_hour, depart_humidity, depart_wind_direction, real_arrival_date, real_arrival_hour, real_duration, arrival_dew_point, arrival_wind_direction)

#Plota boxplot - departure_delay
#plot.boxplot(bfd_num.dd)

#Plota distribuicao de densidade, antes da limpeza - arrival_delay
#colunas <- colnames(dplyr::select_if(bfd.sel, is.numeric))
#for (col in colunas) {
#  grf <- plot.density.class(bfd.sel %>% dplyr::select(arrival_delay, col), class_label="arrival_delay", label_x = col, color=colors[c(2,4,6)])
#  plot(grf)
#}

#Plota matriz de dispersao avancada, antes da limpeza - arrival_delay
#grf <- plot.pair.adv(data=bfd.sel, cnames=colunas, title="bfd antes limpeza", clabel='arrival_delay', colors=colors[c(2,4,6)])
#grf

#DEPOIS DA LIMPEZA
#Plota boxplot - departure_delay
#plot.boxplot(bfd_num.dd)
#Plota distribuicao de densidade - departure_delay
colunas <- colnames(dplyr::select_if(bfd_num.dd, is.numeric))
#for (col in colunas) {
#  grf <- plot.density.class(bfd_num.dd %>% dplyr::select(departure_delay, col), class_label="departure_delay", label_x = col, color=colors[c(2,4,6)])
#  plot(grf)
#}

#Plota matriz de dispersao avancada - departure_delay
#grf <- plot.pair.adv(data=bfd_num.dd, cnames=colunas, title="bfd limpo", clabel='departure_delay', colors=colors[c(2,4,6)])
#grf

#Plota boxplot depois - arrival_delay
#plot.boxplot(bfd_num.ad)

#Plota distribuicao de densidade - arrival_delay
#colunas <- colnames(dplyr::select_if(bfd_num.ad, is.numeric))
#for (col in colunas) {
#  grf <- plot.density.class(bfd_num.ad %>% dplyr::select(arrival_delay, col), class_label="arrival_delay", label_x = col, color=colors[c(2,4,6)])
#  plot(grf)
#}

#Plota matriz de dispersao avancada - arrival_delay
#grf <- plot.pair.adv(data=bfd_num.ad, cnames=colunas, title="bfd limpo", clabel='arrival_delay', colors=colors[c(2,4,6)])
#grf
