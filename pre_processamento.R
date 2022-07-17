#install.packages("factoextra")
#install.packages("naniar")

# version 1.2
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myBasic.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPreprocessing.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphic.R")

library(naniar) #Para verificar valores faltantes
library(stats) #Para PCA
library(factoextra) #Para criar alguns gráficos (PCA)

#Carrega base de dados de viagens
load("~/Trabalho_2/bfd.rda")

#Definicoes de cores
colors <- brewer.pal(11, 'Paired')
font <- theme(text = element_text(size=16))

#Verifica situacao atributos
summary(bfd)

#Remove colunas com valores constantes
bfd <- dplyr::select (bfd, -c (depart_cloudiness, airline_name, origin_name, origin_country, destination_name, destination_country, justification_description, depart_pressure, arrival_pressure))

#Substitui valores N/A, Not Informed nas colunas, para NA
#Essa limpeza e necessaria para possibilitar a verificacao de atributos vazios
bfd <- bfd %>% dplyr::na_if("N/A")
bfd <- bfd %>% dplyr::na_if("Not Informed")
bfd <- bfd %>% replace_with_na(replace = list(depart_wind_direction = 0))
bfd <- bfd %>% replace_with_na(replace = list(arrival_wind_direction = 0))
bfd <- bfd %>% replace_with_na(replace = list(depart_wind_speed = 0))
bfd <- bfd %>% replace_with_na(replace = list(arrival_wind_speed = 0))

#Verifica atributos com valores faltantes
naniar::miss_var_summary(bfd) #sumario
naniar::gg_miss_var(bfd) #formato visual
naniar::gg_miss_upset(bfd) #verifica se tem relacao entre os atributos

#Remove atributos com mais de 30% de valores vazios
bfd <- dplyr::select (bfd, -c (justification_code, arrival_ceiling, depart_ceiling, arrival_cloudiness))

#Sumariza os dados novamente
summary(bfd)

#Exclui atributos categoricos que existe um numerico correspondente
bfd <- dplyr::select (bfd, -c (ds_depart_wind_speed, ds_depart_wind_direction, ds_depart_day_period, ds_arrival_wind_speed, ds_arrival_wind_direction, ds_arrival_day_period))

#Limpa valores com erro de temperatura
lo    <- 0
up    <- 100
bfd.clean <- subset(bfd, bfd$depart_humidity >= lo & bfd$depart_humidity <= up)

#Remove outliers
out_obj <- outliers() #classe analise de outliers
out_obj <- fit(out_obj, bfd.clean) #calculando fronteiras
bfd.clean <- transform(out_obj, bfd.clean) #retorna dados limpos

#Separa atributos numericos e categoricos para analise de correlacao
bfd_num <- dplyr::select_if(bfd.clean, is.numeric)
bfd_cat <- dplyr::select_if(bfd.clean, is.factor)

#Limpa valores infinitos e NA
is.na(bfd_num) <- sapply(bfd_num, is.infinite)
bfd_num[is.na(bfd_num)] <- 0
bfd_num <- na.omit(bfd_num)

#Analisa correlacao das colunas
bfd_num.st <- data.frame (bfd_num,  dplyr::select (bfd_cat, situation_type))
colunas <- colnames(bfd_num)
plot.correlation(bfd_num.st %>% dplyr::select(colunas))

#Dúvida: Quem tirar? expected_duration ou real_duration? departure_delay ou arrival_delay? (mesmo percentual de atributos miss)

#Balanceamento situation_type. Subsampling.
teste_balance <- function(obj, data)  {
  print(class(obj)[1])
  data <- transform(obj, data)
  print(table(data$Species))
  return(data)
}

bfd_num.st.sub <- teste_balance(balance_subsampling("situation_type"), bfd_num.st)

#Plota boxplot
plot.boxplot(bfd_num.st.sub)

#Aplica PCA: https://operdata.com.br/blog/analise-de-componentes-principais-pca-calculo-e-aplicacao-no-r/
dados_pca <- bfd_num
sapply(dados_pca, sd) #Calculo desvio padrão
pca_cov <- prcomp(dados_pca) #PCA com matriz de convariância
summary(pca_cov)

pca_corr <- prcomp(dados_pca, center = TRUE, scale = TRUE) #PCA com matriz de correlação
summary(pca_corr)

fviz_eig(pca_corr) #Interpretação gráfica. Quanto cada componente explica a variância total dos dados

#Aplica PCA: Professor
dados_pca <- dt_pca("situation_type")
dados_pca <- fit(dados_pca, bfd_num.st.sub)
bfd.pca <- transform(dados_pca, bfd_num.st.sub)

print(head(bfd.pca)) #Propriedades PCA
print(head(dados_pca$pca.transf)) #Propriedades PCA

grf <- plot.scatter(bfd.pca %>% dplyr::select(x=PC1, value=PC2, variable=situation_type), colors=colors[c(2:3)])
plot(grf)

#Teste de correlacao variaveis categoricas
#TODO. Ver como faz a correlacao de Person para variaveis categoricas
#chisq.test(bfd_cat$expected_depart_date,bfd_cat$real_depart_date)

#Feature selection
select_features <- function(myfeature, data) {
  myfeature <- fit(myfeature, data)
  print(myfeature$features)
  data <- transform(myfeature, data)
  print(head(data))
}

#Lasso
select_features(feature_selection_lasso("situation_type"), bfd_num.st.sub)

#FSS
select_features(feature_selection_fss("situation_type"), bfd_num.st.sub)

#IG
select_features(feature_selection_ig("situation_type"), bfd_num.st.sub)

#Relief
select_features(feature_selection_relief("situation_type"), bfd_num.st.sub)

#TODO. Normalizacao https://nbviewer.org/github/eogasawara/mylibrary/blob/master/Normalization.ipynb
#TODO. Verificar se vale amostrar? https://nbviewer.org/github/eogasawara/mylibrary/blob/master/Sample.ipynb
