# version 1.2
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myBasic.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myGraphic.R")

#Carrega base de dados de viagens
load('/home/lra/OneDrive/CEFET/Disciplinas/05. Mineração de Dados/Dados/bfd.rda')


#Definicoes de cores
colors <- brewer.pal(11, 'Paired')
font   <- theme(text = element_text(size=9))

dsBfd <- bfd

sum   <- summary(dsBfd)
print(sum)

dsBfd <- select(bfd, departure_delay, depart_temperature, depart_dew_point, depart_humidity, depart_visibility, 
                depart_ceiling, depart_wind_speed, depart_wind_direction, depart_pressure, depart_cloudiness)

#Print dataframe summary
colunas <- colnames(dsBfd)
for (col in colunas) {
  
  sum   <- summary(dsBfd %>% dplyr::select(col))
  print(sum)
}

dsBfd <- bfd

ind <- c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE) 

#depart_temperature
## => não fazer a limpeza para a temperatura, a faixa está coerente
if (ind[1] == TRUE) {
  
  #Limpa os outliers na base de dados final dsBfd
  ql    <- quantile(dsBfd$depart_temperature, probs=c(.25, .75), na.rm = T)
  iq    <- IQR(dsBfd$depart_temperature, na.rm = T)
  lo    <- ql[1]-1.5*iq # Lower Range
  up    <- ql[2]+1.5*iq # Upper Range  
  
  dsBfd <- subset(dsBfd, dsBfd$depart_temperature >= lo & dsBfd$depart_temperature <= up)
} 

#depart_dew_point
## => não fazer a limpeza para a temperatura, a faixa está coerente
if (ind[2]) {
  
  
  ql    <- quantile(dsBfd$depart_dew_point, probs=c(.25, .75), na.rm = T)
  iq    <- IQR(dsBfd$depart_dew_point, na.rm = T)
  lo    <- ql[1]-1.5*iq # Lower Range
  up    <- ql[2]+1.5*iq # Upper Range  
  dsBfd <- subset(dsBfd, dsBfd$depart_dew_point >= lo & dsBfd$depart_dew_point <= up)
} 

#depart_humidity
## => tratar faixa de valor entre 0 e 100
if (ind[3]) {
  
  lo    <- 0   #ql[1]-1.5*iq # Lower Range
  up    <- 100 #ql[2]+1.5*iq # Upper Range  
  dsBfd <- subset(dsBfd, dsBfd$depart_humidity >= lo & dsBfd$depart_humidity <= up)
} 

#depart_visibility
## => Nada a fazer
if (ind[4]) {
  
  ql    <- quantile(dsBfd$depart_visibility, probs=c(.25, .75), na.rm = T)
  iq    <- IQR(dsBfd$depart_visibility, na.rm = T)
  lo    <- ql[1]-1.5*iq # Lower Range
  up    <- ql[2]+1.5*iq # Upper Range  
  dsBfd <- subset(dsBfd, dsBfd$depart_visibility >= lo & dsBfd$depart_visibility <= up)
} 

#depart_ceiling
## => Nada a fazer
if (ind[5]) {
  
  ql    <- quantile(dsBfd$depart_ceiling, probs=c(.25, .75), na.rm = T)
  iq    <- IQR(dsBfd$depart_ceiling, na.rm = T)
  lo    <- ql[1]-1.5*iq # Lower Range
  up    <- ql[2]+1.5*iq # Upper Range  
  dsBfd <- subset(dsBfd, dsBfd$depart_ceiling >= lo & dsBfd$depart_ceiling <= up)
  
} 

#depart_wind_speed
## => Nada a fazer
if (ind[6]) {
  
  ql    <- quantile(dsBfd$depart_wind_speed, probs=c(.25, .75), na.rm = T)
  iq    <- IQR(dsBfd$depart_wind_speed, na.rm = T)
  lo    <- ql[1]-1.5*iq # Lower Range
  up    <- ql[2]+1.5*iq # Upper Range  
  dsBfd <- subset(dsBfd, dsBfd$depart_wind_speed >= lo & dsBfd$depart_wind_speed <= up)
  
} 

#depart_wind_direction
## => Nada a fazer
if (ind[7]) {
  
  ql    <- quantile(dsBfd$depart_wind_direction, probs=c(.25, .75), na.rm = T)
  iq    <- IQR(dsBfd$depart_wind_direction, na.rm = T)
  lo    <- ql[1]-1.5*iq # Lower Range
  up    <- ql[2]+1.5*iq # Upper Range  
  dsBfd <- subset(dsBfd, dsBfd$depart_wind_direction >= lo & dsBfd$depart_wind_direction <= up)
} 

#departure_delay
ql    <- quantile(dsBfd$departure_delay[dsBfd$situation_type == 'REALIZADO'], probs=c(.25, .75), na.rm = T)
iq    <- IQR(dsBfd$departure_delay, na.rm = T)
lo    <- ql[1]-1.5*iq # Lower Range
up    <- ql[2]+1.5*iq # Upper Range  
dsBfd <- subset(dsBfd, dsBfd$departure_delay >= lo & dsBfd$departure_delay <= up)

#Sumariza informacoes de atrasos de saida
sum<-summary(dsBfd$departure_delay)
print(sum)

#Limpeza dos NAs
dsBfd <- dsBfd[!is.na(dsBfd$depart_ceiling),]
dsBfd <- dsBfd[!is.na(dsBfd$depart_visibility),]
dsBfd <- dsBfd[!is.na(dsBfd$depart_wind_direction),]

dsBfd <- select(dsBfd, departure_delay, depart_temperature, depart_dew_point, depart_humidity, depart_visibility, 
                depart_ceiling, depart_wind_speed, depart_wind_direction) %>%
  filter (dsBfd$situation_type == 'REALIZADO')


dsBfdCor <- dsBfd

#Analisa correlacao das colunas
plot.correlation(dsBfdCor %>% dplyr::select(departure_delay, depart_temperature, depart_dew_point, depart_humidity, depart_visibility, depart_ceiling, depart_wind_speed, depart_wind_direction))

#Determina categorias para atraso
lev         <- cut(dsBfd$departure_delay, breaks=c(min(dsBfd$departure_delay)-1, -5, 0, max(dsBfd$departure_delay)+1), ordered=TRUE)
levels(lev) <- c("early", "on_time", "delayed")
dsBfd$departure_delay <- lev

colunas <- colnames(dsBfd)[-1]

#Plota histograma
for (col in colunas) {
  grf <- plot.hist(dsBfd %>% dplyr::select(col), label_x=col, color=colors[length(col)]) + font
  plot(grf)
}

#Plota distribuicao de densidade por classe
for (col in colunas) {
  
  grfd <- plot.density.class(dsBfd %>% dplyr::select(departure_delay, col), class_label="departure_delay", label_x = col, color=colors[c(3:5)])
  plot(grfd)
}

#Plota box-plot por classe
for (col in colunas) {
  
  grf <- plot.boxplot.class(dsBfd %>% dplyr::select(departure_delay, col), class_label="departure_delay", label_x = col, color=colors[c(3:5)]) 
  plot(grf)
}


#Plota Chernoff faces
################################################################################
#dsBfdFaces <- select(dsBfd, departure_delay, depart_humidity, depart_temperature, depart_ceiling)

#set.seed(43)
#sample_rows = sample(1:nrow(dsBfdFaces), 25)

#isample = dsBfdFaces[sample_rows,]
#labels = as.character(isample$departure_delay)
#isample$departure_delay <- NULL

#loadlibrary("aplpack")

#par(mar = c(1, 1, 1, 1))
#faces(isample, labels = labels, scale = TRUE, plot.faces = TRUE, cex = 2)