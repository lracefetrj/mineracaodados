# Necessário Executar a PreProcessamento-Base.R 
#############################################################

#Carrega base de dados de viagens
#load('/home/lra/OneDrive/CEFET/Disciplinas/05. Mineração de Dados/Dados/bfd.rda')
bfd <- bfd

#Definicoes de cores
colors <- brewer.pal(11, 'Paired')
font   <- theme(text = element_text(size=9))

dsBfd <- bfd

sum   <- summary(dsBfd)
print(sum)

dsBfd <- select(bfd, arrival_delay, arrival_temperature, arrival_dew_point, arrival_humidity, arrival_wind_speed, arrival_wind_direction)

#Print dataframe summary
colunas <- colnames(dsBfd)
for (col in colunas) {
    
    sum   <- summary(dsBfd %>% dplyr::select(col))
    print(sum)
}

dsBfd <- bfd

#Sumariza informacoes de atrasos de saida
sum<-summary(dsBfd$arrival_delay)
print(sum)

#Analisa correlacao das colunas
#dsBfdCor <- bfd %>% select( arrival_temperature, arrival_dew_point, arrival_humidity, arrival_wind_speed, arrival_wind_direction)

# Plot com erro em função da não limpeza dos NA
#plot.correlation(data = dsBfdCor)

#rm(dsBfdCor)

dsBfd <- bfd %>% select(arrival_delay, arrival_temperature, arrival_dew_point, arrival_humidity, arrival_wind_speed, arrival_wind_direction) 

colunas <- colnames(dsBfd)[-1]

#Plota histograma
for (col in colunas) {
    grf <- plot.hist(dsBfd %>% dplyr::select(col), label_x=col, color=colors[length(col)]) + font
    plot(grf)
}

#Plota distribuicao de densidade por classe
for (col in colunas) {
    
    grfd <- plot.density.class(dsBfd %>% dplyr::select(arrival_delay, col), class_label="arrival_delay", label_x = col, color=colors[c(3:5)])
    plot(grfd)
}

#Plota box-plot por classe
for (col in colunas) {
    
    grf <- plot.boxplot.class(dsBfd %>% dplyr::select(arrival_delay, col), class_label="arrival_delay", label_x = col, color=colors[c(3:5)]) 
    plot(grf)
}

dsBfd <- bfd %>%
    select(arrival_delay, arrival_temperature, arrival_dew_point, arrival_humidity, arrival_wind_speed, arrival_wind_direction, linetype_code, ds_arrival_day_period, airline_icao, destination_icao) 

#grafico de dispersao relacionando ponto de orvalho e temperatura ambiente para cada tipo de voo
ggplot(dsBfd, aes(y = arrival_temperature, x = arrival_dew_point, shape = arrival_delay, color = arrival_delay)) + geom_point(size = 3, alpha = .7) +
    xlab("Ponto de orvalho") + ylab("Temperatura") + facet_wrap(~ linetype_code, nrow = 2)

#grafico de dispersao relacionando ponto de orvalho e temperatura ambiente para cada tipo de voo
ggplot(dsBfd, aes(y = arrival_temperature, x = arrival_dew_point, shape = arrival_delay, color = arrival_delay)) + geom_point(size = 3, alpha = .7) + 
    xlab("Ponto de orvalho") + ylab("Temperatura") + facet_wrap(~ ds_arrival_day_period , nrow = 2) 

dsBfdGroup <- dsBfd %>% 
    group_by(airline_icao, arrival_delay) %>%
    summarize(total = n())

dsBfdGroup <- dsBfdGroup %>% filter(airline_icao %in% c("AZU", "TAM", "GLO", "ONE", "PTB", "ARG", "PAM", "OWT", "TTL", "LAN"))

options(repr.plot.width = 15, repr.plot.height = 6)

aptsNFlights <- ggplot(data=dsBfdGroup, aes(x=reorder(airline_icao, -total), y = total/1000)) +
    geom_col(aes(fill=arrival_delay)) + 
    xlab('Cia Aérea') + 
    ylab('Quantidade de Voos (milhares)') + 
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14, face="bold")) +
    guides(fill=guide_legend(title="Situação")) +
    scale_fill_brewer(palette="Blues") +
    theme_bw()

aptsNFlights


dsBfdGroup <- dsBfd %>% 
    group_by(destination_icao, arrival_delay) %>%
    summarize(total = n())

dsBfdGroup <- dsBfdGroup %>% filter(destination_icao %in% c("SBGR", "SBSP", "SBBR", "SBCF", "SBKP", "SBRJ", "SBGL", "SBRF", "SBSV", "SBPA", "SBCT", "SBFZ", "SBCY"))

options(repr.plot.width = 15, repr.plot.height = 6)

aptsNFlights <- ggplot(data=dsBfdGroup, aes(x=reorder(destination_icao, -total/1000), y = total / 1000)) +
    geom_col(aes(fill=arrival_delay)) + 
    xlab('Aeroporto') + 
    ylab('Quantidade de Voos (milhares)') + 
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14, face="bold")) +
    guides(fill=guide_legend(title="Situação")) +
    scale_fill_brewer(palette="Blues") +
    theme_bw() 
#+ theme(axis.text.x = element_text(angle = 45, size=9), panel.border = element_blank())

aptsNFlights

dsBfd <- bfd %>% select(arrival_delay, arrival_temperature, arrival_dew_point, arrival_humidity, arrival_wind_speed, arrival_wind_direction) 

colunas <- colnames(dsBfd)[-1]

#Plota matriz de dispersao avancada - arrival_delay
#grf <- plot.pair.adv(data=dsBfd, cnames=colunas, title="Matriz de Dispersão", clabel='arrival_delay', colors=colors[c(2,4,6)])
#grf


