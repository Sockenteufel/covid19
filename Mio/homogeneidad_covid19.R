#install.packages("wbstats")
library(reticulate)
library(knitr)
library(tidyverse) # %>%
library(magrittr)
library(kableExtra)
library(lubridate)
#library(rna)
library(rnaturalearth)
library(plotly)
library(xts)
library(dygraphs)
library(car)
library(wbstats)

covid19 <- read.csv("covid_19_clean_complete.csv")
pobla_data <- wb_data(indicator = "SP.POP.TOTL", start_date = 2018, end_date = 2019)
pobla_data_2 <- wb(indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2019)
View(datos)
View(pobla_data_2)

pobla_data[pobla_data$country =="Chile", ]
pobla_data_2[pobla_data_2$country =="Chile",]


paises<-unique(covid19$Country.Region)
covid19.limpio =c()
for (i in 1:length(paises)){
  if(length(which(paises[i] %in% pobla_data_2$country))>0){
    covid19.limpio =rbind(covid19.limpio, covid19[covid19$Country.Region== paises[i],])
  }
}

covid19.limpio$Date %<>% ymd()
class(covid19.limpio$Date)

infectados.totales.por.dia<- aggregate(covid19.limpio$Confirmed ~ covid19.limpio$Date, FUN = sum)
fallecidos.totales.por.dia<- aggregate(covid19.limpio$Deaths ~ covid19.limpio$Date, FUN = sum)
recuperados.totales.por.dia<-aggregate(covid19.limpio$Recovered ~ covid19.limpio$Date, FUN =sum)
tabla.totales<- data.frame(infectados.totales.por.dia[,1],infectados.totales.por.dia[,2],fallecidos.totales.por.dia[,2],recuperados.totales.por.dia[,2])
names(tabla.totales)<-c("Fecha","Infectados","Fallecidos","Recuperados")

x<-tabla.totales[,1]
ggplot(tabla.totales, aes(x))+
  geom_line(aes(y=Infectados, colour="Infectados"))+
  geom_line(aes(y=Fallecidos, colour="Fallecidos"))+
  geom_line(aes(y=Recuperados, colour="Recuperados"))+
  xlab("Fecha") + ylab("Frecuencias")+
  scale_color_manual(values = c("red","blue","green"))

fecha="2020-03-15"
confirmados.por.pais <- aggregate(covid19.limpio$Confirmed[covid19.limpio$Date==fecha]~covid19.limpio$Country.Region[covid19.limpio$Date==fecha], FUN= sum)
names(confirmados.por.pais)=c("Pais","Confirmados")
View(confirmados.por.pais)


paises<-unique(covid19.limpio$Country.Region)
suma.total.habitantes<-sum(pobla_data_2[pobla_data_2$country %in% paises,]$value)
numero.total.infectados=sum(confirmados.por.pais$Confirmados)

tabla.infectados.paises<-c()
for(i in 1:length(paises)){
  habitantes=pobla_data_2[pobla_data_2$country==paises[i],]$value
  confirmados<-confirmados.por.pais$Confirmados[confirmados.por.pais$Pais==paises[i]]
  confirmados.estimados <- numero.total.infectados*habitantes/suma.total.habitantes
  tabla.infectados.paises<-rbind(tabla.infectados.paises, c(confirmados,confirmados.estimados))
}
tabla.infectados.paises<-as.data.frame(tabla.infectados.paises)
tabla.infectados.paises<-data.frame(paises,tabla.infectados.paises)
names(tabla.infectados.paises)<-c("pais","infectados","infectados.estimados")

paises.con.problemas<- which(tabla.infectados.paises$infectados.estimados < 5)
paises[paises.con.problemas]

tabla.infectados.paises2<- tabla.infectados.paises[-paises.con.problemas]

pais.añadir<-data.frame("problemas", sum(tabla.infectados.paises[tabla.infectados.paises$pais %in% paises[paises.con.problemas],]$infectados),
                        sum(tabla.infectados.paises[tabla.infectados.paises$pais %in% paises[paises.con.problemas],]$infectados.estimados))
names(pais.añadir) <- names(tabla.infectados.paises2)
pais.añadir

tabla.infectados.paises2=rbind(tabla.infectados.paises2,pais.añadir)
chisq.test(tabla.infectados.paises2$infectados, sum(tabla.infectados.paises2$infectados.estimados/sum(tabla.infectados.paises2$infectados))
