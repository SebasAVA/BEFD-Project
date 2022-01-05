#Se eliminan todos los Dataframes del ambiente Global de desarrollo
rm(list=ls())
#Incluir librerias y paquetes
##########################################
#https://otexts.com/fpp2/delphimethod.html
library(readxl)
library(reshape2)
library(lubridate)
library(plyr)
library(dplyr)
library(ggplot2)
library("GGally")
library(ggfortify)
library("lubridate")
require(xts)
library(scales)
library(fpp2)
#Establecer un directorio de trabajo
setwd("C:/Users/sebas/Documents/2021-20/BUSINESS ECONOMIC AND FINANCIAL DATA/BEFD Scripts/Data Merge")

dfEE <- read.csv("EE2.csv")

#Graficas


dfEE$date <- as.Date(dfEE$Fecha)

dfEE <- dfEE %>%
  mutate(SOLAR = coalesce(SOLAR, 0),
         EOLICA = coalesce(EOLICA, 0))



dfEE$ANTIOQUIAMm3 <- dfEE$ANTIOQUIAMm3/24
dfEE$CARIBEMm3 <- dfEE$CARIBEMm3/24
dfEE$CENTROMm3 <- dfEE$CENTROMm3/24
dfEE$ORIENTEMm3 <- dfEE$ORIENTEMm3/24
dfEE$VALLEMm3 <- dfEE$VALLEMm3/24


dfEE$ReservasM3 <- dfEE$ANTIOQUIAMm3+dfEE$CARIBEMm3+dfEE$CENTROMm3+dfEE$ORIENTEMm3+dfEE$VALLEMm3

dfEE$ANTIOQUIAM3 <- dfEE$ANTIOQUIAM3/24
dfEE$CARIBEM3 <- dfEE$CARIBEM3/24
dfEE$CENTROM3 <- dfEE$CENTROM3/24
dfEE$ORIENTEM3 <- dfEE$ORIENTEM3/24
dfEE$VALLEM3 <- dfEE$VALLEM3/24

dfEE$APORTESM3 <- dfEE$ANTIOQUIAM3+dfEE$CARIBEM3+dfEE$CENTROM3+dfEE$ORIENTEM3+dfEE$VALLEM3
dfEE$ANTIOQUIAkWh <- NULL
dfEE$ANTIOQUIAMm3 <- NULL
dfEE$CARIBEkWh <- NULL
dfEE$CARIBEMm3 <- NULL
dfEE$CENTROkWh <- NULL
dfEE$CENTROMm3 <- NULL
dfEE$ORIENTEkWh <- NULL
dfEE$ORIENTEMm3 <- NULL
dfEE$VALLEkWh <- NULL
dfEE$VALLEMm3 <- NULL
dfEE$ANTIOQUIAM3 <- NULL
dfEE$CARIBEM3 <- NULL
dfEE$CENTROM3 <- NULL
dfEE$ORIENTEM3 <- NULL
dfEE$VALLEM3 <- NULL

dfEE$Oferta <- dfEE$COGENERADOR + dfEE$EOLICA + dfEE$HIDRAULICA + dfEE$TERMICA + dfEE$SOLAR

dfEE$Hour <- ifelse(nchar(substring(dfEE$FH, first = 12))<2,paste("0",substring(dfEE$FH, first = 12),":00", sep = "") , paste(substring(dfEE$FH, first = 12),":00", sep = ""))
dfEE$DateTime <- paste(dfEE$Fecha,dfEE$Hour)
dfEE$DateTime = as.POSIXct(strptime(dfEE$DateTime, format ="%Y-%m-%d %H:%M"))

dfEE$DateTime = as.POSIXct(strptime(dfEE$DateTime, format ="%Y-%m-%d %H:%M"))


firstHour <- 24*(as.Date("2003-1-1 00:00:00")-as.Date("2003-1-1 00:00:00"))
tsEE <- ts(dfEE,start=c(2003,firstHour),frequency=24*365)
tsShort <- window(tsEE, 2015,2016)
#sdts = xts(dfEE, order.by = dfEE$DateTime)
# Subset training
Seasonal =  subset(dfEE, DateTime> "2020-12-1 00:00:00" & date < "2020-12-30 00:00:00")

LongTime =  subset(dfEE, DateTime> "2019-1-1 00:00:00" & date < "2020-12-30 00:00:00")
#------------------------------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------------------------------# 
#dfEE <- dfEE[105181:113940,]



ggplot(Seasonal, aes(DateTime))+
  geom_line(aes(y = Demanda, colour = "Demand"),size=1.2)+
  geom_line(aes(y = Oferta ,colour = "Offer"),size=0.8)+
  scale_y_continuous(  labels = scales::number_format( big.mark = ',')) +
  labs(title = "Offer and Demand of energy hourly[kWh]", y = "Energy [kWh]")

ggAcf(tsShort[,"Demanda"], lag=24)

ggplot(Seasonal, aes(DateTime))+
  geom_line(aes(y = Oferta ,colour = "OFFER"),size=1)+
  geom_line(aes(y = EOLICA, colour = "EOLIC"),size=1)+
  geom_line(aes(y = HIDRAULICA ,colour = "HYDROELECTRIC"),size=1)+
  geom_line(aes(y = TERMICA, colour = "TERMIC"),size=1)+
  geom_line(aes(y = SOLAR ,colour = "SOLAR"),size=1)+          
  scale_y_continuous(  labels = scales::number_format( big.mark = ',')) +
  labs(title = "Offer and Sources of energy hourly[kWh]", y = "Energy [kWh]")


ggplot(data=LongTime,aes(x=DateTime, y=PB_Nac)) + 
  geom_path(colour="#009E73",size=0.5) + 
  labs(title = "Price of the energy [$/kWh]", x = "", y = "Price [$/kwh]", color = "Legend Title\n")+ scale_y_continuous(  labels = scales::number_format( big.mark = ','))

#https://www.eltiempo.com/archivo/documento/CMS-16610226
#https://stdrupal01.blob.core.windows.net/temporalportalxm/Boletin_787897.pdf?sig=qDw9PIjzFTlbg%2B3uheoqQ62DvGIwN24PelKF3dHudys%3D&st=2022-01-05T04%3A04%3A45Z&se=2022-01-05T04%3A06%3A45Z&sv=2019-02-02&sp=r&sr=c
#https://stdrupal01.blob.core.windows.net/temporalportalxm/Boletin_768225.pdf?sig=N9xAn64MbVxiHJlwnhj3Zt%2BeBHXcyAFLQhkhDOHKchY%3D&st=2022-01-05T04%3A06%3A20Z&se=2022-01-05T04%3A08%3A20Z&sv=2019-02-02&sp=r&sr=c
#https://stdrupal01.blob.core.windows.net/temporalportalxm/Boletin_771117.pdf?sig=O5X8K0csfAKbIdqssdN4AcjBIcCdZA19Rclu5IBhDDo%3D&st=2022-01-05T04%3A13%3A54Z&se=2022-01-05T04%3A15%3A54Z&sv=2019-02-02&sp=r&sr=c
autoplot(tsShort[,c("PB_Nac","TERMICA","ReservasM3","APORTESM3")], facets=TRUE) +
  xlab("Year: 2014") + ylab("") +
  scale_y_continuous(  labels = scales::number_format( big.mark = ','))+
  ggtitle("Price of electricity, Water Reserves, Termic production")


qplot(TERMICA, PB_Nac, data=as.data.frame(tsShort)) +
   xlab("Termic ENERGY[kWh]") + ylab("Price of Electricity") #ME GUSTA

qplot(ReservasM3, PB_Nac, data=as.data.frame(tsShort)) +
  xlab("Water Reserves[kWh]") + ylab("Price of Electricity") #Casi no me gusta

qplot(APORTESM3, PB_Nac, data=as.data.frame(tsShort)) +
  xlab("Aportes[m3]") + ylab("Price of Electricity") #ME GUSTA


GGally::ggpairs(as.data.frame(tsShort[,c("PB_Nac","TERMICA","ReservasM3","APORTESM3","HIDRAULICA")]))

#AUTOCORRELATION
ggAcf(tsShort[,"PB_Nac"])

#Grafica de demanda y oferta energetica y tsmbien para el agua. 

#Histograma Precio Bolsa Nacional

df_Cor <- data.frame(PB_HIDRAULICA=double(),
                     PB_TERMICA =double(),
                     PB_DEM = double(),
                     PB_ORI = double(),
                     PB_VALL = double())
myvars <- c("PB_Nac", "HIDRAULICA","TERMICA","Demanda","ANTIOQUIAMm3","CARIBEMm3","CENTROMm3","ORIENTEMm3","VALLEMm3","Fecha")
MVC <- dfEE[myvars]
MVC[is.na(MVC)] <- 0
MVC <- MVC[108863:157800,]
for (i in 1:nrow(MVC)) {
  tempCor <- MVC[i:(i+240),]
  #print(tempCor)
  #tempCor[is.na(tempCor)] <- 0
  CorH20 <- cor(tempCor$PB_Nac, tempCor$HIDRAULICA)
  CorAN <- cor(tempCor$PB_Nac, tempCor$TERMICA)
  CorCEN <- cor(tempCor$PB_Nac, tempCor$Demanda)
  CorORI <- cor(tempCor$PB_Nac, tempCor$ANTIOQUIAMm3)
  CorVALL <- cor(tempCor$PB_Nac, tempCor$CENTROMm3)
  
  
  df_Cor <- rbind(df_Cor,c(CorH20,CorAN,CorCEN,CorORI,CorVALL))
  
}


colnames(df_Cor)[1] <- "PB_HIDRAULICA"
colnames(df_Cor)[2] <- "PB_TERMICA"
colnames(df_Cor)[3] <- "PB_DEM"
colnames(df_Cor)[4] <- "PB_ANT"
colnames(df_Cor)[5] <- "PB_CENT"
df_Cor <- cbind(df_Cor,MVC$Fecha)
df_Cor$Fecha <- as.Date(df_Cor$Fecha)
df_Cor <- cbind(df_Cor,MVC$PB_Nac,MVC$HIDRAULICA,MVC$TERMICA,MVC$Demanda)
df_Cor$`MVC$PB_Nac`<- df_Cor$`MVC$PB_Nac`/mean(df_Cor$`MVC$PB_Nac`)
df_Cor$`MVC$HIDRAULICA` <- df_Cor$`MVC$HIDRAULICA`/mean(df_Cor$`MVC$HIDRAULICA`)
df_Cor$`MVC$TERMICA` <- df_Cor$`MVC$TERMICA`/mean(df_Cor$`MVC$TERMICA`)
df_Cor$`MVC$Demanda` <- df_Cor$`MVC$Demanda`/mean(df_Cor$`MVC$Demanda`)

colnames(df_Cor)[7] <- "PB_Mean"
colnames(df_Cor)[8] <- "HIDRA"
colnames(df_Cor)[9] <- "TERMICA"
colnames(df_Cor)[10] <- "Demanda"
df_Cor <- df_Cor[1:8841,]

ggplot(df_Cor, aes(Fecha))+
  geom_line(aes(y = df_Cor$PB_Mean, colour = "Precio Bolsa"),size=0.8)+
  geom_line(aes(y = df_Cor$HIDRA ,colour = "Energía Hidráulica"),size=0.5)+
  geom_line(aes(y = df_Cor$PB_HIDRAULICA ,colour = "Correlación"),size=1.2)+
  scale_x_date(date_breaks = "1 month", date_labels = "%m %Y") +
  labs(title = "Correlación Energía Hidráulica y Precio Bolsa", x = "Tiempo [Mes Año]",y ="")

ggplot(df_Cor, aes(Fecha))+
  geom_line(aes(y = df_Cor$PB_Mean, colour = "Precio Bolsa"))+
  geom_line(aes(y = df_Cor$TERMICA ,colour = "Energía Termica"))+
  geom_line(aes(y = df_Cor$PB_TERMICA ,colour = "Correlación"))+
  scale_x_date(date_breaks = "1 month", date_labels = "%m %Y") +
  labs(title = "Correlación Energía Termica y Precio Bolsa", x = "Tiempo [Mes Año]",y ="")

ggplot(df_Cor, aes(Fecha))+
  geom_line(aes(y = df_Cor$PB_Mean, colour = "Precio Bolsa"),size = 0.8)+
  geom_line(aes(y = df_Cor$Demanda ,colour = "Demanda Total"),size = 0.5)+
  geom_line(aes(y = df_Cor$PB_DEM ,colour = "Correlación"),size = 1.2)+
  scale_x_date(date_breaks = "1 month", date_labels = "%m %Y") +
  labs(title = "Correlación Demanda y Precio Bolsa", x = "Tiempo [Mes Año]",y ="")

ggplot(df_Cor, aes(Fecha))+
  geom_line(aes(y = df_Cor$PB_Mean, colour = "Precio Bolsa"))+
  geom_line(aes(y = df_Cor$HIDRA ,colour = "Energía Hidráulica"))+
  geom_line(aes(y = df_Cor$PB_HIDRAULICA ,colour = "Correlación"))+
  scale_x_date(date_breaks = "1 month", date_labels = "%m %Y") +
  labs(title = "Correlación Energía Hidráulica y Precio Bolsa", x = "Tiempo [Mes Año]",y ="")


myvars <- c("PB_Nac","VB_Nac","CB_Nac", "HIDRAULICA","TERMICA","Demanda","ANTIOQUIAMm3","CARIBEMm3","CENTROMm3","ORIENTEMm3","VALLEMm3","Date")
df_cor_cor <- dfEE[70000:120000,]
df_cor_cor <- df_cor_cor[myvars]
colnames(df_cor_cor) <- c("Precio Bolsa","Venta Bolsa","Compra Bolsa", "HIDRAULICA","TERMICA","Demanda","ANTIOQUIAMm3","CARIBEMm3","CENTROMm3","ORIENTEMm3","VALLEMm3","Date")


ggcorr(df_cor_cor, palette = "RdBu", label = TRUE)

