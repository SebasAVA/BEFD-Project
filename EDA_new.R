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
library(seasonal)
#Establecer un directorio de trabajo
setwd("C:/Users/Andres/Desktop/BEF_DATA/PROYECTO/BEFD-Project/Data Merge")
setwd("C:/Users/sebas/Documents/2021-20/BUSINESS ECONOMIC AND FINANCIAL DATA/BEFD Scripts/Data Merge")


dfEE <- read.csv("EE2.csv")

###########################################################################
###########################################################################
###                                                                     ###
###                        PREPROCESSING OF DATA                        ###
###                                                                     ###
###########################################################################
###########################################################################


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
#timeseries desde el inicio hasta el final

tsEE <- ts(dfEE,start=c(2003,firstHour),frequency=24*365)
#analisis de un año
tsShort <- window(tsEE, 2015,2016)
#demostrar que la demanada tiene seasonality
tsDecomposition <- window(tsEE, 2015,2017)


tsShortAlt <- window(tsEE, 2018,2019)
#sdts = xts(dfEE, order.by = dfEE$DateTime)
# Subset training
Seasonal =  subset(dfEE, DateTime> "2020-12-1 00:00:00" & date < "2020-12-31 12:00:00")

tsSeasonal <- ts(Seasonal$Demanda,start=c(2020,firstHour),frequency=24)

MidTime = subset(dfEE, DateTime> "2010-01-1 00:00:00" & date < "2020-12-31 12:00:00")

LongTime =  subset(dfEE, DateTime> "2019-1-1 00:00:00" & date < "2020-12-30 00:00:00")

############################################################################
############################################################################
###                                                                      ###
###                         CREACION DE GRAFICAS                         ###
###                                                                      ###
############################################################################
############################################################################

#Faltan Histogramas de cositas


##Time series from 01 Dec to 31 Dec
ggplot(Seasonal, aes(DateTime))+
  geom_line(aes(y = Demanda, colour = "Demand"),size=2.0)+
  geom_line(aes(y = Oferta ,colour = "Offer"),size=1.1)+
  scale_y_continuous(  labels = scales::number_format( big.mark = ',')) +
  xlab('Date')+ylab('Energy [kwh]')+
  ggtitle("Energy Offer and Demand freq: Hourgly [kWh]")+
  theme(plot.title = element_text(hjust = 0.5))



##Tendencia de la Demanada en el tiempo

dfEE %>% 
  filter_all(all_vars(!is.infinite(.)))

dfER <-  subset(dfEE, DateTime> "2004-01-1 00:00:00" & date < "2020-12-31 12:00:00")

dfER %>%
  as.data.frame() %>%
  ggplot(aes(x=DateTime, y=Demanda)) +
  ylab("Demand [kWh]") +
  xlab("Year") +
  geom_line()+
  geom_smooth(method="lm", se=FALSE,level=0.95,size=2)+
  ggtitle("Energy demand over time")+
  theme(plot.title = element_text(hjust = 0.5))

## Pandemic period

## Demand decrease during national quarantine due to industries  and commercial activities temporal closure
## mantaining only the public service supply
dfPa <-  subset(dfER, DateTime> "2020-03-1 00:00:00" & date < "2020-05-15 12:00:00")
dfPa %>%
  as.data.frame() %>%
  ggplot(aes(x=DateTime, y=Demanda)) +
  ylab("Demand [kWh]") +
  xlab("Year") +
  geom_line()+
  geom_smooth(method="lm", se=FALSE,level=0.95,size=2,formula= y~x)+
  ggtitle("Energy demand over time Year:2020")+
  theme(plot.title = element_text(hjust = 0.5))


##Plot the autocorrelation function

#The plot implies periodicity or seasonal effect
ggAcf(tsShort[,"Demanda"], lag=24)+
  ggtitle("Energy demand ACF")+
  theme(plot.title = element_text(hjust = 0.5))
  

##Plot over 48 lags

ggAcf(tsShort[,"Demanda"], lag=48)+
  ggtitle("Energy demand ACF")+
  theme(plot.title = element_text(hjust = 0.5))



# mas importante hidraulica y termica
ggplot(Seasonal, aes(DateTime))+
  geom_line(aes(y = Oferta ,colour = "OFFER"),size=1)+
  geom_line(aes(y = EOLICA, colour = "EOLIC"),size=1)+
  geom_line(aes(y = HIDRAULICA ,colour = "HYDROELECTRIC"),size=1)+
  geom_line(aes(y = TERMICA, colour = "THERMAL"),size=1)+
  geom_line(aes(y = SOLAR ,colour = "SOLAR"),size=1)+          
  scale_y_continuous(  labels = scales::number_format( big.mark = ',')) +
  ylab('Engergy [kWh]')+
  xlab('Date')+
  ggtitle("Energy Offer and generation sources [kWh], 12/2020")+
  theme(plot.title = element_text(hjust = 0.5))

##Lets review the solar energy 

ggplot(Seasonal, aes(DateTime))+
  geom_line(aes(y = EOLICA, colour = "EOLIC"),size=1)+
  geom_line(aes(y = SOLAR ,colour = "SOLAR"),size=1)+          
  scale_y_continuous(  labels = scales::number_format( big.mark = ',')) +
  ylab('Engergy [kWh]')+
  xlab('Date')+
  ggtitle("Energy Offer and generation sources [kWh]")+
  theme(plot.title = element_text(hjust = 0.5))
##Wind production

ggplot(dfPa, aes(DateTime))+
  geom_line(aes(y = EOLICA, colour = "EOLIC"),size=1)+
  scale_y_continuous(  labels = scales::number_format( big.mark = ',')) +
  ylab('Engergy [kWh]')+
  xlab('Date')+
  ggtitle("Energy Offer and generation sources [kWh]")+
  theme(plot.title = element_text(hjust = 0.5))

### Energy price 

ggplot(data=LongTime,aes(x=DateTime, y=PB_Nac)) + 
  geom_path(colour="#009E73",size=0.6) + 
  xlab('Date')+
  ylab('Price [[$/kwh]]')+
  ggtitle("Energy price [$/kWh]")+
  theme(plot.title = element_text(hjust = 0.5))



## ADd vertical lines to indicated El niño and guatape accident
ggplot(data=MidTime,aes(x=DateTime, y=PB_Nac)) + 
  geom_path(colour="#009E73",size=0.6) + 
  xlab('Date')+
  ylab('Price [[$/kwh]]')+
  ggtitle("Energy price [$/kWh]")+
  theme(plot.title = element_text(hjust = 0.5))


  

#https://www.eltiempo.com/archivo/documento/CMS-16610226
#https://stdrupal01.blob.core.windows.net/temporalportalxm/Boletin_787897.pdf?sig=qDw9PIjzFTlbg%2B3uheoqQ62DvGIwN24PelKF3dHudys%3D&st=2022-01-05T04%3A04%3A45Z&se=2022-01-05T04%3A06%3A45Z&sv=2019-02-02&sp=r&sr=c
#https://stdrupal01.blob.core.windows.net/temporalportalxm/Boletin_768225.pdf?sig=N9xAn64MbVxiHJlwnhj3Zt%2BeBHXcyAFLQhkhDOHKchY%3D&st=2022-01-05T04%3A06%3A20Z&se=2022-01-05T04%3A08%3A20Z&sv=2019-02-02&sp=r&sr=c
#https://stdrupal01.blob.core.windows.net/temporalportalxm/Boletin_771117.pdf?sig=O5X8K0csfAKbIdqssdN4AcjBIcCdZA19Rclu5IBhDDo%3D&st=2022-01-05T04%3A13%3A54Z&se=2022-01-05T04%3A15%3A54Z&sv=2019-02-02&sp=r&sr=c
autoplot(tsDecomposition[,c("PB_Nac","HIDRAULICA","TERMICA","ReservasM3","APORTESM3")], facets=TRUE) +
  xlab("Date") + ylab("") +
  scale_y_continuous(  labels = scales::number_format( big.mark = ','))+
  ggtitle("Energy Price, Geothermal production, Water reservoir, Hydric contribution")+
  theme(plot.title = element_text(hjust = 0.5))


#añadir linea de tendencia
qplot(TERMICA, PB_Nac, data=as.data.frame(tsShort)) +
   xlab("Thermal generation[kWh]") + ylab("Energy price [$/kWh")+
  ggtitle("Energy Price according to thermal generation [$/kWh]")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method="lm", se=FALSE,level=0.95,size=2,formula= y~x)

  

qplot(ReservasM3, PB_Nac, data=as.data.frame(tsShort)) +
  xlab("Water Reserves[kWh]") + ylab("Price of Electricity") #Casi no me gusta

#añaidr linear de tendencia disminuyen aportes aumente precio
qplot(APORTESM3, PB_Nac, data=as.data.frame(tsShort)) +
  xlab("Contribution[m3]") + ylab("Energy price [$/kWh]")+
  ggtitle("Energy Price according to reservoirs hydraulic contribution [$/kWh]")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method="lm",level=0.95,size=2)


###
qplot(HIDRAULICA, PB_Nac, data=as.data.frame(tsShort)) +
  xlab("Hydroelectric generation [kWh]") + ylab("Energy price [$/kWh]")+
  ggtitle("Energy Price according to hydroelectric contribution [$/kWh]")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method="lm",level=0.95,size=2)




GGally::ggpairs(as.data.frame(tsShortAlt[,c("PB_Nac","Thermal generation [kWh]","Water reserves [m3]","Contribution [m3]","Hydroelectric [kWh]","Demand [kWh]")]))

#AUTOCORRELATION
ggAcf(tsShortAlt[,"PB_Nac"])
#Hacer analisis
## Falta analisis correcto usando las tres series de tiempo

###########################################################################
###########################################################################
###                                                                     ###
###                    TIME SERIES REGRESSION MODELS                    ###
###                                                                     ###
###########################################################################
###########################################################################

#Grafica que podemos utilizar para explicar como a falta de energia Hidrica se aumenta el consumo de la termica y aumenta el precio de la energia

#change labels
tsShort %>%
  as.data.frame() %>%
  ggplot(aes(x=TERMICA, y=PB_Nac)) +
  ylab("Consumption (quarterly % change)") +
  xlab("Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)


#cambiar hidraulica por aportes
tsShort %>%
  as.data.frame() %>%
  ggplot(aes(x=HIDRAULICA, y=PB_Nac)) +
  ylab("Consumption (quarterly % change)") +
  xlab("Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

####################################################################
#FITTING PB

## tslm fit a linear model with time series components 

fit.PB<- tslm(
  PB_Nac ~ APORTESM3 + HIDRAULICA + TERMICA + Demanda,
  data=tsShortAlt)
summary(fit.PB)


fit.PBFN<- tslm(
  PB_Nac ~ APORTESM3 + HIDRAULICA + TERMICA + Demanda,
  data=tsShort)
summary(fit.PBFN)


#tsShortAlt 2018-2019

autoplot(tsShortAlt[,'PB_Nac'], series="Data") +
  autolayer(fitted(fit.PB), series="Fitted model") +
  xlab("Year") + ylab("Energy price [$/kWh]") +
  ggtitle("Energy Price [$/kWh]") +
  guides(colour=guide_legend(title=" "))+
  ggtitle("Energy price vs fitted model Year:[2018-2019]")+
  theme(plot.title = element_text(hjust = 0.5))

## tsShort 2015-2016 fenomeno del niño 2015-2016

autoplot(tsShort[,'PB_Nac'], series="Data") +
  autolayer(fitted(fit.PBFN), series="Fitted model") +
  xlab("Year") + ylab("Energy price [$/kWh]") +
  ggtitle("Energy Price [$/kWh]") +
  guides(colour=guide_legend(title=" "))+
  ggtitle("Energy price vs fitted model Year:[2018-2019]")+
  theme(plot.title = element_text(hjust = 0.5))


cbind(Data = tsShortAlt[,'PB_Nac'],
      Fitted = fitted(fit.PB)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Data, y=Fitted)) +
  geom_point() +
  ylab("Fitted (predicted values)") +
  xlab("Data (actual values)") +
  ggtitle("Price of Electricity")+
  geom_abline(intercept=0, slope=1,size=2.0,colour='blue')+
  ggtitle("Fitted model data vs Energy original data")+
  theme(plot.title = element_text(hjust = 0.5))
  


#SELECTING PREDICTORS

##IDEALMENTE SE DEBEN HACER TODAS LAS POSIBLES COMBINACIONES PARA 

# Revisar seccion 5.5
CV(fit.PB)


###################################################################
#Evaluating Residuals

checkresiduals(fit.PB)

df <- as.data.frame(tsShortAlt)
df[,"Residuals"]  <- as.numeric(residuals(fit.PB))
p1 <- ggplot(df, aes(x=APORTESM3, y=Residuals)) +
  geom_point()
p2 <- ggplot(df, aes(x=HIDRAULICA, y=Residuals)) +
  geom_point()
p3 <- ggplot(df, aes(x=TERMICA, y=Residuals)) +
  geom_point()
p4 <- ggplot(df, aes(x=Demanda, y=Residuals)) +
  geom_point()
gridExtra::grid.arrange(p1, p2, p3, p4, nrow=2)

#### Importante hablar de Outliers and influential observations and Spurious Regression
# como se comporta la demanda

fit.Dem <- tslm(tsSeasonal~ trend + season)
summary(fit.Dem)
autoplot(tsSeasonal, series="Data") +
  autolayer(fitted(fit.Dem), series="Fitted") +
  xlab("Year") + ylab("Deamnd") +
  ggtitle("Energy Demand")
#cambiar el titulo
#erase x axis erase xlab aumentar grosor linea

#### WITH FORECASTING

fcast <- forecast(fit.Dem, h=48)
autoplot(fcast) +
  ggtitle("Forecasts of energy demand") +
  xlab("Year") + ylab("kWh")


###########################################################################
###########################################################################
###                                                                     ###
###                      TIME SERIES DECOMPOSITION                      ###
###                                                                     ###
###########################################################################
###########################################################################



#Moving Averages podria servir para mostrar el valor del precio de la electricidad mas claramente
#Solo dejando la linea roja
autoplot(tsEE[,"PB_Nac"], series="Data") +
  autolayer(ma(tsEE[,"PB_Nac"],720), series="24-MA",size=1.2) +
  xlab("Year") + ylab("price [$/kWh]") +
  ggtitle("Price of the energy [$]") +
  scale_colour_manual(values=c("Data"="grey50","24-MA"="red"),
                      breaks=c("Data","24-MA"))


####CON ESTO PODEMOS DEMOSTRAR si es SEASONAL la demanda mientras que el precio no lo es.
tsSeasonal %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition
    of electrical equipment index")
##Trend no dice mucho, la tendencia es la que se ve a través de los 
# de aca se saca el season 
# demanda en todos los años, esta pendiente nos dice la pendiente del crecimiento de la demanda

fit <- stl(tsSeasonal, t.window=48, s.window="periodic",
           robust=TRUE)
fit %>% seasadj() %>% naive() %>%
  autoplot() + ylab("New orders index") +
  ggtitle("Naive forecasts of seasonally adjusted data")

fit %>% forecast(method="naive") %>%
  autoplot() + ylab("New orders index")

###########################################################################
###########################################################################
###                                                                     ###
###                                ARIMA                                ###
###                                                                     ###
###########################################################################
###########################################################################


#EL PRECIO SE MODELA NO SEASONAL
fit <- auto.arima(tsEE[,"PB_Nac"], seasonal=FALSE)
fit
fit %>% forecast(h=24) %>% autoplot(include=100)





fit2 <- Arima(tsShort[,"PB_Nac"], order=c(3,0,0))
fit2
fit2 %>% forecast(h=24) %>% autoplot(include=80)

fit3 <- auto.arima(tsShort[,"PB_Nac"], seasonal=FALSE,
                    stepwise=FALSE, approximation=FALSE)
fit3
fit3 %>% forecast(h=24) %>% autoplot(include=80)


#LA DEMANDA SE PUEDE MODELAR SEASONAL POR LAS SIGUIENTES HORAS

tsSeasonal %>%
  Arima(order=c(0,1,1), seasonal=c(0,1,1)) %>%
  residuals() %>% ggtsdisplay()


#TOCA HACER UN RMSE CON LOS POSIBLES VALORES DEL ARIMA Y DEL SEASONAL
fit3 <- Arima(tsSeasonal, order=c(0,1,3), seasonal=c(0,1,1))
checkresiduals(fit3)


fit3 %>% forecast(h=168) %>% autoplot(include=124)
#Grafica de demanda y oferta energetica y tsmbien para el agua. 

#Histograma Precio Bolsa Nacional


###
##1. Revisar modelos titulos de graficas 

##2. Revisar como setear ARIMA

##3. 


