#Se eliminan todos los Dataframes del ambiente Global de desarrollo
rm(list=ls())
#Incluir librerias y paquetes
library(readxl)
library(reshape2)
library(lubridate)
library(plyr)
library(dplyr)

#Establecer un directorio de trabajo
setwd("C:/Users/sebas/Documents/2021-20/BUSINESS ECONOMIC AND FINANCIAL DATA/Datos")
#Funciones
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

#Años de lectura
Años <- c(2003,2004,2005, 2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
Años_VBInt <- c(2003,2004,2008,2009,2011,2012,2013,2014,2015,2016,2017)
Años_CBInt <- c(2003,2008,2009,2011,2012,2013,2014,2015,2016,2017)
#Lectura Precio bolsa Internacional
PB_Int <- data.frame(Fecha=as.Date(character()),
                     variable=factor(), 
                     value=numeric()) 

for (i in 1:17) {
  
  PB_A <- read_excel(paste("Precio Bolsa/Precio_Bolsa_Internacional_($kwh)_",Años[i],".xlsx", sep = ""),skip=2)
  PB_A$Version <- NULL
  PB_A$Fecha <- as.Date(PB_A$Fecha, "%Y-%m-%d")
  colnames(PB_A) <- c("Fecha","H0","H1","H2","H3","H4","H5","H6","H7","H8","H9","H10","H11","H12","H13","H14","H15","H16","H17","H18","H19","H20","H21","H22","H23")
  PB_A <- as.data.frame(PB_A)
  PB_A <- completeFun(PB_A,"Fecha")
  meltPB <- melt(PB_A, id="Fecha")
  meltPB <- meltPB[order(as.Date(meltPB$Fecha, format="%Y-%m-%d")),]
  rownames(meltPB) <- NULL
  PB_Int <- rbind(PB_Int,meltPB)
}

#Lectura Precio bolsa Nacional
PB_Nac <- data.frame(Fecha=as.Date(character()),
                     variable=factor(), 
                     value=numeric()) 

for (i in 1:17) {
  PB_A <- read_excel(paste("Precio Bolsa/Precio_Bolsa_Nacional_($kwh)_",Años[i],".xlsx", sep = ""),skip=2)
  PB_A$Version <- NULL
  PB_A$X__1 <- NULL
  PB_A$X__2 <- NULL
  PB_A$Fecha <- as.Date(PB_A$Fecha, "%Y-%m-%d")
  colnames(PB_A) <- c("Fecha","H0","H1","H2","H3","H4","H5","H6","H7","H8","H9","H10","H11","H12","H13","H14","H15","H16","H17","H18","H19","H20","H21","H22","H23")
  PB_A <- as.data.frame(PB_A)
  PB_A <- completeFun(PB_A,"Fecha")
  PB_A$Fecha<- na.omit(PB_A$Fecha)
  meltPB <- melt(PB_A, id="Fecha")
  meltPB <- meltPB[order(as.Date(meltPB$Fecha, format="%Y-%m-%d")),]
  rownames(meltPB) <- NULL
  PB_Nac <- rbind(PB_Nac,meltPB)
}

#Lectura Ventas Bolsa NACIONAL

VB_A <- read_excel(paste("Ventas Bolsa Energia/Ventas_Bolsa_Nacional_(kwh)_","2003",".xlsx", sep = ""),skip=2)
VB_A <- as.data.frame(VB_A)
VB_A$Version <- NULL
VB_A <- completeFun(VB_A,"Fecha")
colnames(VB_A) <- c("Fecha","Agente","H0","H1","H2","H3","H4","H5","H6","H7","H8","H9","H10","H11","H12","H13","H14","H15","H16","H17","H18","H19","H20","H21","H22","H23")
VB_A$Agente <- NULL
meltVB <- melt(VB_A, id=c("Fecha"))
rownames(meltVB) <- NULL
meltVB[is.na(meltVB)] <- 0
meltVB <- aggregate(value~Fecha+variable, meltVB, sum)
meltVB <- meltVB[order(as.Date(meltVB$Fecha, format="%Y-%m-%d")),]
VB_Nac <- meltVB

for (i in 2:14) {
  VB_A <- read_excel(paste("Ventas Bolsa Energia/Ventas_Bolsa_Nacional_(kwh)_",Años[i],".xlsx", sep = ""),skip=2)
  VB_A <- as.data.frame(VB_A)
  VB_A$Version <- NULL
  VB_A <- completeFun(VB_A,"Fecha")
  colnames(VB_A) <- c("Fecha","Agente","H0","H1","H2","H3","H4","H5","H6","H7","H8","H9","H10","H11","H12","H13","H14","H15","H16","H17","H18","H19","H20","H21","H22","H23")
  VB_A$Agente <- NULL
  meltVB <- melt(VB_A, id=c("Fecha"))
  rownames(meltVB) <- NULL
  meltVB[is.na(meltVB)] <- 0
  meltVB <- aggregate(value~Fecha+variable, meltVB, sum)
  meltVB <- meltVB[order(as.Date(meltVB$Fecha, format="%Y-%m-%d")),]
  VB_Nac <- rbind.fill(VB_Nac,meltVB)
}

#Lectura Ventas Bolsa Internacional

VB_A <- read_excel(paste("Ventas Bolsa Energia/Ventas_Bolsa_Internacional_(kwh)_","2003",".xlsx", sep = ""),skip=2)
VB_A <- as.data.frame(VB_A)
VB_A$Version <- NULL
names(VB_A)[1]<-"Fecha"
names(VB_A)[2]<-"Agente"
VB_A <- completeFun(VB_A,"Fecha")
VB_A$Agente <- NULL
meltVB <- melt(VB_A, id=c("Fecha"))
meltVB[is.na(meltVB)] <- 0
meltVB <- aggregate(value~Fecha+variable, meltVB, sum)
meltVB <- meltVB[order(as.Date(meltVB$Fecha, format="%Y-%m-%d")),]
rownames(meltVB) <- NULL
VB_Int <- meltVB

for (i in 2:10) {
  VB_A <- read_excel(paste("Ventas Bolsa Energia/Ventas_Bolsa_Internacional_(kwh)_",Años_VBInt[i],".xlsx", sep = ""),skip=2)
  VB_A <- as.data.frame(VB_A)
  VB_A$Version <- NULL
  names(VB_A)[1]<-"Fecha"
  names(VB_A)[2]<-"Agente"
  VB_A <- completeFun(VB_A,"Fecha")
  VB_A$Agente <- NULL
  meltVB <- melt(VB_A, id=c("Fecha"))
  meltVB[is.na(meltVB)] <- 0
  meltVB <- aggregate(value~Fecha+variable, meltVB, sum)
  meltVB <- meltVB[order(as.Date(meltVB$Fecha, format="%Y-%m-%d")),]
  rownames(meltVB) <- NULL
  VB_Int <- rbind.fill(VB_Int,meltVB)
}

VB_Int$variable <- paste("H",VB_Int$variable,sep="")

#Lectura Compras Bolsa Nacional 
CB_A <- read_excel(paste("Compras Bolsa Energia/Compras_Bolsa_Nacional_(kWh)_","2003",".xlsx", sep = ""),skip=2)
CB_A <- as.data.frame(CB_A)
CB_A$Version <- NULL
CB_A <- completeFun(CB_A,"Fecha")
names(CB_A)[1]<-"Fecha"
names(CB_A)[2]<-"Agente"
CB_A$Agente <- NULL
meltVB <- melt(CB_A, id=c("Fecha"))
meltVB[is.na(meltVB)] <- 0
meltVB <- aggregate(value~Fecha+variable, meltVB, sum)
meltVB <- meltVB[order(as.Date(meltVB$Fecha, format="%Y-%m-%d")),]
rownames(meltVB) <- NULL
CB_Nac <- meltVB

for (i in 2:14) {
  CB_A <- read_excel(paste("Compras Bolsa Energia/Compras_Bolsa_Nacional_(kWh)_",Años[i],".xlsx", sep = ""),skip=2)
  CB_A <- as.data.frame(CB_A)
  CB_A$Version <- NULL
  CB_A <- completeFun(CB_A,"Fecha")
  names(CB_A)[1]<-"Fecha"
  names(CB_A)[2]<-"Agente"
  CB_A$Agente <- NULL
  meltVB <- melt(CB_A, id=c("Fecha"))
  meltVB[is.na(meltVB)] <- 0
  meltVB <- aggregate(value~Fecha+variable, meltVB, sum)
  meltVB <- meltVB[order(as.Date(meltVB$Fecha, format="%Y-%m-%d")),]
  rownames(meltVB) <- NULL
  CB_Nac <- rbind.fill(CB_Nac,meltVB)
}
CB_Nac$variable <- paste("H",CB_Nac$variable,sep="")

#Lectura Compras Bolsa Internacional
CB_A <- read_excel(paste("Compras Bolsa Energia/Compras_Bolsa_Internacional_(kWh)_","2003",".xlsx", sep = ""),skip=2)
CB_A <- as.data.frame(CB_A)
CB_A$Version <- NULL
CB_A <- completeFun(CB_A,"Fecha")
names(CB_A)[1]<-"Fecha"
names(CB_A)[2]<-"Agente"
CB_A$Agente <- NULL
meltVB <- melt(CB_A, id=c("Fecha"))
meltVB[is.na(meltVB)] <- 0
meltVB <- aggregate(value~Fecha+variable, meltVB, sum)
meltVB <- meltVB[order(as.Date(meltVB$Fecha, format="%Y-%m-%d")),]
rownames(meltVB) <- NULL
CB_Int <- meltVB

for (i in 2:9) {
  CB_A <- read_excel(paste("Compras Bolsa Energia/Compras_Bolsa_Internacional_(kWh)_",Años_CBInt[i],".xlsx", sep = ""),skip=2)
  CB_A <- as.data.frame(CB_A)
  CB_A$Version <- NULL
  CB_A <- completeFun(CB_A,"Fecha")
  names(CB_A)[1]<-"Fecha"
  names(CB_A)[2]<-"Agente"
  CB_A$Agente <- NULL
  meltVB <- melt(CB_A, id=c("Fecha"))
  meltVB[is.na(meltVB)] <- 0
  meltVB <- aggregate(value~Fecha+variable, meltVB, sum)
  meltVB <- meltVB[order(as.Date(meltVB$Fecha, format="%Y-%m-%d")),]
  rownames(meltVB) <- NULL
  CB_Int <- rbind.fill(CB_Int,meltVB)
}
CB_Int$variable <- paste("H",CB_Int$variable,sep="")

#Lectura Generacion
O_A <- read_excel(paste("Oferta/Generacion_(kWh)_","2003",".xlsx", sep = ""),skip=2)
O_A <- as.data.frame(O_A)
O_A$Version <- NULL
O_A$Recurso <- NULL
O_A$Combustible <- NULL
O_A$`Tipo Despacho`<- NULL
O_A$`Código Agente` <- NULL
O_A <- completeFun(O_A,"Fecha")
names(O_A)[1]<-"Fecha"
names(O_A)[2]<-"Tipo"
O_A[is.na(O_A)] <- 0
meltVB <- melt(O_A, id=c("Fecha","Tipo"))
meltVB <- meltVB[order(as.Date(meltVB$Fecha, format="%Y-%m-%d")),]
rownames(meltVB) <- NULL
CastO <- dcast(meltVB, Fecha + variable ~ Tipo,fun.aggregate = sum)
OfertaG <- CastO

for (i in 2:14) {
  O_A <- read_excel(paste("Oferta/Generacion_(kWh)_",Años[i],".xlsx", sep = ""),skip=2)
  O_A <- as.data.frame(O_A)
  O_A$Version <- NULL
  O_A$Recurso <- NULL
  O_A$Combustible <- NULL
  O_A$`Tipo Despacho`<- NULL
  O_A$`Código Agente` <- NULL
  O_A$`Es Menor` <- NULL
  O_A$`Es Autogenerador` <- NULL
  O_A <- completeFun(O_A,"Fecha")
  names(O_A)[1]<-"Fecha"
  names(O_A)[2]<-"Tipo"
  O_A[is.na(O_A)] <- 0
  meltVB <- melt(O_A, id=c("Fecha","Tipo"))
  meltVB <- meltVB[order(as.Date(meltVB$Fecha, format="%Y-%m-%d")),]
  rownames(meltVB) <- NULL
  CastO <- dcast(meltVB, Fecha + variable ~ Tipo,fun.aggregate = sum)
  OfertaG <- rbind.fill(OfertaG,CastO)
}
OfertaG$variable <- paste("H",OfertaG$variable,sep="")


#Lectura demanda por distribuidor
DD_A <- read_excel(paste("Demanda/Demanda_Comercial_Por_Distribuidor_","2003",".xlsx", sep = ""),skip=2)
DD_A <- as.data.frame(DD_A)
DD_A$Version <- NULL
DD_A$X__1 <- NULL
DD_A <- completeFun(DD_A,"Fecha")
names(DD_A)[1]<-"Fecha"
names(DD_A)[2]<-"Agente"
meltVB <- melt(DD_A, id=c("Fecha","Agente"))
meltVB <- meltVB[order(as.Date(meltVB$Fecha, format="%Y-%m-%d")),]
rownames(meltVB) <- NULL
CastDD <- dcast(meltVB, Fecha + variable ~ Agente)
DDist <- CastDD

for (i in 2:14) {
  DD_A <- read_excel(paste("Demanda/Demanda_Comercial_Por_Distribuidor_",Años[i],".xlsx", sep = ""),skip=2)
  DD_A <- as.data.frame(DD_A)
  DD_A$Version <- NULL
  DD_A <- completeFun(DD_A,"Fecha")
  names(DD_A)[1]<-"Fecha"
  names(DD_A)[2]<-"Agente"
  meltVB <- melt(DD_A, id=c("Fecha","Agente"))
  meltVB <- meltVB[order(as.Date(meltVB$Fecha, format="%Y-%m-%d")),]
  rownames(meltVB) <- NULL
  CastDD <- dcast(meltVB, Fecha + variable ~ Agente)
  DDist <- rbind.fill(DDist,CastDD)
}
DDist$variable <- paste("H",DDist$variable,sep="")

#Hidrologia APoRTES M3/s
ADH <- read_excel(paste("Hidrologia/Aportes_Diario_",Años[1],".xlsx", sep = ""),skip=2)
ADH <- as.data.frame(ADH)
ADH$`Nombre Río` <- NULL
ADH$`Aportes %` <- NULL
ADH$`Aportes Energía kWh` <- NULL
names(ADH)[1]<-"Fecha"
names(ADH)[2]<-"Region"
ADH[is.na(ADH)] <- 0
CastADH <- dcast(ADH, Fecha ~ Region,fun.aggregate = sum)
AporteM3 <- CastADH

for (i in 2:14) {
  ADH <- read_excel(paste("Hidrologia/Aportes_Diario_",Años[i],".xlsx", sep = ""),skip=2)
  ADH <- as.data.frame(ADH)
  ADH$`Nombre Río` <- NULL
  ADH$`Aportes %` <- NULL
  ADH$`Aportes Energía kWh` <- NULL
  names(ADH)[1]<-"Fecha"
  names(ADH)[2]<-"Region"
  ADH[is.na(ADH)] <- 0
  CastADH <- dcast(ADH, Fecha ~ Region,fun.aggregate = sum)
  AporteM3 <- rbind.fill(AporteM3,CastADH)
}
AporteM3$`RIOS ESTIMADOS` <- NULL
colnames(AporteM3) <- c("Fecha", "ANTIOQUIAM3","CARIBEM3","CENTROM3","ORIENTEM3","VALLEM3")


#Hidrologia ReservasMm3
ADH <- read_excel(paste("Hidrologia/Reservas_Diario_",Años[1],".xlsx", sep = ""),skip=2)
ADH <- as.data.frame(ADH)
varsRes <- c("Fecha","Region Hidrologica","Volumen Útil Diario Mm3")
ADH <- ADH[varsRes]
names(ADH)[1]<-"Fecha"
names(ADH)[2]<-"Region"
names(ADH)[3]<-"Volumen_Util_Mm3"
ADH[is.na(ADH)] <- 0
CastADH <- dcast(ADH, Fecha ~ Region,fun.aggregate = sum)
AporteMm3 <- CastADH

for (i in 2:14) {
  ADH <- read_excel(paste("Hidrologia/Reservas_Diario_",Años[i],".xlsx", sep = ""),skip=2)
  ADH <- as.data.frame(ADH)
  ADH <- as.data.frame(ADH)
  varsRes <- c("Fecha","Region Hidrologica","Volumen Útil Diario Mm3")
  ADH <- ADH[varsRes]
  names(ADH)[1]<-"Fecha"
  names(ADH)[2]<-"Region"
  names(ADH)[3]<-"Volumen_Util_Mm3"
  ADH[is.na(ADH)] <- 0
  CastADH <- dcast(ADH, Fecha ~ Region,fun.aggregate = sum)
  AporteMm3 <- rbind.fill(AporteMm3,CastADH)
}
colnames(AporteMm3) <- c("Fecha", "ANTIOQUIAMm3","CARIBEMm3","CENTROMm3","ORIENTEMm3","VALLEMm3")


#Hidrologia APoRTES kWh
ADH <- read_excel(paste("Hidrologia/Aportes_Diario_",Años[1],".xlsx", sep = ""),skip=2)
ADH <- as.data.frame(ADH)
ADH$`Nombre Río` <- NULL
ADH$`Aportes %` <- NULL
ADH$`Aportes Caudal m3/s` <- NULL
names(ADH)[1]<-"Fecha"
names(ADH)[2]<-"Region"
ADH[is.na(ADH)] <- 0
CastADH <- dcast(ADH, Fecha ~ Region,fun.aggregate = sum)
AportekWh <- CastADH

for (i in 2:14) {
  ADH <- read_excel(paste("Hidrologia/Aportes_Diario_",Años[i],".xlsx", sep = ""),skip=2)
  ADH <- as.data.frame(ADH)
  ADH$`Nombre Río` <- NULL
  ADH$`Aportes %` <- NULL
  ADH$`Aportes Caudal m3/s`<-  NULL
  names(ADH)[1]<-"Fecha"
  names(ADH)[2]<-"Region"
  ADH[is.na(ADH)] <- 0
  CastADH <- dcast(ADH, Fecha ~ Region,fun.aggregate = sum)
  AportekWh<- rbind.fill(AportekWh,CastADH)
}
AportekWh$`RIOS ESTIMADOS` <- NULL
colnames(AportekWh) <- c("Fecha", "ANTIOQUIAkWh","CARIBEkWh","CENTROkWh","ORIENTEkWh","VALLEkWh")


#Variables de interes PB_Int PB_Nac VB_Int VB_Nac CB_int CB_Nac OfertaG DDist
#Se Borran todas las demas variables 

rm(CastDD,CastO,CB_A,DD_A,meltPB,meltVB,O_A,PB_A,VB_A)

#lectura precio Dolar
Dolar <- read_excel("Dolar/Dolar_Historico.xlsx",skip=7)



#Creacion de Key para unir data frames
CB_Nac$FH <- paste(CB_Nac$Fecha,CB_Nac$variable,sep="")
CB_Int$FH <- paste(CB_Int$Fecha,CB_Int$variable,sep="")
DDist$FH <- paste(DDist$Fecha,DDist$variable,sep="")
OfertaG$FH <- paste(OfertaG$Fecha,OfertaG$variable,sep="")
PB_Int$FH <- paste(PB_Int$Fecha,PB_Int$variable,sep="")
PB_Nac$FH <- paste(PB_Nac$Fecha,PB_Nac$variable,sep="")
VB_Int$FH <- paste(VB_Int$Fecha,VB_Int$variable,sep="")
VB_Nac$FH <- paste(VB_Nac$Fecha,VB_Nac$variable,sep="")

colnames(CB_Int)[3] <- "CB_Int"
colnames(CB_Nac)[3] <- "CB_Nac"
colnames(PB_Int)[3] <- "PB_Int"
colnames(PB_Nac)[3] <- "PB_Nac"
colnames(VB_Int)[3] <- "VB_Int"
colnames(VB_Nac)[3] <- "VB_Nac"



#CB_Nac$Fecha <- NULL 
CB_Int$Fecha <- NULL 
DDist$Fecha <- NULL 
OfertaG$Fecha <- NULL 
PB_Int$Fecha <- NULL 
PB_Nac$Fecha <- NULL 
VB_Int$Fecha <- NULL 
VB_Nac$Fecha <- NULL

#CB_Nac$variable <- NULL 
CB_Int$variable <- NULL 
DDist$variable <- NULL 
OfertaG$variable <- NULL 
PB_Int$variable <- NULL 
PB_Nac$variable <- NULL 
VB_Int$variable <- NULL 
VB_Nac$variable <- NULL
#Union de todos los data frames
library(plyr)

EE <- join(CB_Nac,CB_Int)
EE <- join(EE,DDist)
EE <- join(EE,OfertaG)
EE <- join(EE,PB_Int)
EE <- join(EE,PB_Nac)
EE <- join(EE,VB_Int)
EE <- join(EE,VB_Nac)
#EE <- join(EE,AporteM3)
EE <- join(EE,AporteMm3)
EE <- join(EE,AportekWh)


EE <- join(EE,Dolar)

rm(ADH,AportekWh,AporteM3,CastADH,CB_Int,CB_Nac,DDist,OfertaG,PB_Int,PB_Nac,VB_Nac,VB_Int)


EEcor <- EE

EEcor$Fecha <- NULL
EEcor$FH <- NULL
EEcor$variable <- NULL


#NewDataPBN <- na.omit(PB_Nac)
#time <- time[-c(length(time-1)) ]
time <- seq(ISOdate(2003,01,01), ISOdate(2017,01,01), "hour")
time <- time[-c(length(time-1)) ]

EE$New <- time




write.csv(EE, file = "EE2.csv")