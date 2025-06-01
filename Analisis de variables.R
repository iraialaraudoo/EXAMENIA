#EXAMEN PRACTICA CHAT DATA SCIENCE RETO04

#Cargar librerias
library(dplyr)
library(lubridate)

#Cargar datos
data("airquality")
datos<- airquality

#Analizar datos
str(datos)
dim(datos)
head(datos)
summary(datos)

#Estructura variables
datos$Ozone<- as.numeric(datos$Ozone)
datos$Solar.R<- as.numeric(datos$Solar.R)
datos$Wind<- as.numeric(datos$Wind)
datos$Temp<- as.numeric(datos$Temp)
datos$Month<- as.numeric(datos$Month)
datos$Day<- as.numeric(datos$Day)


