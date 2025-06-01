#DATA DISCOVERING
str(datos)
summary(datos)

library(nortest)
library(VIM)
library(naniar)


#Outliers
boxplot(datos$Ozone, main="Boxplot Ozone")
lillie.test(datos$Ozone) #No sigue 

outliers_ozone<- datos$Ozone > quantile(datos$Ozone, 0.99, na.rm=T) | datos$Ozone< quantile(datos$Ozone,0.01, na.rm=T)
factor<- ifelse(outliers_ozone== TRUE ,1,0)
suma_outliers_ozone<- sum(factor, na.rm=TRUE)
#4 posibles outliers
#Vamos a analizarlos para ver si verdaderamnete son outliers
valores_outliers_ozone<- ifelse(factor == 1, datos$Ozone, factor)
quantile(datos$Ozone, probs = c(0.01, 0.99), na.rm = TRUE)

#Los cuatro valores se consideran outliers
datos$Ozone<- ifelse(datos$Ozone== 4, NA, datos$Ozone)
datos$Ozone<- ifelse(datos$Ozone== 1, NA, datos$Ozone)
datos$Ozone<- ifelse(datos$Ozone== 135, NA, datos$Ozone)
datos$Ozone<- ifelse(datos$Ozone== 168, NA, datos$Ozone)

ss<- summary(datos$Ozone)
temp_sin_na<- na.omit(datos$Ozone)
q1<- ss[2]
q3<- ss[5]
IQR_val<- IQR(temp_sin_na)

lower_imit<- q1 -1.5*IQR_val
higher_limit<- q3 + 1.5*IQR_val

c(1,4,135, 168) < lower_imit | c(1,4,135, 168)  > higher_limit
#135 y 168 si son outliers.



#......
boxplot(datos$Solar.R, main="Boxplot Ozone")
lillie.test(datos$Temp) #No sigue 

outliers_temp<- datos$Temp > quantile(datos$Temp, 0.99, na.rm=T) | datos$Temp< quantile(datos$Temp,0.01, na.rm=T)
factor<- ifelse(outliers_temp== TRUE ,1,0)
suma_outliers_Temp<- sum(factor, na.rm=TRUE)
#3 posibles outliers
#Vamos a analizarlos para ver si verdaderamnete son outliers
valores_outliers_Temp<- ifelse(factor == 1, datos$Temp, factor)
quantile(datos$Temp, probs = c(0.01, 0.99), na.rm = TRUE)

ss<- summary(datos$Temp)
temp_sin_na<- na.omit(datos$Temp)
q1<- ss[2]
q3<- ss[5]
IQR_val<- IQR(temp_sin_na)

lower_imit<- q1 -1.5*IQR_val
higher_limit<- q3 + 1.5*IQR_val

c(56,97,96) < lower_imit | c(56, 97, 96) > higher_limit
#NO son outliers


#.......
boxplot(datos$Solar.R, main="Boxplot Ozone")
lillie.test(datos$Solar.R) #No sigue 

outliers_Solar.R<- datos$Solar.R > quantile(datos$Solar.R, 0.99, na.rm=T) | datos$Solar.R< quantile(datos$Solar.R,0.01, na.rm=T)
factor<- ifelse(outliers_Solar.R== TRUE ,1,0)
suma_outliers_Solar.R<- sum(factor, na.rm=TRUE)
#4 posibles outliers
#Vamos a analizarlos para ver si verdaderamnete son outliers
valores_outliers_Solar.R<- ifelse(factor == 1, datos$Solar.R, factor)

# Eliminar NAs
datos_solar_sin_na <- na.omit(datos$Solar.R)

sumary<- summary(datos$Solar.R)
Q1<- sumary[2]
Q3<- sumary[5]
IQR_val <- IQR(datos_solar_sin_na)

lower_limit <- Q1 - 1.5 * IQR_val
upper_limit <- Q3 + 1.5 * IQR_val

# Revisar si 7 o 332 están fuera de los límites
c(7,8, 332,334) < lower_limit | c(7,8, 332,334) > upper_limit
#NO hay outliers

#Analizar la sensibilidad de Ozone usando rdoublex.

#Resume por mes la media de ozone
media_ozone_mes<- datos%>% group_by(Month) %>% summarise(Media_ozone = mean(Ozone, na.rm=T))



#Visualiza la distribución de los valores NA usando un muestreo aleatorio del 1% del dataset.
filas_muestra <- datos[sample(1:nrow(datos), size = 0.01 * nrow(datos)), ]


#Imputar
miss_var_summary(datos)
datos<- kNN(datos, variable=c("Ozone", "Solar.R"), dist_var = c("Month", "Temp"), k=5)

#Anonimiza las columnas ID y StationID usando un hash irreversible (por ejemplo, SHA256).
anonimizado_datos<- datos %>% mutate(ID_Anonimizado= sapply(ID, function(x) digest(x, algo="sha256")),
                                     SATUSID_anonimiazo == sapply(SATIR, functio(X) difest(x, ))

                                     
#Privacidad diferencial
#Consulta 1: Calcula la media de Ozone para los meses 5 y 7.

library(smoothmest)
ozone_57<- datos%>% filter(Month %in% c(5,7))

max<- max(ozone_57$Ozone)
min<- min(ozone_57$Ozone)

media<- mean(ozone_57$Ozone)

sensibilidad<- (max - min) / nrow(ozone_57)
epsilon<-1
privacidad<- rdoublex(1, media, sensibilidad/epsilon)
#40.48

#Consulta 2: Calcula la proporción de observaciones donde Ozone > 100.
sensibilidad_proporcin<- 1/nrow(datos)
datos_100<- datos%>% filter(Ozone>100)
datos_reales<- nrow(datos_100) / nrow(datos)
privacidad<- rdoublex(1, datos_reales, sensibilidad/epsilon)





#* @apiTitle API AirQuality Iraia Laraudo
#* @apiDescription Consultas zonas con alta contamiancion de ozono
#* @get /ozone_query?threshold=
#* @param Numero
function(Numero){
  Numero<- as.numeric(Numero)
  media_ozone_statin<- datos%>% group_by(StationId) %>% summarise(MediaOzone=mean(Ozone))
  resultado<- media_ozone_statin %>% filter(MediaOzone> Numero) %>% select(StationID)
  return(resultado)
}