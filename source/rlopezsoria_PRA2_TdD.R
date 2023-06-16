
#Código para el análisis del CSV práctica PR2


#Se instala y se cargan los paquetes necesarios
#Lectura de CSV cambiando formatos
install.packages("tidyverse")
install.packages("rminer")
install.packages("caret")
library(tidyverse)

#Librería para representación gráfica de dataframes
library(ggplot2)

#Librería arules
library(arules)

#Librería rminer
library(rminer)

library(caret)

#Se utiliza crea la variable temporal y se carga el archivo a estudiar. 
#El archivo debe estar en la misma carpeta donde se ejecuta el programa
ArchivoCSV <- tempfile()
ArchivoCSV = "heart.csv"


datosHeart <- read.csv(file = ArchivoCSV, header = TRUE, sep = ",")

#Una vez obtenidos los datos en un dataframe se vala a visualizar su contenido
summary(datosHeart)


#Se hace un estudio previo para comprobar si los datos  no tienen valores inválidos o nulos.
colSums(is.na(datosHeart))

#Con esto se muestra que no se tienen valores nulos o inválidos

#Comprobamos si hay duplicados
sum(duplicated(datosHeart))

#Se observa que hay un duplicado, por lo que usamos la función unique para eliminar el duplicado
datosHeartUnique = unique(datosHeart)


#Se pueden observar que hay columnas que no están definidas, estas son 'oldpeak', 'slp',
# y 'thall'. Por tanto se eliminarán estas columnas, ya que no pueden formar parte de un 
#estudio sin estar definidas por el dataset cual es su descripción.

datosHeartUniqueLess = subset(datosHeartUnique, select = -c(oldpeak, slp, thall))

summary(datosHeartUniqueLess)


#Se eliminan datos que tienen como caa superior a 3, ya que según enunciado esto no tiene sentido, está fuera de rango
datosHeartUniqueLess_1 <- datosHeartUniqueLess[datosHeartUniqueLess$caa < 4, ]

summary(datosHeartUniqueLess_1)

#Para averiguar los outlier
outlier <- boxplot(datosHeartUniqueLess_1$chol, main = "Colesterol")
outlier$out

#Se elimina el dato que contiene un colesterol de 564
datosHeartUniqueLess_1 <- datosHeartUniqueLess_1[datosHeartUniqueLess_1$chol < 564 , ]

outlier <- boxplot(datosHeartUniqueLess_1$trtbps, main = "Pres Arterial")
outlier$out

#No se quita ninguno de presión arterial

outlier <- boxplot(datosHeartUniqueLess_1$thalachh, main = "FreqCarMax")
outlier$out
#Se elimina el valor por debajo de 71, hay un outlier
datosHeartUniqueLess_1 <- datosHeartUniqueLess_1[datosHeartUniqueLess_1$thalachh > 71 , ]




#Se realiza una normalización de las variables numéricas, 'trtbps', 'chol' y 'thalachh'

x <- datosHeartUniqueLess_1[,c(4,5,8)]
#Se realiza un escalado con especial cuidado de no cambiar el centro puesto que el valor, si es positivo o
#negativo es importante a la hora de realizar los análisis, 
#al igual que no tendría sentido un valores negativos.
x <- scale(x,scale = TRUE, center = FALSE)

datosAnalisis = datosHeartUniqueLess_1
datosAnalisis[,c(4,5,8)] <- x

summary(datosAnalisis)


#Se añade una columna que identifique con una frase el estado del output
datosAnalisis$resultado <- ifelse(datosAnalisis$output == 1, "Ataque","No Ataque")
datosAnalisis$sexo <- ifelse(datosAnalisis$sex == 0, "Hombre","Mujer")

#Buena referencia para realizar gráficos con ggplot 
#https://rpubs.com/dsulmont/37913

ggplot(data=datosAnalisis,aes(x=sexo,fill= resultado))+geom_bar(position = "fill")+ylab("Porcentaje")

ggplot(data=datosAnalisis,aes(x=cp,fill=resultado))+geom_bar()+ylab("Frecuencia")
ggplot(data=datosAnalisis,aes(x=cp,fill=resultado))+geom_bar(position = "fill")+ylab("Porcentaje")

ggplot(data=datosAnalisis,aes(x=fbs,fill=resultado))+geom_bar(position = "fill")+ylab("Porcentaje")
ggplot(data=datosAnalisis,aes(x=restecg,fill=resultado))+geom_bar(position = "fill")+ylab("Porcentaje")

#Gráfico de correlación entre variables no discretas.
plot(datosAnalisis[,c(1,4,5,11)])

#Discretización por mediante clustering del colesterol
hist(datosAnalisis$chol, breaks = 20, main = "Colesterol")
abline(v=discretize(datosAnalisis$chol, method = "cluster", breaks = 3, onlycuts = TRUE))

nomColValor = c("Bajo","Medio","Alto")
v=discretize(datosAnalisis$chol, method = "cluster", breaks = 3, labels = nomColValor)
summary(v)

#Se añade al dataframe
datosAnalisis$Colesterol = v

#A continuación hacemos un gráfico.
ggplot(data=datosAnalisis,aes(x=Colesterol,fill= resultado))+geom_bar(position = "fill")+ylab("Porcentaje")
ggplot(data=datosAnalisis,aes(x=Colesterol,fill= resultado))+geom_bar()+ylab("Frecuencia")


#Discretización por mediante clustering de la frecuencia cardiaca
hist(datosAnalisis$thalachh, breaks = 20, main = "Freq.Cardiaca")
abline(v=discretize(datosAnalisis$thalachh, method = "cluster", breaks = 3, onlycuts = TRUE))

v=discretize(datosAnalisis$thalachh, method = "cluster", breaks = 3, labels = nomColValor)
summary(v)

#Se añade al dataframe
datosAnalisis$Frecuencia = v

#A continuación hacemos varios gráficos
ggplot(data=datosAnalisis,aes(x=Frecuencia,fill= resultado))+geom_bar(position = "fill")+ylab("Porcentaje")
ggplot(data=datosAnalisis,aes(x=Frecuencia,fill= resultado))+geom_bar()+ylab("Frecuencia")


#Comprobación de Normalidad
shapiro.test(datosAnalisis$thalachh)


#División para el muestreo
h <- holdout(datosAnalisis$resultado, ratio = 2/3, mode = "stratified")
data_train <- datosAnalisis[h$tr,]
data_test <- datosAnalisis[h$ts,]
print(table(data_train$resultado))
print(table(data_test$resultado))

#Uso de la función predict para predecir el resultado con los datos del
#subconjunto dividido
data_train_y = data_train$resultado
data_train_x = data_train[,1:10]

data_test_x = data_test[,1:10]
data_test_y = data_test$resultado

train_control <- trainControl(method = "cv", number = 4)
mod <- train(data_train_x, data_train_y, method="rf", trControl = train_control)

pred = predict(mod, newdata = data_test_x)
df1 <- factor(pred)
df2 <- factor(data_test_y)
confusionMatrix(df1, df2)


