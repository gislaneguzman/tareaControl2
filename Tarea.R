setwd("C:/Users/Gislane/Desktop/tarea")
file <- paste0(getwd(),"/EMPRESAS2.csv")
data<- read.csv2(file=file,head=TRUE)
str(data)

#Media de las variables (obtenerlas)
Prparau <- mean(data$RPARAU)
Prqui <- mean(data$RQUI)
Prsk <- mean(data$RSK)
Prigpa <- mean(data$RIGPA)

# Desviacion estandar de las variables (obtenerlas)
sdparau <- sd(data$RPARAU)
sdqui <- sd(data$RQUI)
sdsk <- sd(data$RSK)
sdigpa <-  sd(data$RIGPA)

# Correlación entre los retornos
x <- data.frame(data$RPARAU,data$RQUI,data$RSK,data$RIGPA)
Mcorr <- cor(x)

# Resgresiones lineales
lmparau <- lm(data$RPARAU ~ data$RIGPA , data=data)
summary(lmparau)

lmqui <- lm(data$RQUI ~ data$RIGPA , data=data)
summary(lmqui)

lmsk <- lm(data$RSK ~ data$RIGPA , data=data)
summary(lmsk)


# se puede instalar para graficar
#install.packages("ggplot2")
#library(ggplot2)

# grafico de variables respecto a la fecha

qplot (data$fecha1,data$RPARAU,data=data)
qplot (data$fecha1,data$RQUI,data=data)
qplot (data$fecha1,data$RSK,data=data)
qplot (data$fecha1,data$RIGPA,data=data)

qplot (data$fecha1,data$PRPARAU,data=data)
qplot (data$fecha1,data$PRQUI,data=data)
qplot (data$fecha1,data$PRSK,data=data)
qplot (data$fecha1,data$PRIGPA,data=data)