rm(list=ls())
setwd("~/Desktop/Curso R/2021-1")

### Alberto Prado 19 de agosto 2021
### Curso Intersemestral R UNAM ENES J
### Día 3 MAnejo y análisis de datos 

################################################################
### Repaso Data Frame y uso de fechas en R
### Fechas
dat <- data.frame(nombres=c("Juan","Ines", "Pablo", "Igor"), 
                  calificación = c(8.6, 7.8, 6.9, 8.5), 
                  Fecha= c("1985-09-24", "1978-02-04", "1988-12-31", "1980-05-14"))
dat
str(dat)                   
## OJO la fecha esta interpretada como un factor
plot(dat$calificación~dat$Fecha)
# Cambiar tipo de dato a fecha
dat$Fecha <- as.Date(dat$Fecha)
str(dat)

plot(dat$calificación~dat$Fecha)

# ¿Como cambiar si la fecha esta en otro formato?
Fechas2 <- c("24-09-1985","04-02-1978","31-12-1988")
as.Date(Fechas2, tryFormats = "%d-%m-%Y")

########################################
#### Ordenar filas
# Se puede usar la funcion order() dentro de la indexación del DataFrame
dat1 <- dat[order(dat$nombres),]
dat2 <- dat[order(dat$calificación),]
dat2 <- dat[order(dat$calificación, decreasing=T),]
dat3 <- dat[order(dat$Fecha, decreasing=T),]

dat4 <- dat[order(dat$nombres, dat$calificación),]
dat4 <- dat[order(dat$nombres, -dat$calificación),]

### Ordenar columnas
str(dat)
dat6 <- dat[,c(3,1,2)]

#### Transposición
dat7 <- t(dat)
colnames(dat7) <- dat7[1,]
dat7 <- dat7[-1,]

######################################
#### Funciones de la Familia Apply

## Ejercicio 1
rm(list=ls())
concurso <- read.csv2("DataApply.csv")

row.names(concurso) <- paste(concurso$RefIndiv,concurso$Methode, sep="_")

concurso$RefIndiv <- NULL

## promedio por filas
concurso$promedio <- apply(concurso[,3:7], 1, mean, na.rm=T)

## promedio por columnas
apply(concurso[,3:7], 2, mean, na.rm=T)

############## sapply ###################################
### sapply == apply(x,2,FUN)

sapply(concurso[,3:7], mean, na.rm=T)

#### tapply #######################
tapply(concurso$Examinateur1,concurso$Animal, mean, na.rm=T)


###########################################################

#install.packages("reshape2")
library("reshape2")

#### Ejercicio 2  

conc_largo <- melt(concurso[,1:7], id.vars=c("Animal","Methode"))
str(conc_largo)

tapply(conc_largo$value, list(conc_largo$Animal,conc_largo$variable), 
       mean, na.rm=T)

tapply(conc_largo$value, list(conc_largo$Animal,conc_largo$variable,
                              conc_largo$Methode), mean, na.rm=T)


###################################################################

#############################################
#### Ejercicio 3

calabazas <- read.csv("calabazas.csv")
str(calabazas)
tapply(calabazas$VitC, list(calabazas$Cult,calabazas$Date),mean)


