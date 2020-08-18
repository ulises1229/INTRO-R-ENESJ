rm(list=ls())
setwd("~/Desktop/Curso R/2021-1")

### Alberto Prado 18 de agosto 2021
### Curso Intersemestral R UNAM ENES J
### Estasdistica Descriptiva


###### Ejercicio de repaso
mtcars <- read.csv("mtcars.csv") ## cargar archivo
str(mtcars) # estructura del objeto R
plot(mtcars[,2:5]) #gráficas explorativas
plot(mtcars[,c(2,5,6,7,9)])

row.names(mtcars) <- mtcars$X # Usar datos de la columna X como nombres de las filas
mtcars <- mtcars[,-1] # Eliminar columna X 

# Cambiar nombres de columnas
names(mtcars) <- c("mpg","num.cilindros","Desp","CF","drat","peso","qsec","vd","am","velocidades","carb")

# Histograma de MPG
hist(mtcars$mpg)

# Gráfico XY de CF en función del peso del auto
plot(CF~peso, data=mtcars)
# Agregar color por automatico o manual
plot(CF~peso, data=mtcars, col=(mtcars$am))
# Ojo como am esta codificado con 0s y 1s, el color 0 es nulo
# Solución agregar un 1 a am
plot(CF~peso, data=mtcars, col=(mtcars$am+1))

##########################################
# Ejercicio 1
## Caja y bigote
# Función boxplot
boxplot(peso~am, data=mtcars)
boxplot(peso~am, data=mtcars, notch=TRUE)

# Cambiar nombres de los niveles de un factor
mtcars$am <- factor(mtcars$am, labels=c("automático", "manual"))
boxplot(peso~am, data=mtcars, col=c("orange","blue"))

# Cambiar los niveles de un factor
mtcars$am <- factor(mtcars$am, levels=c( "manual", "automático"))
boxplot(peso~am, data=mtcars, col=c("blue","purple"))


#######################
# Ejercicio 2
## Prueba T Student para comparar medias

t.test(peso~am, data=mtcars)

# Prueba de homogeneidad de varianzasa
var(mtcars[mtcars$am=="automático","peso"])
bartlett.test(peso~am, data=mtcars)

## Caja y bigote
boxplot(CF~vd, data=mtcars, notch=TRUE)

# Histograma
hist(mtcars$CF, main="Histograma hist()", xlab="Caballos de Fuerza", ylab="Frecuencia", xlim=c(0,350))

## Cleavland plot
dotchart(mtcars$CF, groups = mtcars$am, main="Cleveland Plot dotchart()", xlab="Caballos de Fuerza")

#####################
# Ejercicio 3
## Leer archivo cars.csv
cars <- read.csv2("cars.csv")
str(cars) # revisar estructura

dotchart(cars$speed)
hist(cars$speed)
plot(density(cars$speed), main="Función de densidad de probabilidad de la velocidad") 

dotchart(cars$dist)
hist(cars$dist)
plot(density(cars$dist), main="Función de densidad de probabilidad de la distacia de frenado") 


###########################################
# Una librería es un código compilado que define funciones 
# con las cuales no contamos. 
# Las librerías incluyen su manual de uso y son revisadas y actualizadas. 

### ¿Cómo cargar librerías?

## La librería MASS es una librería de funciones para estadística
install.packages("MASS")
library("MASS")

########################################
#### Ejercicio 4

# Ajustar distribución normal
# 2 parámetros media y la desvición estandar
fitdistr(cars$speed,"normal")

############# 
## Ejercicio 5
# Instalar librería visualize para visualizar distribuciones
install.packages("visualize")
library("visualize")

# Visualizar distribución normal
visualize.norm(mu = 15.4, sd = 5.2345, stat=25, section="upper")


### Estandarización de datos
## Ejercicio 7
# Histograma
hist(cars$dist)

# Crear nueva columna
cars$dist.estandar <- (cars$dist- mean(cars$dist)) / sd(cars$dist)
# Histograma de datos estandarizados
hist(cars$dist.estandar)
# Ajustar curva normal
fitdistr(cars$dist.estandar,"normal")

visualize.norm(mu = 1.2583e-16, sd = 9.899e-01, stat=(20-mean(cars$dist))/sd(cars$dist), section="lower")

############################





###################################################################
###################################################################
##############################################
### Covarianza y correlación

cov(cars$dist,cars$speed) 
cor(cars$dist,cars$speed)   

cor(mtcars[,1:7])


###############
### Ejercicio 9
## advertising dataset
pub <- read.csv("advertising.csv")
cor(pub)

########
## Regresiones lineales

lm0 <- lm(cars$dist~cars$speed)
lm0
summary(lm0)
plot(cars$dist~cars$speed)
abline(a=-17.58, b=3.93)
text(x=5,y=110,"r2=0.64", cex = 0.8)

###############################
## Ejercicio 10
# Modelo lineal 1
lm1 <- lm(pub$Sales~pub$TV)
lm1
summary(lm1)
# Gráfica de ventas en funcion de publicidad TV
plot(pub$Sales~pub$TV)
# Agregar modelo
abline(a=6.974821,b=0.05546)
# Agreagr coeficiente de determinacion
text(x=25,y=25,"r2=0.81")

# Modelo lineal 2
lm2 <- lm(pub$Sales~pub$Radio)
lm2
summary(lm2)
# Gráfica de ventas en función de publicidad Radio
plot(pub$Sales~pub$Radio)
# Agregar modelo
abline(a=12.2357,b=0.1244)
# Agregar ceof determinación
text(x=5,y=25,"r2=0.12")

# Modelo lineal 3
lm3 <- lm(pub$Sales~pub$Newspaper)
lm3
summary(lm3)
# Gráfica de ventas en función de publicidad Radio
plot(pub$Sales~pub$Newspaper)
# Agregar modelo
abline(a=13.9595,b=0.03832)
# Agregar coef determinación
text(x=95,y=4,"r2=0.02")

#### colores y lineas
# Vamos a graficar dos modelos uno donde se obliga  la linea 
# a tener intercepto 0 y otro no 

plot(pub$Sales~pub$Radio, col="cornflowerblue", pch=20, ylim=c(0,25))
# Modelo lineal libre
abline(a=12.2357,b=0.1244, col="red", lty=2)

# Modelo obligado a pasar por el cero
lm4 <- lm(pub$Sales~ 0 + pub$Radio)
lm4
abline(a=0,b=0.4987, col="darkgreen", lty=1)

legend(0.5, 25, legend=c("Modelo 1", "Modelo 2"), col=c("red", "darkgreen"), lty=c(2,1), cex=0.5)


