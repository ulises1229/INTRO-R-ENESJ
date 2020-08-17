### Alberto Prado 17 de agosto 2020
### Curso Intersemestral Introdicción a R UNAM ENES J
### Día 1 Introducción


rm(list=ls()) ## Eliminar todas las listas
## Este comando asegura que empazmos con el ambiente R vacío

setwd("~/Desktop/Curso R/2021-1") ## Dónde esta el directorio de trabajo

############## Objetos R #####################################
## El objeto R más sencillo es un valor
## a es 1
a <- 1
## al preguntar por a R nos contestará 1
a
#Podemos hacer operaciones con estos valores
b <- 2
a+b
c <- a+b
#La raíz cuadrada se obtiene con la función sqrt()
sqrt(2)
# El logaritmo natural se obtiene con la función log()
log(2)

# Vector
# Un vector es una colección de datos del mismo tipo.
X <-    c(1,2,3,4,5)
Y <-    c("Hugo", "Paco", "Luis")
X
2*X
# Se usan los [ ] para indexar la posición dentro de un objeto R.
Y[2]

# Matriz
# Una matriz es una colección de datos del mismo tipo en un número específico de filas y columnas.
mat  <-   matrix(1:9, nrow = 3, ncol = 3)
mat
# Al tener dos dimensiones requerimos dos numeros para indexar una posición
mat[2,1]

# Data Frame
# Un Data frame es una estructura bi-dimensional de datos, 
# dónde las filas son las observaciones y las columnas son las variables.  
# A diferencia de una matriz, las variables pueden ser de diferentes tipos de datos. 
dat <- data.frame (id = letters[1:10], x = 1:10, y = 11:20) 
dat
dat[ 5, 3]
dat[ 5, 1:3]

# Una función muy útil es str(), nos permite revisar la estructura de un elemento R
str(mat)
str(dat)

# Lista
# Una lista es un contenedor de cualquier tipo de objeto R, inclusive otras listas. 
# Se puede entender como un vector de cualquier tipo de dato u otro objeto R. 
x  <-  list(1, "a", TRUE, 2+4*a)
x
# El ambiente R es la lista que contiene a todas las otras listas, así como todos los objetos 

## Funciones 
## Una función es una secuencia de líneas que realizan una acción específica.
## Algunas funciones vienen ya con la librería base, miles de otras se pueden obtener al 
## descargar nuevas librerías. Otra opción es crear tus propias funciones.

### ¿Como crear una función?
### Usando la función "function" 
### Le asignamos un nombre a nuestra función
### Tenemos que especificar ¿qué va recibir como input?
### ¿qué va a hacer con ese input? 
### ¿qué me va a regresar?

fahrenheit_a_celsius <- function(temp_F) 
{ temp_C <- (temp_F - 32) * 5 / 9 
return(temp_C) }

fahrenheit_a_celsius(100)


suma <- function(x,y){
  resul <- x+y
  return(resul)
}
suma(1.23,3.45)


###############################################################
##############################################################
#### Importar datos ########################################

read.csv("iris.csv")

iris <- read.csv("iris.csv")

## Revisar estructura del Data Frame
str(iris)

## Eliminar columna llamada "X"
iris <- iris[,-1]

## cambiar nombres de las columnas
names(iris) # indica los nombres
names(iris) <- c("L.sepalo","A.sepalo","L.petalo", "A.petalo","Especie") 

names(iris)[2] <- "columna2"
str(iris)
names(iris)[2] <- "A.sepalo"

## Nombre o numero de fila
rownames(iris)

## Calcular Media, mediana y desviación estándar
mean(iris$L.petalo)
median(iris$A.sepalo)
sd(iris$A.petalo)


### numero de elementos por categoría
table(iris$Especie)

## Gráficos descriptivos básicas
# Histograma
hist(iris$L.sepalo)
str(iris)
#Gráfico de dispersión todas las variables
plot(iris)
# Escogiendo solo algunas variables
plot(iris[,3:5])
# Cleveland plot
dotchart(iris$L.sepalo)

# Gráfico de dispersion XY
plot(x=iris$L.sepalo, y=iris$A.sepalo)
# Color por especie
plot(x=iris$L.sepalo, y=iris$A.sepalo, col=iris$Especie)
# Cambiar títulos
plot(x=iris$L.sepalo, y=iris$A.sepalo, col=iris$Especie, 
     main= "Mi gráfica", xlab="Longitud del sepalo (cm)", ylab="Ancho del sepalo (cm)")

# caja y bigote
boxplot(iris$L.sepalo)
boxplot(iris$Long.sepalo~iris$Especie)

## Mas sobre la indexación 
## Podemos seleccionar secciones de nuestro data frame usando operadores relacionales
## "==" igual a
## ">" mayor a
## "|" OR
iris[iris$Especie=="setosa",]

setosa <- iris[iris$Especie=="setosa",]

mean(setosa$L.sepalo)
hist(iris[iris$Especie=="setosa",1])
hist(iris[iris$Especie=="setosa",]$Long.sepalo)

## agregar columna 
iris$col.nueva <- c(1,2,3,4)

#################################
## Ejercicio: Wage data set

Wage <- read.csv("Wage.csv")
