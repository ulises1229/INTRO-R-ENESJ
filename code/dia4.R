# Autor: Ulises Olivares
# uolivares@unam.mx
# 20 Agosto de 2020

#install.packages("ggplot2")

# ruta IMAC 
setwd("/Volumes/OneDrive/OneDrive - UNAM/0. UNAM - Juriquilla/0. MATERIAS ENESJ/2. Cursos intersemestrales/2021-1/Módulo 1. Introducción a R/Presentaciones/1_Ulises/Día 4")

library(ggplot2)

consumo <- read.csv("mpg.csv")

str(consumo)

# Generar un diagrama de dispersión
ggplot(data = consumo, aes(x = displ, y = hwy)) +
  geom_point()

# Representación equivalente
ggplot(data = consumo ) + 
  geom_point(aes(x = displ, y = hwy))

# Renombrar variables
names(consumo) <-  c("num","fabricante", "modelo", "desp", "año", 
                     "cil", "trans", "trac", "ciudad", "autop", "comb", "clase")

# desp vs autop
ggplot(data = consumo, aes(x = desp, y = ciudad)) +
  geom_point()

# Cilindros vs autopista
ggplot(data = consumo, aes(x = cil, y = autop)) +
  geom_point()

# Fabricante vs mpg autopista
ggplot(data = consumo, aes(x = fabricante, y = autop)) +
  geom_point()

# Clase vs transmisión
ggplot(data = consumo, aes(x = clase, y = trans)) +
  geom_point()

# Agregar una estética (color) con títulos
ggplot(data = consumo, aes(x = desp, y = autop, colour = clase)) + 
  geom_point() + 
  labs(x = "Desplazamiento (lts)", y = "Rendimiento en autopista (mpg)", title = 
         "Desplazamiento vs Autopista")

# utilizando dos estéticas
ggplot(data = consumo, aes(x = desp, y = autop, size = autop, colour = clase))+
  geom_point()+
  labs(x = "Desplazamiento (lts)", y = "Rendimiento en autopista (mpg)", 
       title = "Desplazamiento vs Autopista")

# Utilizando estética alpha
ggplot(data = consumo, aes(x = desp, y = autop, alpha = desp))+
geom_point()+
  labs(x = "Desplazamiento (lts)", y = "Rendimiento en autopista (mpg)", 
       title = "Desplazamiento vs Autopista")

# Utilizando estética forma 
ggplot(data = consumo, aes(x = desp, y = autop, shape = clase))+
  geom_point()+
  labs(x = "Desplazamiento (lts)", y = "Rendimiento en autopista (mpg)", 
       title = "Desplazamiento vs Autopista")

# Asignar una estética a todo el gráfico
ggplot(data = consumo, aes(x = desp, y = autop))+
  geom_point(size = 1)+
  labs(x = "Desplazamiento (lts)", y = "Rendimiento en autopista (mpg)", 
       title = "Desplazamiento vs Autopista")

# Asiganar una condición a una estética
ggplot(data = consumo, aes(x = desp, y = autop, color = desp<=5))+
  geom_point()+
  labs(x = "Desplazamiento (lts)", y = "Rendimiento en autopista (mpg)", 
       title = "Desplazamiento vs Autopista")

# Agregar dos nuevas columnas al df en km/lt
consumo$km_autop <- consumo$autop / 2.352
consumo$km_ciudad <-  consumo$ciudad / 2.352

# Gráfico Desplazamiento vs km/lt autopista
ggplot(data = consumo, aes(x = desp, y = km_autop))+
  geom_point()

# Gráfico Desplazamiento vs km/lt ciudad
ggplot(data = consumo, aes(x = desp, y = km_ciudad))+
  geom_point()

# Agregando facetas al gráfico
ggplot(data = consumo, aes(x = desp, y = km_ciudad, color = clase))+
  geom_point() + 
  facet_wrap(~ clase, nrow = 2) 

# Utilizando grids en el gráfico
ggplot(data = consumo, aes(x = desp, y = km_ciudad, color = clase))+
  geom_point() +
  facet_grid(cil ~ trac)

# importar data frame iris
iris2 <- read.csv("iris.csv")

names(iris2) <- c("X", "long_sep", "ancho_sep", "long_pet", "ancho_pet", "Especie")

# Generar gráfico longitud sépalo vs longitud pétalo
ggplot(data = iris2, aes(x = long_sep, y=long_pet, color = Especie))+
  geom_point()+
  facet_grid(.~Especie)

# importar datos de temperatura
temp <- read.csv("temp.csv")

# traducir nombres
names(temp ) <- c("Fuente", "Año", "avg")

str(temp)

#gráfico de línea
ggplot(data = temp, aes(x = Año, y = avg, color = Fuente))+
  geom_line()+
  geom_point()+
  facet_grid(Fuente~.)

# Regresión líneal
ggplot(data = consumo, aes(x = desp, y = autop))+
  geom_point() + 
  geom_smooth(method = "lm")

# Regresión polinomial
ggplot(data = consumo, aes(x = desp, y = autop))+
  geom_point() + 
  geom_smooth()


# agragar estéticas a  geom_smoth()
ggplot(data = consumo, aes(x = desp, y = autop, ))+
  geom_point(aes(color = clase)) + 
  geom_smooth()

# Consultar ayuda de geom_smoth
?geom_smooth

# Combinar estéticas y facetas con gráficos de dispersión, línea y tendencia. 
ggplot(data = consumo, aes(x = desp, y = autop, color = clase))+
  geom_point() + 
  geom_smooth() + 
  facet_wrap(~clase)

# Generación de boxplot
ggplot(data = consumo, aes(x = clase, y = autop, fill = clase))+
  geom_boxplot(notch = TRUE)+
  geom_jitter(aes(color = clase), alpha = 0.5)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red", fill="red")

# Empleando la librería tidyverse para hacer un filtrado de datos
install.packages("tidyverse")

#  cargar libraría
library(tidyverse)

# Convertir un data frame (df) a un tibble.
consumo2 <- as_tibble(consumo)

# generar un nuevo df pickup 
pickup <- consumo2 %>% filter(consumo2$clase == "pickup")

# Graficar la función de densidad de probabilidad de pickup$autop
plot(density(pickup$autop))

# Generar un nuevo df para automóviles compactos
compactos <- consumo2 %>% filter(consumo2$clase == "compact")

# Generar un boxplot por marca vs rendimiento en autopista
ggplot(data = compactos, aes(x = fabricante, y= autop, fill = fabricante))+ 
  geom_boxplot(notch = TRUE)+ 
  geom_jitter(aes(color = fabricante), alpha = 0.5)+
  coord_flip()

# Importar datos de covid-19 
covid <- read.csv("https://raw.githubusercontent.com/ulises1229/INTRO-R-ENESJ/master/datos/covid19.csv")

# Transformar a tibble
covid2 <- as_tibble(covid)

str(covid2)

# Generar un nuevo df con los registros de México
mexico <- covid2 %>% filter(covid2$Country == "Mexico")

# Convertir fecha a formato de fecha
mexico$Date_reported <- as.Date(mexico$Date_reported)

# Revisar estructura
str(mexico)

# Generar un gráfico de línea
ggplot(mexico, aes(x = Date_reported, y = Cumulative_cases, color = Country))+
  geom_line()

# Generar un conjunto de datos para México, USA, Brasil, Francia y España
top5 <- covid2 %>% filter(covid2$Country == "Mexico" | 
                            covid2$Country == "Brazil" |
                            covid2$Country == "France" |
                            covid2$Country == "United States of America" |
                            covid2$Country == "Spain")

# Convertir a formato de fecha
top5$Date_reported <-  as.Date(top5$Date_reported)

# Generar un gráfico de línea para el top5
ggplot(data = top5, aes(x = Date_reported, y = Cumulative_cases, color = Country))+
  geom_line()

#########################
# Diagramas de barra

diamantes <- read.csv("https://raw.githubusercontent.com/ulises1229/INTRO-R-ENESJ/master/datos/diamonds.csv")

# Importar df
diamantes <- diamonds

# Diagrama de barras
ggplot(diamantes, aes(x = cut, fill = clarity))+
  geom_bar()+
  coord_flip()










