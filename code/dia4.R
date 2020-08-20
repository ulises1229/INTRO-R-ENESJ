# -----------------------------
# Author Ulises Olivares Pinto
# ENES Unidad Juriquilla
# uolivares@unam.mx
# -----------------------------

# Importar librería ggplot2
library(ggplot2)

#install package 
#install.packages('dplyr')
library(dplyr)

# Limpiar ambiente de R (memoria RAM) 
rm(list=ls())

consumo <- mpg

# Detalles sobre el dataframe
?mpg

# Estadísticos sencillos del data frame
summary(consumo)

## Generar sistema de coordenadas 
ggplot(data = consumo)

# Número de filas y columas del data frame
nrow(consumo)
ncol(consumo)



########################################
# Gráficos de dispersión
########################################

# Generar un gráfico de dispersión sencillo desplazamiento vs rendimiento en autopista
ggplot(data = consumo, aes(x = desp, y = autop )) +
  geom_point()

#########################################
# Estéticas
########################################
    
    ggplot(data = consumo, aes(x = desp, y = autop, color = clase)) +
      geom_point()
    
    # Tamaño
    ggplot(data = consumo, aes(x = desp, y = autop, size = clase)) +
      geom_point()
    
    # Transparencia
    ggplot(data = consumo, aes(x = desp, y = autop, alpha = clase)) +
      geom_point()
    
    # Formas
    ggplot(data = consumo, aes(x = desp, y = autop, shape = clase)) +
      geom_point()
    
    # Un solo color
    ggplot(data = consumo, aes(x = desp, y = autop)) +
      geom_point(color = "blue")
    
    # ##############

#######################################
# Facetas
######################################

# Facetas con dos filas 
ggplot(data = consumo) + 
      geom_point(aes(x= desp, y= autop))+
      facet_wrap(~ clase, nrow = 2)
    
# Grids cil ~ trac
   ggplot(data = consumo) + 
      geom_point(aes(x= desp, y= autop))+
      facet_grid(cil ~ trac)
# Grids con un solo dato
  ggplot(data = consumo) + 
      geom_point(aes(x= desp, y= autop))+
      facet_grid(. ~ cil)
  
    
# 4- Gráfico con títulos
plot1 <- ggplot(data = consumo) + 
  geom_point(mapping = aes(x = displ, y = hwy, color=class))+
  labs(
    x = "Desplazamiento",              # título del eje x
    y = "Rendimiento autopista",   # t??tulo del eje y
    title = "Desplazamiento vs Rendimiento en autopista",   # t??tulo principal de la figura
    color = "Clase de automovil"   # t??tulo de la leyenda
    )+  
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), plot.title = element_text(hjust = 0.5))
ggsave(filename = "consumo.png", plot = plot1, width = 16, height = 9, dpi = 300, units = "cm")

milage <- consumo

# Anotaciones en gráfico
best_in_class <- consumo 
  group_by(milage$class) 
  filter(row_number(desc(milage$hwy)) == 1)
  
  ggplot(consumo, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
    geom_text(aes(label = model), data = best_in_class)
  
ggplot(data = consumo) + 
  geom_point(mapping = aes(x = cyl, y = hwy))

ggplot(data = consumo) + 
  geom_point(mapping = aes(x = year, y = hwy))

ggplot(data = consumo) + 
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = consumo) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))

ggplot(data = consumo) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class, size = cyl))


?geom_point


ggplot(data = consumo) + 
  geom_point(mapping = aes(x = drv, y = cyl))

ggplot(data = consumo) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = consumo) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

ggplot(data = consumo) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

ggplot(consumo, aes(displ, hwy), color=consumo$class) +
  geom_point() +
  geom_smooth(method = "lm")
  

?geom_smooth
?mehtod
?surveys_complete
  

?diamonds

diamonds <- diamonds  

?geom_bar

ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

diamonds <- diamonds

ggplot(data = diamonds) + 
  geom_histogram(aes(x = price , fill = cut )) +
  labs(x = "Precio", y = "Número de elementos")

casos <- as_tibble(read.csv("casos.csv"))

covid <- as_tibble(read.csv("covid19.csv"))

top5 <- covid %>% filter(Country_code == "MX" | Country_code == "US" | Country_code == "BR" | Country_code == "ES" | Country_code == "FR")

top5$Date_reported <-  as.Date(top5$Date_reported)

ggplot(data=top5) + 
  geom_line(aes(x= Date_reported, y = Cumulative_cases, color = Country))+
  labs(x = "Fecha", y = "Casos Acumulados", title = "Casos acumulados por país")
  


bar + coord_flip()
bar + coord_polar()


ggplot(data = consumo) +
  geom_line(aes(x = clase, y= ciudad))


########################
## Gráficos de línea
########################

################################
# Temperatura media de la tierra
################################
setwd("/Volumes/OneDrive/OneDrive - UNAM/0. UNAM - Juriquilla/0. MATERIAS ENESJ/2. Cursos intersemestrales/2021-1/Módulo 1. Introducción a R/Presentaciones/1_Ulises/Día 4")
temp <- read.csv("temp_anual.csv")

names(temp) <- c("Fuente", "Año", "Media")

# Gráfico de línea
ggplot(data=temp) +
  geom_line(aes(x = Año, y=Media, color = Fuente))
  
# Agregar títulos y uso de facetas
ggplot(data=temp) +
  geom_line(aes(x = Año, y=Media, color = Fuente))+
    geom_point(aes(x = Año, y=Media, color = Fuente))+
  labs(title = "Temperatura media de 1980 a 2016", x = "Año", y = ("Temperatura media en ºC")) +
  facet_wrap(temp$Fuente)

#################
# Regresiones
#################

# Regresión lineal
ggplot (data=consumo, aes(x = desplazamiento, y = autopista))+
  geom_point()+
  geom_smooth(method = "lm")
  
# Regresión polinomial
ggplot (data=consumo, aes(x = desplazamiento, y = autopista))+
  geom_point()+
  geom_smooth()
  

#########################################
# 3.3 Boxplots
########################################
ggplot(data=consumo, aes(x =clase, y=autopista, , fill= clase))+
  geom_boxplot()

# Boxplot extras geom_jitter
ggplot(data=consumo, aes(x =clase, y=autopista, , fill= clase))+
  geom_boxplot(notch = TRUE)+
  geom_jitter(aes(color = clase), alpha= 0.8)


?geom_boxplot()

# demonstrate usage of tidyverse 
#install.packages(tidyverse)
library(tidyverse)

data <- as_tibble(consumo)

str(data)

pickup <- data %>% filter(consumo$clase == "pickup")

plot(density(pickup$autopista))



compactos <- data %>% filter(consumo$clase == "compact")








