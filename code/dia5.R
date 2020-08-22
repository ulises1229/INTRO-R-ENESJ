# Autor: Ulises Olivares
# uolivares@unam.mx
# 21 de agosto de 2020


# Descargar el set de datos 
sueño <- read.csv("https://raw.githubusercontent.com/ulises1229/INTRO-R-ENESJ/master/datos/sleep.csv")

# Transformar Subject a factor
sueño$Subject <- as.factor(sueño$Subject)

# Revisar estructura de sueño
str(sueño)

## Generar un diagrama de dispersión de los datos de sueño
ggplot(sueño, aes(x = Days, y = Reaction, color = Subject))+
  geom_point()+
  geom_smooth(method= "lm")+
  facet_wrap(~Subject)

# Ayuda de geom_smooth
?geom_smooth

# Importar librería nlme
library(nlme)
via <- Rail

#Exportar csv
write.csv(via, "rail.csv")

# Generar gráfico de dispersión
ggplot(via, aes(x = Rail, y = travel, color = Rail))+
  geom_point()

# Generar modelo lineal de via
railLm <- lm(travel~Rail, data=via)
summary(railLm)

# Importar librería lattice
library(lattice)

# Desplegar variación entre datos
with(via,bwplot(Rail~residuals(railLm)))

# importar librería lme4 para Modelos mixtos
library(lme4)
# Generar modelo mixto
modeloMixto <-lmer(travel ~ 1 + (1|Rail), REML = FALSE, data = via)

# Revisión de estadísticos
summary(modeloMixto)