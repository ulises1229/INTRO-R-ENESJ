# Autor: Ulises Olivares
# uolivares@unam.mx
# 21 de agosto de 2020
# Modelos lineales mixtos

# Leer datos desde URL
d = read.csv("http://www.bodowinter.com/uploads/1/2/9/3/129362560/politeness_data.csv")

str(d)

# Renombrar dataframe
names(d) <- c("sujeto", "género", "escenario", "situación", "tono")

# Desplegar un resumen
table(d$sujeto, d$situación)

# Cargar librería ggplot2
library(ggplot2)

# Generar un boxplot
ggplot(data = d, aes(x = situación, y = tono, color = sujeto))+
  geom_boxplot()+
  facet_wrap(~sujeto, nrow=1)

library (lmerTest) # Mixed model package by Douglas Bates, comes w/ pvalues! 
library (texreg) #Helps us make tables of the mixed models
library (afex) # Easy ANOVA package to compare model fits
library (plyr) # Data manipulator package
library (ggplot2) # GGplot package for visualizing data
library (lme4)

gpa_mixed = lmer(gpa ~ occasion + (1|student), data=gpa)

sleepstudy <- sleepstudy

write.csv(sleepstudy, "sleep.csv")

data <- pizzadata

load('data/gpa.RData')

