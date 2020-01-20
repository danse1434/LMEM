##------------------------------------------------------------------------#
## Nombre del Script: Análisis por Modelo de Efectos Mixtos Lineales ------
##  
## Proposito del Script: Ejemplo de análisis de modelo de regresión LMEM
##  
## Autor: Daniel S. Parra Gonzalez 
## Fecha de creacion:  
##  
## Copyright (c) Daniel S. Parra, 2020 
##  
## Email: dsparrag@unal.edu.co 
##------------------------------------------------------------------------#
##########################################################################-
# Carga de paquetes
require(tidyverse)
require(lme4)
require(nlme)

##########################################################################-
# Creación de datos agrupados ---------------------------------------------
##########################################################################-
# Lectura de archivo de datos
data <- read_csv("Datos/Datos.csv", 
                 col_types = cols(dia = col_integer(), 
                                  grupo = col_factor(levels = c(1:4)), 
                                  sujeto = col_factor(levels = c(1:28))))
data <- data %>%
  mutate(
    label = case_when(
      grupo == 1 ~ "Control",
      grupo == 2 ~ "X 100mg/kg PO",
      grupo == 3 ~ "X 10mg/kg PO",
      grupo == 4 ~ "X 10mg/kg IP",
      TRUE ~ NA_character_
    )
  )
##########################################################################-
# Crear objeto de datos agrupados
data1 = groupedData(Ln ~ dia | sujeto / grupo, 
                    data = data, 
                    labels = list(x = "Tiempo (d)", 
                                  y = "Log. Vol. Tumoral"))

##########################################################################-
# Visualizar grupos en objeto
unique(getGroups(data1))
gsummary(data1, invar=TRUE)





# G1 <- 
  data1 %>% 
  ggplot(mapping = aes(x = dia, y = Ln, group = sujeto, col = sujeto)) +
  geom_point() +
  facet_wrap(.~label, ncol = 2)+
  geom_line() +
  stat_summary(fun.y = mean, geom = 'line', lty = 'dashed', 
               aes(group = grupo))
  



##########################################################################-
# Ajuste de modelos lineales en S con LM ----------------------------------
##########################################################################-
# Modelo Lineal simple con peso y grupo
fm1.lme <- lm(Ln ~ peso + (as.factor(grupo) * dia), data1)
par(mfrow = c(2, 2))
plot(fm1.lme)
summary(fm1.lme)

##########################################################################-
# Modelo Lineal simple con peso y grupo, intercepto
fm2.lme <- lm(Ln ~ 0 + peso + (grupo * dia), data1)
par(mfrow = c(2, 2))
plot(fm2.lme)
summary(fm2.lme)

# Boxplot de residuales por sujeto
lattice::bwplot(getGroups(data1)$sujeto ~ resid(fm2.lme),
                main = "Gráfico de Residuales por sujeto",
                xlab = "Valor de residuales")

# Boxplot de residuales por grupo
lattice::bwplot(getGroups(data1)$grupo ~ resid(fm2.lme), col = "green", 
                main = "Gráfico de Residuales por grupo de tratamiento", 
                xlab = "Valor de residuales")

# Regresión de lista con especificaciones
fm1.lis <- lmList(Ln ~ 0 + peso + (grupo * dia) | sujeto / grupo, data1)
summary(fm1.lis)

# Correlación entre efectos
pairs(fm1.lis, adj = -0.5, fill="black", col="black")
intervals(fm1.lis)

# Gráfico de Residuales
plot(intervals(fm1.lis))

##########################################################################-
# Modelo Linear Efectos Mixtos --------------------------------------------
##########################################################################-

fm1.lme <- lme(Ln ~ 0 + (peso * dia) + (grupo * dia), data1 , 
               random = ~ 0 + dia | sujeto)





fm1.lme <- lme(Ln ~ 0 + peso + (grupo * dia), data1 , 
               random = ~ 0 + dia | sujeto / grupo)

fm1.lme.data1 <- lme(Ln ~ 0 + peso + (grupo * dia),data1 ,
               random = ~ 0 + dia|sujeto/grupo, method="ML")

fitted(fm1.lme, level=0:2) # valores predecidos

par( mfrow=c(2,2) )
plot(x=data1$Ln,y=fitted(fm1.lme, level=0:2)[,1])
plot(x=data1$Ln,y=fitted(fm1.lme, level=0:2)[,2])
plot(x=data1$Ln,y=fitted(fm1.lme, level=0:2)[,3])
par( mfrow=c(1,1) )

resid(fm1.lme, level=1, type="pearson")

plot(fm1.lme)

compar.1 <- compareFits(coef(fm1.lis),coef(fm1.lme))
plot(compar.1, mark=fixef(fm1.lme))

#plot(comparePred(fm1.lis,fm1.lme, length.out=2, level=1, primary=data1$dia))

# Gráfico de Efectos Aleatorios de acuerdo a data1étodo de estimación (ML o REML)
plot(compareFits(ranef(fm1.lme, level=1), ranef(fm1.lme.data1, level=1)), mark=c(0,0))


