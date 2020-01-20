
# Inicio ------------------------------------------------------------------
# Gráficos de Regresión Lineal
dat = as.data.frame(read.csv(file.choose()))


# Creación de gráficos de Logaritmo de Volumen tumoral vs Día -------------
# Regresión Lineal del Logaritmo de Volumen tumoral vs Día
lm.grupoA <- lm(Valor.A ~ Tiempo + 1, data=dat)
summary(lm.grupoA) # Grupo Placebo
lm.grupoB <- lm(Valor.B ~ Tiempo + 1, data=dat)
summary(lm.grupoB) # Grupo Fármaco X (100 mg/kg) PO
lm.grupoC <- lm(Valor.C ~ Tiempo + 1, data=dat)
summary(lm.grupoC) # Grupo Fármaco X (10 mg/kg) PO
lm.grupoD <- lm(Valor.D ~ Tiempo + 1, data=dat)
summary(lm.grupoD) # Grupo Fármaco X (10 mg/kg) IP

library(ggplot2)
library(grid)
library(scales)
library(MASS)

theme_set(theme_classic())
grafico.1 <- ggplot() +
  geom_point(data=dat,aes(x=Tiempo, y=Valor.A), color="red") +
  geom_smooth(data=dat,aes(x=Tiempo, y=Valor.A),method="lm",se=TRUE,
              color="red",fill="red1",linetype="dashed", alpha=I(0.1))+
  geom_point(data=dat,aes(x=Tiempo, y=Valor.B), color="blue") +
  geom_smooth(data=dat,aes(x=Tiempo, y=Valor.B),method="lm",se=TRUE,
              color="blue4",fill="blue1",linetype="dashed", alpha=I(0.1))+
  geom_point(data=dat,aes(x=Tiempo, y=Valor.C), color="green") +
  geom_smooth(data=dat,aes(x=Tiempo, y=Valor.C),method="lm",se=TRUE,
              color="green4",fill="green1",linetype="dashed", alpha=I(0.1))+
  geom_point(data=dat,aes(x=Tiempo, y=Valor.D), color="orange") +
  geom_smooth(data=dat,aes(x=Tiempo, y=Valor.D),method="lm",se=TRUE,
              color="orange4",fill="orange1",linetype="dashed", alpha=I(0.1))+
  labs(title="Gráficos de Dispersión",
       subtitle="Residuales",
       x="Día de tratamiento",
       y="Volúmen tumoral (mm^3)") +
  scale_x_continuous(breaks=c(seq(0,40,by=5)), 
                     limits=c(0,40),
                     sec.axis=dup_axis(name=NULL,labels = NULL))+
  scale_y_continuous(breaks=c(seq(0,8,by=1)), 
                     limits=c(0,8),
                     sec.axis=dup_axis(name=NULL,labels = NULL))
grafico.1



# Creación de marco de datos agrupados ------------------------------------
#Creación de Marco de Datos Agrupado
data <- groupedData(Ln ~ dia |sujeto | grupo,
                    data=m,
                    labels = list(x = "Día", y = "Logaritmo volumen tumoral"),
                    units = list(y = "(mm^3)"))

data1 <- groupedData(Ln ~ dia | sujeto/grupo,
                     data=m,
                     labels = list(x = "Día", y = "Logaritmo volumen tumoral"),
                     units = list(y = "(mm^3)"))

fm1 <- lme(Ln ~ dia*grupo + peso + 0,
           random=1|data1$sujeto/data1$grupo, data=data1)


plot(m, aspect = 3, display=1, collapse=1)








