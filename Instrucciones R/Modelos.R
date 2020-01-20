# MODELO DE EFECTOS MIXTOS LINEALES
# Apertura de archivo
m <- read.csv(file.choose())
head(m)

library("nlme")

# variables: grupo dia sujeto peso volumen Ln
fm1.gls <- gls(Ln ~ 0 + peso + (grupo*dia),
               data=m,method="ML", 
               correlation = corSymm(form = ~ 1|sujeto))
fm2.gls <- update(fm1.gls, correlation = corCompSymm(form = ~ 1|sujeto))
fm3.gls <- update(fm1.gls, correlation = corARMA(form = ~ 1|sujeto, p=3, q=0))
fm4.gls <- update(fm1.gls, correlation = corCAR1(form = ~ 1|sujeto))
fm5.gls <- update(fm1.gls, correlation = corExp(form = ~ 1|sujeto))
fm6.gls <- update(fm1.gls, correlation = corGaus(form = ~ 1|sujeto))

AIC(fm1.gls,fm2.gls,fm3.gls,fm4.gls,fm5.gls,fm6.gls)
BIC(fm1.gls,fm2.gls,fm3.gls,fm4.gls,fm5.gls,fm6.gls)


valores.p <- cbind(summary(fm1.gls)[["tTable"]][,4], # Simple
                   summary(fm2.gls)[["tTable"]][,4], # Simetría compuesta
                   summary(fm3.gls)[["tTable"]][,4], # Toepliz
                   summary(fm4.gls)[["tTable"]][,4], # Potencial
                   summary(fm5.gls)[["tTable"]][,4], # Exponencial
                   summary(fm6.gls)[["tTable"]][,4]) # Gaussiana

# De acuerdo a criterio AIC, la estructura a escoger es matriz R con forma toepliz.
# De acuerdo a criterio BIC, la estructura a escoger es matriz R con forma toepliz.

# Regresión con tiempo en forma continua
fm1.lme <- lme(Ln ~ 0 + peso + dia + grupo:dia + grupo + I(dia^2) + I(dia^2):grupo,
              data=m,method="ML",
              random= list(sujeto=pdDiag(~ 0 + dia + grupo:dia+ I(dia^2) + I(dia^2):grupo)),
              control= lmeControl(maxIter=5000, msMaxIter=5000,msVerbose=T,msMaxEval=1000))

fm2.lme <- update(fm1.lme, random= list(sujeto=pdCompSymm(~ 0 + dia + grupo:dia+ I(dia^2) + I(dia^2):grupo)))
fm3.lme <- update(fm1.lme, random= list(sujeto=pdSymm(~ 0 + dia + grupo:dia+ I(dia^2) + I(dia^2):grupo))) 

AIC(fm1.lme,fm2.lme,fm3.lme)
BIC(fm1.lme,fm2.lme,fm3.lme)

valores.p1 <- cbind(summary(fm1.lme)[["tTable"]][,5], # Simple
                   summary(fm2.lme)[["tTable"]][,5], # Simetría compuesta
                   summary(fm3.lme)[["tTable"]][,5]) # No estructurada

# Regresión con matrices combinadas R y G

fm4.lme <- fm3.lme
fm5.lme <- update(fm4.lme, correlation = corCompSymm(form = ~ 1|sujeto, value=0.9))
fm6.lme <- update(fm5.lme, correlation = corARMA(form = ~ 1|sujeto, p=3, q=0, value=c(0.25,0.25,0.25)))
fm7.lme <- update(fm4.lme, correlation = corCAR1(form = ~ 1|sujeto, value=0.9))                 
fm8.lme <- update(fm4.lme, correlation = corExp(form = ~ 1|sujeto, value=0.5))                 
fm9.lme <- update(fm4.lme, correlation = corGaus(form = ~ 1|sujeto, value = 0.5))

AIC(fm4.lme,fm5.lme,fm6.lme, fm7.lme,fm8.lme,fm9.lme)
BIC(fm4.lme,fm5.lme,fm6.lme, fm7.lme,fm8.lme,fm9.lme)

valores.p2 <- cbind(summary(fm4.lme)[["tTable"]][,5], # Simple
                    summary(fm5.lme)[["tTable"]][,5], # Simetría compuesta
                    summary(fm6.lme)[["tTable"]][,5], # Toepliz
                    summary(fm7.lme)[["tTable"]][,5], # Espacial Potencial
                    summary(fm8.lme)[["tTable"]][,5], # Espacial Exponencial
                    summary(fm9.lme)[["tTable"]][,5]) # Espacial Gaussiana

# Regresión con matriz final (G, no estructurada; R, Espacial Potencial)
fm.1 <- lme(Ln ~ 0 + dia + grupo:dia + grupo + I(dia^2) + I(dia^2):grupo,
            data=m,method="ML",
            random= list(sujeto=pdSymm(~ 0 + dia + grupo:dia+ I(dia^2) + I(dia^2):grupo)),
            correlation = corCAR1(form = ~ 1|sujeto, value=0.9),
            control= lmeControl(maxIter=5000, msMaxIter=5000,msVerbose=T,msMaxEval=1000))

fm.2 <- update(fm.1, method="REML")

summary(fm.1)
summary(fm.2)

# Gráfico de Residuales
m[,7] = resid(fm.1)

library(ggplot2)
library(grid)
library(scales)
library(MASS)

theme_set(theme_classic())

ggplot(data=m,aes(x=dia, y=V7)) +
  geom_point(color="red",data=m,aes(x=dia, y=V7)) +
  geom_smooth(method="loess",color="red4",fill="red1",linetype="dashed", alpha=I(0.01))+
  labs(title="Gráficos de Residuales - LMEM",
       subtitle="Modelo Final, matriz G no estructurada, matriz R espacial potencial",
       x="Día de administración",
       y="Residual") +
  scale_x_continuous(breaks=c(seq(0,35,by=5)), 
                     limits=c(0,35),
                     sec.axis=dup_axis(name=NULL,labels = NULL))+
  scale_y_continuous(breaks=c(seq(-4,4,by=1)), 
                     limits=c(-4,4),
                     sec.axis=dup_axis(name=NULL,labels = NULL))

ggplot(m,aes(V7)) +
  geom_histogram(
    binwidth=0.2, 
    col="blue4",
    fill="azure",
    size=.01) + 
  labs(title="Histograma Residuales - LMEM",
       subtitle="Modelo Final, matriz G no estructurada, matriz R espacial potencial",
       x="Residual",
       y="Frecuencia") +
  expand_limits(x=c(-4,4),y=c(0,100))

#############################################################################
# Gráfico de Residuales (GOF)
#############################################################################
m[,8] = c(predict(fm.1,m))
m[,9] = c(seq(0,10,length=280))
m[,10] = c(seq(0,10,length=280))


ggplot(data=m,aes(x=Ln, y=V8)) +
  geom_point(color="black",data=m,aes(x=Ln, y=V8)) +
  geom_line(data=m,aes(x=V9,y=V10),size=1) + 
  labs(title="GOF - LMEM",
       subtitle="Modelo Final, matriz G no estructurada, matriz R espacial potencial",
       x="Volumen tumoral observado",
       y="Volumen tumoral predecido") +
  scale_x_continuous(breaks=c(seq(0,10,by=1)), 
                     limits=c(0,10),
                     sec.axis=dup_axis(name=NULL,labels = NULL))+
  scale_y_continuous(breaks=c(seq(0,10,by=1)), 
                     limits=c(0,10),
                     sec.axis=dup_axis(name=NULL,labels = NULL))






