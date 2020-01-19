# 1. Creación de datos agrupados ------------------------------------------
da = read.csv(file.choose()) # Escoger archivo datos.csv
m = groupedData(Ln ~ dia | sujeto/grupo,
                data=da,
                labels=list(x="Dia de administración", y="Logaritmo de volumen tumoral"))

unique(getGroups(m))
gsummary(m, invar=TRUE)
plot(m, layout=c(2,2), between=list(x=c(1),y=c(1)), display=1, collapse = 1)

# Gráfico por sujeto experimental
plot(m, displayLevel = 2)
# Gráfico por grupo de tratamiento
plot(m, display=1, collapse = 1, aspect=1, outer= ~ as.factor(grupo))
plot(m, display=1, collapse = 2, aspect=1, outer= ~ as.factor(grupo), 
     between=list(x=c(1),y=c(1)), FUN=mean(grupo)) # Sin leyenda

unique(getCovariate(m))


# 2. Ajuste de modelos lineales en S con LM -------------------------------
fm1.lme <- lm(Ln ~ peso + (as.factor(grupo) * dia),m)
par( mfrow=c(2,2) )
plot(fm1.lme)
summary(fm1.lme)

fm2.lme <- lm(Ln ~ 0 + peso + (grupo * dia),m)
par( mfrow=c(2,2) )
plot(fm2.lme)
summary(fm2.lme)

# Boxplot de residuales por sujeto
bwplot(getGroups(m)$sujeto ~ resid(fm2.lme),
       main="Gráfico de Residuales por sujeto",
       xlab="Valor de residuales")
# Boxplot de residuales por grupo
bwplot(getGroups(m)$grupo ~ resid(fm2.lme), col="green", 
       main="Gráfico de Residuales por grupo de tratamiento",
       xlab="Valor de residuales")

fm1.lis <- lmList(Ln ~ 0 + peso + (grupo * dia)|sujeto/grupo,m)
summary(fm1.lis)

# Correlación entre efectos
pairs(fm1.lis, adj = -0.5, fill="black", col="black")
intervals(fm1.lis)

# Gráfico de Residuales
plot(intervals(fm1.lis))

fm1.lme <- lme(Ln ~ 0 + peso + (grupo * dia),m ,
               random = ~ 0 + dia|sujeto/grupo)
fm1.lme.M <- lme(Ln ~ 0 + peso + (grupo * dia),m ,
               random = ~ 0 + dia|sujeto/grupo, method="ML")

fitted(fm1.lme, level=0:2) # valores predecidos

par( mfrow=c(2,2) )
plot(x=m$Ln,y=fitted(fm1.lme, level=0:2)[,1])
plot(x=m$Ln,y=fitted(fm1.lme, level=0:2)[,2])
plot(x=m$Ln,y=fitted(fm1.lme, level=0:2)[,3])
par( mfrow=c(1,1) )

resid(fm1.lme, level=1, type="pearson")

plot(fm1.lme)

compar.1 <- compareFits(coef(fm1.lis),coef(fm1.lme))
plot(compar.1, mark=fixef(fm1.lme))

#plot(comparePred(fm1.lis,fm1.lme, length.out=2, level=1, primary=m$dia))

# Gráfico de Efectos Aleatorios de acuerdo a método de estimación (ML o REML)
plot(compareFits(ranef(fm1.lme, level=1), ranef(fm1.lme.M, level=1)), mark=c(0,0))


