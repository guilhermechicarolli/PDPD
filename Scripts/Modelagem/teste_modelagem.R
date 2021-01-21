library(dismo)
library(installr)
library(rJava)
library(tidyverse)


path<-"Lonchophylla_mordax\ocorrencias_L_mordax.csv"
arq <- read.csv(path)
arq <- select(arq, nomecientifico, longitude, latitude)


library(maptools)
## Checking rgeos availability: TRUE
data(wrld_simpl)
plot(wrld_simpl, xlim=c(-80,-30), ylim=c(-35,5), axes=TRUE, col="light yellow")
# restore the box around the map
box()
# add the points
points(arq$longitude, arq$latitude, col='orange', pch=20, cex=0.75)
# plot points again to add a border, for better visibility
points(acgeo$lon, acgeo$lat, col='red', cex=0.75)



my0 = getData("GADM", country="BRA", level=0)
plot(my0)



climate=getData("worldclim", var="bio",res=0.5, lat=-7, lon=-45)
plot(climate$bio1_34, main="Annual Mean Temperature")

pontos = arq[,c('longitude','latitude')]

group <- kfold(pontos,5)
pres_train <- pontos[group!=1,]
pres_test <- pontos[group==1,]


ext = extent(-60,-25,-30,0)
southamerworldclim=crop(climate,ext)


xm <- maxent(southamerworldclim,pres_train)
plot(xm)


backg = randomPoints(southamerworldclim, n=1000, ext=ext, extf=1.25)
colnames(backg) <- c("longitude","latitude")
group=kfold(backg, 5)
backg_train <- backg[group!=1,]
backg_test <- backg[group==1,]


e = evaluate(pres_test, backg_test, xm, southamerworldclim)
plot(e,'ROC')

p <- predict(southamerworldclim, xm, ext=ext, progress='')
plot(p, main="Maxent, raw values - Present")


futbio<-getData('CMIP5', var='bio', res=2.5, rcp=45, model='MI', year=70)

names(futbio)<-names(climate)
pfut<-predict(futbio, xm, ext=ext, progress='')

plot(pfut, main="Maxent, raw values - Future")
plot(my0,add=T)