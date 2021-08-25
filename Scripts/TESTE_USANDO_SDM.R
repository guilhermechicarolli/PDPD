if (!require(sdm)) install.packages('sdm')

installAll() # Rodar na primeira vez

if (!require(dismo)) install.packages('dismo')
if (!require(dplyr)) install.packages('dplyr')
if (!require(tidyr)) install.packages('tidyr')
if (!require(mapview)) install.packages('mapview')
if (!require(raster)) install.packages('raster')

proj_WGS <- sp::CRS(
    "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


#------

# Carregar dados E. subsecundum

spg <- read.csv('./Dados/Ocorrencias/E_subsecundum_corrigido.csv')

spg$species <- 1
spg

# Adicionar projeção
sp::coordinates(spg) <- ~x+y
raster::crs(spg) <- proj_WGS
spg

#------

# Camadas presente
bioc <- list.files(path='./Dados/Camadas_biovars_res_2.5_brasil/Presente/',
                       pattern = '.asc', full.names = TRUE)
bioc <- raster::stack(bioc)

# Adicionar a projeção geográfica
raster::crs(bioc) <- proj_WGS
bioc

plot(bioc$wc2.1_2.5m_bio_1)
points(spg)

#------
# Teste VIF
if (!require(usdm)) install.packages('usdm')

vif(bioc)

ex <- raster::extract(bioc,spg)
head(ex)

v <- vifstep(ex)
v

# Deixar apenas as vars sem problema de colinearidade
bioc <- exclude(bioc, v)
bioc

#-------

# método mais simples!
d <- sdmData(species~., spg, predictors = bioc)
d

# adicionar bg
d <- sdmData(species~., spg, predictors = bioc, bg=list(method='gRandom', n=10000))
d

getmethodNames()

# ajustar os modelos
m <- sdm(species~., d, methods='Maxent', replication=c('sub', 'boot'),
         test.p=30, n=10, parallelSettings=list(ncore=5, method='parallel') )

m
plot(m@models$species$maxent$`1`@object)

gui(m)

p1 <- predict(m, bioc, filename='./Resultados_teste_SDM/presente.img')
p1


en1 <- ensemble(m, bioc, filename = './Resultados_teste_SDM/ensemble.img', 
                setting =list(method='weighted', stat='tss', opt=2))

raster::crs(en1) <- proj_WGS
en1

plot(p1)

plot(en1)

#### FUTURO

biof <- list.files(path='./Dados/Camadas_biovars_res_2.5_brasil/selec_subs_45/',
                   pattern = '.asc', full.names = TRUE)
biof <- raster::stack(biof)


en2 <- ensemble(m, biof, filename='./Resultados_teste_SDM/futuro.img',
                setting =list(method='weighted', stat='tss', opt=2))

plot(en2)

#-------
cl<-colorRampPalette(c('#3E49BB', '#3498DB', 'yellow', 'orange', 'red', 'darkred'))

plot(en1, col=cl(200))
plot(en2, col=cl(200))

mapview(stack(en1,en2)) + spg

proj4string(spg) <- projection(en1)


#-------

ch <- en2 - en1
cl2<-colorRampPalette(c('darkred', 'red', 'orange', 'yellow', '#3498DB', '#3E49BB'))

plot(ch, col=cl2(200))


#-------

df <- as.data.frame(d)
df <- data.frame(species=df$species, coordinates(d))
xy = as.matrix(df[,c('x', 'y')])
head(xy)

p<-raster::extract(en1,xy)

ev <- evaluates(df$species,p)
ev@statistics

ev@threshold_based

th <- ev@threshold_based$threshold[2]
th

pa1 <- raster(en1)
pa1[] <- ifelse(en1[] >= th, 1,0)
plot(pa1)


pa2 <- raster(en2)
pa2[] <- ifelse(en2[] >= 0.015, 1,0)
plot(pa2)
