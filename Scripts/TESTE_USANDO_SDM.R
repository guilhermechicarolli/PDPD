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

en1 <- ensemble(m, bio, filename = './Resultados_teste_SDM/ensemble.img', 
                setting =list(id=c(1,3,4,8,12), method='weighted', stat='tss', opt=2))
