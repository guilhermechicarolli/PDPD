############## SCRIPTS DO PROJETO DE PDPD ##############

# 1. Carregamento e corte das camadas ambientais do presente
# 2. Carregamento e corte das camadas ambientais do futuro (RCP45)
# 3. Carregamento e corte das camadas ambientais do futuro (RCP85)
# 4. Rodagem dos modelos de distribuição para a espécie 
#    de planta Encholirium subsecundum
# 5. Criação das projeções no presente
# 6. Criação das projeções no cenário futuro de RCP 4.5
# 7. Criação das projeções no cenário futuro de RCP 8.5
# 8. Classificação das alterações de área

################################################################################

##### Carregamento das bibliotecas necessárias
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(sdm)) install.packages('sdm')
if (!require(dismo)) install.packages('dismo')
if (!require(dplyr)) install.packages('dplyr')
if (!require(mapview)) install.packages('mapview')
if (!require(raster)) install.packages('raster')
if (!require(rgdal)) install.packages('rgdal')
if (!require(usdm)) install.packages('usdm')

# Rodar na primeira vez para instalar as dependêcias que o 'sdm' precisa.
installAll() 

################################################################################

#--------- 1. CARREGAMENTO E CORTE DAS 
#                CAMADAS DO PRESENTE  ---------#

#------

# Carregar dados E. subsecundum
spg <- read.csv('./Dados/Ocorrencias/E_subsecundum_corrigido.csv')

# Adicionamos uma coluna com a espécie (necesária como argumento na modelagem)
spg$species <- 1

# Exploramos o data frame
head(spg)

# Transformar as coordenadas em um objeto "Spatial"
sp::coordinates(spg) <-c('x','y')
spg

#------

# Criar um objeto com o contorno (poligono) do Brasil
mascara <- raster::shapefile('Dados/Mascaras/mascara_brasil.shp')

# baixar camadas presente
bio <- raster::getData('worldclim', var='bio',res=2.5) # pode demorar

# cortar as camadas com o shape do brasil
corte <- bio

cam_rep <- bio[[1]]

# Reduzir o tamanho da camada representante para um retângulo

cam_rep <- raster::crop(cam_rep, extent(mascara))

# Reduzir o tamanho de todas as camadas ambientais do presente

bioCams <- raster::resample(corte, cam_rep, method="bilinear", 
                                   snap='out', bylayer=TRUE, progress='text')

# Cortar as camadas ambientais e cortar a partir da máscara criada
bioCams <- raster::mask(bioCams, mascara, bylayer=TRUE)

# Verificação
plot(bioCams)

#------
# MODELO CHEIO

# adicionar bg para fazer um MODELO CHEIO
dC <- sdm::sdmData(species~., spg, predictors = bioCams, 
                   bg=list(method='gRandom', n=10000))
dC

getmethodNames()

# ajustar os modelos
mC <- sdm::sdm(species~., dC, methods = 'maxent', replication=c('sub', 'boot'),
               test.p=30, n=10, parallelSettings=list(ncore=5, method='parallel'))

# NOTA: Para o MaxEnt funcionar o Java do PC deve estar atualizado

mC

# Plot das contribuições das variáveis
plot(mC@models$species$maxent$`1`@object)

# plot da importância das variáveis
plot(getVarImp(mC), 'AUC') # Biovars: 6, 17, 19, 14
bioc

# Para abrir uma interface de exploração do modelo
sdm::gui(mC)

#------

# TESTE VIF COM AS VARIÁVEIS COM MAIOR Importância

bioCams
# bioc <- raster::subset(bioCams, c(6, 17, 19, 14)) 

# O fator de inflação da variância das camadas selecionadas
# vif(bioc)

# Tiramos a camada 14 (alto vif e importância menor do que a correlata 17)
bioc <- raster::subset(bioCams, c(6, 17, 19))
vif(bioc)

# Essa parte é desnecessária pois já fizemos de forma manual

# ex <- raster::extract(bioc,spg)
# head(ex)

# v <- vifstep(ex)
# cor(ex)

# Deixar apenas as vars sem problema de colinearidade
# bioc <- exclude(bioc, v)
# bioc

#-------
# MODELAGEM COM AS CAMADAS SELECIONADAS

# adicionar bg 
d <- sdmData(species~., spg, predictors = bioc, bg=list(method='gRandom', n=10000))
d

getmethodNames()

# ajustar os modelos
m <- sdm(species~., d, methods='maxent', replication=c('sub', 'boot'),
         test.p=30, n=10, parallelSettings=list(ncore=5, method='parallel'))

m

# PLOT DAS CONTRIBUIÇÕES
getVarImp(m)
plot(getVarImp(m)) 


gui(m)


#-------
# PREDIÇÃO PARA O PRESENTE

p1 <- predict(m, bioc, filename='./Resultados_teste_SDM#2/presente.img',
              overwrite=TRUE)
p1


en1 <- ensemble(m, bioc, filename = './Resultados_teste_SDM#2/ensemble.img', 
                setting =list(method='weighted', stat='tss', opt=2), 
                overwrite=TRUE)

en1

plot(p1)
plot(en1)

#--------
# PREDIÇÃO PARA O FUTURO

# Donwload das camadas futuras
biof1 <- raster::getData('CMIP5', var='bio', res=2.5, rcp=45, model='CN', year=50)
names(biof1) <- names(bio)

# cortar as camadas com o shape do brasil
cortef <- biof1

cam_repf <- biof1[[1]]
# Reduzir o tamanho da camada representante para um retângulo
cam_repf <- raster::crop(cam_repf, extent(mascara))

# Reduzir o tamanho de todas as camadas ambientais do presente
biofC <- raster::resample(cortef, cam_repf, method="bilinear", 
                         snap='out', bylayer=TRUE, progress='text')

# Cortar as camadas ambientais e cortar a partir da máscara criada
biofC <- raster::mask(biofC, mascara, bylayer=TRUE)

# Verificação
plot(biofC)
 

# Selecionar apenas as biovars 6, 17, 19, 2
biof <- raster::subset(biofC, c(6, 17, 19, 2))
plot(biof)

#------

# predição
p1 <- predict(m, biof, filename='./Resultados_teste_SDM#2/presente.img',
              overwrite=TRUE)
p1

# Predição de um modelo consenso
en2 <- ensemble(m, biof, filename='./Resultados_teste_SDM#2/futuro.img',
                setting =list(method='weighted', stat='tss', opt=2), 
                overwrite=TRUE)

plot(en2)

#-------

cl<-colorRampPalette(c('#3E49BB', '#3498DB', 'yellow', 'orange', 'red', 'darkred'))

plot(en1, col=cl(200))
plot(en2, col=cl(200))

mapview(stack(en1,en2)) 

proj4string(spg) <- projection(en1)

#-------

# MAPA DE ALTERAÇÃO DE ADEQUABILIDADE (NÃO BINÁRIO) 
ch <- en2 - en1
cl2<-colorRampPalette(c('#3E49BB', '#3498DB', 'yellow', 'orange', 'red', 'darkred'))

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

th <- ev@threshold_based$threshold[2]    # Threshold pelo método SSS: máx(espec+sens)
th

# MAPA BINÁRIO DO PRESENTE
pa1 <- raster(en1)
pa1[] <- ifelse(en1[] >= th, 1,0)
plot(pa1)

# MAPA BINÁRIO DO FUTURO
pa2 <- raster(en2)
pa2[] <- ifelse(en2[] >= th, 1,0)
plot(pa2)

# MAPA BINÁRIO DE ALTERAÇÃO DE ADEQUABILIDADE
chp <- pa2 - pa1
plot(chp, col=c('red','gray','blue'))

#--------


cell_size<-area(pa1, na.rm=TRUE, weights=FALSE)
cell_size<-cell_size[!is.na(cell_size)]

#-------- 
# ÁREA PRESENTE  (KM^2)

mapa <- pa1$layer@data@values==1
tamanho <- sum(mapa[!is.na(mapa)])

area <- tamanho*median(cell_size)
area

#--------
# ÁREA FUTURA  (KM^2)

mapaF <- pa2$layer@data@values==1
tamanhoF <- sum(mapaF[!is.na(mapaF)])

areaF <- tamanhoF*median(cell_size)
areaF

#-------
# ÁREA ALTERADA (KM^2)

# area de perda < 0
# area de ganho > 0

mapaS <- chp$layer@data@values < 0
tamanhoS <- sum(mapaS[!is.na(mapaS)])

areaS <- tamanhoS*median(cell_size)
areaS


mapaG <- chp$layer@data@values >0
tamanhoG <- sum(mapaG[!is.na(mapaS)])

areaG <- tamanhoG*median(cell_size)
areaG

# porcentagem de ganho
(areaG/area)*100

# porcentagem de perda
(areaS/area)*100


################################ FIM ###########################################