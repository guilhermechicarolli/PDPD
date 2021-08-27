############## SCRIPTS DO PROJETO DE PDPD ##############

# 1. Carregamento e corte das camadas ambientais do presente
# 2. Rodagem dos modelos de distribuicao para a especie 
#    de planta Encholirium subsecundum e selecao das camadas
# 3. Rodagem do modelo final com as camadas selecionadas
# 4. Criacao das projecoes no presente
# 5. Criacao das projecoes no cenario futuro de RCP 4.5
# 6. Criacao das projecoes no cenario futuro de RCP 8.5
# 7. Construcao dos mapaP45 simples, binarios e n√£o binarios
# 7. Classificacao das alteracoes de area

################################################################################

##### Carregamento das bibliotecas necessarias
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(sdm)) install.packages('sdm')
if (!require(dismo)) install.packages('dismo')
if (!require(dplyr)) install.packages('dplyr')
if (!require(mapview)) install.packages('mapview')
if (!require(raster)) install.packages('raster')
if (!require(rgdal)) install.packages('rgdal')
if (!require(usdm)) install.packages('usdm')

# Rodar na primeira vez para instalar as dependencias que o 'sdm' precisa.
installAll() 

################################################################################

#--------- 1. CARREGAMENTO E CORTE DAS CAMADAS
#           AMBIENTAIS E DADOS DE E. SUBSECUNDUM  ---------#

### CARREGAR DADOS DE E. SUBSECUNUDUM
spg <- read.csv('./Dados/Ocorrencias/E_subsecundum_corrigido.csv')

# Adicionamos uma coluna com a especie (necesaria como argumento na modelagem)
spg$species <- 1

# Exploramos o data frame
head(spg)

# Transformar as coordenadas em um objeto "Spatial"
sp::coordinates(spg) <-c('x','y')
spg

#----------
### CARREGAR CAMADAS DO PRESENTE

# Criar um objeto com o contorno (poligono) do Brasil
mascara <- raster::shapefile('Dados/Mascaras/mascara_brasil.shp')

# Baixar camadas presente do database online Worldclim
bio <- raster::getData('worldclim', var='bio',res=2.5, path="./Camadas_presente/") # pode demorar
plot(bio)

# cortar as camadas com o shape do brasil
corte <- bio
cam_rep <- bio[[1]]

# Reduzir o tamanho da camada representante para um retangulo
cam_rep <- raster::crop(cam_rep, extent(mascara))

# Reduzir o tamanho de todas as camadas ambientais do presente
bioCams <- raster::resample(corte, cam_rep, method="bilinear", 
                            snap='out', bylayer=TRUE, progress='text')

# Cortar as camadas ambientais e cortar a partir da mascara criada
bioCams <- raster::mask(bioCams, mascara, bylayer=TRUE)

# Verificacao
plot(bioCams)

#----------
### CARREGAR CAMADAS DO FUTURO RCP45, ANO 2050

# Donwload das camadas futuras (RCP 45) do database online Worldclim 
biof45 <- raster::getData('CMIP5', var='bio', res=2.5, rcp=45, model='CN', 
                          year=50, path="./Camadas_RCP45/")
plot(biof45)

# As camadas futuras devem ter o mesmo nome que as do presente
names(biof45) <- names(bio)

# Carregar as camadas e uma camada representante (bio 1) para corte das camadas
cortef <- biof45
cam_repf <- biof45[[1]]

# Reduzir o tamanho da camada representante para um retangulo
cam_repf <- raster::crop(cam_repf, extent(mascara))

# Reduzir o tamanho de todas as camadas ambientais RCP 45
bio45 <- raster::resample(cortef, cam_repf, method="bilinear", 
                          snap='out', bylayer=TRUE, progress='text')

# Cortar as camadas ambientais e cortar a partir da mascara criada do Brasil
bio45 <- raster::mask(bio45, mascara, bylayer=TRUE)

# Verificacao
plot(bio45)

#----------
### CARREGAR CAMADAS DO FUTURO RCP85, ANO 2050

# Donwload das camadas futuras (RCP 45) do database online Worldclim 
biof85 <- raster::getData('CMIP5', var='bio', res=2.5, rcp=85, model='CN', 
                          year=50, path="./Camadas_RCP85/")
plot(biof85)

# As camadas futuras devem ter o mesmo nome que as do presente
names(biof85) <- names(bio)

# Carregar as camadas e uma camada representante (bio 1) para corte das camadas
cortef85 <- biof85
cam_repf85 <- biof85[[1]]

# Reduzir o tamanho da camada representante para um retangulo
cam_repf85 <- raster::crop(cam_repf85, extent(mascara))

# Reduzir o tamanho de todas as camadas ambientais RCP 85
bio85 <- raster::resample(cortef85, cam_repf85, method="bilinear", 
                          snap='out', bylayer=TRUE, progress='text')

# Cortar as camadas ambientais e cortar a partir da mascara criada do Brasil
bio85 <- raster::mask(bio85, mascara, bylayer=TRUE)

# Verificacao
plot(bio85)

################################################################################
#--------- 2. RODAGEM DO MODELO PREVIO E 
#               SELECAO DAS VARIAVEIS   ---------#

# MODELO CHEIO

# Adicionar os dados previo para fazer um modelo com todas as variaveis
dC <- sdm::sdmData(species~., spg, predictors = bioCams, 
                   bg=list(method='gRandom', n=10000))
dC

# Ajustar e criar os modelos
mC <- sdm::sdm(species~., dC, methods = 'maxent', replication=c('sub', 'boot'),
               test.p=30, n=10, parallelSettings=list(ncore=5, method='parallel'))

# NOTAS: 1) Para o MaxEnt funcionar o Java do computador deve estar atualizado. 
# 2) O parametro ncore È a quantidade de cores de processamento utilizados para
# a modelagem, altere conforme a capacidade do computador

mC

# Plot da importancia das variaveis
plot(getVarImp(mC), 'AUC') # Biovars: 6, 17, 19, 14
getVarImp(mC)

# Para abrir uma interface de exploracao do modelo
sdm::gui(mC)

#----------
# TESTE VIF COM AS VARIAVEIS COM MAIOR IMPORTANCIA


# Tiramos a camada 14 (alto vif e import√¢ncia menor do que a correlata 17)
bioc <- raster::subset(bioCams, c(6, 17, 19))
vif(bioc)

# Essa parte È desnecessaria pois ja fizemos de forma manual
# ex <- raster::extract(bioc,spg)
# head(ex)

# v <- usdm::vifstep(ex)
# cor(ex)

# Deixar apenas as vars sem problema de colinearidade
# bioc <- usdm::exclude(bioc, v)
# bioc

################################################################################
#--------- 3. MODELAGEM COM AS CAMADAS SELECIONADAS  
#                    NA SECAO ANTERIOR   ---------#

# Adicionar os dados previos: ocorrencias, camadas e pontos de background
d <- sdm::sdmData(species~., spg, predictors = bioc, bg=list(method='gRandom', 
                                                             n=10000))
d

# Ajustar os modelos, 50 replicacoes, 25 por Subsampling e 25 por Bootstrap
m <- sdm::sdm(species~., d, methods='maxent', replication=c('sub', 'boot'),
              test.p=30, n=25, parallelSettings=list(ncore=5, method='parallel'))

# NOTAS: Como mencionado no item 2, o parametro ncore È a quantidade de cores de
# processamento utilizados para a modelagem, altere conforme a configuracao do 
# computador

m

# Plot das contribuicoes das variaveis
plot(getVarImp(m), 'AUC')
sdm::getVarImp((m))

# Para abrir uma interface de exploracao do modelo
sdm::gui(m)


################################################################################
#--------- 4. PROJECAO DO MODELO PARA O PRESENTE   ---------#

# Projecao dos 50 modelos criados na se√ß√£o anterior para o presente
p1 <- predict(m, bioc, filename='./Resultados_subsecundum/presente.img',
              overwrite=TRUE)
p1

# Obter um modelo consenso dentre os 50 criados para o presente por meio da 
# mediana ponderada
en <- sdm::ensemble(m, bioc, filename = 
                        './Resultados_subsecundum/ensemble_presente.img', 
                    setting =list(method='weighted', stat='tss', opt=2), 
                    overwrite=TRUE)
en

# Verificacao
plot(p1)
plot(en)

################################################################################
#--------- 5. PROJECAO DO MODELO PARA O FUTURO (RCP45)   ---------#

# Selecionar apenas as biovariaveis 6, 17, 19
bioS45 <- raster::subset(bio45, c(6, 17, 19))
plot(bioS45)

# Predicao utilizando o modelo criado na secao 3 para as camadas de RCP45
p2 <- predict(m, bioS45, filename='./Resultados_subsecundum/modelos_RCP45.img',
              overwrite=TRUE)
p2

# Obter um modelo consenso dentre os 50 criados para o futuro RCP45 por meio da 
# mediana ponderada
en45 <- sdm::ensemble(m, bioS45, filename='./Resultados_subsecundum/futuro_RCP45.img',
                      setting =list(method='weighted', stat='tss', opt=2), 
                      overwrite=TRUE)
en45

# Verificacao
plot(p2)
plot(en45)

################################################################################
#--------- 6. PROJECAO DO MODELO PARA O FUTURO (RCP85)   ---------#

# Selecionar apenas as biovariaveis 6, 17, 19
bioS85 <- raster::subset(bio85, c(6, 17, 19))
plot(bioS85)

# Predicao utilizando o modelo criado na secao 3 para as camadas de RCP85
p3 <- predict(m, bioS85, filename='./Resultados_subsecundum/modelos_RC85.img',
              overwrite=TRUE)
p3

# Obter um modelo consenso dentre os 50 criados para o futuro RCP85 por meio da 
# mediana ponderada
en85 <- sdm::ensemble(m, bioS85, filename='./Resultados_subsecundum/futuro_RC85.img',
                      setting =list(method='weighted', stat='tss', opt=2), 
                      overwrite=TRUE)

plot(en85)

################################################################################
#--------- 7. CONSTRUCAO DE mapaP45 BINARIOS E
#             DE DENSIDADE DE PROBABILIDADE  ---------#

# Paleta de cores
cores <- grDevices::colorRampPalette(c(
    '#3E49BB', '#3498DB', 'yellow', 'orange', 'red', 'darkred'))

# Plot dos mapa com a nova paleta de cores
plot(en, col=cores(200))
plot(en45, col=cores(200))
plot(en85, col=cores(200))

# Visualizacao alteranativa dos mapaP45 por meio do mapview
mapview::mapview(stack(en,en45,en85)) 

#----------
### Mapa de alteracao de adequabilidade (NAO BINARIO) entre o presente e o futuro 
# de RCP 45
ch45 <- en45 - en

cores2<-grDevices::colorRampPalette(c('red', 'orange', 'yellow', 'gray', 
                                      'green', 'blue'))
plot(ch45, col=cores2(200))
# Areas mais proximas ao azul representam areas ganhas no futuro (RCP45)
# Areas mais proximas ao vermelho representam areas ganhas perdidas (RCP45)
# Areas cinzas permanceram inalteradas no futuro

#----------
### Mapa de alteracao de adequabilidade (NAO BINARIO) entre o presente e o futuro 
# de RCP 85
ch85 <- en85 - en

cores2<-grDevices::colorRampPalette(c('red', 'orange', 'yellow', 'gray', 
                                      'green', 'blue'))
plot(ch85, col=cores2(200))


#----------
### OBTER MEDIDAS DE THRESHOLD PARA A CONSTRUCAO DOS mapaP45 BINARIOS
df <- as.data.frame(d)
df <- data.frame(species=df$species, coordinates(d))
xy = as.matrix(df[,c('x', 'y')])
head(xy)

# Extrair do raster da predicao do presente os valores das biovariaveis nos 
# pontos de ocorrencias das especies
p<-raster::extract(en,xy)

# Avaliacao do modelo
ev <- evaluates(df$species,p)
ev@statistics

# Medidadas de threshold
ev@threshold_based

th <- ev@threshold_based$threshold[2] #Threshold pelo metodo SSS: max(espec+sens)
th

#----------
### MAPA BINARIO DO PRESENTE UTILIZANDO O VALOR DE THRESHOLD th
pa1 <- raster(en)
pa1[] <- ifelse(en[] >= th, 1,0)
plot(pa1)

### MAPA BINARIO DO FUTURO (RCP45)
pa2 <- raster(en45)
pa2[] <- ifelse(en45[] >= th, 1,0)
plot(pa2)

### MAPA BINARIO DO FUTURO (RCP85)
pa3 <- raster(en85)
pa3[] <- ifelse(en85[] >= th, 1,0)
plot(pa3)

### MAPA BINARIO DE ALTERACAO DE ADEQUABILIDADE (Futuro RCP45 - Presente)
chp45 <- pa2 - pa1 
plot(chp45, col=c('red','gray','blue'))

### MAPA BINARIO DE ALTERACAO DE ADEQUABILIDADE (Futuro RCP85 - Presente)
chp85 <- pa3 - pa1 
plot(chp85, col=c('red','gray', 'blue'))

################################################################################
#--------- 7. CLASSIFICACAO DAS ALTERACOES DE AREA   ---------#

# Obter os tamanhos das celulas
cel_tam<-area(pa1, na.rm=TRUE, weights=FALSE)
cel_tam<-cel_tam[!is.na(cel_tam)]

#----------
### AREA PRESENTE  (KM^2)
mapa <- pa1$layer@data@values==1
tamanho <- sum(mapa[!is.na(mapa)])

area <- tamanho*median(cel_tam)
area

#----------
###  AREA FUTURA (RCP45)  (KM^2)
mapaF45 <- pa2$layer@data@values==1
tamanhoF45 <- sum(mapaF45[!is.na(mapaF45)])

areaF45 <- tamanhoF45*median(cel_tam)
areaF45

#----------
###  AREA FUTURA (RCP85)  (KM^2)
mapaF85 <- pa3$layer@data@values==1
tamanhoF85 <- sum(mapaF85[!is.na(mapaF85)])

areaF85 <- tamanhoF85*median(cel_tam)
areaF85

#----------
###  AREA ALTERADA ENTRE O PRESENTE E O FUTURO RCP45 (KM^2)
# area de perda < 0
# area de ganho > 0

mapaP45 <- chp45$layer@data@values < 0
tamanhoP45 <- sum(mapaP45[!is.na(mapaP45)])
areaP45 <- tamanhoP45*median(cel_tam)
areaP45        # Area perdida

mapaG45 <- chp45$layer@data@values >0
tamanhoG45 <- sum(mapaG45[!is.na(mapaG45)])
areaG45 <- tamanhoG45*median(cel_tam)
areaG45        # Area ganha

# Porcentagem de ganho
(areaG45/area)*100
# Porcentagem de perda
(areaP45/area)*100

#----------
###  AREA ALTERADA ENTRE O PRESENTE E O FUTURO RCP85 (KM^2)
# area de perda < 0
# area de ganho > 0

mapaP85 <- chp85$layer@data@values < 0
tamanhoP85 <- sum(mapaP85[!is.na(mapaP85)])
areaP85 <- tamanhoP85*median(cel_tam)
areaP85        # Area perdida


mapaG85 <- chp85$layer@data@values >0
tamanhoG85 <- sum(mapaG85[!is.na(mapaG85)])
areaG85 <- tamanhoG85*median(cel_tam)
areaG85        # Area ganha

# Porcentagem de ganho
(areaG85/area)*100
# Porcentagem de perda
(areaP85/area)*100

################################ FIM ###########################################