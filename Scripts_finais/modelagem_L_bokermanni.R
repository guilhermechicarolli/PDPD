############## SCRIPTS DO PROJETO DE PDPD ##############

# 1. Carregamento e corte das camadas ambientais do presente
# 2. Rodagem dos modelos de distribuicao para a especie 
#    de morcego Lonchophylla bokermanni e selecao das camadas
# 3. Rodagem do modelo final com as camadas selecionadas
# 4. Criacao das projecoes no presente
# 5. Criacao das projecoes no cenario futuro de RCP 4.5
# 6. Criacao das projecoes no cenario futuro de RCP 8.5
# 7. Construcao dos mapaP45 simples, binarios e não binarios
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
spgM <- read.csv('./Dados/Ocorrencias/L_bokermanni_corrigido.csv')

# Adicionamos uma coluna com a especie (necesaria como argumento na modelagem)
spgM$species <- 1

# Exploramos o data frame
head(spgM)

# Transformar as coordenadas em um objeto "Spatial"
sp::coordinates(spgM) <-c('x','y')
spgM

#----------
# ***NOTA: Caso ja tenha carregado as camadas do presente e futuro, e elas ja
# estejam no ambiente R, pule para a secao 2

### CARREGAR CAMADAS DO PRESENTE

# Criar um objeto com o contorno (poligono) do Brasil
mascara <- raster::shapefile('Dados/Mascaras/mascara_brasil.shp')

# Baixar camadas presente do database online WorldCMlim
bio <- raster::getData('worldCMlim', var='bio',res=2.5, path="./Camadas_presente/") # pode demorar
plot(bio)

# cortar as camadas com o shape do brasil
corte <- bio
cam_rep <- bio[[1]]

# Reduzir o tamanho da camada representante para um retangulo
cam_rep <- raster::crop(cam_rep, extent(mascara))

# Reduzir o tamanho de todas as camadas ambientais do presente
biocams <- raster::resample(corte, cam_rep, method="bilinear", 
                            snap='out', bylayer=TRUE, progress='text')

# Cortar as camadas ambientais e cortar a partir da mascara criada
biocams <- raster::mask(bioCams, mascara, bylayer=TRUE)

# Verificacao
plot(biocams)

#----------
### CARREGAR CAMADAS DO FUTURO RCP45, ANO 2050

# Donwload das camadas futuras (RCP 45) do database online WorldCMlim 
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

# Donwload das camadas futuras (RCP 45) do database online WorldCMlim 
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
dCM <- sdm::sdmData(species~., spgM, predictors = bioCams, 
                   bg=list(method='gRandom', n=10000))
dCM

# Ajustar e criar os modelos
mCM <- sdm::sdm(species~., dCM, methods = 'maxent', replication=c('sub', 'boot'),
               test.p=30, n=25, parallelSettings=list(ncore=5, method='parallel'))

# NOTAS: 1) Para o MaxEnt funcionar o Java do computador deve estar atualizado. 
# 2) O parametro ncore e a quantidade de cores de processamento utilizados para
# a modelagem, altere conforme a capacidade do computador

mCM

# Plot da importancia das variaveis
plot(getVarImp(mCM), 'AUC', main="Importância relativa das biovariáveis", 
     ylab='Variáveis', xlab="Importância relativa da variável") # Biovars: 17, 9, 15, 19
getVarImp(mCM)

# Para abrir uma interface de exploracao do modelo
sdm::gui(mCM)

#----------
# TESTE VIF COM AS VARIAVEIS COM MAIOR IMPORTANCIA


# Tiramos a camada 19 (problema de colinearidade e importância menor do que a 
# correlata bio15)
biocM <- raster::subset(bioCams, c(9, 17, 15))
vif(biocM)

# Essa parte e desnecessaria pois ja fizemos de forma manual
# ex <- raster::extract(biocM,spgM)
# head(ex)

# v <- usdm::vifstep(ex)
# v
# cor(ex)

# Deixar apenas as vars sem problema de colinearidade
# biocM <- usdm::exclude(biocM, v)
# biocM

################################################################################
#--------- 3. MODELAGEM COM AS CAMADAS SELECIONADAS  
#                    NA SECAO ANTERIOR   ---------#

# Adicionar os dados previos: ocorrencias, camadas e pontos de background
dM <- sdm::sdmData(species~., spgM, predictors = biocM, bg=list(method='gRandom', 
                                                             n=10000))
dM

# Ajustar os modelos, 50 replicacoes, 25 por Subsampling e 25 por Bootstrap
mM <- sdm::sdm(species~., dM, methods='maxent', replication=c('sub', 'boot'),
              test.p=30, n=25, parallelSettings=list(ncore=5, method='parallel'))

# NOTAS: Como mencionado no item 2, o parametro ncore e a quantidade de cores de
# processamento utilizados para a modelagem, altere conforme a configuracao do 
# computador

mM

# Plot das contribuicoes das variaveis
plot(getVarImp(mM), 'AUC')
sdm::getVarImp((mM))

# Para abrir uma interface de exploracao do modelo
sdm::gui(mM)


################################################################################
#--------- 4. PROJECAO DO MODELO PARA O PRESENTE   ---------#

# Projecao dos 50 modelos criados na seção anterior para o presente
p1M <- predict(mM, biocM, filename='./Resultados_bokermanni/presente.img',
              overwrite=TRUE)
p1M

# Obter um modelo consenso dentre os 50 criados para o presente por meio da 
# mediana ponderada
enM <- sdm::ensemble(mM, biocM, filename = 
                        './Resultados_bokermanni/ensemble_presente.img', 
                    setting =list(method='weighted', stat='tss', opt=2), 
                    overwrite=TRUE)
enM

# Verificacao
plot(p1M)
plot(enM)

################################################################################
#--------- 5. PROJECAO DO MODELO PARA O FUTURO (RCP45)   ---------#

# Selecionar apenas as biovariaveis 9, 17, 15
bioS45M <- raster::subset(bio45, c(9, 17, 15))
plot(bioS45M)

# Predicao utilizando o modelo criado na secao 3 para as camadas de RCP45
p2M <- predict(mM, bioS45M, filename='./Resultados_bokermanni/modelos_RCP45.img',
              overwrite=TRUE)
p2M

# Obter um modelo consenso dentre os 50 criados para o futuro RCP45 por meio da 
# mediana ponderada
en45M <- sdm::ensemble(mM, bioS45M, filename='./Resultados_bokermanni/futuro_RCP45.img',
                      setting =list(method='weighted', stat='tss', opt=2), 
                      overwrite=TRUE)
en45M

# Verificacao
plot(p2M)
plot(en45M)

################################################################################
#--------- 6. PROJECAO DO MODELO PARA O FUTURO (RCP85)   ---------#

# Selecionar apenas as biovariaveis 9, 17, 15
bioS85M <- raster::subset(bio85, c(9, 17, 15))
plot(bioS85M)

# Predicao utilizando o modelo criado na secao 3 para as camadas de RCP85
p3M <- predict(mM, bioS85M, filename='./Resultados_bokermanni/modelos_RC85.img',
              overwrite=TRUE)
p3M

# Obter um modelo consenso dentre os 50 criados para o futuro RCP85 por meio da 
# mediana ponderada
en85M <- sdm::ensemble(mM, bioS85M, filename='./Resultados_bokermanni/futuro_RC85.img',
                      setting =list(method='weighted', stat='tss', opt=2), 
                      overwrite=TRUE)

plot(en85M)

################################################################################
#--------- 7. CONSTRUCAO DE mapaP45 BINARIOS E
#             DE DENSIDADE DE PROBABILIDADE  ---------#

# Paleta de cores
cores <- grDevices::colorRampPalette(c(
    '#3E49BB', '#3498DB', 'yellow', 'orange', 'red', 'darkred'))

# Plot dos mapa com a nova paleta de cores
plot(enM, col=cores(200))
plot(en45M, col=cores(200))
plot(en85M, col=cores(200))

# Visualizacao alteranativa dos mapaP45 por meio do mapview
mapview::mapview(stack(enM,en45M,en85M)) 

#----------
### Mapa de alteracao de adequabilidade (NAO BINARIO) entre o presente e o futuro 
# de RCP 45
ch45M <- en45M - enM

cores2<-grDevices::colorRampPalette(c('red', 'orange', 'yellow', 'gray', 
                                      'green', 'blue'))
plot(ch45M, col=cores2(200))
# Areas mais proximas ao azul representam areas ganhas no futuro (RCP45)
# Areas mais proximas ao vermelho representam areas ganhas perdidas (RCP45)
# Areas cinzas permanceram inalteradas no futuro

#----------
### Mapa de alteracao de adequabilidade (NAO BINARIO) entre o presente e o futuro 
# de RCP 85
ch85M <- en85M - enM

cores2<-grDevices::colorRampPalette(c('red', 'orange', 'yellow', 'gray', 
                                      'green', 'blue'))
plot(ch85M, col=cores2(200))


#----------
### OBTER MEDIDAS DE THRESHOLD PARA A CONSTRUCAO DOS mapaP45 BINARIOS
dfM <- as.data.frame(dM)
dfM <- data.frame(species=dfM$species, coordinates(dM))
xyM = as.matrix(dfM[,c('x', 'y')])
head(xyM)

# Extrair do raster da predicao do presente os valores das biovariaveis nos 
# pontos de ocorrencias das especies
pM<-raster::extract(enM,xyM)

# Avaliacao do modelo
evM <- evaluates(dfM$species,pM)
evM@statistics

# Medidadas de threshold
evM@threshold_based

thM <- evM@threshold_based$threshold[2] #Threshold pelo metodo SSS: max(espec+sens)
thM

#----------
### MAPA BINARIO DO PRESENTE UTILIZANDO O VALOR DE THRESHOLD th
pa1M<- raster(enM)
pa1M[] <- ifelse(enM[] >= thM, 1,0)
plot(pa1M)

### MAPA BINARIO DO FUTURO (RCP45)
pa2M <- raster(en45M)
pa2M[] <- ifelse(en45M[] >= thM, 1,0)
plot(pa2M)

### MAPA BINARIO DO FUTURO (RCP85)
pa3M <- raster(en85M)
pa3M[] <- ifelse(en85M[] >= thM, 1,0)
plot(pa3M)

### MAPA BINARIO DE ALTERACAO DE ADEQUABILIDADE (Futuro RCP45 - Presente)
chp45M <- pa2M - pa1M 
plot(chp45M, col=c('red','gray','blue'))

### MAPA BINARIO DE ALTERACAO DE ADEQUABILIDADE (Futuro RCP85 - Presente)
chp85M <- pa3M - pa1M 
plot(chp85M, col=c('red','gray', 'blue'))

################################################################################
#--------- 7. CLASSIFICACAO DAS ALTERACOES DE AREA   ---------#

# Obter os tamanhos das celulas
cel_tam<-area(pa1M, na.rm=TRUE, weights=FALSE)
cel_tam<-cel_tam[!is.na(cel_tam)]

#----------
### AREA PRESENTE  (KM^2)
mapaM <- pa1M$layer@data@values==1
tamanhoM <- sum(mapaM[!is.na(mapaM)])
areaM <- tamanhoM*median(cel_tam)
areaM

#----------
###  AREA FUTURA (RCP45)  (KM^2)
mapaF45M <- pa2M$layer@data@values==1
tamanhoF45M <- sum(mapaF45M[!is.na(mapaF45M)])
areaF45M <- tamanhoF45M*median(cel_tam)
areaF45M

#----------
###  AREA FUTURA (RCP85)  (KM^2)
mapaF85M <- pa3M$layer@data@values==1
tamanhoF85M <- sum(mapaF85M[!is.na(mapaF85M)])
areaF85M <- tamanhoF85M*median(cel_tam)
areaF85M

#----------
###  AREA ALTERADA ENTRE O PRESENTE E O FUTURO RCP45 (KM^2)
# area de perda < 0
# area de ganho > 0

mapaP45M <- chp45M$layer@data@values < 0
tamanhoP45M <- sum(mapaP45M[!is.na(mapaP45M)])
areaP45M <- tamanhoP45M*median(cel_tam)
areaP45M        # Area perdida

# Porcentagem de perda
(areaP45M/area)*100


mapaG45M <- chp45M$layer@data@values >0
tamanhoG45M <- sum(mapaG45M[!is.na(mapaG45M)])
areaG45M <- tamanhoG45M*median(cel_tam)
areaG45M        # Area ganha

# Porcentagem de ganho
(areaG45M/area)*100


#----------
###  AREA ALTERADA ENTRE O PRESENTE E O FUTURO RCP85 (KM^2)
# area de perda < 0
# area de ganho > 0

mapaP85M <- chp85M$layer@data@values < 0
tamanhoP85M <- sum(mapaP85M[!is.na(mapaP85M)])
areaP85M <- tamanhoP85M*median(cel_tam)
areaP85M        # Area perdida

# Porcentagem de perda
(areaP85M/area)*100


mapaG85M <- chp85M$layer@data@values >0
tamanhoG85M <- sum(mapaG85M[!is.na(mapaG85M)])
areaG85M <- tamanhoG85M*median(cel_tam)
areaG85M        # Area ganha

# Porcentagem de ganho
(areaG85M/area)*100

################################################################################

# Salvar os mapas dos modelos como .png

# Mapa presente
png('./Graficos/L_bokermanni_modelos/L_bokermanni_presente.png', height=nrow(enM), 
    width=ncol(enM)) 
plot(enM, maxpixels=ncell(enM))
dev.off()

# Mapa RCP45
png('./Graficos/L_bokermanni_modelos/L_bokermanni_RCP45.png', height=nrow(en45M), 
    width=ncol(en45M)) 
plot(en45M, maxpixels=ncell(en45M))
dev.off()

# Mapa RCP85
png('./Graficos/L_bokermanni_modelos/L_bokermanni_RCP85.png', height=nrow(en85M), 
    width=ncol(en85M)) 
plot(en85M, maxpixels=ncell(en85M))
dev.off()


# Mapa presente binário
png('./Graficos/L_bokermanni_modelos/L_bokermanni_presente_binário.png', 
    height=nrow(pa1M), width=ncol(pa1M)) 
plot(pa1M, maxpixels=ncell(pa1M))
dev.off()

# Mapa RCP45 binário
png('./Graficos/L_bokermanni_modelos/L_bokermanni_RCP45_binário.png', 
    height=nrow(pa2M), width=ncol(pa2M)) 
plot(pa2M, maxpixels=ncell(pa2M))
dev.off()

# Mapa RCP85 binário
png('./Graficos/L_bokermanni_modelos/L_bokermanni_RCP85_binário.png', 
    height=nrow(pa3M), width=ncol(pa3M)) 
plot(pa3M, maxpixels=ncell(pa3M))
dev.off()


# Mapa de alteracao RCP45
png('./Graficos/L_bokermanni_modelos/L_bokermanni_ch45.png', height=nrow(ch45M), 
    width=ncol(ch45M)) 
plot(ch45M, maxpixels=ncell(ch45M))
dev.off()

# Mapa de alteracao rcp85
png('./Graficos/L_bokermanni_modelos/L_bokermanni_ch85.png', height=nrow(ch85M), 
    width=ncol(ch85M)) 
plot(ch85M, maxpixels=ncell(ch85M))
dev.off()

# Mapa de alteracao RCP45 binario
png('./Graficos/L_bokermanni_modelos/L_bokermanni_ch45_binario.png', height=nrow(chp45M), 
    width=ncol(chp45M)) 
plot(chp45M, col=c('red','gray','blue'))
dev.off()

# Mapa de alteracao RCP85 binario
png('./Graficos/L_bokermanni_modelos/L_bokermanni_ch85_binario.png', height=nrow(chp85M), 
    width=ncol(chp85M)) 
plot(chp85M, col=c('red','gray','blue'))
dev.off()

################################ FIM ###########################################