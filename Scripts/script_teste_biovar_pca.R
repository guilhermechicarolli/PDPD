 ############## SCRIPTS DO PROJETO DE PDPD ##############

# 1. Transformação do conjunto de 19 camadas ambientais (bio variáveis) 
#    em 3 conjuntos de camadas através do PCA (Principal Component Analysis),
#    tanto das camadas presente como futuras


################################################################################

##### Carregamento das bibliotecas necessárias
 
if (!require(RStoolbox)) install.packages('RStoolbox')
if (!require(raster)) install.packages('raster')
if (!require(sp)) install.packages('sp')

 
# Permite que dados espaciais sejam associados com o sistema de coordenadas,
# criando uma projeção que pode ser utilizada nos rasters
proj_WGS <- sp::CRS(
    "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

################################################################################

#--------- 1. TRANSFORMAÇÃO DAS BIOVARIÁVEIS DO  
#                  PRESENTE EM CAMADAS PCA  ---------#

### CARREGAMENTO DAS CAMADAS DO PRESENTE (RESOLUÇÃO DE 2.5 ARC SEC)

camadas_presente <- list.files(
    path='./Dados/Camadas_res_2.5_2050_cortadas/Presente_teste_acaule/', 
    pattern='.asc', full.names=TRUE) 

camadas_presente <- raster::stack(camadas_presente)
raster::crs(camadas_presente) <- proj_WGS

# Verificação
camadas_presente
plot(camadas_presente)


# Transformação das camadas camadas usando PCA
pca_raster <- RStoolbox::rasterPCA(camadas_presente, nSamples=NULL, nComp = 19, 
                                   spca = TRUE)

# Verificação
summary(pca_raster$model)
pca_raster$model$loadings

# Stack e atribuição da projeção geográfica
camadas_pca <- stack(pca_raster$map)
raster::crs(camadas_pca) <- proj_WGS

# Verificação
plot(camadas_pca)

# Salvar os arquivos raster
writeRaster(camadas_pca, filename=names(camadas_pca), bylayer=TRUE,
            format="ascii")

################################################################################

#--------- 2. TRANSFORMAÇÃO DAS BIOVARIÁVEIS DO FUTURO
#                  RCP 45 DE 2050 EM CAMADAS PCA  ---------#

### CARREGAMENTO DAS CAMADAS DO FUTURO RCP 45 (RESOLUÇÃO DE 2.5 ARC SEC)

camadas_rcp45 <- list.files(
    path='./Dados/Camadas_res_2.5_2050_cortadas/RCP45_teste_acaule/', 
    pattern='.asc', full.names=TRUE) 

camadas_rcp45 <- raster::stack(camadas_rcp45)
raster::crs(camadas_rcp45) <- proj_WGS

# Verificação
camadas_rcp45
plot(camadas_rcp45)


# Transformação das camadas camadas usando PCA
pca_raster_rcp45 <- RStoolbox::rasterPCA(camadas_rcp45, nSamples=NULL, nComp = 19, 
                                   spca = TRUE)

# Verificação
summary(pca_raster_rcp45$model)
pca_raster_rcp45$model$loadings

# Stack e atribuição da projeção geográfica
camadas_pca_rcp45 <- stack(pca_raster_rcp45$map)
raster::crs(camadas_pca_rcp45) <- proj_WGS

# Verificação
plot(camadas_pca_rcp45)

# Salvar os arquivos raster
writeRaster(camadas_pca_rcp45, filename=names(camadas_pca_rcp45), bylayer=TRUE,
            format="ascii")


################################ FIM ###########################################