library(dismo)
if (!require(RStoolbox)) install.packages('RStoolbox')


proj_WGS <- sp::CRS(
    "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")



presvals <- read.csv('./Dados/TESTE_MODELAGEM/pca_scores_acaule.csv')


# Carregamento das camadas ambientais raster cortadas no script 02 
predictors <- list.files(path='./Dados/Camadas_res_2.5_2050_cortadas/Presente_teste_acaule/', pattern='.asc', 
                      full.names=TRUE) 
predictors <- raster::stack(predictors)
# Adicionar a projeção geográfica 
raster::crs(predictors) <- proj_WGS
# Verificação dos dados
predictors



pca_raster <- rasterPCA(predictors, nSamples=NULL, nComp = 3, spca = TRUE)
