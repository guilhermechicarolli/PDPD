############## SCRIPTS DO PROJETO DE PDPD ##############

# 1. CONSTRUÇÃO DA MÁSCARA COM BASE NO SHAPE FILE DO BRASIL
#
# 2. DOWNLOAD DAS CAMADAS DO PRESENTE
#
# 3. TRATAMENTO DAS VARIÁVEIS AMBIENTAIS DO PRESENTE: Cortes usando
#    a máscara criada anteriormente, reprojeção e conversão 
#    para o formato .asc que será utilizado na modelagem
#
# 4. TRATAMENTO DAS VARIÁVEIS AMBIENTAIS DA PROJEÇÃO
#    DE 2070 (RCP 4.5 e 8.0)


################################################################################

##### Carregamento das bibliotecas necessárias

if (!require(raster)) install.packages('raster')
if (!require(rgdal)) install.packages('rgdal')
if (!require(sp)) install.packages('sp')



# Permite que dados espaciais sejam associados com o sistema de coordenadas,
# criando uma projeção que pode ser utilizada nos rasters

proj_WGS <- sp::CRS(
    "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


################################################################################

#--------- 1. CONSTRUÇÃO DA MÁSCARA COM BASE 
#               NO SHAPE FILE DO BRASIL ---------#


# Carregamento dos shape files do brasil 
brasil <- raster::getData("GADM", country='Brazil', level=0, 
                          path="./Dados/Mascaras/")

# Verificar os dados
head(brasil)

# Verificação dos dados a partir do plot do mapa
sp::plot(brasil)

# Acréscimo da projeção 
raster::crs(brasil) <- proj_WGS

# Salvar a máscara criada na pasta Dados/Mascaras
rgdal::writeOGR(brasil, "./Dados/Mascaras", "mascara_brasil",
               driver="ESRI Shapefile", overwrite_layer = T)



################################################################################

#--------- 2. DOWNLOAD DAS CAMADAS DO PRESENTE ---------#

vars <- getData(name='worldclim', var='bio', res=0.5, lon=-10, lat=-40, 
                path='./Dados/Camadas_brutas/') 

vars

plot(vars) # PLOT EM BRANCO (?)



k################################################################################

#--------- 3. TRATAMENTO DAS CAMADAS AMBIENTAIS DO PRESENTE
#                    (variáveis bioclimáticas) ---------#


# Carregamento da máscara criada no passo 01
mascara <- raster::shapefile('Dados/Mascaras/mascara_brasil.shp')

# Adicionar a projeção definida anteriormente
raster::crs(mascara) <- proj_WGS

# Verificar dados
mascara
plot(mascara)

# Carregamento de uma camada representante, escolhida a camada 'bio1', que 
# representa a média anual de temperatura, com resolução de 30 arcsegundos
camada_rep <- raster::raster('Dados/Camadas_brutas/wc2.1_30s_bio_1.tif')

# Adicionar a projeção
raster::crs(camada_rep) <- proj_WGS

# Verificação
plot(camada_rep)


# Carregamento de todas as variáveis ambientais raster 
# OBS: A pasta "Camadas_brutas", que contém todas as camadas raster foi 
# apagada após o corte das mesmas, por necessitarem de cerca de 15 Gb de 
# armazenamento, sobrando então apenas as camadas já cortadas com o shape file
# do Brasil

camadas <- list.files(path='Dados/Camadas_brutas/', pattern='.tif', 
                      full.names = TRUE)

camadas <- raster::stack(camadas)

# Adicionar a projeção às camadas
crs(camadas) <- proj_WGS

# Verificar dados
camadas



# Reduzir o tamanho da camada representante para um retângulo, que será depois
# cortado a partir da máscara
corte_cam <- raster::crop(camada_rep, extent(mascara))

# Verificação
plot(corte_cam)


# Reduzir o tamanho de todas as camadas ambientais do presente
cortes_final <- raster::resample(camadas, corte_cam, method="bilinear", 
                                snap='out', bylayer=TRUE, progress='text')

# Verificação
cortes_final
plot(cortes_final)

# Cortar as camadas ambientais e cortar a partir da máscara
camadas_final <- raster::mask(cortes_final, mascara, bylayer=TRUE)

# Verificação
plot(camadas_final)



# Salvar as camadas na pasta "Camadas_presente" no formato ".asc"
raster::writeRaster(camadas_final, paste0("Dados/Camadas_presente/", 
                                    paste0(names(camadas_final),".asc")), 
            driver='ascii', bylayer=TRUE)


################################################################################

#--------- 4. TRATAMENTO DAS CAMADAS AMBIENTAIS DO FUTURO: RCP 4.5
#                    (variáveis bioclimáticas)  ---------#








################################ FIM ###########################################