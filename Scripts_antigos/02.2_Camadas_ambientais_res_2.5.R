############## SCRIPTS DO PROJETO DE PDPD ##############

# 1. CONSTRUÇÃO DA MÁSCARA COM BASE NO SHAPE FILE DO BRASIL
#
# 2. TRATAMENTO DAS VARIÁVEIS AMBIENTAIS DO PRESENTE: Cortes usando
#    a máscara criada anteriormente, reprojeção e conversão 
#    para o formato .asc que será utilizado na modelagem
#
# 3. TRATAMENTO DAS VARIÁVEIS AMBIENTAIS DA PROJEÇÃO
#    DE 2070 (RCP 4.5 e 8.0) E 2.5 arc min DE RESOLUÇÃO


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

#--------- 2. TRATAMENTO DAS CAMADAS AMBIENTAIS DO PRESENTE
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
camada_rep <- raster::raster(
    'Dados/Camadas_brutas_res_2.5Presente_res_2.5/wc2.1_2.5m_bio_1.tif')

# Adicionar a projeção
raster::crs(camada_rep) <- proj_WGS

# Verificação
plot(camada_rep)


# Carregamento de todas as variáveis ambientais raster 
camadas <- list.files(path='Dados/Camadas_brutas/Presente/', pattern='.tif', 
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

# Carregamento da máscara criada anteriormente
mascara <- raster::shapefile('Dados/Mascaras/mascara_brasil.shp')

# Adicionar a projeção
raster::crs(mascara) <- proj_WGS

# Verificar dados
mascara
plot(mascara)

# Carregamento de uma camada representante, escolhida a camada 'bio1'
camada_rep45 <- raster::raster(
    'Dados/Camadas_brutas/Futuro_RCP45/ac45bi701.tif')

# Adicionar a projeção
raster::crs(camada_rep45) <- proj_WGS

# Verificação
plot(camada_rep45)


# Carregamento de todas as variáveis ambientais raster 
camadas45 <- list.files(path='Dados/Camadas_brutas/Futuro_RCP45/',
                        pattern='.tif', full.names = TRUE)

camadas45 <- raster::stack(camadas45)

# Adicionar a projeção às camadas
crs(camadas45) <- proj_WGS

# Verificar dados
camadas45


# Reduzir o tamanho da camada representante para um retângulo
corte_cam45 <- raster::crop(camada_rep45, extent(mascara))

# Verificação
plot(corte_cam45)


# Reduzir o tamanho de todas as camadas ambientais do presente
cortes_final45 <- raster::resample(camadas45, corte_cam45, method="bilinear", 
                                   snap='out', bylayer=TRUE, progress='text')

# Verificação
cortes_final45
plot(cortes_final45)

# Cortar as camadas ambientais e cortar a partir da máscara criada
camadas_final45 <- raster::mask(cortes_final45, mascara, bylayer=TRUE)

# Verificação
plot(camadas_final45)


# Salvar as camadas na pasta "Camadas_Futuro_RCP45" no formato ".asc"
raster::writeRaster(camadas_final45, paste0("Dados/Camadas_Futuro_RCP45/", 
                                            paste0(names(camadas_final45),".asc")), 
                    driver='ascii', bylayer=TRUE)



################################################################################

#--------- 5. TRATAMENTO DAS CAMADAS AMBIENTAIS DO FUTURO: RCP 8.5
#                    (variáveis bioclimáticas)  ---------#


# Carregamento da máscara criada anteriormente
mascara <- raster::shapefile('Dados/Mascaras/mascara_brasil.shp')

# Adicionar a projeção
raster::crs(mascara) <- proj_WGS

# Verificar dados
mascara
plot(mascara)

# Carregamento de uma camada representante, escolhida a camada 'bio1'
camada_rep85 <- raster::raster(
    'Dados/Camadas_brutas/Futuro_RCP85/ac85bi701.tif')

# Adicionar a projeção
raster::crs(camada_rep85) <- proj_WGS

# Verificação
plot(camada_rep85)


# Carregamento de todas as variáveis ambientais raster 
camadas85 <- list.files(path='Dados/Camadas_brutas/Futuro_RCP85/',
                        pattern='.tif', full.names = TRUE)

camadas85 <- raster::stack(camadas85)

# Adicionar a projeção às camadas
crs(camadas85) <- proj_WGS

# Verificar dados
camadas85


# Reduzir o tamanho da camada representante para um retângulo
corte_cam85 <- raster::crop(camada_rep85, extent(mascara))

# Verificação
plot(corte_cam85)


# Reduzir o tamanho de todas as camadas ambientais do presente
cortes_final85 <- raster::resample(camadas85, corte_cam85, method="bilinear", 
                                   snap='out', bylayer=TRUE, progress='text')

# Verificação
cortes_final85
plot(cortes_final85)

# Cortar as camadas ambientais e cortar a partir da máscara criada
camadas_final85 <- raster::mask(cortes_final85, mascara, bylayer=TRUE)

# Verificação
plot(camadas_final85)


# Salvar as camadas na pasta "Camadas_Futuro_RCP85" no formato ".asc"
raster::writeRaster(camadas_final85, paste0("Dados/Camadas_Futuro_RCP85/",
                                            paste0(names(camadas_final85),".asc")), 
                    driver='ascii', bylayer=TRUE)



################################ FIM ###########################################