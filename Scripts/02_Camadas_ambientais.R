############## SCRIPTS DO PROJETO DE PDPD ##############

# 1. CONSTRUÇÃO DA MÁSCARA COM BASE NO SHAPE FILE DOS 
#    BIOMAS DA MATA ATLÂNTICA, CAATINGA E CERRADO
#
# 2. TRATAMENTO DAS VARIÁVEIS AMBIENTAIS DO PRESENTE: Cortes usando
#    a máscara criada anteriormente, reprojeção e conversão 
#    para o formato .asc que será utilizado na modelagem
#
# 3. TRATAMENTO DAS VARIÁVEIS AMBIENTAIS DA PROJEÇÃO
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
#              NO SHAPE FILE DOS BIOMAS ---------#


# Carregamento dos shape files dos biomas
biomas <- rgdal::readOGR("Dados/Biomas_250mil/lm_bioma_250.shp")

head(biomas)

# Verificação dos dados a partir do plot do mapa
plot(biomas)

# Selecionar apenas os biomas do Cerrado, Caatinga e Mata Atlântica
CE_CA_MA <-biomas[biomas$Bioma=="Cerrado" | biomas$Bioma=="Caatinga" |
                      biomas$Bioma=="Mata AtlÃ¢ntica",]

head(CE_CA_MA)

# Verificação dos dados
plot(CE_CA_MA)

# Conversão para SpatialPolygonsDataFrame
CE_CA_MA <- as(CE_CA_MA, "SpatialPolygonsDataFrame")

# Acréscimo da projeção 
raster::crs(CE_CA_MA) <- proj_WGS

# Salvar a máscara criada na pasta Dados/Mascaras
rgal::writeOGR(CE_CA_MA, "./Dados/Mascaras", "mascara_biomas",
               driver="ESRI Shapefile", overwrite_layer = T)



################################################################################

#--------- 2. TRATAMENTO DAS CAMADAS AMBIENTAIS DO PRESENTE
#                    (variáveis bioclimáticas) ---------#


# Carregamento da máscara criada no passo 01
mascara <- shapefile('Dados/Mascaras/mascara_biomas.shp')

# Adicionar a projeção definida anteriormente
raster::crs(mascara) <- proj_WGS

# Verificar dados
mascara
plot(mascara)

# Carregamento de uma camada representante, escolhida a camada 'bio1', que 
# representa a média anual de temperatura, com resolução de 30 arcsegundos
camada_rep <- raster('Dados/wc2.1_30s_bio_1.tif')

# Adicionar a projeção
raster::crs(camada_rep) <- proj_WGS

# Verificação
plot(camada_rep)


# CONTINUA AINDA



################################ FIM ###########################################