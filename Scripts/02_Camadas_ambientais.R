############## SCRIPTS DO PROJETO DE PDPD ##############

# 1. CONSTRUÇÃO DA MÁSCARA COM BASE NO SHAPE FILE DO BRASIL
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
if (!require(sf)) install.packages('sf')


# Permite que dados espaciais sejam associados com o sistema de coordenadas,
# criando uma projeção que pode ser utilizada nos rasters

proj_WGS <- sp::CRS(
    "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


################################################################################

#--------- 1. CONSTRUÇÃO DA MÁSCARA COM BASE 
#               NO SHAPE FILE DO BRASIL ---------#


# Carregamento dos shape files do brasil
brasil <- rgdal::readOGR(dsn="Dados/Mascaras/", layer='UFEBRASIL')
brasil <- shapefile("Dados/Mascaras/UFEBRASIL.shp")
brasil <- read_sf("Dados/Mascaras/UFEBRASIL.shp")


head(brasil)

# Verificação dos dados a partir do plot do mapa
plot(brasil)

# Conversão para SpatialPolygonsDataFrame
brasil <- as(brasil, "SpatialPolygonsDataFrame")

# Acréscimo da projeção 
raster::crs(brasil) <- proj_WGS

# Salvar a máscara criada na pasta Dados/Mascaras
rgal::writeOGR(brasil, "./Dados/Mascaras", "mascara_brasil",
               driver="ESRI Shapefile", overwrite_layer = T)



################################################################################

#--------- 2. TRATAMENTO DAS CAMADAS AMBIENTAIS DO PRESENTE
#                    (variáveis bioclimáticas) ---------#


# Carregamento da máscara criada no passo 01
mascara <- shapefile('Dados/Mascaras/mascara_brasil.shp')

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