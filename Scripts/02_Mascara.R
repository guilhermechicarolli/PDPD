############## SCRIPTS DO PROJETO DE PDPD ##############

# 1. CONSTRUÇÃO DA MÁSCARA COM BASE NO SHAPE FILE DOS 
#    BIOMAS DA MATA ATLÂNTICA, CAATINGA E CERRADO


################################################################################

##### Carregamento das bibliotecas necessárias

if (!require(rgdal)) install.packages('rgdal')
if (!require(raster)) install.packages('raster')
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


################################ FIM ###########################################