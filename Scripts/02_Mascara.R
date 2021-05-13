############## SCRIPTS DO PROJETO DE PDPD ##############

# 1. CONSTRUÇÃO DA MÁSCARA COM BASE NO SHAPE FILE DOS 
#    BIOMAS DA MATA ATLÂNTICA, CAATINGA E CERRADO


################################################################################

# Carregamento dos pacotes necessários

if (!require(rgdal)) install.packages('rgdal')
if (!require(raster)) install.packages('raster')
if (!require(maptools)) install.packages('maptools')
if (!require(sp)) install.packages('sp')
if (!require(rgeos)) install.packages('rgeos')
if (!require(sampSurf)) install.packages('sampSurf')


# Permite que dados espaciais sejam associados com o sistema de coordenadas
# criando uma projeção

longlat_WGS <- CRS(
    "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


################################################################################

# 1. CONSTRUÇÃO DA MÁSCARA COM BASE NO SHAPE FILE DOS BIOMAS

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
crs(CE_CA_MA) = longlat_WGS

# Salvar a máscara criada na pasta Dados/Mascaras
writeOGR(CE_CA_MA, "./Dados/Mascaras", "mascara_biomas",
         driver="ESRI Shapefile", overwrite_layer = T)


################################ FIM ###########################################