############## SCRIPTS DO PROJETO DE PDPD ##############

# 1. TRATAMENTO DAS VARIÁVEIS AMBIENTAIS: Cortesusando
#    a máscara criada anteriormente, reprojeção econversão 
#    para o formato .asc que será utilizado na modelagem


################################################################################

##### Carregamento das bibliotecas necessárias

if (!require(raster)) install.packages('raster')
if (!require(rgdal)) install.packages('rgdal')

# Permite que dados espaciais sejam associados com o sistema de coordenadas,
# criando uma projeção que pode ser utilizada nos rasters

proj_WGS <- sp::CRS(
    "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


################################################################################

#--------- 1. TRATAMENTO DAS CAMADAS AMBIENTAIS ---------#


# Carregamento da máscara criada no script 02
mascara <- shapefile('Dados/Mascaras/mascara_biomas.shp')

# Adicionar a projeção definida anteriormente
raster::crs(mascara) <- proj_WGS

# Verificar dados
mascara
plot(mascara)

