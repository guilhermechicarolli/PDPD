############## SCRIPTS DO PROJETO DE PDPD ##############

# 1. Seleção das variáveis mais adequadas para a modelagem
#    através da Análise de Componentes Principais (PCA) para
#    a espécie de planta Mimosa lewisii
#
# 2. Seleção das variáveis através do PCA para a espécies de 
#    morcego Lonchophylla bokermanni


################################################################################

##### Carregamento das bibliotecas necessárias

if (!require(dismo)) install.packages('dismo')
if (!require(rgdal)) install.packages('rgdal')
if (!require(raster)) install.packages('raster')
if (!require(sp)) install.packages('sp')
if (!require(usdm)) install.packages('usdm')
if (!require(vegan)) install.packages('vegan')



# Permite que dados espaciais sejam associados com o sistema de coordenadas,
# criando uma projeção que pode ser utilizada nos rasters
proj_WGS <- sp::CRS(
    "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

################################################################################

#--------- 1. SELEÇÃO DE VARIÁVEIS USANDO PCA PARA A
#                ESPÉCIE DE PLANTA MIMOSA LEWISII  ---------#

### CARREGAMENTO DOS DADOS

# Carregamento dos pontos 
pontos_planta <- read.csv('./Dados/Ocorrencias/E_subsecundum_corrigido.csv')

# Converção dos pontos geográficos para SpatialPoints
sp::coordinates(pontos_planta) <- ~Longitude+Latitude

# Adicionar a projeção
raster::crs(pontos_planta) <- proj_WGS

# Verificação dos dados
pontos_planta


# Carregamento das camadas ambientais raster cortadas no script 02 
camadas <- list.files(path='./Dados/Camadas_presente/', pattern='.asc', 
                      full.names=TRUE) 

camadas <- raster::stack(camadas)

# Adicionar a projeção geográfica 
raster::crs(camadas) <- proj_WGS

# Verificação dos dados
camadas


# Obter os valores das camadas ambientais nos pontos de ocorrência
valores_planta <- raster::as.data.frame(raster::extract(camadas, pontos_planta))

# Verificação dos dados
summary(valores_planta)


### ANÁLISE DE COMPONENTES PRINCIPAIS (PCA)












################################################################################

#--------- 1. SELEÇÃO DE VARIÁVEIS USANDO PCA PARA A
#          ESPÉCIE DE MORCEGO LONCHOPHYLLA BOKERMANNI ---------#

### CARREGAMENTO DOS DADOS

# Carregamento dos pontos 
pontos_morcego <- read.csv('./Dados/Ocorrencias/L_bokermanni_corrigido.csv')

# Converção dos pontos geográficos para SpatialPoints
sp::coordinates(pontos_morcego) <- ~Longitude+Latitude

# Adicionar a projeção
raster::crs(pontos_morcego) <- proj_WGS

# Verificação dos dados
pontos_morcego


# Obter os valores das camadas ambientais nos pontos de ocorrência da espécie
valores_morcego <- raster::as.data.frame(
    raster::extract(camadas, pontos_morcego))

# Verificação dos dados
summary(valores_morcego)


### ANÁLISE DE COMPONENTES PRINCIPAIS (PCA)












################################ FIM ###########################################