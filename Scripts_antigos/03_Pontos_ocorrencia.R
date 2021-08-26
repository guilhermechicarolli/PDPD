############## SCRIPTS DO PROJETO DE PDPD ##############

# 1. TRATAMENTO DOS PONTOS DE OCORRÊNCIA DA ESPÉCIE DE 
#    PLANTA E.SUBSECUNDUMPARA A REMOÇÃO DO VIÉS ESPACIAL:
#    Remoção de pontos duplicados e criar também um buffer
#    com raio de 5 km em cada ponto e escolher apenas um 
#    ponto dentro de cada buffer
#
# 2. TRATAMENTO DOS PONTOS DE OCORRÊNCIA DA ESPÉCIES DE
#    MORCEGO L. BOKERMANNI


################################################################################

##### Carregamento das bibliotecas necessárias

if (!require(dismo)) install.packages('dismo')
if (!require(rgdal)) install.packages('rgdal')
if (!require(raster)) install.packages('raster')
if (!require(rgeos)) install.packages('rgeos')
if (!require(sp)) install.packages('sp')
if (!require(tidyverse)) install.packages('tidyverse')



# Permite que dados espaciais sejam associados com o sistema de coordenadas,
# criando uma projeção que pode ser utilizada nos rasters
proj_WGS <- sp::CRS(
    "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

################################################################################

#--------- 1. TRATAMENTO DO PONTOS DE OCORRÊNCIA
#           DA ESPÉCIE DE PLANTA E. SUBSECUNDUM ---------#



# Carregamento dos pontos de ocorrência, dando o nome da variável de 'planta'
planta <- read.csv('Dados/Ocorrencias/E_subsecundum.csv')

# Verificação
head(planta)


# Carregamento da camada representante para usar de molde
camada_rep <- raster::raster('./Dados/Camadas_presente/wc2.1_30s_bio_1.asc')

# Adicionar projeção
raster::crs(camada_rep) <- proj_WGS

# Verificação
plot(camada_rep)


##### REMOÇÃO DOS PONTOS DUPLICADOS

planta <- planta %>%
    distinct(Latitude, Longitude, .keep_all=TRUE)

# 48 pontos
length(planta[,1])


##### REMOÇÃO DO VIÉS GEOGRÁFICO AMOSTRAL


pontos_planta <- planta[,2:3]

# Converção dos pontos geográficos para SpatialPoints
sp::coordinates(pontos_planta) <- ~Longitude+Latitude

# Adicionar a projeção
raster::crs(pontos_planta) <- proj_WGS

# Adicionar um buffer de 5 km à cada ponto de ocorrência
buffer <- dismo::circles(pontos_planta, d = 5000, lonlat=TRUE, dissolve=TRUE)

# Verificação
plot(buffer)


# conversão dos buffers (círculos) para polígonos
buffer <- polygons(buffer)

# Rasterização dos polígonos
buffer <- rasterize(buffer, camada_rep)


# Seleção de apenas um ponto de ocorrência por círculo 
E_sub <- gridSample(pontos_planta, buffer, n=1)

# 37 pontos de ocorrência resultantes
length(E_sub[,1])


# Salvar os pontos resultante como data frame na pasta Dados/Ocorrencias
resultado_E_subsecundum <- as.data.frame(E_sub)

write.csv(resultado_E_subsecundum, 
          './Dados/Ocorrencias/E_subsecundum_corrigido.csv', row.names = FALSE)


################################################################################

#--------- 2. TRATAMENTO DO PONTOS DE OCORRÊNCIA
#           DA ESPÉCIE DE MORCEGO L. BOKERMANNI ---------#



# Carregamento dos pontos de ocorrência, dando o nome da variável de 'morcego'
morcego <- read.csv('Dados/Ocorrencias/L_bokermanni.csv')

# Verificação
head(morcego)


##### REMOÇÃO DOS PONTOS DUPLICADOS

morcego <- morcego %>%
    distinct(Latitude, Longitude, .keep_all=TRUE)

# 9 pontos
length(morcego[,1])


##### REMOÇÃO DO VIÉS GEOGRÁFICO AMOSTRAL


pontos_morcego <- morcego[,2:3]

# Converção dos pontos geográficos para SpatialPoints
sp::coordinates(pontos_morcego) <- ~Longitude+Latitude

# Adicionar a projeção
raster::crs(pontos_morcego) <- proj_WGS


# Adicionar um buffer de 5 km à cada ponto de ocorrência
buffer2 <- dismo::circles(pontos_morcego, d = 5000, lonlat=TRUE, dissolve=TRUE)

# Verificação
plot(buffer2)


# conversão dos buffers (círculos) para polígonos
buffer2 <- polygons(buffer2)

# Rasterização dos polígonos
buffer2 <- rasterize(buffer2, camada_rep)


# Seleção de apenas um ponto de ocorrência por círculo 
L_boker <- gridSample(pontos_morcego, buffer, n=1)

# 8 pontos de ocorrência resultantes
length(L_boker[,1])


# Salvar os pontos resultante como data frame na pasta Dados/Ocorrencias
resultado_E_subsecundum <- as.data.frame(L_boker)

write.csv(resultado_E_subsecundum, 
          './Dados/Ocorrencias/L_bokermanni_corrigido.csv', row.names = FALSE)



################################ FIM ###########################################