############## SCRIPTS DO PROJETO DE PDPD ##############

# 1. Criacao dos mapas dos modelos de distribuicao potencial
#    da especie E. subsecundum
# 2. Criacao dos mapas dos modelos de distribuicao potencial
#    da especie L. bokermanni
# 3. Criacao dos mapas de sobreposicao

################################################################################

##### Carregamento das bibliotecas necessarias
if (!require(sdm)) install.packages('sdm')
if (!require(dismo)) install.packages('dismo')
if (!require(raster)) install.packages('raster')


library(ggplot2)
library(tidyverse)

################################################################################

#--------- 1. MAPAS E. SUBSECUNDUM ---------#

raster <- en
raster

brasil <- rnaturalearth::ne_countries(country="brazil", returnclass = 'sf')
plot(brasil)

# converter os dados do raster para dataframe
rasdf <- as.data.frame(raster, xy=TRUE) %>% 
    drop_na()
head(rasdf)

# plot
