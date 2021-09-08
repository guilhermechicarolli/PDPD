############## SCRIPTS DO PROJETO DE PDPD ##############

# 1. Criacao dos mapas de sobreposicao da distribuicao
#    potencial entre as espécies L. bokermanni e E. subsecundum

################################################################################

##### Carregamento das bibliotecas necessarias
if (!require(raster)) install.packages('raster')
if (!require(rgeos)) install.packages('rgeos')
if (!require(rgdal)) install.packages('rgdal')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(sf)) install.packages('sf')
if (!require(ggnewscale)) install.packages('ggnewscale')
if (!require(patchwork)) install.packages('patchwork')

################################################################################

#--------- 1. MAPAS DE SOBREPOSICAO ---------#


# Transformar os rasters dos modelos em poligonos
# Mapa binario do presente
SobPol <- raster::rasterToPolygons(Sob, dissolve = TRUE)
plot(SobPol)

# Mapa binario do futuro RCP45
SobPol2 <- raster::rasterToPolygons(Sob2, dissolve = TRUE)
plot(SobPol2)

# Mapa binario do futuro RCP85
SobPol3 <- raster::rasterToPolygons(Sob3, dissolve = TRUE)
plot(SobPol3)


################################################################################

# Carregar o mapa através do pacote Mapdata
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Extrair os dados dos biomas
biomas <- rgdal::readOGR("Dados/Biomas_250mil/lm_bioma_250.shp")
# Extrair dados da mata atlântica e Caatinga
MA <- ggplot2::fortify(biomas[biomas$Bioma=="Mata AtlÃ¢ntica",])
CA <- ggplot2::fortify(biomas[biomas$Bioma=="Caatinga",])
CE <- ggplot2::fortify(biomas[biomas$Bioma=="Cerrado",])

MA_CA_CE <- rbind(MA, CA, CE)



#### MAPA BINÁRIO PRESENTE DE SOBREPOSICAO

tS1 <- ggplot2::fortify(SobPol)
unique(tS1$id)
head(tS1)
tS1 <- tS1[which(tS1$id!=1),]


ggplot2::ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = tS1, aes( x = long, y = lat, group = group, fill=id))



# Com biomas
mapa_sobreposicaoB<- ggplot2::ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = tS1, aes( x = long, y = lat, group = group, fill=id))+
    # Adicionar a barra de escala
    ggspatial::annotation_scale(location = "br", width_hint = 0.2,
                                bar_cols = c("grey30", "white")) +
    
    # Adicionar a flecha de orientação para o Norte
    ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                      height = unit(1.5, "cm"), 
                                      width = unit(1.5, "cm"),
                                      style = ggspatial::north_arrow_fancy_orienteering(
                                          fill = c("white","grey30"))) +
    #Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    
    # Adicionar as legendas
    scale_fill_manual(name="Sobreposição",
                      values = c('coral1','darkolivegreen', '#0072B5FF'),
                      labels = c("Apenas morcego", "Apenas planta", 'Sobreposição')) +
    
    guides(color = guide_legend(override.aes = list(fill = "white"))) +
    
    # Ajustar a legenda 
    theme(legend.position = c(0.86,0.2),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(0.5,4), "lines")) +
    
    new_scale_fill() +
    
    # Adicionar os poligonos
    geom_polygon(data = MA_CA_CE, aes(x = long, y = lat, group=group), fill=NA, 
                 color='darkred', size = 0.5, linetype=2)

mapa_sobreposicaoB


# Exportar o mapa como uma imagem PNG
png("./Graficos/sobreposicoes_mapas_feitos/presente_e_biomas.png", res = 300,
    width = 2000, height = 2200, unit = "px")
mapa_sobreposicaoB
dev.off()


# Sem os biomas
mapa_sobreposicao<- ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = tS1, aes( x = long, y = lat, group = group, fill=id))+
    # Adicionar a barra de escala
    ggspatial::annotation_scale(location = "br", width_hint = 0.2,
                                bar_cols = c("grey30", "white")) +
    
    # Adicionar a flecha de orientação para o Norte
    ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                      height = unit(1.5, "cm"), 
                                      width = unit(1.5, "cm"),
                                      style = ggspatial::north_arrow_fancy_orienteering(
                                          fill = c("white","grey30"))) +
    #Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    
    # Adicionar as legendas
    scale_fill_manual(name="Sobreposição",
                      values = c('coral1','darkolivegreen', '#0072B5FF'),
                      labels = c("Apenas morcego", "Apenas planta", 'Sobreposição')) +
    
    guides(color = guide_legend(override.aes = list(fill = "white"))) +
    
    # Ajustar a legenda 
    theme(legend.position = c(0.86,0.2),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(0.5,4), "lines")) 


mapa_sobreposicao


# Exportar o mapa como uma imagem PNG
png("./Graficos/sobreposicoes_mapas_feitos/presente.png", res = 300,
    width = 2000, height = 2200, unit = "px")
mapa_sobreposicao
dev.off()


#### MAPA BINÁRIO RCP 45 DE SOBREPOSICAO

tS2 <- ggplot2::fortify(SobPol2)
unique(tS2$id)
head(tS2)
tS2 <- tS2[which(tS2$id!=1),]



ggplot2::ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = tS1, aes( x = long, y = lat, group = group, fill=id))



# Com biomas
mapa_sobreposicaoB2<- ggplot2::ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = tS2, aes( x = long, y = lat, group = group, fill=id))+
    # Adicionar a barra de escala
    ggspatial::annotation_scale(location = "br", width_hint = 0.2,
                                bar_cols = c("grey30", "white")) +
    
    # Adicionar a flecha de orientação para o Norte
    ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                      height = unit(1.5, "cm"), 
                                      width = unit(1.5, "cm"),
                                      style = ggspatial::north_arrow_fancy_orienteering(
                                          fill = c("white","grey30"))) +
    #Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    
    # Adicionar as legendas
    scale_fill_manual(name="Sobreposição",
                      values = c('coral1','darkolivegreen', '#0072B5FF'),
                      labels = c("Apenas morcego", "Apenas planta", 'Sobreposição')) +
    
    guides(color = guide_legend(override.aes = list(fill = "white"))) +
    
    # Ajustar a legenda 
    theme(legend.position = c(0.86,0.2),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(0.5,4), "lines")) +
    
    new_scale_fill() +
    
    # Adicionar os poligonos
    geom_polygon(data = MA_CA_CE, aes(x = long, y = lat, group=group), fill=NA, 
                 color='darkred', size = 0.5, linetype=2)

mapa_sobreposicaoB2


# Exportar o mapa como uma imagem PNG
png("./Graficos/sobreposicoes_mapas_feitos/RCP45_e_biomas.png", res = 300,
    width = 2000, height = 2200, unit = "px")
mapa_sobreposicaoB2
dev.off()


# Sem os biomas
mapa_sobreposicao2<- ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = tS2, aes( x = long, y = lat, group = group, fill=id))+
    # Adicionar a barra de escala
    ggspatial::annotation_scale(location = "br", width_hint = 0.2,
                                bar_cols = c("grey30", "white")) +
    
    # Adicionar a flecha de orientação para o Norte
    ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                      height = unit(1.5, "cm"), 
                                      width = unit(1.5, "cm"),
                                      style = ggspatial::north_arrow_fancy_orienteering(
                                          fill = c("white","grey30"))) +
    #Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    
    # Adicionar as legendas
    scale_fill_manual(name="Sobreposição",
                      values = c('coral1','darkolivegreen', '#0072B5FF'),
                      labels = c("Apenas morcego", "Apenas planta", 'Sobreposição')) +
    
    guides(color = guide_legend(override.aes = list(fill = "white"))) +
    
    # Ajustar a legenda 
    theme(legend.position = c(0.86,0.2),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(0.5,4), "lines")) 


mapa_sobreposicao2


# Exportar o mapa como uma imagem PNG
png("./Graficos/sobreposicoes_mapas_feitos/RCP45.png", res = 300,
    width = 2000, height = 2200, unit = "px")
mapa_sobreposicao2
dev.off()


#### MAPA BINÁRIO RCP 85 DE SOBREPOSICAO

tS3 <- ggplot2::fortify(SobPol3)
unique(tS3$id)
head(tS3)
tS3 <- tS3[which(tS3$id!=1),]



ggplot2::ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = tS3, aes( x = long, y = lat, group = group, fill=id))



# Com biomas
mapa_sobreposicaoB3<- ggplot2::ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = tS3, aes( x = long, y = lat, group = group, fill=id))+
    # Adicionar a barra de escala
    ggspatial::annotation_scale(location = "br", width_hint = 0.2,
                                bar_cols = c("grey30", "white")) +
    
    # Adicionar a flecha de orientação para o Norte
    ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                      height = unit(1.5, "cm"), 
                                      width = unit(1.5, "cm"),
                                      style = ggspatial::north_arrow_fancy_orienteering(
                                          fill = c("white","grey30"))) +
    #Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    
    # Adicionar as legendas
    scale_fill_manual(name="Sobreposição",
                      values = c('coral1','darkolivegreen', '#0072B5FF'),
                      labels = c("Apenas morcego", "Apenas planta", 'Sobreposição')) +
    
    guides(color = guide_legend(override.aes = list(fill = "white"))) +
    
    # Ajustar a legenda 
    theme(legend.position = c(0.86,0.2),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(0.5,4), "lines")) +
    
    new_scale_fill() +
    
    # Adicionar os poligonos
    geom_polygon(data = MA_CA_CE, aes(x = long, y = lat, group=group), fill=NA, 
                 color='darkred', size = 0.5, linetype=2)

mapa_sobreposicaoB3


# Exportar o mapa como uma imagem PNG
png("./Graficos/sobreposicoes_mapas_feitos/RCP85_e_biomas.png", res = 300,
    width = 2000, height = 2200, unit = "px")
mapa_sobreposicaoB3
dev.off()


# Sem os biomas
mapa_sobreposicao3<- ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = tS3, aes( x = long, y = lat, group = group, fill=id))+
    # Adicionar a barra de escala
    ggspatial::annotation_scale(location = "br", width_hint = 0.2,
                                bar_cols = c("grey30", "white")) +
    
    # Adicionar a flecha de orientação para o Norte
    ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                      height = unit(1.5, "cm"), 
                                      width = unit(1.5, "cm"),
                                      style = ggspatial::north_arrow_fancy_orienteering(
                                          fill = c("white","grey30"))) +
    #Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    
    # Adicionar as legendas
    scale_fill_manual(name="Sobreposição",
                      values = c('coral1','darkolivegreen', '#0072B5FF'),
                      labels = c("Apenas morcego", "Apenas planta", 'Sobreposição')) +
    
    guides(color = guide_legend(override.aes = list(fill = "white"))) +
    
    # Ajustar a legenda 
    theme(legend.position = c(0.86,0.2),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(0.5,4), "lines")) 


mapa_sobreposicao3


# Exportar o mapa como uma imagem PNG
png("./Graficos/sobreposicoes_mapas_feitos/RCP85.png", res = 300,
    width = 2000, height = 2200, unit = "px")
mapa_sobreposicao3
dev.off()

