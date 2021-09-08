############## SCRIPTS DO PROJETO DE PDPD ##############

# 1. Criacao dos mapas dos modelos de distribuicao potencial
#    da especie L. bokermanni

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

#--------- 1. MAPAS L. BOKERMANNI ---------#


# Transformar os rasters dos modelos em poligonos
# Mapa binario do presente
enPolM <- raster::rasterToPolygons(pa1M, dissolve = TRUE)
plot(enPolM)
raster::shapefile(enPolM, './Rasters_mapas/L_bokermanni/presente_binario.shp')

# Mapa binario do futuro RCP45
enPolM2 <- raster::rasterToPolygons(pa2M, dissolve = TRUE)
plot(enPolM2)
raster::shapefile(enPolM2, './Rasters_mapas/L_bokermanni/RCP45_binario.shp')

# Mapa binario do futuro RCP85
enPolM3 <- raster::rasterToPolygons(pa3M, dissolve = TRUE)
plot(enPolM3)
raster::shapefile(enPolM3, './Rasters_mapas/L_bokermanni/RCP85_binario.shp')

# Mapa binario de alteracao de adequabilidade do futuro RCP45
enPolM4 <- raster::rasterToPolygons(chp45M, dissolve = TRUE)
plot(enPolM4)
raster::shapefile(enPolM4, './Rasters_mapas/L_bokermanni/alteracao_RCP45_binario.shp')

# Mapa binario de alteracao de adequabilidade do futuro RCP85
enPolM5 <- raster::rasterToPolygons(chp85M, dissolve = TRUE)
plot(enPolM5)
raster::shapefile(enPolM5, './Rasters_mapas/L_bokermanni/alteracao_RCP85_binario.shp')

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


#### MAPA BINÁRIO PRESENTE

t1M <- ggplot2::fortify(enPolM)
t1M$id <- as.factor(t1M$id)
t1M$hole <- as.factor(t1M$hole)
head(t1M)

ggplot2::ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = t1M, aes( x = long, y = lat, group = group, fill=id))

t1M$id[t1M$hole == TRUE] <- 1


# Com biomas
presentePM<- ggplot2::ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = t1M, aes( x = long, y = lat, group = group, fill=id))+
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
    scale_fill_manual(name="Adequabilidade",
                      values = c("#6BBC19", "lightskyblue"),
                      labels = c("Não adequado", "Adequado")) +
    
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
                 color='red', size = 0.5, linetype=2)

presentePM


# Exportar o mapa como uma imagem PNG
png("./Graficos/L_bokermanni_mapas_feitos/presente_e_biomas.png", res = 300,
    width = 2000, height = 2200, unit = "px")
presentePM
dev.off()


# Sem os biomas
presenteP2M<- ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = t1M, aes( x = long, y = lat, group = group, fill=id), 
                 color='black')+
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
    scale_fill_manual(name="Adequabilidade",
                      values = c("#6BBC19", "lightskyblue"),
                      labels = c("Não adequado", "Adequado")) +
    
    guides(color = guide_legend(override.aes = list(fill = "white"))) +
    
    # Ajustar a legenda 
    theme(legend.position = c(0.86,0.2),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(0.5,4), "lines")) 


presenteP2M


# Exportar o mapa como uma imagem PNG
png("./Graficos/L_bokermanni_mapas_feitos/presente.png", res = 300,
    width = 2000, height = 2200, unit = "px")
presenteP2M
dev.off()


#### MAPA BINÁRIO RCP 45

t2M <- ggplot2::fortify(enPolM2)
t2M$id <- as.factor(t2M$id)
t2M$hole <- as.factor(t2M$hole)
head(t2M)
t2M$id[t2M$hole == TRUE] <- 1

ggplot2::ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = t2M, aes( x = long, y = lat, group = group, fill=id))


# Com biomas
futuro45MB <- ggplot2::ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = t2M, aes( x = long, y = lat, group = group, fill=id))+
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
    scale_fill_manual(name="Adequabilidade",
                      values = c("#6BBC19", "lightskyblue"),
                      labels = c("Não adequado", "Adequado")) +
    
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
                 color='red', size = 0.5, linetype=2)

futuro45MB


# Exportar o mapa como uma imagem PNG
png("./Graficos/L_bokermanni_mapas_feitos/RCP45_e_biomas.png", res = 300,
    width = 2000, height = 2200, unit = "px")
futuro45MB
dev.off()


# Sem os biomas
futuro45M<- ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = t2M, aes( x = long, y = lat, group = group, fill=id), 
                 color='black')+
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
    scale_fill_manual(name="Adequabilidade",
                      values = c("#6BBC19", "lightskyblue"),
                      labels = c("Não adequado", "Adequado")) +
    
    guides(color = guide_legend(override.aes = list(fill = "white"))) +
    
    # Ajustar a legenda 
    theme(legend.position = c(0.86,0.2),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(0.5,4), "lines")) 


futuro45M


# Exportar o mapa como uma imagem PNG
png("./Graficos/L_bokermanni_mapas_feitos/RCP45.png", res = 300,
    width = 2000, height = 2200, unit = "px")
futuro45M
dev.off()


#### MAPA BINÁRIO RCP 85

t3M <- ggplot2::fortify(enPolM3)
t3M$id <- as.factor(t3M$id)
t3M$hole <- as.factor(t3M$hole)
head(t3M)
t3M$id[t3M$hole == TRUE] <- 1

ggplot2::ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = t3M, aes( x = long, y = lat, group = group, fill=id))


# Com biomas
futuro85MB <- ggplot2::ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = t3M, aes( x = long, y = lat, group = group, fill=id))+
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
    scale_fill_manual(name="Adequabilidade",
                      values = c("#6BBC19", "lightskyblue"),
                      labels = c("Não adequado", "Adequado")) +
    
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
                 color='red', size = 0.5, linetype=2)

futuro85MB


# Exportar o mapa como uma imagem PNG
png("./Graficos/L_bokermanni_mapas_feitos/RCP85_e_biomas.png", res = 300,
    width = 2000, height = 2200, unit = "px")
futuro85MB
dev.off()


# Sem os biomas
futuro85M<- ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = t3M, aes( x = long, y = lat, group = group, fill=id), 
                 color='black')+
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
    scale_fill_manual(name="Adequabilidade",
                      values = c("#6BBC19", "lightskyblue"),
                      labels = c("Não adequado", "Adequado")) +
    
    guides(color = guide_legend(override.aes = list(fill = "white"))) +
    
    # Ajustar a legenda 
    theme(legend.position = c(0.86,0.2),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(0.5,4), "lines")) 


futuro85M


# Exportar o mapa como uma imagem PNG
png("./Graficos/L_bokermanni_mapas_feitos/RCP85.png", res = 300,
    width = 2000, height = 2200, unit = "px")
futuro85M
dev.off()


#### MAPA BINÁRIO DE ALTERACOES RCP 45

t4M <- ggplot2::fortify(enPolM4)

t4M <- t4M[which(t4M$id!=2),]
unique(t4M$id)
t4M$id[t4M$hole == TRUE] <- 4

ggplot2::ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = t4M, aes( x = long, y = lat, group = group, fill=id))


alterac45BM <- ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = t4M, aes( x = long, y = lat, group = group, fill=id))+
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
    scale_fill_manual(name="Alteração",
                      values = c("red", 'blue', 'gray'),
                      labels = c("Área perdida", "Área ganha", '')) +
    
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
                 color='brown', size = 0.5, linetype=2)

alterac45BM


# Exportar o mapa como uma imagem PNG
png("./Graficos/L_bokermanni_mapas_feitos/Alteracao_RCP45_e_biomas.png", res = 300,
    width = 2000, height = 2200, unit = "px")
alterac45BM
dev.off()


### Sem os biomas

alterac45M<- ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = t4M, aes( x = long, y = lat, group = group, fill=id))+
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
    scale_fill_manual(name="Alteração",
                      values = c("red", 'blue', 'gray'),
                      labels = c("Área perdida", "Área ganha", '')) +
    
    guides(color = guide_legend(override.aes = list(fill = "white"))) +
    
    # Ajustar a legenda 
    theme(legend.position = c(0.86,0.2),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(0.5,4), "lines")) 


alterac45M


# Exportar o mapa como uma imagem PNG
png("./Graficos/L_bokermanni_mapas_feitos/Alteracao_RCP45.png", res = 300,
    width = 2000, height = 2200, unit = "px")
alterac45M
dev.off()


#### MAPA BINÁRIO DE ALTERACOES RCP 85

t5M <- ggplot2::fortify(enPolM5)

t5M <- t5M[which(t5M$id!=2),]
unique(t5M$id)
t5M$id[t5M$hole == TRUE] <- 4

ggplot2::ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = t5M, aes( x = long, y = lat, group = group, fill=id))



alterac85BM <- ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = t5M, aes( x = long, y = lat, group = group, fill=id))+
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
    scale_fill_manual(name="Alteração",
                      values = c("red",'blue', 'gray'),
                      labels = c("Área perdida",'Área ganha', '')) +
    
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
                 color='brown', size = 0.5, linetype=2)

alterac85BM


# Exportar o mapa como uma imagem PNG
png("./Graficos/L_bokermanni_mapas_feitos/Alteracao_RCP85_e_biomas.png", res = 300,
    width = 2000, height = 2200, unit = "px")
alterac85BM
dev.off()

### Sem os biomas

alterac85M<- ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = t5M, aes( x = long, y = lat, group = group, fill=id))+
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
    scale_fill_manual(name="Alteração",
                      values = c("red",'blue', 'gray'),
                      labels = c("Área perdida",'Área ganha', '')) +
    
    guides(color = guide_legend(override.aes = list(fill = "white"))) +
    
    # Ajustar a legenda 
    theme(legend.position = c(0.86,0.2),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(0.5,4), "lines")) 


alterac85M


# Exportar o mapa como uma imagem PNG
png("./Graficos/L_bokermanni_mapas_feitos/Alteracao_RCP85.png", res = 300,
    width = 2000, height = 2200, unit = "px")
alterac85M
dev.off()

################################ FIM ###########################################