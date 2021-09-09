############## SCRIPTS DO PROJETO DE PDPD ##############

# 1. Criacao dos mapas dos modelos de distribuicao potencial
#    da especie E. subsecundum

################################################################################

##### Carregamento das bibliotecas necessarias
if (!require(raster)) install.packages('raster')
if (!require(rgeos)) install.packages('rgeos')
if (!require(rgdal)) install.packages('rgdal')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(sf)) install.packages('sf')
if (!require(ggnewscale)) install.packages('ggnewscale')
if (!require(patchwork)) install.packages('patchwork')

################################################################################

#--------- 1. MAPAS E. SUBSECUNDUM ---------#


# Carregar os shapefiles das distribuições

enPol <- rgdal::readOGR('./Rasters_mapas/E_subsecundum/presente_binario.shp')

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

t1 <- ggplot2::fortify(enPol)
t1$id[t1$hole == TRUE] <- 1

t1 <- t1[t1$id == 1,]
unique(t1$id)
t1$id <- '4' # mudei a identidade para 4 (assim evito a sobreposição com a outra camada)
unique(t1$hole)
t1 <- t1[which(t1$hole==FALSE),]

# MAPA

presenteP <- ggplot2::ggplot(data = world) +
    geom_sf(colour = "white", fill = "#d3d3d3") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, 
             crs = st_crs(4326)) +
    theme_bw() + 
    
    # Adicionei todos os poligonos
    geom_polygon(data = MA_CA_CE, aes(x = long, y = lat, 
                                      group = group, fill = id), alpha = 0.6) +
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
    scale_fill_manual(name = "Biomas",
                      values = c("#6BBC19", "goldenrod2", 
                                 "lightskyblue"),
                      breaks = c("3", "1", "2"),
                      labels = c("Mata Atlântica", "Caatinga", "Cerrado")) +
    
    guides(color = guide_legend(override.aes = list(fill = "white"))) +
    
    # Ajustar a legenda 
    theme(legend.position = c(0.86,0.2),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(0.5,4), "lines")) +
    
    new_scale_fill() +
    
    # Adicionar os poligonos
    geom_polygon(data = t1, aes(x = long, y = lat, group = group, fill = id)) +
    scale_fill_manual(name = " ", values = "#820101", 
                      labels = "Distribuição de\nE. subsecundum")

presenteP



# Exportar o mapa como uma imagem PNG

ggsave(file = "./Graficos/E_subsecundum_mapas_feitos/presente_e_biomas.jpeg",
       plot = presenteP,
       device = 'png',
       width = 1200, 
       height = 1300, 
       unit = "px",
       dpi = 200)



# Sem os biomas
presenteP2<- ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = t1, aes( x = long, y = lat, group = group, fill=id), 
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


presenteP2


# Exportar o mapa como uma imagem PNG
png("./Graficos/E_subsecundum_mapas_feitos/presente.png", res = 300,
    width = 2000, height = 2200, unit = "px")
presenteP2
dev.off()



#### MAPA BINÁRIO RCP 45

t2 <- fortify(enPol2)
ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = t2, aes( x = long, y = lat, group = group, fill=id))


t2$hole <- as.factor(t2$hole)
head(t2)

t2$id[t2$hole == TRUE] <- 4

# Com biomas
futuro45B<- ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = t2, aes( x = long, y = lat, group = group, fill=id))+
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
                      values = c("#6BBC19", "lightskyblue",),
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

futuro45B


# Exportar o mapa como uma imagem PNG
png("./Graficos/E_subsecundum_mapas_feitos/RCP45_e_biomas.png", res = 300,
    width = 2000, height = 2200, unit = "px")
futuro45B
dev.off()


# Sem os biomas
futuro45<- ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = t2, aes( x = long, y = lat, group = group, fill=id), 
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


futuro45


# Exportar o mapa como uma imagem PNG
png("./Graficos/E_subsecundum_mapas_feitos/RCP45.png", res = 300,
    width = 2000, height = 2200, unit = "px")
futuro45
dev.off()


#### MAPA BINÁRIO RCP 85

t3 <- ggplot2::fortify(enPol3)
t3$id <- as.factor(t3$id)
ggplot2::ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = t3, aes( x = long, y = lat, group = group, fill=id))


t3$hole <- as.factor(t3$hole)
head(t3)

t2$id[t2$hole == TRUE] <- 1

futuro85B<- ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = t3, aes( x = long, y = lat, group = group, fill=id))+
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

futuro85B


# Exportar o mapa como uma imagem PNG
png("./Graficos/E_subsecundum_mapas_feitos/RCP85_e_biomas.png", res = 300,
    width = 2000, height = 2200, unit = "px")
futuro85B
dev.off()

### Sem os biomas

futuro85<- ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = t3, aes( x = long, y = lat, group = group, fill=id), 
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


futuro85


# Exportar o mapa como uma imagem PNG
png("./Graficos/E_subsecundum_mapas_feitos/RCP85.png", res = 300,
    width = 2000, height = 2200, unit = "px")
futuro85
dev.off()


#### MAPA BINÁRIO DE ALTERACOES RCP 45

t4 <- ggplot2::fortify(enPol4)

t4 <- t4[which(t4$id!=2),]
unique(t4$id)
t4$id[t4$hole == TRUE] <- 4

ggplot2::ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = t4, aes( x = long, y = lat, group = group, fill=id))


alterac45B <- ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = t5teste, aes( x = long, y = lat, group = group, fill=id))+
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

alterac45B


# Exportar o mapa como uma imagem PNG
png("./Graficos/E_subsecundum_mapas_feitos/Alteracao_RCP45_e_biomas.png", res = 300,
    width = 2000, height = 2200, unit = "px")
alterac45B
dev.off()


### Sem os biomas

alterac45<- ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = t4, aes( x = long, y = lat, group = group, fill=id))+
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


alterac45


# Exportar o mapa como uma imagem PNG
png("./Graficos/E_subsecundum_mapas_feitos/Alteracao_RCP45.png", res = 300,
    width = 2000, height = 2200, unit = "px")
alterac45
dev.off()


#### MAPA BINÁRIO DE ALTERACOES RCP 85

t5 <- ggplot2::fortify(enPol5)

t5 <- t5[which(t5$id!=2),]
unique(t5$id)
t5$id[t5$hole == TRUE] <- 4

ggplot2::ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = t5, aes( x = long, y = lat, group = group, fill=id))



alterac85B <- ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = t5, aes( x = long, y = lat, group = group, fill=id))+
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
                      values = c("red",'gray'),
                      labels = c("Área perdida",'')) +
    
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

alterac85B


# Exportar o mapa como uma imagem PNG
png("./Graficos/E_subsecundum_mapas_feitos/Alteracao_RCP85_e_biomas.png", res = 300,
    width = 2000, height = 2200, unit = "px")
alterac85B
dev.off()

### Sem os biomas

alterac85<- ggplot(data = world) +
    geom_sf(colour = "white", fill = "gray") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_gray() + 
    
    geom_polygon(data = t5, aes( x = long, y = lat, group = group, fill=id))+
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
                      values = c("red",'gray'),
                      labels = c("Área perdida",'')) +
    
    guides(color = guide_legend(override.aes = list(fill = "white"))) +
    
    # Ajustar a legenda 
    theme(legend.position = c(0.86,0.2),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(0.5,4), "lines")) 


alterac85


# Exportar o mapa como uma imagem PNG
png("./Graficos/E_subsecundum_mapas_feitos/Alteracao_RCP85.png", res = 300,
    width = 2000, height = 2200, unit = "px")
alterac85
dev.off()


################################ FIM ###########################################