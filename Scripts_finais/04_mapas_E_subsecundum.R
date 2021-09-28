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
if (!require(ggpolypath)) install.packages('ggpolypath')

################################################################################

#--------- 1. MAPAS E. SUBSECUNDUM ---------#


# Carregar o mapa através do pacote Mapdata
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Extrair os dados dos biomas
biomas <- rgdal::readOGR("Dados/Biomas_250mil/lm_bioma_250.shp")

# Extrair dados da mata atlântica e Caatinga
MA <- ggplot2::fortify(biomas[biomas$Bioma=="Mata AtlÃ¢ntica",])
CA <- ggplot2::fortify(biomas[biomas$Bioma=="Caatinga",])
CE <- ggplot2::fortify(biomas[biomas$Bioma=="Cerrado",])

# Juntar todas as camadas shapefile, inclusive as da distribuição

MA_CA_CE <- rbind(MA, CA, CE) %>%
    dplyr::mutate(id = factor(id, levels = c("1", "2", "3")))
unique(MA$id)
unique(CA$id)
unique(CE$id)


#### MAPA BINÁRIO PRESENTE

# Carregar os shapefiles das distribuições

enPol <- rgdal::readOGR('./Rasters_mapas/E_subsecundum/presente_binario.shp')

t1 <- ggplot2::fortify(enPol)
unique(t1$id)

# MAPA

presenteP <- ggplot2::ggplot(data = world) +
    geom_sf(colour = "white", fill = "#d3d3d3") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, 
             crs = st_crs(4326)) +
    theme_bw() + 
    
    # Adicionei todos os poligonos
    geom_polygon(data = MA_CA_CE, alpha = 0.6, aes(x = long, y = lat, 
                                                  group = group, fill = id)) +
    
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
    scale_fill_manual(name = " ",
                      values = c('goldenrod2', 'lightskyblue', '#6BBC19'),
                      labels = c("Caatinga", "Cerrado", "Mata Atlântica")) +
    
    # Adicionar a distribuição
    
    ggpolypath::geom_polypath(data = t1, aes(x = long, y = lat, group = group),
                              fill = '#4E6641') +
    
    guides(color = guide_legend(override.aes = list(fill = "white"))) +
    
    # Ajustar a legenda 
    theme(legend.position = c(0.86,0.2),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(0.5,4), "lines"))
    

presenteP



# Exportar o mapa como uma imagem PNG

ggsave(file = "./Graficos/E_subsecundum_mapas_feitos/presente_e_biomas.jpeg",
       plot = presenteP,
       device = 'png',
       width = 1200, 
       height = 1300, 
       unit = "px",
       dpi = 200)



#### MAPA BINÁRIO RCP 45

# Carregar os shapefiles das distribuições

enPol2 <- rgdal::readOGR('./Rasters_mapas/E_subsecundum/RCP45_binario.shp')


t2 <- fortify(enPol2)

# MAPA

futuro45 <- ggplot2::ggplot(data = world) +
    geom_sf(colour = "white", fill = "#d3d3d3") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, 
             crs = st_crs(4326)) +
    theme_bw() + 
    
    # Adicionei todos os poligonos
    geom_polygon(data = MA_CA_CE, alpha = 0.6, aes(x = long, y = lat, 
                                                   group = group, fill = id)) +
    
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
    scale_fill_manual(name = " ",
                      values = c('goldenrod2', 'lightskyblue', '#6BBC19'),
                      labels = c("Caatinga", "Cerrado", "Mata Atlântica")) +
    
    # Adicionar a distribuição
    
    ggpolypath::geom_polypath(data = t2, aes(x = long, y = lat, group = group),
                              fill = '#4E6641') +
    
    guides(color = guide_legend(override.aes = list(fill = "white"))) +
    
    # Ajustar a legenda 
    theme(legend.position = c(0.86,0.2),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(0.5,4), "lines"))


futuro45


# Exportar o mapa como uma imagem PNG

ggsave(file = "./Graficos/E_subsecundum_mapas_feitos/RCP45.jpeg",
       plot = futuro45,
       device = 'png',
       width = 1200, 
       height = 1300, 
       unit = "px",
       dpi = 200)


#### MAPA BINÁRIO RCP 85

# Carregar os shapefiles das distribuições

enPol3 <- rgdal::readOGR('./Rasters_mapas/E_subsecundum/RCP85_binario.shp')


t3 <- fortify(enPol3)

# MAPA

futuro85 <- ggplot2::ggplot(data = world) +
    geom_sf(colour = "white", fill = "#d3d3d3") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, 
             crs = st_crs(4326)) +
    theme_bw() + 
    
    # Adicionei todos os poligonos
    geom_polygon(data = MA_CA_CE, alpha = 0.6, aes(x = long, y = lat, 
                                                   group = group, fill = id)) +
    
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
    scale_fill_manual(name = " ",
                      values = c('goldenrod2', 'lightskyblue', '#6BBC19'),
                      labels = c("Caatinga", "Cerrado", "Mata Atlântica")) +
    
    # Adicionar a distribuição
    
    ggpolypath::geom_polypath(data = t3, aes(x = long, y = lat, group = group),
                              fill = '#4E6641') +
    
    guides(color = guide_legend(override.aes = list(fill = "white"))) +
    
    # Ajustar a legenda 
    theme(legend.position = c(0.86,0.2),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(0.5,4), "lines"))


futuro85



# Exportar o mapa como uma imagem PNG

ggsave(file = "./Graficos/E_subsecundum_mapas_feitos/RCP85.jpeg",
       plot = futuro85,
       device = 'png',
       width = 1200, 
       height = 1300, 
       unit = "px",
       dpi = 200)




#### MAPA BINÁRIO DE ALTERACOES RCP 45

# Carregar os shapefiles das distribuições

enPol4 <- rgdal::readOGR('./Rasters_mapas/E_subsecundum/alteracao_RCP45_binario.shp')


t4 <- fortify(enPol4)
unique(t4$id)

# MAPA

alterac45 <- ggplot2::ggplot(data = world) +
    geom_sf(colour = "white", fill = "#d3d3d3") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, 
             crs = st_crs(4326)) +
    theme_bw() + 
    
    # Adicionei todos os poligonos
    geom_polygon(data = MA_CA_CE, alpha = 0.6, aes(x = long, y = lat, 
                                                   group = group, fill = id)) +
    
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
    scale_fill_manual(name = "",
                      values = c('goldenrod2', 'lightskyblue', '#6BBC19'),
                      labels = c("Caatinga", "Cerrado", "Mata Atlântica")) +
    
    new_scale_fill() +
    ggpolypath::geom_polypath(data = t2, aes(x = long, y = lat, group = group),
                              fill = '#4E6641') +
    # Adicionar a distribuição
    new_scale_fill() +
    
    ggpolypath::geom_polypath(data = t4, aes(x = long, y = lat, group = group,
                              fill = id)) +
    
    scale_fill_manual(name = " ",
                      values = c("#FFE32D", "#1320BF"),
                      labels = c("Perda de área", "Ganho de área")) +
    

    guides(color = guide_legend(override.aes = list(fill = "white"))) +
    
    # Ajustar a legenda 
    theme(legend.position = c(0.83,0.22),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(0.5,4), "lines"))


alterac45



# Exportar o mapa como uma imagem PNG

ggsave(file = "./Graficos/E_subsecundum_mapas_feitos/alteracao_RCP45.jpeg",
       plot = alterac45,
       device = 'png',
       width = 1200, 
       height = 1300, 
       unit = "px",
       dpi = 200)





#### MAPA BINÁRIO DE ALTERACOES RCP 85

# Carregar os shapefiles das distribuições

enPol5 <- rgdal::readOGR('./Rasters_mapas/E_subsecundum/alteracao_RCP85_binario.shp')


t5 <- fortify(enPol5)
unique(t5$id)

# MAPA

alterac85 <- ggplot2::ggplot(data = world) +
    geom_sf(colour = "white", fill = "#d3d3d3") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, 
             crs = st_crs(4326)) +
    theme_bw() + 
    
    # Adicionei todos os poligonos
    geom_polygon(data = MA_CA_CE, alpha = 0.6, aes(x = long, y = lat, 
                                                   group = group, fill = id)) +
    
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
    scale_fill_manual(name = "",
                      values = c('goldenrod2', 'lightskyblue', '#6BBC19'),
                      labels = c("Caatinga", "Cerrado", "Mata Atlântica")) +
    
    new_scale_fill() +
    ggpolypath::geom_polypath(data = t3, aes(x = long, y = lat, group = group),
                              fill = '#4E6641') +
    # Adicionar a distribuição
    new_scale_fill() +
    
    ggpolypath::geom_polypath(data = t5, aes(x = long, y = lat, group = group,
                                             fill = id)) +
    
    scale_fill_manual(name = " ",
                      values = c("#FFE32D", "#1320BF"),
                      labels = c("Perda de área", "Ganho de área")) +
    
    
    guides(color = guide_legend(override.aes = list(fill = "white"))) +
    
    # Ajustar a legenda 
    theme(legend.position = c(0.83,0.22),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(0.5,4), "lines"))


alterac85



# Exportar o mapa como uma imagem PNG

ggsave(file = "./Graficos/E_subsecundum_mapas_feitos/alteracao_RCP85.jpeg",
       plot = alterac85,
       device = 'png',
       width = 1200, 
       height = 1300, 
       unit = "px",
       dpi = 200)


################################ FIM ###########################################
