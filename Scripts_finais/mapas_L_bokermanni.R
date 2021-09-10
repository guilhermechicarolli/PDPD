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

enPolM <- rgdal::readOGR('./Rasters_mapas/L_bokermanni/presente_binario.shp')

t1M <- ggplot2::fortify(enPolM)
unique(t1M$id)

# MAPA

presentePM <- ggplot2::ggplot(data = world) +
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
    
    ggpolypath::geom_polypath(data = t1M, aes(x = long, y = lat, group = group),
                              fill = '#9F0000') +
    
    guides(color = guide_legend(override.aes = list(fill = "white"))) +
    
    # Ajustar a legenda 
    theme(legend.position = c(0.86,0.2),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(0.5,4), "lines"))


presentePM



# Exportar o mapa como uma imagem PNG

ggsave(file = "./Graficos/L_bokermanni_mapas_feitos/presente_e_biomas.jpeg",
       plot = presentePM,
       device = 'png',
       width = 1200, 
       height = 1300, 
       unit = "px",
       dpi = 200)




#### MAPA BINÁRIO RCP 45

# Carregar os shapefiles das distribuições

enPol2M <- rgdal::readOGR('./Rasters_mapas/L_bokermanni/RCP45_binario.shp')

t2M <- ggplot2::fortify(enPol2M)
unique(t2M$id)

# MAPA

futuro45M <- ggplot2::ggplot(data = world) +
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
    
    ggpolypath::geom_polypath(data = t2M, aes(x = long, y = lat, group = group),
                              fill = '#9F0000') +
    
    guides(color = guide_legend(override.aes = list(fill = "white"))) +
    
    # Ajustar a legenda 
    theme(legend.position = c(0.86,0.2),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(0.5,4), "lines"))


futuro45M



# Exportar o mapa como uma imagem PNG

ggsave(file = "./Graficos/L_bokermanni_mapas_feitos/RCP45.jpeg",
       plot = futuro45M,
       device = 'png',
       width = 1200, 
       height = 1300, 
       unit = "px",
       dpi = 200)



#### MAPA BINÁRIO RCP 85

enPol3M <- rgdal::readOGR('./Rasters_mapas/L_bokermanni/RCP85_binario.shp')

t3M <- ggplot2::fortify(enPol3M)
unique(t3M$id)

# MAPA

futuro85M <- ggplot2::ggplot(data = world) +
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
    
    ggpolypath::geom_polypath(data = t3M, aes(x = long, y = lat, group = group),
                              fill = '#9F0000') +
    
    guides(color = guide_legend(override.aes = list(fill = "white"))) +
    
    # Ajustar a legenda 
    theme(legend.position = c(0.86,0.2),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(0.5,4), "lines"))


futuro85M



# Exportar o mapa como uma imagem PNG

ggsave(file = "./Graficos/L_bokermanni_mapas_feitos/RCP85.jpeg",
       plot = futuro85M,
       device = 'png',
       width = 1200, 
       height = 1300, 
       unit = "px",
       dpi = 200)



#### MAPA BINÁRIO DE ALTERACOES RCP 45

# Carregar os shapefiles das distribuições

enPol4M <- rgdal::readOGR('./Rasters_mapas/L_bokermanni/alteracao_RCP45_binario.shp')


t4M <- fortify(enPol4M)
unique(t4M)

# MAPA

alterac45M <- ggplot2::ggplot(data = world) +
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
    
    ggpolypath::geom_polypath(data = t2M, aes(x = long, y = lat, group = group),
                              fill = 'gray56') +
    
    # Adicionar a distribuição
    new_scale_fill() +
    
    ggpolypath::geom_polypath(data = t4M, aes(x = long, y = lat, group = group,
                                             fill = id),show.legend = FALSE) +
    
    scale_fill_manual(values = c("#9F0000", "blue")) +
    
    
    guides(color = guide_legend(override.aes = list(fill = "white"))) +
    
    # Ajustar a legenda 
    theme(legend.position = c(0.86,0.2),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(0.5,4), "lines"))


alterac45M



# Exportar o mapa como uma imagem PNG

ggsave(file = "./Graficos/L_bokermanni_mapas_feitos/alteracao_RCP45.jpeg",
       plot = alterac45M,
       device = 'png',
       width = 1200, 
       height = 1300, 
       unit = "px",
       dpi = 200)




#### MAPA BINÁRIO DE ALTERACOES RCP 85

# Carregar os shapefiles das distribuições

enPol5M <- rgdal::readOGR('./Rasters_mapas/L_bokermanni/alteracao_RCP85_binario.shp')


t5M <- fortify(enPol5M)
unique(t5M)

# MAPA

alterac85M <- ggplot2::ggplot(data = world) +
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
    ggpolypath::geom_polypath(data = t3M, aes(x = long, y = lat, group = group),
                              fill = 'gray56') +
    
    # Adicionar a distribuição
    new_scale_fill() +
    
    ggpolypath::geom_polypath(data = t5M, aes(x = long, y = lat, group = group,
                                              fill = id),show.legend = FALSE) +
    
    scale_fill_manual(values = c("#9F0000", "blue")) +
    
    
    guides(color = guide_legend(override.aes = list(fill = "white"))) +
    
    # Ajustar a legenda 
    theme(legend.position = c(0.86,0.2),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(0.5,4), "lines"))


alterac85M



# Exportar o mapa como uma imagem PNG

ggsave(file = "./Graficos/L_bokermanni_mapas_feitos/alteracao_RCP85.jpeg",
       plot = alterac85M,
       device = 'png',
       width = 1200, 
       height = 1300, 
       unit = "px",
       dpi = 200)

################################ FIM ###########################################