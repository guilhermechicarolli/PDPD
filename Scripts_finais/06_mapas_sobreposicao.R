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

# Carregar os shapefiles das distribuições
SobPol <- rgdal::readOGR('./Rasters_mapas/sobreposicao/sobreposicao_presente.shp')


tS1 <- ggplot2::fortify(SobPol)


# MAPA

sob_presente <- ggplot2::ggplot(data = world) +
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
    
    # Adicionar a distribuição
    new_scale_fill() +
    
    ggpolypath::geom_polypath(data = tS1, aes(x = long, y = lat, group = group,
                                              fill = id)) +
    
    scale_fill_manual(name = "",
                      values = c("#A75400", "#4E6641",'#9F0000'),
                      labels = c("Apenas o morcego", "Apenas a planta", "Sobreposição")) +
    
    guides(color = guide_legend(override.aes = list(fill = "white"))) +
    
    # Ajustar a legenda 
    theme(legend.position = c(0.86,0.24),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(0.5,4), "lines"))


sob_presente



# Exportar o mapa como uma imagem PNG

ggsave(file = "./Graficos/sobreposicoes_mapas_feitos/presente.jpeg",
       plot = sob_presente,
       device = 'png',
       width = 1200, 
       height = 1300, 
       unit = "px",
       dpi = 200)





#### MAPA BINÁRIO RCP 45 DE SOBREPOSICAO

# Carregar os shapefiles das distribuições
SobPol2 <- rgdal::readOGR('./Rasters_mapas/sobreposicao/sobreposicao_RCP45.shp')


tS2 <- ggplot2::fortify(SobPol2)


# MAPA

sob_RCP45 <- ggplot2::ggplot(data = world) +
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
    
    # Adicionar a distribuição
    new_scale_fill() +
    
    ggpolypath::geom_polypath(data = tS2, aes(x = long, y = lat, group = group,
                                              fill = id)) +
    
    scale_fill_manual(name = "",
                      values = c("#A75400", "#4E6641",'#9F0000'),
                      labels = c("Apenas o morcego", "Apenas a planta", "Sobreposição")) +
    
    guides(color = guide_legend(override.aes = list(fill = "white"))) +
    
    # Ajustar a legenda 
    theme(legend.position = c(0.86,0.24),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(0.5,4), "lines"))


sob_RCP45



# Exportar o mapa como uma imagem PNG

ggsave(file = "./Graficos/sobreposicoes_mapas_feitos/RCP45.jpeg",
       plot = sob_RCP45,
       device = 'png',
       width = 1200, 
       height = 1300, 
       unit = "px",
       dpi = 200)



#### MAPA BINÁRIO RCP 85 DE SOBREPOSICAO

# Carregar os shapefiles das distribuições
SobPol3 <- rgdal::readOGR('./Rasters_mapas/sobreposicao/sobreposicao_RCP85.shp')


tS3 <- ggplot2::fortify(SobPol3)


# MAPA

sob_RCP85 <- ggplot2::ggplot(data = world) +
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
    
    # Adicionar a distribuição
    new_scale_fill() +
    
    ggpolypath::geom_polypath(data = tS3, aes(x = long, y = lat, group = group,
                                              fill = id)) +
    
    scale_fill_manual(name = "",
                      values = c("#A75400", "#4E6641",'#9F0000'),
                      labels = c("Apenas o morcego", "Apenas a planta", "Sobreposição")) +
    
    guides(color = guide_legend(override.aes = list(fill = "white"))) +
    
    # Ajustar a legenda 
    theme(legend.position = c(0.86,0.24),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(0.5,4), "lines"))


sob_RCP85



# Exportar o mapa como uma imagem PNG

ggsave(file = "./Graficos/sobreposicoes_mapas_feitos/RCP85.jpeg",
       plot = sob_RCP85,
       device = 'png',
       width = 1200, 
       height = 1300, 
       unit = "px",
       dpi = 200)


################################ FIM ###########################################
