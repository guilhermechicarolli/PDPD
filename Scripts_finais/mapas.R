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
if (!require(rgeos)) install.packages('rgeos')
if (!require(rgdal)) install.packages('rgdal')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(tidyverse)) install.packages('tidyverse')

################################################################################

#--------- 1. MAPAS E. SUBSECUNDUM ---------#


# Transformar os rasters dos modelos em poligonos
# Mapa binario do presente
enPol <- raster::rasterToPolygons(pa1, dissolve = TRUE)
plot(enPol)
raster::shapefile(enPol, './Rasters_mapas/E_subsecundum/presente_binario.shp')

# Mapa binario do futuro RCP45
enPol2 <- raster::rasterToPolygons(pa2, dissolve = TRUE)
plot(enPol2)
raster::shapefile(enPol2, './Rasters_mapas/E_subsecundum/RCP45_binario.shp')

# Mapa binario do futuro RCP85
enPol3 <- raster::rasterToPolygons(pa3, dissolve = TRUE)
plot(enPol3)
raster::shapefile(enPol3, './Rasters_mapas/E_subsecundum/RCP85_binario.shp')

# Mapa binario de alteracao de adequabilidade do futuro RCP45
enPol4 <- raster::rasterToPolygons(chp45, dissolve = TRUE)
plot(enPol4)
raster::shapefile(enPol4, './Rasters_mapas/E_subsecundum/alteracao_RCP45_binario.shp')

# Mapa binario de alteracao de adequabilidade do futuro RCP85
enPol5 <- raster::rasterToPolygons(chp85, dissolve = TRUE)
plot(enPol5)
raster::shapefile(enPol5, './Rasters_mapas/E_subsecundum/alteracao_RCP85_binario.shp')

################################################################################

# Carregar o mapa através do pacote Mapdata
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")


#### TESTE MAPA MODELO PRESENTE PLANTA

t1 <- fortify(enPol)
t1$id <- as.factor(t1$id)
t1$hole <- as.factor(t1$hole)
head(t1)

t2 <- t1[which(t1$hole == TRUE),]
t2

for(i in t1$hole) if(i == TRUE) t1$id <- '1'
t1$id <- as.factor(t1$id)


ggplot(data = world) +
    geom_sf(colour = "white", fill = "#d3d3d3") +
    coord_sf(xlim = c(-56, -31), ylim = c(-30,0), expand = FALSE, crs=st_crs(4326))+
    theme_bw() + 
    
    geom_polygon(data = t2, aes( x = long, y = lat, group = group, fill=id),  
                 color="black")+
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
    
    
