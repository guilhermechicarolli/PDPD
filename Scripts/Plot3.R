#---- SCRIPT DO PLOT GEOGRÁFICO DAS ESPÉCIESL. BOKERMANNI E E. SUBSECUNDUM ----#


###############################################################################
# Deletar objetos passados
rm(list= ls())


###############################################################################
# Carregamento dos pacotes necessários


if (!require(dplyr)) install.packages('dplyr')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(grDevices)) install.packages('grDevices')
if (!require(sf)) install.packages('sf')
if (!require(rnaturalearth)) install.packages('rnaturalearth')
if (!require(rnaturalearthdata)) install.packages('rnaturalearthdata')
if (!require(ggspatial)) install.packages('ggspatial')
if (!require(rgeos)) install.packages('rgeos')
if (!require(rgdal)) install.packages('rgdal')


###############################################################################
# Importar os dados
###############################################################################

# Importar os dados geográficos
sites_P <- read.csv("Dados/registros_E_subsecundum.csv", encoding = "UTF-8", sep=",")
sites_M <- read.csv("Dados/registros_L_bokermanni.csv", encoding = "UTF-8", sep=",")
interacao <- data.frame("Longitude" = -45.489916, "Latitude"=-22.690245)


# Extrair os dados dos biomas
biomas <- rgdal::readOGR("Dados/Biomas_250mil/lm_bioma_250.shp")


# Selecionar as colunas com as coordenadas geográficas
sites_short_P <- sites_P %>% 
    dplyr::select(Latitude, Longitude)

sites_short_M <- sites_M %>% 
    dplyr::select(Latitude, Longitude) 


# Carregar o mapa através do pacote Mapdata
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Extrair dados da mata atlântica e Caatinga

MA <- ggplot2::fortify(biomas[biomas$Bioma=="Mata AtlÃ¢ntica",])
CA <- ggplot2::fortify(biomas[biomas$Bioma=="Caatinga",])
CE <- ggplot2::fortify(biomas[biomas$Bioma=="Cerrado",])

MA_CA_CE <- rbind(MA, CA, CE) # Juntei os dois data frames para dar cores diferentes a cada id


################################################################################
##### PLOT THE MAP
################################################################################


# Construir o mapa base
g2 <- ggplot(data = world) +
    geom_sf(colour = "white", fill = "#d3d3d3") +
    coord_sf(xlim = c(-55, -30), ylim = c(-30,0), expand = FALSE) +
    theme_bw() + 
    
    # Adicionar os poligonos
    geom_polygon(data = MA_CA_CE, aes(x = long, y = lat, group = group, 
                                      fill = id), show.legend = TRUE) +
    
    # Plotar os pontos geográficos 
    geom_point(data = sites_short_M, aes(x = Longitude, y = Latitude,
                                       colour = "black"), 
               alpha = 0.6, size = 3) +
    
    geom_point(data = sites_short_P, aes(x = Longitude, y = Latitude,
                                         colour = "blue"), 
               alpha = 0.35, size = 3) +
    
    geom_point(data=interacao, aes(x = Longitude, y = Latitude, 
                                   colour="red"),
               alpha=0.8, size=3, shape=18) +
    
    
    # Adicionar a barra de escala
    ggspatial::annotation_scale(location = "br", width_hint = 0.2,
                                bar_cols = c("grey30", "white")) +
    
    # Adicionar a flecha de orientação para o Norte
    ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                      height = unit(1.5, "cm"), 
                                      width = unit(1.5, "cm"),
                                      style = ggspatial::north_arrow_fancy_orienteering(
                                          fill = c("white","grey30"))) +
    
    # Configurar a descrição dos eixos X e Y
    labs(x = "Longitude", y = "Latitude") +
    
    # Adicionar as legendas
    scale_fill_manual(name="Biomas",
                      values = c("#6BBC19", "goldenrod2", "lightskyblue"),
                      breaks = c("3", "1", "2"),
                      labels = c("Mata Atlântica", "Caatinga", "Cerrado")) +
    
    scale_colour_manual(name = "Registros", values = c("blue", "red", "grey19"),
                        labels = c(expression(italic("L. bokermanni")), 
                                   expression(italic("E. subsecundum")),
                                   expression("Interação confirmada"))) +
    
    guides(color = guide_legend(override.aes = list(fill = "white")), ) +
    
    # Ajustar a legenda 
    theme(legend.position = c(0.86,0.23),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(0.5,4), "lines"))

g2

# Exportar o mapa como uma imagem PNG
png("./Dados/Figure_3.png", res = 300,
    width = 2000, height = 2200, unit = "px")
g2

dev.off()


################################ FIM ###########################################