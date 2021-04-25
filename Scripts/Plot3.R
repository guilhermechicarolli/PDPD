#---- SCRIPT DO PLOT GEOGRÁFICO DAS ESPÉCIESL. BOKERMANNI E E. SUBSECUNDUM ----#


###############################################################################
# Deletar objetos passados
rm(list= ls())


###############################################################################
# Carregamento dos pacotes necessários


if (!require(tidyverse)) install.packages('tidyverse')
if (!require(grDevices)) install.packages('grDevices')
if (!require(sf)) install.packages('sf')
if (!require(rnaturalearth)) install.packages('rnaturalearth')
if (!require(rnaturalearthdata)) install.packages('rnaturalearthdata')
if (!require(ggspatial)) install.packages('ggspatial')
if (!require(rgeos)) install.packages('rgeos')
if (!require(rgdal)) install.packages('rgdal')
if (!require(ggnewscale)) install.packages('ggnewscale')


###############################################################################
# Importar os dados
###############################################################################

# Importar os dados geográficos
sites_P <- read.csv("Dados/registros_E_subsecundum.csv", encoding = "UTF-8", sep=",")
sites_M <- read.csv("Dados/registros_L_bokermanni.txt", encoding = "UTF-8", sep=",")
interacao <- data.frame("Latitude" = -19.093374, "Longitude" = -43.470373)

# Separar só as colunas de latitude e longitude
sites_P <- sites_P %>% dplyr::select(Latitude, Longitude)
sites_M <- sites_M %>% dplyr::select(Latitude, Longitude)

# Todos os sitios juntos
sites <- rbind(sites_M, sites_P, interacao)

# adicionar a etiqueta e um vetor de cores
sites <- sites %>%
  dplyr::mutate("Grupo" = c(rep("Morcego", 24), rep("Planta", 82), "Interacao"))


# Extrair os dados dos biomas
biomas <- rgdal::readOGR("Dados/Biomas_250mil/lm_bioma_250.shp")



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
  
  # Adicionar a barra de escala
  ggspatial::annotation_scale(location = "br", width_hint = 0.2,
                              bar_cols = c("grey30", "white")) +
  
  # Adicionar a flecha de orientação para o Norte
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                    height = unit(1.5, "cm"), 
                                    width = unit(1.5, "cm"),
                                    style = ggspatial::north_arrow_fancy_orienteering(
                                      fill = c("white","grey30"))) +
  
  
  # Adicionar os poligonos
  geom_polygon(data = MA_CA_CE, aes(x = long, y = lat, group = group, 
                                    fill = id)) +
  
  # para a legenda do poligono
  scale_fill_manual(name="Biomas",
                    values = c("#6BBC19", "goldenrod2", "lightskyblue"),
                    breaks = c("3", "1", "2"),
                    labels = c("Mata Atlântica", "Caatinga", "Cerrado")) +
  
  # para colocar uma escala nova
  new_scale_fill() +
  
  # Plotar os pontos geográficos 
  geom_point(data = sites, aes(x = Longitude, y = Latitude,
                               shape = Grupo, fill = Grupo),
             alpha = 0.6, size = 2, colour = "black") +
  
  scale_fill_manual(name = "Registros", values = c("darkred", "black", "violet"),
                     breaks = c("Morcego", "Planta", "Interacao"),
                     labels = c(expression(italic("L. bokermanni")), 
                                expression(italic("E. subsecundum")),
                                "Interação confirmada")) +
  
  scale_shape_manual(name = "Registros", values = c(21, 22, 24),
                     breaks = c("Morcego", "Planta", "Interacao"),
                     labels = c(expression(italic("L. bokermanni")), 
                                expression(italic("E. subsecundum")),
                                "Interação confirmada")) +
  
  # Configurar a descrição dos eixos X e Y
  labs(x = "Longitude", y = "Latitude") +
    
  # Ajustar a legenda 
  theme(legend.position = c(0.82, 0.23),
        panel.grid = element_blank(),
        legend.background = element_rect(fill = "NA"),
        legend.key = element_rect(fill = "NA"),
        legend.text.align = 0,
        plot.margin = unit(rep(0.5,4), "lines"))
g2

# Exportar o mapa como uma imagem PNG
png("./Gráficos/Figure_3.png", res = 300,
    width = 2000, height = 2200, unit = "px")

dev.off()


################################ FIM ###########################################