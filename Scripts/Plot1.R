################################################################################
#
# FIGURE 1
# Geographic distribution of sampling sites by study type
#
################################################################################

#Delete all previous objects
rm(list= ls())


################################################################################
##### LOAD THE PACKAGES
################################################################################

if (!require(dplyr)) install.packages('dplyr')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(grDevices)) install.packages('grDevices')
if (!require(sf)) install.packages('sf')
if (!require(rnaturalearth)) install.packages('rnaturalearth')
if (!require(rnaturalearthdata)) install.packages('rnaturalearthdata')
if (!require(ggspatial)) install.packages('ggspatial')
if (!require(rgeos)) install.packages('rgeos')
if (!require(rgdal)) install.packages('rgdal')


################################################################################
##### IMPORT THE DATA
################################################################################


# Import the data set with site coordinates
sites <- read.csv("Dados/registros_L_bokermanni.csv", encoding = "UTF-8")

# extrair os dados dos biomas
biomas <- rgdal::readOGR("Dados/Biomas_250mil/lm_bioma_250.shp")


# Check the data
class(sites)
str(sites)
head(sites)

# Select the columns with the coordinates and study types
sites_short <- sites %>% 
    dplyr::select(Latitude, Longitude)

#Check the data
head(sites_short)

# Load the world map from the mapdata package
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Extrair dados da mata atlântica e Caatinga

MA <- ggplot2::fortify(biomas[biomas$Bioma=="Mata AtlÃ¢ntica",])
CA <- ggplot2::fortify(biomas[biomas$Bioma=="Caatinga",])


# OI ADICIONEI ESSA LINHA

################################################################################
##### PLOT THE MAP
################################################################################


# Make the base map
g1 <- ggplot(data = world) +
    geom_sf(colour = "white", fill = "#d3d3d3") +
    coord_sf(xlim = c(-55, -30), ylim = c(-30,0), expand = FALSE) +
    theme_bw() + 
    
    # Adicionar a Mata atlântica
    geom_polygon(data = MA, aes(x = long, y = lat, group = group), 
                 fill = "green", show.legend = TRUE) +
    
    # Adicionar a Caatinga
    geom_polygon(data = CA, aes(x = long, y = lat, group = group), 
                 fill = "yellow", show.legend = TRUE) +
    
    # Plot the sites
    geom_point(data = sites_short, aes(x = Longitude, y = Latitude), 
               alpha = 0.6, size = 3, colour = "darkred") +
    
    # Add a scale bar
    ggspatial::annotation_scale(location = "br", width_hint = 0.2,
                                bar_cols = c("grey30", "white")) +
    
    # Add a north arrow
    ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                      height = unit(1.5, "cm"), 
                                      width = unit(1.5, "cm"),
                                      style = ggspatial::north_arrow_fancy_orienteering(
                                          fill = c("white","grey30"))) +
    
    # Eixo X e Y
    labs(x = "Longitude", y = "Latitude") +
    
 
    # Tentativa quase certa de criar as legendas dos layers 
    
    scale_color_manual(name="Biomas",
                       labels="Caatinga", "Mata Atlântica",
                       values=c("green", "green")) 
    
g1
    
    ####### Customize the colors and labels 
    
    scale_color_manual(values = c("Yellow","Green")) + 
    theme(plot.subtitle =c("Mata Atlântica", "Cerrado"),
          panel.grid = element_blank(),
          legend.text = element_text(size = 11),
          legend.title = element_text(face = "bold", size = 11),
          axis.text = element_text(size = 11, colour = "black"),
          axis.title.x = element_text(size = 11, colour = "black", vjust = -4,
                                      face = "bold"),
          axis.title.y = element_text(size = 11, colour = "black", vjust = 1,
                                      face = "bold"),
          legend.position = c(0.8,0.3),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(1,4), "lines")) 


# See the map
x11()
g1

# Export the map as a PNG image
png("./Dados/Figure_1.png", res = 300,
    width = 2000, height = 2200, unit = "px")
g1

dev.off()


################################ END ###########################################