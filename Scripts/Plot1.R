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

MA_CA <- rbind(MA, CA) # Juntei os dois data frames para dar cores diferentes a cada id


################################################################################
##### PLOT THE MAP
################################################################################


# Make the base map
g1 <- ggplot(data = world) +
    geom_sf(colour = "white", fill = "#d3d3d3") +
    coord_sf(xlim = c(-55, -30), ylim = c(-30,0), expand = FALSE) +
    theme_bw() + 
    
    # Adicionar os poligonos
    geom_polygon(data = MA_CA, aes(x = long, y = lat, group = group, 
                                   fill = id), show.legend = TRUE) +
    
    # Plot the sites
    geom_point(data = sites_short, aes(x = Longitude, y = Latitude,
                                       colour = "#5C058C"), 
               alpha = 0.6, size = 3) +
    
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
    
    # legendas
    scale_fill_manual(name="Biomas",
                      values = c("#6BBC19", "#D0A61C"),
                      breaks = c("3", "1"),
                      labels = c("Mata Atlântica", "Caatinga")) +
    
    scale_colour_manual(name = "Registros", values = "#5C058C",
                        labels = expression(italic("L. bokermanni"))) +
    
    guides(color = guide_legend(override.aes = list(fill = "white"))) +
    
    # Ajustando a legenda e algumas configuracoes do plot
    theme(legend.position = c(0.87,0.2),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "NA"),
          legend.key = element_rect(fill = "NA"),
          plot.margin = unit(rep(0.5,4), "lines"))


# Export the map as a PNG image
png("./Dados/Figure_1.png", res = 300,
    width = 2000, height = 2000, unit = "px")
g1

dev.off()


################################ END ###########################################