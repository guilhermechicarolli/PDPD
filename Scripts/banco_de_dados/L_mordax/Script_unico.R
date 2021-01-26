#-------  SCRIPT COM TODOS OS BANCOS DE DADOS DE L. MORDAX REUNIDOS

library(tidyverse)

# L_mordax_ocor, variável com todas as ocorrências, já limpas e organizadas

ocorrencias_L_mordax <- read_csv("Lonchophylla_mordax/L_mordax.txt") %>%
    distinct(`latitude`, `longitude`, .keep_all = TRUE) %>%
    distinct(`localidade`, .keep_all=TRUE)


# Criando arquivo com o nome da espécies, longitude e latitude, que serão usados na modelagem

ocorrencias_L_mordax <- select(ocorrencias_L_mordax, nomecientifico, longitude, latitude)
path2 <- "C:\\Users\\guich\\Documents\\PDPD\\Lonchophylla_mordax\\"
write_csv(ocorrencias_L_modax, paste(path2,'ocorrencias_L_mordax.csv'))

view(ocorrencias_L_mordax)

# MAPA
library(maps)

plot.new()
plot(ocorrencias_L_mordax$longitude, ocorrencias_L_mordax$latitude,pch=19,col="red",
     xlim=c(-80,-30),ylim=c(-35,5),
     xlab="Longitude",ylab="Latitude",)
map(add=T)
