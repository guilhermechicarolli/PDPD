#-------  SCRIPT COM TODOS OS BANCOS DE DADOS DE M. LEWISII REUNIDOS

library(tidyverse)

# ocorrencias_M_lewisii, variável com todas as ocorrências, já limpas e organizadas

ocorrencias_M_lewisii <- read_csv("Mimosa_lewisii\\SpeciesLink\\M_lewisii_SpeciesLink_limpo.csv") 

ocorrencias_M_lewisii <- ocorrencias_M_lewisii %>%
    select(scientificname, longitude_final, latitude_final) %>%
    rename(nomecientifico='scientificname', longitude='longitude_final', latitude='latitude_final')


# Criando o arquivo csv 

path3 <- "C:\\Users\\guich\\Documents\\PDPD\\Mimosa_lewisii\\"
write_csv(ocorrencias_M_lewisii, paste(path3,'ocorrencias_M_lewisii.csv'))

view(ocorrencias_M_lewisii)


# MAPA
ocorr <- read_csv("Mimosa_lewisii\\ ocorrencias_M_lewisii.csv")

library(maps)

plot.new()
plot(ocorr$longitude,ocorr$latitude,pch=19,col="red",
     xlim=c(-80,-30),ylim=c(-35,5),
     xlab="Longitude",ylab="Latitude",)
map(add=T)
