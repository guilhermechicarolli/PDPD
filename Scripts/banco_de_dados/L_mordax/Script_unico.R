#-------  SCRIPT COM TODOS OS BANCOS DE DADOS DE L. MORDAX REUNIDOS

library(tidyverse)

# L_mordax_ocor, variável com todas as ocorrências, já limpas e organizadas

L_mordax_ocor <- read_csv("Lonchophylla_mordax/L_mordax.txt") %>%
    distinct(`latitude`, `longitude`, .keep_all = TRUE) %>%
    distinct(`localidade`, .keep_all=TRUE)

# Criando o arquivo csv L_mordax_ocor

path <- "C:\\Users\\guich\\Documents\\PDPD\\Lonchophylla_mordax\\"
write_csv(L_mordax_ocor, paste(path,'L_mordax_ocor.csv'))

view(L_mordax_occor)


# MAPA

plot.new()
plot(L_mordax_occor$longitude,L_mordax_occor$latitude,pch=19,col="red",
     xlim=c(-80,-30),ylim=c(-35,5),
     xlab="Longitude",ylab="Latitude",)
map(add=T)
