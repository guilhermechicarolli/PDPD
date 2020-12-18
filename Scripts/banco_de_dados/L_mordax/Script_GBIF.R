#-------- BANCO DE DADOS DA ESPÉCIE LONCHOPHYLLA MORDAX OBTIDOS NO GBIF
#                    (12 REGISTROS, sem filtragem)

library(tidyverse)

# Variável que representa o banco de dados da espécie L. mordax obtidos do GBIF
L_mordax2 <- read_delim('Lonchophylla_mordax/GBIF/occurrence.txt', delim='\t')
L_mordax2 <- as_tibble(L_mordax2)


#--------

# Remoção de observações com a mesma referência geográfica de latitude e 
# longitude, permanecendo apenas uma

L_mordax2 <- L_mordax2 %>%
    distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)


# Criando o arquivo CSV limpo (6 ocorrências)
arquivo2 <- L_mordax2 %>%
    select(gbifID, scientificName, catalogNumber, modified, countryCode, 
           stateProvince,municipality, locality, decimalLatitude, 
           decimalLongitude)

write_csv(arquivo2, 'L_mordax_GBIF_limpo.csv')

#--------

view(arquivo2)
view(L_mordax2)

#---------
# PLOT DAS OCORRENCIAS

plot (arquivo2$decimalLongitude, arquivo2$decimalLatitude,
      xlim=c(-80,-30),ylim=c(-35,5), col='red',pch=19,
      xlab='Longitude',ylab='Latitude' )
map(add=T)
