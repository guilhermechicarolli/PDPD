#------------------------- Mimosa lewisii --------------------------------------

library(tidyverse)

#-------- BANCO DE DADOS DA ESPÉCIE MIMOSA LEWISII OBTIDOS NO SPECIESLINK
# (364 registros, sem filtragem)

# Variável que representa o banco de dados
M_lewisii1 <- read_delim('Mimosa_lewisii/SpeciesLink/speciesLink_all_67587_20201211193651.txt', delim='\t')
M_lewisii1 <- as.tibble(M_lewisii1)

#--------

# Mudando os valores NAs das colunas longitute_num e latitude_num para 0
M_lewisii1 <- M_lewisii1 %>%
    mutate(longitude_mun = coalesce(longitude_mun, 0.0), latitude_mun = coalesce(
        latitude_mun, 0.0))

# Juntando as coodernadas nas observações, que estavam divididas em 4 colunas
M_lewisii1 <- M_lewisii1 %>%
    mutate(latitude_final = latitude + latitude_mun, longitude_final = longitude+
               longitude_mun) 

# Retirando observações sem coordenadas geográficas
M_lewisii1 <- M_lewisii1[!(M_lewisii1$latitude == 0.00 & M_lewisii1$latitude_mun == 0.00),]

# Removendo observações com coordenadas iguais
M_lewisii1 <- M_lewisii1 %>%
    distinct(latitude_final, longitude_final, .keep_all = TRUE)

# Criando o arquivo CSV limpo
arquivo <- M_lewisii1 %>% 
    select(datelastmodified, scientificname, catalognumber,country, stateprovince,
           county, locality, latitude_final, longitude_final)

write_csv(arquivo, 'M_lewisii_SpeciesLink_limpo.csv')

#---------

view(arquivo)

view(M_lewisii1)


