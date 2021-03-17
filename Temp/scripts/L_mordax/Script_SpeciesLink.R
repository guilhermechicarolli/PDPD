#------------------------------------------------------------------------------#
# Scripts para o projeto de PDPD "Impactos das mudanças climaticas: Mismatches #
#     e alterações na distribução de platas e morcegos polinizadores"          #
#------------------------------------------------------------------------------#

library (tidyverse)

#------------------------LONCHOPHYLLA MORDAX------------------------------------

#-------- BANCO DE DADOS DA ESPÉCIE LONCHOPHYLLA MORDAX OBTIDOS NO SPECIESLINK
#                   (18 REGISTROS, sem filtragem)

# Variável que representa o banco de dados da espécie L. mordax obtidos do Specieslink
L_mordax3 <- read_delim('Lonchophylla_mordax/SpeciesLink/speciesLink_L_mordax.txt', delim='\t')
L_mordax3 <- as_tibble(L_mordax3)

#--------

# Mudando os valores NAs das colunas longitute_num e latitude_num para 0
L_mordax3 <- L_mordax3 %>%
    mutate(longitude_mun = coalesce(longitude_mun, 0.0), latitude_mun = coalesce(
        latitude_mun, 0.0))

# Juntando as coodernadas nas observações, que estavam divididas em 4 colunas
L_mordax3 <- L_mordax3 %>%
    mutate(latitude_final = latitude + latitude_mun, longitude_final = longitude+
               longitude_mun) 

# Retirando observações sem coordenadas geográficas
L_mordax3 <- L_mordax3 %>%
    filter(!(L_mordax3$latitude == 0.0 & L_mordax3$latitude_mun == 0.0))

# Removendo observações com coordenadas iguais
L_mordax3 <- L_mordax3 %>%
    distinct(latitude_final, longitude_final, .keep_all = TRUE)

# Criando o arquivo CSV limpo (10 ocorrências)
arquivo <- L_mordax3 %>% 
    select(scientificname, catalognumber,country, stateprovince,
           county, locality, latitude_final, longitude_final)

# Renomeando as colunas para tornar mais simples
arquivo <- rename(arquivo, nomecientifico = scientificname, ID=catalognumber, pais=country,
                  estado=stateprovince, municipio=county, localidade=locality, 
                  latitude=latitude_final, longitude=longitude_final)

path3<- "C:\\Users\\guich\\Documents\\PDPD\\Lonchophylla_mordax\\SpeciesLink\\"
write_csv(arquivo, paste(path3, 'L_mordax_SpeciesLink_limpo.csv'))

#--------

view(arquivo)
view(L_mordax3)

#---------
# PLOT DAS OCORRENCIAS

library(maps)

plot (arquivo$longitude, arquivo$latitude,
     xlim=c(-80,-30),ylim=c(-35,5), col='red',pch=19,
     xlab='Longitude',ylab='Latitude' )
map(add=T)


