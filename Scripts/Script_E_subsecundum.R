#--------- SCRIPTS DO BANCO DE DADOS DA ESPÉCIE ENCHOLIRIUM SUBSECUNDUM --------------#


# Carregando bibliotecas necessárias
library(tidyverse)


#--------------------------------------------------------------------------------------

# Importando o arquivo CSV com os registros do speciesLink:
registros <- read.csv("Dados/DataSets_temporarios/speciesLink_E_subsecundum.csv", 
                      encoding = "UTF-8") %>%
    as_tibble(registros)

# Verificando os dados
head(registros)
view(registros)

# Selecionando as colunas de interesse para o trabalho
registros <- registros %>%
    select(catalognumber, stateprovince, county, locality, longitude_mun, latitude_mun, 
           yearcollected, collector, institutioncode)

# Salvando o data_frame resultante na pasta Dados

path <- "./Dados/"
write_csv(registros, paste(path,'registros_E_subsecundum.csv'))


#--------------------------------------------------------------------------------------


# Importando o arquivo CSV com os registros do GBIF:
registros2 <- read.csv("Dados/DataSets_temporarios/GBIF_E_subsecundum.txt", sep='\t', 
                      encoding = "UTF-8") %>%
    as_tibble(registros2)

# Verificando os dados
head(registros2)
view(registros2)

# Selecionando as colunas de interesse para o trabalho
registros2 <- registros2 %>%
    select(catalogNumber, stateProvince, county, municipality, decimalLongitude, 
           decimalLatitude, year, recordedBy, datasetName) %>%
    drop_na(decimalLatitude, decimalLongitude)

# Preenchendo células vazias por NA
registros2[registros2 == ""] <- NA

# Salvando o data_frame resultante na pasta Dados
path <- "./Dados/DataSets_temporarios/"
write_csv(registros2, paste(path,'registros2_E_subsecundum.csv'))


#--------------------------------------------------------------------------------------


# Importando o arquivo CSV com os registros do SiBBr:
registros3 <- read.csv("Dados/DataSets_temporarios/SiBBr_E_subsecundum.csv", 
                       encoding = "UTF-8") %>%
    as_tibble(registros3)

# Verificando os dados
head(registros3)
view(registros3)

# Selecionando as colunas de interesse para o trabalho e removendo colunas sem coordenadas
registros3 <- registros3 %>%
    select(Catalogue.Number, State...parsed,Geodetic.datum...original,Locality,
           Longitude...original,Latitude...original,Year,Collector,Data.Resource.Name) %>%
    drop_na(Longitude...original,Latitude...original) %>%
    filter(!(Longitude...original == 0 | Latitude...original == 0))

# Preenchendo células vazias por NA
registros3[registros3 == ""] <- NA

# Salvando o data_frame resultante na pasta Dados
path <- "./Dados/DataSets_temporarios/"
write_csv(registros3, paste(path,'registros3_E_subsecundum.csv'))
