############## SCRIPTS DO PROJETO DE PDPD ##############

# 1. SCRIPTS DO BANCO DE DADOS DA ESPÉCIE ENCHOLIRIUM SUBSECUNDUM
# 2. SCRIPTS DO BANCO DE DADOS DA ESPÉCIE LONCHOPHYLLA BOKERMANNI

################################################################################

#--------- 1. SCRIPTS DO BANCO DE DADOS DA ESPÉCIE 
#                ENCHOLIRIUM SUBSECUNDUM --------------#


##### CARREGAMENTO DOS PACOTES NECESSÁRIOS #####

if (!require(tidyverse)) install.packages('tidyverse')


##### IMPORTANDO O ARQUIVO CSV COM OS REGISTROS DO SPECIESLINK #####

registros <-read.csv("Dados/DataSets_temporarios/speciesLink_E_subsecundum.csv", 
                      encoding = "UTF-8") %>%
    as_tibble(registros)


# Verificando os dados
head(registros)
view(registros)


# Selecionando as colunas de interesse para o trabalho
registros <- registros %>%
    select(catalognumber, stateprovince, county, locality, longitude_mun, 
           latitude_mun, yearcollected, collector, institutioncode)


# Salvando o data_frame resultante na pasta Dados
path <- "./Dados/"
write_csv(registros, paste(path,'registros_E_subsecundum.csv'))


##### IMPORTANDO O ARQUIVO CSV COM OS REGISTROS DO GBIF #####

registros2 <- read.csv("Dados/DataSets_temporarios/GBIF_E_subsecundum.txt",
                       sep='\t', encoding = "UTF-8") %>%
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


##### IMPORTANDO O ARQUIVO CSV COM OS REGISTROS DO SIBBR #####

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


################################################################################

#-------- SCRIPTS DO BANCO DE DADOS DA ESPÉCIE 
#               LONCHOPHYLLA BOKERMANNI --------------#


# Exportando o arquivo CSV criado:
registros <- read.csv("Dados/registros_L_bokermanni.csv", 
                      encoding = "UTF-8") %>%
    as_tibble(registros)


# Verificando os dados
head(registros)
view(registros)

