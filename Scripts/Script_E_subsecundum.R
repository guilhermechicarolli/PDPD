#-------- SCRIPTS DO BANCO DE DADOS DA ESPÉCIE ENCHOLIRIUM SUBSECUNDUM --------------#


# Carregando bibliotecas necessárias
library(tidyverse)


#-------------------------------------------------------------------------------------

# Exportando o arquivo CSV criado:
registros <- read.csv("Dados/speciesLink_E_subsecundum.csv", 
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



