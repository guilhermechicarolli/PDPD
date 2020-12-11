#----- BANCO ESPÉCIE LONCHOPHYLLA MORDAX OBTIDOS NO SiBBr (61 registros, sem filtragem)

library(tidyverse)

# VariÃ¡vel que representa o banco de dados da espÃ©cie L. mordax obtidos do SiBBr
L_mordax1 <- read_csv('Lonchophylla_mordax/SiBBr/data_lonchophylla_mordax.csv')  
L_mordax1 <- as_tibble(L_mordax1)    
view(L_mordax1)

# Meta dados das colunas
colunas1 <- read_csv('Lonchophylla_mordax/SiBBr/headings.csv') 
colunas_sibbr <- as_tibble(colunas1)   

#---------

# RemoÃ§Ã£o de colunas redundantes ou irrelevantes ao projeto
L_mordax1 <- L_mordax1 %>%
    select(-'Institution ID', -'Data Resource ID', -'Collection ID', -'Institution Code',
           -'Collection Code', -'identified _ by', -'identified _ date', -'Collector', 
           -'Minimum elevation in meters', -'Maximum elevation in meters', -'Scientific Name - original',
           -'Vernacular name - original', -'Sex', -'Vernacular name', -'Catalogue Number')

# RemoÃ§Ã£o de colunas preenchidas apenas por NAs ou valores fixos
L_mordax1 <- L_mordax1 %>%
    select(-'Minimum depth in meters', -'Maximum depth in meters', -'Coordinate Uncertainty in Metres',
           -'Country - parsed', -'State - parsed', -'Occurrence status', 
           -('raw _ sampling _ protocol' : 'Type status not recognised'), -'Basis Of Record - original',
           -'Taxon Rank', -'Geodetic datum - original')

# RemoÃ§Ã£o de observaÃ§Ãµes sem referÃªncia geogrÃ¡fica de latitude e longitude
L_mordax1 <- L_mordax1 %>%
    filter(!is.na(`Latitude - original` | `Longitude - original`))

# RemoÃ§Ã£o de observaÃ§Ãµes com a mesma referÃªncia geogrÃ¡fica de latitude e longitude, permanecendo apenas uma
L_mordax1 <- L_mordax1 %>%
    distinct(`Latitude - original`, `Longitude - original`, .keep_all = TRUE)

view(L_mordax1)
view(colunas_sibbr)



