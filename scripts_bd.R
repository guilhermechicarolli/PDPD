#-------------------------------------------------------------------------------#
# Scripts para o projeto de PDPD "Impactos das mudanças climaticas: Mismatches  #
# e alterações na distribuição de platas e morcegos polinizadores"              #
#-------------------------------------------------------------------------------#

library (tidyverse)
library (ggplot2)

#------------------------LONCHOPHYLLA MORDAX------------------------------------

#----- BANCO ESPÉCIE LONCHOPHYLLA MORDAX OBTIDOS NO SiBBr (61 registros, sem filtragem)


# Variável que representa o banco de dados da espécie L. mordax obtidos do SiBBr
L_mordax1 <- read_csv('https://raw.githubusercontent.com/guilhermechicarolli/PDPD/main/Lonchophylla_mordax/SiBBr/data_lonchophylla_mordax.csv')  
L_mordax1 <- as_tibble(L_mordax1)    

# Meta dados das colunas
colunas1 <- read_csv('https://raw.githubusercontent.com/guilhermechicarolli/PDPD/main/Lonchophylla_mordax/SiBBr/headings.csv') 
colunas_sibbr <- as_tibble(colunas1)   

#---------

# Remoção de colunas redundantes ou irrelevantes ao projeto
L_mordax1 <- L_mordax1 %>%
    select(-'Institution ID', -'Data Resource ID', -'Collection ID', -'Institution Code',
           -'Collection Code', -'identified _ by', -'identified _ date', -'Collector', 
           -'Minimum elevation in meters', -'Maximum elevation in meters', -'Scientific Name - original',
           -'Vernacular name - original', -'Sex', -'Vernacular name', -'Catalogue Number')

# Remoção de colunas preenchidas apenas por NAs ou valores fixos
L_mordax1 <- L_mordax1 %>%
    select(-'Minimum depth in meters', -'Maximum depth in meters', -'Coordinate Uncertainty in Metres',
           -'Country - parsed', -'State - parsed', -'Occurrence status', 
           -('raw _ sampling _ protocol' : 'Type status not recognised'), -'Basis Of Record - original',
           -'Taxon Rank', -'Geodetic datum - original')

# Remoção de observações sem referência geográfica de latitude e longitude
L_mordax1 <- L_mordax1 %>%
    filter(!is.na(`Latitude - original` | `Longitude - original`))

# Remoção de observações com a mesma referência geográfica de latitude e longitude, permanecendo apenas uma
L_mordax1 <- L_mordax1 %>%
    distinct(`Latitude - original`, `Longitude - original`, .keep_all = TRUE)

view(L_mordax1)
view(colunas_sibbr)



#-------- BANCO DE DADOS DA ESPÉCIE LONCHOPHYLLA MORDAX OBTIDOS NO GBIF (12 REGISTROS, sem filtragem)

# Variável que representa o banco de dados da espécie L. mordax obtidos do GBIF
L_mordax2 <- read_delim('https://raw.githubusercontent.com/guilhermechicarolli/PDPD/main/Lonchophylla_mordax/GBIF/occurrence.txt', delim='\t')
L_mordax2 <- as_tibble(L_mordax2)


#--------

# Removendo todas as colunas com apenas NAs
L_mordax2 <- L_mordax2 %>%
    select_if(~all(!is.na(.)))

# Remoção de observações com a mesma referência geográfica de latitude e longitude, permanecendo apenas uma
L_mordax2 <- L_mordax2 %>%
    distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)

view(L_mordax2)


#-------- BANCO DE DADOS DA ESPÉCIE LONCHOPHYLLA MORDAX OBTIDOS NO SPECIESLIN (18 REGISTROS, sem filtragem)

# Variável que representa o banco de dados da espécie L. mordax obtidos do Specieslink
L_mordax3 <- read_delim('https://raw.githubusercontent.com/guilhermechicarolli/PDPD/main/Lonchophylla_mordax/SpeciesLink/speciesLink_L_mordax.txt', delim='\t')
L_mordax3 <- as_tibble(L_mordax3)

#--------

# Removendo todas as colunas com apenas NAs
L_mordax3 <- L_mordax3 %>%
    select_if(~all(!is.na(.)))

view(L_mordax3)
