#------------------------------------------------------------------------------#
# Scripts para o projeto de PDPD "Impactos das mudanças climaticas: Mismatches#
# e alterações na distribuição de platas e morcegos polinizadores"         #
#------------------------------------------------------------------------------#

library (tidyverse)

#------------------------LONCHOPHYLLA MORDAX------------------------------------

#-------- BANCO DE DADOS DA ESPÉCIE LONCHOPHYLLA MORDAX OBTIDOS NO SPECIESLIN 
# (18 REGISTROS, sem filtragem)

# Variável que representa o banco de dados da espécie L. mordax obtidos do Specieslink
L_mordax3 <- read_delim('Lonchophylla_mordax/SpeciesLink/speciesLink_L_mordax.txt', delim='\t')
L_mordax3 <- as_tibble(L_mordax3)

#--------

# Removendo todas as colunas com apenas NAs
L_mordax3 <- L_mordax3 %>%
    select_if(~all(!is.na(.)))

view(L_mordax3)
