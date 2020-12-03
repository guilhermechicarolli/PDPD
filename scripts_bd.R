#-------------------------------------------------------------------------------#
# Scripts para o projeto de PDPD "Impactos das mudanças climaticas: Mismatches  #
# e alterações na distribuição de platas e morcegos polinizadores"              #
#-------------------------------------------------------------------------------#

library (tidyverse)
library (ggplot2)

#-------------------------------------------------------------------------------
# BANCO ESPÉCIE LONCHOPHYLLA MORDAX

# Variável que representa o banco de dados da espécie L. mordax
L_mordax <- read_csv('https://raw.githubusercontent.com/guilhermechicarolli/PDPD/main/Lonchophylla_mordax/data_lonchophylla_mordax.csv?token=AQHJ74WBF5E4MS5MNEYW6AK7ZE2GA')  
L_mordax    

# meta dados das colunas
colunas <- read_csv('https://raw.githubusercontent.com/guilhermechicarolli/PDPD/main/Lonchophylla_mordax/headings.csv?token=AQHJ74TEG6KQVHTUH2PJ3P27ZE2LA') 
colunas    
