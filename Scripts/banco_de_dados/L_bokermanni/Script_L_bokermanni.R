#-------- SCRIPTS DO BANCO DE DADOS DA ESPÉCIE LONCHOPHYLLA BOKERMANNI --------------#


# Carregando bibliotecas necessárias
library(tidyverse)


#---------------------------------------------------------------------------

# Exportando o arquivo CSV criado:
registros <- read.csv("Dados/registros_L_bokermanni.csv", 
                      encoding = "UTF-8")

# Verificando os dados
head(registros)
view(registros)


