#-------- BANCO ESPÉCIE LONCHOPHYLLA MORDAX OBTIDOS NO SiBBr 
#                (61 registros, sem filtragem)

library(tidyverse)

# Variável que representa o banco de dados da espécie L. mordax obtidos do SiBBr
L_mordax1 <- read_csv('Lonchophylla_mordax/SiBBr/data_lonchophylla_mordax.csv')  
L_mordax1 <- as_tibble(L_mordax1)    
view(L_mordax1)

# Meta dados das colunas
colunas1 <- read_csv('Lonchophylla_mordax/SiBBr/headings.csv') 
colunas_sibbr <- as_tibble(colunas1)   

#---------

# Remoção de valores NA nas coordenads geográficas
L_mordax1 <- L_mordax1 %>%
    filter(!is.na(`Latitude` | `Longitude`))

# Remoção de observações com a mesma referência geográfica de latitude e longitude,
# permanecendo apenas uma observação

L_mordax1 <- L_mordax1 %>%
    distinct(`Latitude`, `Longitude`, .keep_all = TRUE)

# Criando o arquivo CSV limpo (28 ocorrências)
arquivo1 <- L_mordax1 %>% 
    select(`Scientific Name`, `Record ID`, Locality, Latitude, Longitude)

# Renomeando as colunas para tornar mais simples
arquivo1 <- rename(arquivo1, nomecientifico =`Scientific Name`, ID=`Record ID`, 
                   localidade=Locality, latitude=Latitude, longitude=Longitude)

path1 <- "C:\\Users\\guich\\Documents\\PDPD\\Lonchophylla_mordax\\SiBBr\\"
write_csv(arquivo1, paste(path1, 'L_mordax_SiBBr_limpo.csv'))

arquivos <- read_csv(file="Lonchophylla_mordax/SiBBr/L_mordax_SiBBr_limpo.txt")
view(arquivos)
#---------

view(arquivo1) 
view(L_mordax1)
view(colunas_sibbr)

#----------
# PLOT DAS OCORRENCIAS
library(maps)
plot.new()
plot (arquivos$longitude, arquivos$latitude,
      xlim=c(-80,-30),ylim=c(-35,5), col='red',pch=19,
      xlab='Longitude',ylab='Latitude' )
map(add=T)




