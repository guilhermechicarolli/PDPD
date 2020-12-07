#-------- BANCO DE DADOS DA ESPÃCIE LONCHOPHYLLA MORDAX OBTIDOS NO GBIF (12 REGISTROS, sem filtragem)

# VariÃ¡vel que representa o banco de dados da espÃ©cie L. mordax obtidos do GBIF
L_mordax2 <- read_delim('https://raw.githubusercontent.com/guilhermechicarolli/PDPD/main/Lonchophylla_mordax/GBIF/occurrence.txt', delim='\t')
L_mordax2 <- as_tibble(L_mordax2)


#--------

# Removendo todas as colunas com apenas NAs
L_mordax2 <- L_mordax2 %>%
    select_if(~all(!is.na(.)))

# RemoÃ§Ã£o de observaÃ§Ãµes com a mesma referÃªncia geogrÃ¡fica de latitude e longitude, permanecendo apenas uma
L_mordax2 <- L_mordax2 %>%
    distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)

view(L_mordax2)