############## SCRIPTS DO PROJETO DE PDPD ##############

# 1. Calculo das sobreposicoes entre L. bokermanni e
# E. subsecundum 

################################################################################

##### Carregamento das bibliotecas necessarias
if (!require(sdm)) install.packages('sdm')
if (!require(dismo)) install.packages('dismo')
if (!require(raster)) install.packages('raster')


################################################################################

#--------- 1. CALCULO DAS SOBREPOSICOES ---------#

pa1  # distribuicao do presente da planta
pa2  # distribuicao rcp45
pa3  # distribuicao rcp85

pa1M  # distribuicao do presente do morcego
pa2M  # distribuicao rcp45
pa3M  # distribuicao rcp85


#----------
# SOBREPOSICAO NO PRESENTE

Sob <- pa1*2+ pa1M
# Area com valor igual a 3 = sobreposicao
# Area com valor igual a 2 = area com planta, mas sem o morcego
# Area com valor igual a 1 = area com morcego, mas sem a planta
plot(Sob, col=c('#669966','#BF87B3','#7F5AA2','blue'))  


Sp <- pa1 - pa1M # area negativa ou positiva = area de mismatch

# Com relacao a distribuicao da planta
pS <- Sp$layer@data@values == 1
tamanhopS <- sum(pS[!is.na(pS)])
areapS <- tamanhopS*median(cel_tam)
areapS  # area da distribuicao da planta sem contato com a ditribuicao do morcego

porcentagem_pS <- (areapS / area)*100
porcentagem_pS    # porcentagem de mismatch com relecao a area de distribuicao da planta


# Com relacao a distribuicao do morcego
mS <- Sp$layer@data@values == -1
tamanhomS <- sum(mS[!is.na(mS)])
areamS <- tamanhomS*median(cel_tam)
areamS  # area da distribuicao do morcego sem contato com a ditribuicao do morcego

porcentagem_mS <- (areamS / areaM)*100
porcentagem_mS    # porcentagem de mismatch com relecao a area de distribuicao do morcego


# area de sobreposicao presente
area_Sob <- areaM - areamS
area_Sob

#----------
# SOBREPOSICAO NO FUTURO RCP 45 

Sob2 <- pa2*2+ pa2M
# Area com valor igual a 3 = sobreposicao
# Area com valor igual a 2 = area com planta, mas sem o morcego
# Area com valor igual a 1 = area com morcego, mas sem a planta
plot(Sob2, col=c('#669966','#BF87B3','#7F5AA2','blue'))  


Sp2 <- pa2 - pa2M      # area negativa ou positiva = area de mismatch
plot(Sp2)

# Com relacao a distribuicao da planta
pS45 <- Sp2$layer@data@values == 1
tamanhopS45 <- sum(pS45[!is.na(pS45)])
areapS45 <- tamanhopS45*median(cel_tam)
areapS45  # area da distribuicao da planta sem contato com a ditribuicao do morcego

porcentagem_pS45 <- (areapS45 / areaF45)*100
porcentagem_pS45    # porcentagem de sobreposicao futura RCP45 que se manteve em relacao 
                    # a distribuicao presente


# Com relacao a distribuicao do morcego
mS45 <- Sp2$layer@data@values == -1
tamanhomS45 <- sum(mS45[!is.na(mS45)])
areamS45 <- tamanhomS45*median(cel_tam)
areamS45  # area da distribuicao do morcego sem contato com a ditribuicao da planta

porcentagem_mS45 <- (areamS45 / areaF45M)*100
porcentagem_mS45    # porcentagem de sobreposicao futura RCP45 que se manteve em relacao 
                    # a distribuicao presente


# area de sobreposicao RCP45
area_Sob45 <- areaF45M - areamS45
area_Sob45

# Porcentagem de area de sobreposicao perdida (RCP45)
porcentagem_Sob45 <- (area_Sob45/area_Sob)*100
porcentagem_Sob45

# Porcentagem de area de sobreposicao perdida
100 - porcentagem_Sob45


#----------
# SOBREPOSICAO NO FUTURO RCP 85

Sob3 <- pa3*2+ pa3M
# Area com valor igual a 3 = sobreposicao
# Area com valor igual a 2 = area com planta, mas sem o morcego
# Area com valor igual a 1 = area com morcego, mas sem a planta
plot(Sob3, col=c('#669966','#BF87B3','#7F5AA2','blue')) 


Sp3 <- pa3 - pa3M      # area negativa ou positiva = area de mismatch
plot(Sp3)

# Com relacao a distribuicao da planta
pS85 <- Sp3$layer@data@values == 1
tamanhopS85 <- sum(pS85[!is.na(pS85)])
areapS85 <- tamanhopS85*median(cel_tam)
areapS85  # area da distribuicao da planta sem contato com a ditribuicao do morcego

################## REVERRRRR

porcentagem_pS85 <- (areapS85 / areaF85)*100
porcentagem_pS85    # porcentagem de mismatch com relecao a area de distribuicao da planta


# Com relacao a distribuicao do morcego
mS85 <- Sp3$layer@data@values == -1
tamanhomS85 <- sum(mS85[!is.na(mS85)])
areamS85 <- tamanhomS85*median(cel_tam)
areamS85  # area da distribuicao do morcego sem contato com a ditribuicao da planta

porcentagem_mS85 <- (areamS85 / areaF85M)*100
porcentagem_mS85 # porcentagem de mismatch com relecao a area de distribuicao do morcego


# area de sobreposicao RCP85
area_Sob85 <- areaF85M - areamS85
area_Sob85

# Porcentagem de area de sobreposicao perdida (RCP85)
porcentagem_Sob85 <- (area_Sob85/area_Sob)*100

# Porcentagem de area de sobreposicao perdida
100 - porcentagem_Sob85



################################################################################

# Salvar os rasters dos mapas

writeRaster(Sob, './Mapas_raster/sobreposicao_presente.asc')
writeRaster(Sob2, './Mapas_raster/sobreposicao_RCP45.asc')
writeRaster(Sob3, './Mapas_raster/sobreposicao_RCP85.asc')

# Salvar como .png
# Presente
png('./Graficos/sobreposicoes/presente.png', height=nrow(Sob), 
    width=ncol(Sob)) 
plot(Sob, maxpixels=ncell(Sob), col=c('#669966','#BF87B3','#7F5AA2','blue'))
dev.off()

# RCP45
png('./Graficos/sobreposicoes/RCP45.png', height=nrow(Sob2), 
    width=ncol(Sob2)) 
plot(Sob2, maxpixels=ncell(Sob2), col=c('#669966','#BF87B3','#7F5AA2','blue'))
dev.off()

# RCP85
png('./Graficos/sobreposicoes/RCP85.png', height=nrow(Sob3), 
    width=ncol(Sob3)) 
plot(Sob3, maxpixels=ncell(Sob3), col=c('#669966','#BF87B3','#7F5AA2','blue'))
dev.off()

################################ FIM ###########################################