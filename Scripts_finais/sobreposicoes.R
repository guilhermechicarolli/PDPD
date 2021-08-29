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
Sp <- pa1 - pa1M      # area negativa ou positiva = area de mismatch
plot(Sp)

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
areamS  # area da distribuicao do morcego sem contato com a ditribuicao da planta

porcentagem_mS <- (areamS / areaM)*100
porcentagem_mS    # porcentagem de mismatch com relecao a area de distribuicao da planta


# area de sobreposicao presente
area_Sob <- areaM - areamS
area_Sob

#----------
# SOBREPOSICAO NO FUTURO RCP 45 

Sp2 <- pa2 - pa2M      # area negativa ou positiva = area de mismatch
plot(Sp2)

# Com relacao a distribuicao da planta
pS45 <- Sp2$layer@data@values == 1
tamanhopS45 <- sum(pS45[!is.na(pS45)])
areapS45 <- tamanhopS45*median(cel_tam)
areapS45  # area da distribuicao da planta sem contato com a ditribuicao do morcego

porcentagem_pS45 <- (areapS45 / areaF45)*100
porcentagem_pS45    # porcentagem de mismatch com relecao a area de distribuicao da planta


# Com relacao a distribuicao do morcego
mS45 <- Sp2$layer@data@values == -1
tamanhomS45 <- sum(mS45[!is.na(mS45)])
areamS45 <- tamanhomS45*median(cel_tam)
areamS45  # area da distribuicao do morcego sem contato com a ditribuicao da planta

porcentagem_mS45 <- (areamS45 / areaF45M)*100
porcentagem_mS45 # porcentagem de mismatch com relecao a area de distribuicao da planta


# area de sobreposicao RCP45
area_Sob45 <- areaF45M - areamS45
area_Sob45

#----------
# SOBREPOSICAO NO FUTURO RCP 85

Sp3 <- pa3 - pa3M      # area negativa ou positiva = area de mismatch
plot(Sp3)

# Com relacao a distribuicao da planta
pS85 <- Sp3$layer@data@values == 1
tamanhopS85 <- sum(pS85[!is.na(pS85)])
areapS85 <- tamanhopS85*median(cel_tam)
areapS85  # area da distribuicao da planta sem contato com a ditribuicao do morcego

porcentagem_pS85 <- (areapS85 / areaF85)*100
porcentagem_pS85    # porcentagem de mismatch com relecao a area de distribuicao da planta


# Com relacao a distribuicao do morcego
mS85 <- Sp3$layer@data@values == -1
tamanhomS85 <- sum(mS85[!is.na(mS85)])
areamS85 <- tamanhomS85*median(cel_tam)
areamS85  # area da distribuicao do morcego sem contato com a ditribuicao da planta

porcentagem_mS85 <- (areamS85 / areaF85M)*100
porcentagem_mS85 # porcentagem de mismatch com relecao a area de distribuicao da planta


# area de sobreposicao RCP85
area_Sob85 <- areaF85M - areamS85
area_Sob85

################################ FIM ###########################################