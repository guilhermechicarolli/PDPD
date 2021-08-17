############## SCRIPTS DO PROJETO DE PDPD ##############

# 1. Análise de colinearidade para as variáveis climáticas
#    utilizando o Fator de Inflação da Variância (VIF) para
#    a espécie de planta Mimosa lewisii
#
# 2. Análise de colinearidade para as variáveis climáticas
#    utilizando o Fator de Inflação da Variância (VIF) para
#    a espécie de morcego Lonchophylla bokermanni


################################################################################

##### Carregamento das bibliotecas necessárias

if (!require(usdm)) install.packages('usdm')
if (!require(raster)) install.packages('raster')
if (!require(corrplot)) install.packages('corrplot')
if (!require(sp)) install.packages('sp')
if (!require(psych)) install.packages('psych')


# Permite que dados espaciais sejam associados com o sistema de coordenadas,
# criando uma projeção que pode ser utilizada nos rasters
proj_WGS <- sp::CRS(
    "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

################################################################################

#--------- 1. SELEÇÃO DE VARIÁVEIS USANDO VIF PARA A
#                ESPÉCIE DE PLANTA MIMOSA LEWISII  ---------#

### CARREGAMENTO DOS DADOS

# Carregamento dos pontos 
pontos_planta <- read.csv('./Dados/Ocorrencias/E_subsecundum_corrigido.csv')

# Converção dos pontos geográficos para SpatialPoints
sp::coordinates(pontos_planta) <- ~Longitude+Latitude

# Adicionar a projeção
raster::crs(pontos_planta) <- proj_WGS

# Verificação dos dados
pontos_planta


# Carregamento das camadas ambientais raster cortadas no script 02 
camadas <- list.files(path='./Dados/Camadas_presente/', pattern='.asc', 
                      full.names=TRUE) 

camadas <- raster::stack(camadas)

# Adicionar a projeção geográfica 
raster::crs(camadas) <- proj_WGS

# Verificação dos dados
camadas


# Obter os valores das camadas ambientais nos pontos de ocorrência
valores_planta <- raster::as.data.frame(raster::extract(camadas, pontos_planta))

# Verificação dos dados
summary(valores_planta)



### Gráfico da matriz de correlação entre variáveis climática
corrplanta <- corrplot::corrplot(cor(valores_planta))



### ANÁLISE DE COLINEARIDADE USANDO VIF (Variance Inflation Factors)

# Análise das variáveis para checar colinearidade
colinVars <- usdm::vifstep(valores_planta)

# Verificação
colinVars                # "14 variables from the 19 input variables 
                         #  have collinearity problem"


# Salvar os resultados das variáveis selecionadas
write.csv(colinVars@results, 
          "./Dados/Resultados_VIF/E_subsecundum/VIF_Variaveis_Presente.csv", 
          quote = F)


# Verificar a correlação existente ainda entre as variáveis selecionadas
colinVars@corMatrix         # Ainda existem graus de correlação consideráveis 
                            # entre algumas variáveis: wc2.1_30s_bio_3 ~ 
                            # wc2.1_30s_bio_4: -0.6794082
                            # wc2.1_30s_bio_10 ~ wc2.1_30s_bio_18: -0.5917641


### x <- usdm::exclude(valores_planta, v)
### psych::pca(valores_planta, nfactors = 5)$loadings

################################################################################

#--------- 2. SELEÇÃO DE VARIÁVEIS USANDO VIF PARA A
#          ESPÉCIE DE MORCEGO LONCHOPHYLLA BOKERMANNI  ---------#

### CARREGAMENTO DOS DADOS

# Carregamento dos pontos 
pontos_morcego <- read.csv('./Dados/Ocorrencias/L_bokermanni_corrigido.csv')

# Converção dos pontos geográficos para SpatialPoints
sp::coordinates(pontos_morcego) <- ~Longitude+Latitude

# Adicionar a projeção
raster::crs(pontos_morcego) <- proj_WGS

# Verificação dos dados
pontos_morcego


# Carregamento das camadas ambientais raster cortadas no script 02 
camadas <- list.files(path='./Dados/Camadas_presente/', pattern='.asc', 
                      full.names=TRUE) 

camadas <- raster::stack(camadas)

# Adicionar a projeção geográfica 
raster::crs(camadas) <- proj_WGS

# Verificação dos dados
camadas


# Obter os valores das camadas ambientais nos pontos de ocorrência
valores_morcego <- raster::as.data.frame(raster::extract(camadas, 
                                                         pontos_morcego))

# Verificação dos dados
summary(valores_morcego)



### Gráfico da matriz de correlação entre variáveis climática
corrmorcego <- corrplot::corrplot(cor(valores_morcego))



### ANÁLISE DE COLINEARIDADE USANDO VIF (Variance Inflation Factors)

# Análise das variáveis para checar colinearidade
colinVarsM <- usdm::vifstep(valores_morcego)

# Verificação
colinVarsM                # "16 variables from the 19 input variables 
                          #  have collinearity problem"


# Salvar os resultados das variáveis selecionadas
write.csv(colinVarsM@results, 
          "./Dados/Resultados_VIF/L_bokermanni/VIF_Variaveis_Presente.csv", 
          quote = F)


# Verificar a correlação existente ainda entre as variáveis selecionadas
colinVarsM@corMatrix         # Ainda existem graus de correlação consideráveis 
                             # entre algumas variáveis: wc2.1_30s_bio_3 ~ 
                             # wc2.1_30s_bio_8: 0.6794448
                             # wc2.1_30s_bio_8 ~ wc2.1_30s_bio_2: 0.6413619


################################ FIM ###########################################