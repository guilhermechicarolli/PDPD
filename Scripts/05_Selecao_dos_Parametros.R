############## SCRIPTS DO PROJETO DE PDPD ##############

# 1. Configurações dos parâmetros do Maxent: Escolha das
#    features class e parâmetros de regularização para
#    a espécie de planta (Encholirium subsecundum) e de
#    morcego (Lonchophylla bokermanni)

################################################################################

##### Carregamento das bibliotecas necessárias

if (!require(rJava)) install.packages('rJava')
if (!require(dismo)) install.packages('dismo')
if (!require(rgdal)) install.packages('rgdal')
if (!require(raster)) install.packages('raster')
if (!require(parallel)) install.packages('parallel')
if (!require(plyr)) install.packages('plyr')
if (!require(ENMeval)) install.packages('ENMeval')


# Permite que dados espaciais sejam associados com o sistema de coordenadas,
# criando uma projeção que pode ser utilizada nos rasters

proj_WGS <- sp::CRS(
    "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


################################################################################

#--------- 1. PARÂMETROS DO MAXENT PARA 
#              ENCHOLIRIUM SUBSECUNDUM  ---------#

# Aumento da memória do Java no R para 5 gb
options(java.parameters = "-Xmx5g")

jar = paste("./Maxent/maxent.jar")

### CARREGAMENTO DOS DADOS

# Carregamento dos pontos de ocorrência de Encholirium subsecundum no script 03
ocorrP = read.csv("./Dados/Ocorrencias/E_subsecundum_corrigido.csv")

# Verificação 
ocorrP

# Carregar as variáveis ambientais selecionadas para E. subsecundum no script 04
camadasP = list.files(path="./Dados/Camadas_selecionadas_PCA/E_subsecundum", 
                      pattern =".asc", full.names=TRUE)
camadasP = raster::stack(camadasP)

# Verificação
camadasP

# Carregamento de 10 mil pontos de pseudoausência dentro da área de estudo 
# (pontos de background)

pontos_bgP = dismo::randomPoints(camadasP, n=10000)


### FUNÇÃO ENMevaluate

# Teste dos parâmetros

# Classes features
FC = c("L", "LQ", "H", "LQH")

# Regularização/beta
RM = seq(0.5, 2, 0.5)

# Análise ENMevaluate com o Maxent
evalP = ENMeval::ENMevaluate (ocorrP,
                      camadasP,
                      bg.coords = pontos_bgP,
                      RMvalues = RM,
                      fc = FC,
                      algorithm = "maxent.jar",
                      method = "block",
                      clamp = FALSE,
                      overlap= FALSE,
                      progbar  = TRUE,
                      updateProgress = TRUE,
                      parallel = TRUE,
                      numCores = 3)

evalP = ENMeval::ENMevaluate (ocorrP,
                      camadasP,
                      bg.coords = pontos_bgP,
                      RMvalues = RM,
                      fc = FC,
                      method = "block",
                      clamp = FALSE,
                      overlap= FALSE,
                      rasterPreds = TRUE,
                      progbar  = TRUE,
                      parallel = TRUE,
                      numCores = 4)

# Verificação dos dados
evalP

# Salvar os resultados como Rdata
saveRDS(evalP, file = 
            "./Dados/Parametros_maxent/ParametrosMaxEnt_E_subsecundum.rds")


### ESCOLHA DOS MELHORES VALORES ANALISADOS DE FEATURE CLASS E REGULARIZAÇÃO

# Todos os resultados de performance dos modelos
evalP@results

# Organização a partir de valores de AICc
resultadosP = plyr::arrange(evalP@results, evalP@results$AICc)

# Verificação dos dados
resultadosP

# Considerar os melhores modelos, no qual deltaAICc < 2 (desconsiderando NA)
melhores_modelosP = resultadosP[resultadosP$delta.AICc<2  &
                                    !is.na(resultadosP$delta.AICc), ]

# Salvar o arquivo csv
write.csv(melhores_modelosP,
          file="./Dados/Parametros_maxent/melhores_parametros_E_subsecundum.csv", 
          row.names = F)


################################################################################

#--------- 2. PARÂMETROS DO MAXENT PARA 
#             LONCHOPHYLLA BOKERMANNI  ---------#


# Aumento da memória do Java no R para 5 gb
options(java.parameters = "-Xmx5g")

jar = paste("./Maxent/maxent.jar")

### CARREGAMENTO DOS DADOS

# Carregamento dos pontos de ocorrência de Encholirium subsecundum no script 03
ocorrM = read.csv("./Dados/Ocorrencias/L_bokermanni_corrigido.csv")

# Verificação 
ocorrM

# Carregar as variáveis ambientais selecionadas para L. bokermanni no script 04

camadasM = list.files(path="./Dados/Camadas_selecionadas_PCA/L_bokermanni", 
                      pattern =".asc", full.names=TRUE)
camadasM = raster::stack(camadasM)

# Verificação
camadasM

# Carregamento de 10 mil pontos de pseudoausência dentro da área de estudo 
# (pontos de background)

pontos_bgM = dismo::randomPoints(camadasM, n=10000)


### FUNÇÃO ENMevaluate

# Teste dos parâmetros

# Classes features
FC = c("L", "LQ", "H", "LQH")

# Regularização/beta
RM = seq(0.5, 2, 0.5)

# Análise ENMevaluate com o Maxent
evalM = ENMeval::ENMevaluate (ocorrM,
                              camadasM,
                              bg.coords = pontos_bgM,
                              RMvalues = RM,
                              fc = FC,
                              algorithm = "maxent.jar",
                              method = "block",
                              clamp = FALSE,
                              overlap= FALSE,
                              progbar  = TRUE,
                              updateProgress = TRUE,
                              parallel = TRUE,
                              numCores = 3)

evalM = ENMeval::ENMevaluate (ocorrM,
                              camadasM,
                              bg.coords = pontos_bgM,
                              RMvalues = RM,
                              fc = FC,
                              method = "block",
                              clamp = FALSE,
                              overlap= FALSE,
                              rasterPreds = TRUE,
                              progbar  = TRUE,
                              parallel = TRUE,
                              numCores = 4)

# Verificação dos dados
evalM

# Salvar os resultados como Rdata
saveRDS(evalM, file = 
            "./Dados/Parametros_maxent/ParametrosMaxEnt_L_bokermanni.rds")


### ESCOLHA DOS MELHORES VALORES ANALISADOS DE FEATURE CLASS E REGULARIZAÇÃO

# Todos os resultados de performance dos modelos
evalM@results

# Organização a partir de valores de AICc
resultadosM = plyr::arrange(evalM@results, evalM@results$AICc)

# Verificação dos dados
resultadosM

# Considerar os melhores modelos, no qual deltaAICc < 2 (desconsiderando NA)
melhores_modelosM = resultadosM[resultadosM$delta.AICc<2  &
                                    !is.na(resultadosM$delta.AICc), ]

# Salvar o arquivo csv
write.csv(melhores_modelosM,
          file="./Dados/Parametros_maxent/melhores_parametros_L_bokermanni.csv", 
          row.names = F)


################################ FIM ###########################################