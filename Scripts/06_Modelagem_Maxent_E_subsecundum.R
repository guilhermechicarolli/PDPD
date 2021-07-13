############## SCRIPTS DO PROJETO DE PDPD ##############

# 1. Rodagem dos modelos de distribuição para a espécie 
#    de planta Encholirium subsecundum;
# 2. Validação e escolha do melhor modelo para a projeção
#    futura;
# 3. Construção dos mapas finais com as projeções do 
#    presente e do futuro;
# 4. Classificação da sobreposição geográfica e análise
#    de mismatch

################################################################################

##### Carregamento das bibliotecas necessárias

if (!require(tidyverse)) install.packages('tidyverse')
if (!require(dismo)) install.packages('dismo')
if (!require(maptools)) install.packages('maptools')
if (!require(sp)) install.packages('sp')
if (!require(raster)) install.packages('raster')
if (!require(rgeos)) install.packages('rgeos')
if (!require(biomod2)) install.packages('biomod2')


# Permite que dados espaciais sejam associados com o sistema de coordenadas,
# criando uma projeção que pode ser utilizada nos rasters
proj_WGS <- sp::CRS(
    "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


# Aumento da memória do Java no R para 6 gb. Leve em consideração a quantidade 
# de memória que deseje utilizar e muda os parâmetros
options(java.parameters = "-Xmx6g")


################################################################################


#--------- 1. RODAGEM DO MAXENT PARA GERAR MODELOS 
#              DA ESPÉCIE ENCHOLIRIUM SUBSECUNDUM  ---------#


### CARREGAMENTO DOS DADOS

camadasP <- list.files(path='./Dados/Camadas_selecionadas_PCA/E_subsecundum',
                       pattern = '.asc', full.names = TRUE)

camadasP <- raster::stack(camadasP)

# Adicionar a projeção geográfica
raster::crs(camadasP) <- proj_WGS

# Verificação dos dados
camadasP

# Utilizar a primeira camadas ambiental .asc como modelo para a máscara
mascaraP <- raster::raster(camadasP[1])


# Carregamento dos pontos de ocorrência de E. subsecundum
ocorrP <- read.csv("./Dados/Ocorrencias/E_subsecundum_corrigido.csv")

# Adicionar a projeção geográfica
sp::coordinates(ocorrP) <- ~x+y

raster::crs(ocorrP) <- proj_WGS

# Verificação dos dados
ocorrP


# Formatação dos dados para usar no biomod2
nomeP <- "subsecundum"

explP <- camadasP

xyP <- ocorrP@coords

respP = rep(1, length(ocorrP))


### DEFINIÇÃO DOS PONTOS DE BACKGROUND

# Definir os pontos de background.pseudoausência, concentrando os pontos entre 
# 20 e 500 km de distância das ocorrências

pontos_backgP <- biomod2::BIOMOD_FormatingData (resp.var = respP, 
                                       expl.var = explP, 
                                       resp.xy = xyP, 
                                       resp.name = nomeP, 
                                       PA.nb.rep = 1,	
                                       PA.nb.absences = 50,	
                                       PA.strategy = 'disk',
                                       PA.dist.min = 20000,	
                                       PA.dist.max = 500000,	
                                       na.rm = TRUE)

# Verificação dos valores das camadas ambientais nos pontos de background
head(back@data.env.var)


### DEFINIÇÃO DOS PARÂMETROS DO MAXENT

# Alteração dos parâmetros (feature clas e RM) com base nos parâmetros 
# analisados e obtidos no script 05 a partir da analise usando ENMevaluate
# Features: LQ, RM = 0.5

opcoes_maxentP = biomod2::BIOMOD_ModelingOptions(
    MAXENT.Phillips = list(
        path_to_maxent.jar = "/Dados/Maxent",
        memory_allocated = NULL, 
        maximumiterations = 5000,    # quantidade de interações
        # Features:
        linear = TRUE, 
        quadratic = TRUE, 
        product = FALSE,            
        threshold = FALSE,          
        hinge = FALSE,              
        lq2lqptthreshold = 80,
        l2lqthreshold = 10,
        hingethreshold = 15,
        beta_threshold = -1,
        beta_categorical = -1,
        beta_lqp = -1,
        beta_hinge = -1,
        betamultiplier = 0.5,        # valor de beta/regularização
        defaultprevalence =.5))


### RODAGEM DO MODELO NO MAXENT

modelo_maxentP = biomod2::BIOMOD_Modeling(pontos_backgP,
                                 models=c("MAXENT.Phillips"), 
                                 models.options = opcoes_maxentP, 
                                 NbRunEval = 10,	 # quantidade de replicações
                                 # % de dados de presença para treino
                                 DataSplit = 70,	
                                 Prevalence = 0.5,
                                 # quantidade de permutações para estimar a 
                                 # importÂncia das variaveis
                                 VarImport = 10, 
                                 models.eval.meth = c("TSS", "ROC"),
                                 SaveObj = TRUE,
                                 rescal.all.models = TRUE,
                                 do.full.models = FALSE,
                                 modeling.id = paste(nomeP))


################################################################################


#--------- 2. VALIDAÇÃO DOS MODELOS GERADOS  ---------#

# Parâmetros de validação
modelos_validacaoP = biomod2::get_evaluations(modelo_maxentP)

# Verificação
modelos_validacaoP


# Organização dos resultados do TSS
modelos_validacaoP["TSS","Testing.data",,,]
modelos_validacaoP["TSS","Sensitivity",,,]
modelos_validacaoP["TSS","Specificity",,,]
modelos_validacaoP["TSS","Cutoff",,,]

# Armazenamento dos resultados em uma tabela
TSS_resultadosP<-as.data.frame(cbind(modelos_validacao["TSS","Testing.data",,,],
                                      modelos_validacao["TSS","Sensitivity",,,],
                                      modelos_validacao["TSS","Specificity",,,], 
                                      modelos_validacao["TSS","Cutoff",,,] ))

# alterar o nome das colunas
colnames(TSS_resultadosP) = c("TSS", "Sensitivity", "Specificity", "Threshold")

# Verificação
head(TSS_resultados)

# Salvar os resultados em um arquivo csv
write.csv(TSS_resultados, 
          "./Dados/Resultados_E_subsecundum/subsecundum_TSS_tabela.csv")


# Organização dos resultados do ROC/AUC
modelos_validacaoP["ROC","Testing.data",,,]
modelos_validacaoP["ROC","Sensitivity",,,]
modelos_validacaoP["ROC","Specificity",,,]
modelos_validacaoP["ROC","Cutoff",,,]

# Armazenamento dos resultados em uma tabela
AUC_resultadosP<-as.data.frame(cbind(modelos_validacao["ROC","Testing.data",,,],
                                     modelos_validacao["ROC","Sensitivity",,,],
                                     modelos_validacao["ROC","Specificity",,,], 
                                     modelos_validacao["ROC","Cutoff",,,] ))

# alterar o nome das colunas
colnames(AUC_resultadosP) = c("AUC", "Sensitivity", "Specificity", "Threshold")

# Verificação
head(AUC_resultadosP)

# Salvar os resultados em um arquivo csv
write.csv(AUC_resultadosP, 
          "./Dados/Resultados_E_subsecundum/subsecundum_AUC_tabela.csv")


### ESCOLHA DO MELHOR MODELO A PARTIR DAS MÉTRICAS CALCULADAS (será usada para
### as prjeções futuras)

# Escolha de modelos com TSS > 0.4 e AUC > 0.7
# Sensitividade e especificidade mais próximas de 100 são melhores

# Testa e seleciona os modelos:
AUC_resultadosP[which(AUC_resultadosP[,1] > 0.75), ]
TSS_resultadosP[which(AUC_resultadosP[,1] > 0.75), ]

posicao_modelosP = which(AUC_resultados[,1] > 0.75)

# Seleção do nome dos melhores modelos e do melhor modelo:

# Verificação dos nomes
modelo_maxentP@models.computed

# Nomes dos melhores modelos
melhores_modelosP = modelo_maxentP@models.computed[posicao_modelosP]

# Nome do melhor modelo
melhor_modeloP = modelo_maxentP@models.computed[5]


