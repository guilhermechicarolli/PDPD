############## SCRIPTS DO PROJETO DE PDPD ##############

# 1. Rodagem dos modelos de distribuição para a espécie 
#    de morcego Lonchophylla bokermanni
# 2. Validação e escolha do melhor modelo, além de 
#    cálculo da importância das variáveis
# 3. Criação das projeções no presente e dos mapas
# 4. Criação das projeções no cenário futuro de RCP 4.5
# 5. Criação das projeções no cenário futuro de RCP 8.5
# 6. Classificação da sobreposição geográfica e análise
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
#              DA ESPÉCIE LONCHOPHYLLA BOKERMANNI  ---------#


### CARREGAMENTO DOS DADOS

camadasM <- list.files(path='./Dados/Camadas_selecionadas_PCA/L_bokermanni',
                       pattern = '.asc', full.names = TRUE)

camadasM <- raster::stack(camadasM)

# Adicionar a projeção geográfica
raster::crs(camadasM) <- proj_WGS

# Verificação dos dados
camadasM

# Utilizar a primeira camadas ambiental .asc como modelo para a máscara
mascaraM <- raster::raster(camadasM[1])


# Carregamento dos pontos de ocorrência de E. subsecundum
ocorrM <- read.csv("./Dados/Ocorrencias/L_bokermanni_corrigido.csv")

# Adicionar a projeção geográfica
sp::coordinates(ocorrM) <- ~x+y

raster::crs(ocorrM) <- proj_WGS

# Verificação dos dados
ocorrM


# Formatação dos dados para usar no biomod2
nomeM <- "bokermanni"

explM <- camadasM

xyM <- ocorrM@coords

respM <- rep(1, length(ocorrM))


### DEFINIÇÃO DOS PONTOS DE BACKGROUND

# Definir os pontos de background.pseudoausência, concentrando os pontos entre 
# 20 e 500 km de distância das ocorrências. 10000 pontos de background.

pontos_backgM <- biomod2::BIOMOD_FormatingData (resp.var = respM, 
                                                expl.var = explM, 
                                                resp.xy = xyM, 
                                                resp.name = nomeM, 
                                                PA.nb.rep = 1,	
                                                PA.nb.absences = 10000,	
                                                PA.strategy = 'disk',
                                                PA.dist.min = 20000,	 # Distância mínima para criar as PA
                                                PA.dist.max = 500000,	
                                                na.rm = TRUE)

# Verificação dos valores das camadas ambientais nos pontos de background
head(pontos_backgM@data.env.var)


### DEFINIÇÃO DOS PARÂMETROS DO MAXENT

# Alteração dos parâmetros (feature clas e RM) com base nos parâmetros 
# analisados e obtidos no script 05 a partir da analise usando ENMevaluate
# Features: LQ, RM = 0.5

opcoes_maxentM <- biomod2::BIOMOD_ModelingOptions(
    MAXENT.Phillips = list(
        path_to_maxent.jar = "~/R/win-library/4.0/dismo/java",
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

modelo_maxentM <- biomod2::BIOMOD_Modeling(pontos_backgM,
                                           models=c("MAXENT.Phillips"), 
                                           models.options = opcoes_maxentM, 
                                           NbRunEval = 10,	 # quantidade de replicações
                                           # % de dados de presença para treino
                                           DataSplit = 70,	
                                           Prevalence = 0.5,
                                           # quantidade de permutações para estimar a 
                                           # importÂncia das variaveis
                                           VarImport = 10, 
                                           models.eval.meth = c("TSS", "ROC"),  # Testes estatísticos
                                           SaveObj = TRUE,
                                           rescal.all.models = TRUE,
                                           do.full.models = FALSE,
                                           modeling.id = paste(nomeP))


################################################################################

#--------- 2. VALIDAÇÃO DOS MODELOS GERADOS  ---------#

# Parâmetros de validação
modelos_validacaoM <- biomod2::get_evaluations(modelo_maxentM)

# Verificação
modelos_validacaoM


# Organização dos resultados do TSS
modelos_validacaoM["TSS","Testing.data",,,]
modelos_validacaoM["TSS","Sensitivity",,,]
modelos_validacaoM["TSS","Specificity",,,]
modelos_validacaoM["TSS","Cutoff",,,]

# Armazenamento dos resultados em uma tabela
TSS_resultadosM<-as.data.frame(cbind(modelos_validacaoM["TSS","Testing.data",,,],
                                     modelos_validacaoM["TSS","Sensitivity",,,],
                                     modelos_validacaoM["TSS","Specificity",,,], 
                                     modelos_validacaoM["TSS","Cutoff",,,] ))

# alterar o nome das colunas
colnames(TSS_resultadosM) <- c("TSS", "Sensitivity", "Specificity", "Threshold")

# Verificação
head(TSS_resultadosM)

# Salvar os resultados em um arquivo csv
write.csv(TSS_resultados, 
          "./Dados/Resultados_Modelagem_L_bokermanni/bokermanni_TSS_tabela.csv")


# Organização dos resultados do ROC/AUC
modelos_validacaoM["ROC","Testing.data",,,]
modelos_validacaoM["ROC","Sensitivity",,,]
modelos_validacaoM["ROC","Specificity",,,]
modelos_validacaoM["ROC","Cutoff",,,]

# Armazenamento dos resultados em uma tabela
AUC_resultadosM<-as.data.frame(cbind(modelos_validacaoM["ROC","Testing.data",,,],
                                     modelos_validacaoM["ROC","Sensitivity",,,],
                                     modelos_validacaoM["ROC","Specificity",,,], 
                                     modelos_validacaoM["ROC","Cutoff",,,] ))

# alterar o nome das colunas
colnames(AUC_resultadosM) <- c("AUC", "Sensitivity", "Specificity", "Threshold")

# Verificação
head(AUC_resultadosM)

# Salvar os resultados em um arquivo csv
write.csv(AUC_resultadosM, 
          "./Dados/Resultados_modelagem_L_bokermanni/bokermanni_AUC_tabela.csv")


### ESCOLHA DO MELHOR MODELO A PARTIR DAS MÉTRICAS CALCULADAS (será usado para
### as projeções futuras)

# Escolha de modelos com TSS > 0.4 e AUC > 0.7
# Sensitividade e especificidade mais próximas de 100 são melhores

# Testa e seleciona os modelos:
AUC_resultadosM[which(AUC_resultadosM[,1] > 0.75), ]
TSS_resultadosM[which(AUC_resultadosM[,1] > 0.75), ]

posicao_modelosM <- which(AUC_resultadosM[,1] > 0.75)

# Seleção do nome dos melhores modelos e do melhor modelo:

# Verificação dos nomes
modelo_maxentM@models.computed

# Nomes dos melhores modelos
melhores_modelosM <- modelo_maxentM@models.computed[posicao_modelosM]

# Nome do melhor modelo
melhor_modeloM <- modelo_maxentM@models.computed[5]



### CÁLCULO DA IMPORTÂNCIA DAS VARIÁVEIS

# Considerando todos os modelos
importancia_varsM <- t(as.data.frame(biomod2::get_variables_importance(
    modelo_maxentM)))                                                           ##### TESTAR ESSA PARTE

# Verificação
importancia_varsM

# Salvar os resultados
write.csv(importancia_varsM, 
          './Dados/Resultados_modelagem_L_bokermanni/bokermanni_importancia_variaveis.csv')


# Considerar apenas os melhores modelos
melhores_impM <- importancia_varsM[posicao_modelosM, ]

# Cálculo da média da importância 
media_impM <- c(mean(melhores_impM[,1]), mean(melhores_impM[,2]), 
                mean(melhores_impM[,3]), mean(melhores_impM[,4]), 
                mean(melhores_impM[,5]))                                         # NUM DE VARIAVEIS VERIFICAR APOS A RODAGEM DOS MODELOS

# Adicionar uma última linha com as médias de importância
melhores_mediasM <- rbind(melhores_impM, media_impM)

# Verificação
tail(melhores_mediasM)

# Salvar os resultados
write.csv(melhores_mediasM, 
          "./Dados/Resultados_modelagem_L_bokermanni/bokermanni_importancia_vars_melhores_modelos.csv")


### GRÁFICOS DA IMPORTÂNCIA DAS VARIÁVEIS

# Gráfico da importância das variáveis para todos os modelos
ggplot2::ggplot(gather(as.data.frame(importancia_varsM)),
                aes(x = reorder(key, value, fun = median,), y = value)) + 
    
    geom_boxplot() + 
    
    scale_x_discrete(name="Variáveis")+
    
    scale_y_continuous(name="Importância (%)")+
    
    theme_bw(base_size = 14)

# Gráfico da importância das variáveis dos melhores modelos
ggplot2::ggplot(gather(as.data.frame(melhores_impM))
                ,aes(x = reorder(key, value, fun = median,), y = value)) + 
    
    geom_boxplot() + 
    
    scale_x_discrete(name="Variáveis")+
    
    scale_y_continuous(name="Importância (%)")+
    
    theme_bw(base_size = 14)



### CURVAS DE RESPOSTAS DAS VARIÁVEIS PARA OS MODELOS

modelosM<-biomod2::BIOMOD_LoadModels(modelo_maxentM, models = "MAXENT.Phillips")

# Verificação dos dados
modelosM


# Construir as curvas de resposta para os melhores modelos
curvas_melhores_modelosM <- biomod2::response.plot2(
    models = modelosM[posicao_modelosM], # Escolher os modelos 
    Data = get_formal_data(modelo_maxentM, 'expl.var') , #RasterStack com as variáveis
    show.variables = get_formal_data(modelo_maxentM, 'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = "mean",
    save.file = "pdf",                       # Formato do arquivo de imagem
    name = "./Dados/Resultados_modelagem_L_bokermanni/Curva_resposta_bokermanni_melhores_modelos", # Nome do modelo
    ImageSize = 480,                         # Resolução da imagem
    col = c("blue", "red", "black", "gray"), # Cores para as curvas de acordo com o número de modelos
    legend = TRUE,
    data_species = get_formal_data(modelo_maxentM, 'resp.var'))

# Curvas de resposta para o melhor modelo
curvas_melhor_modelosM <- biomod2::response.plot2(
    models = modelosM[5],                                                       ##### ESCOLHER QUAL FOI O MELHOR MODELO
    Data = get_formal_data(modelo_maxentM, 'expl.var') , #RasterStack com as variáveis
    show.variables = get_formal_data(modelo_maxentM, 'expl.var.names'),
    do.bivariate = FALSE,
    fixed.var.metric = "mean",
    save.file = "pdf",                       # Formato do arquivo de imagem
    name = "./Dados/Resultados_modelagem_L_bokermanni/Curva_resposta_bokermanni_melhores_modelos", # Nome do modelo
    ImageSize = 480,                         # Resolução da imagem
    col = c("blue", "red", "black", "gray"), # Cores para as curvas de acordo com o número de modelos
    legend = TRUE,
    data_species = get_formal_data(modelo_maxentM, 'resp.var'))


