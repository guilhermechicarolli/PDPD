############## SCRIPTS DO PROJETO DE PDPD ##############

# 1. Rodagem dos modelos de distribuição para a espécie 
#    de planta Encholirium subsecundum
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
# 20 e 500 km de distância das ocorrências. 10000 pontos de background.

pontos_backgP <- biomod2::BIOMOD_FormatingData (resp.var = respP, 
                                       expl.var = explP, 
                                       resp.xy = xyP, 
                                       resp.name = nomeP, 
                                       PA.nb.rep = 1,	
                                       PA.nb.absences = 10000,	
                                       PA.strategy = 'disk',
                                       PA.dist.min = 20000,	
                                       PA.dist.max = 500000,	
                                       na.rm = TRUE)

# Verificação dos valores das camadas ambientais nos pontos de background
head(pontos_backgP@data.env.var)


### DEFINIÇÃO DOS PARÂMETROS DO MAXENT

# Alteração dos parâmetros (feature clas e RM) com base nos parâmetros 
# analisados e obtidos no script 05 a partir da analise usando ENMevaluate
# Features: LQ, RM = 0.5

opcoes_maxentP <- biomod2::BIOMOD_ModelingOptions(
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

modelo_maxentP <- biomod2::BIOMOD_Modeling(pontos_backgP,
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
modelos_validacaoP <- biomod2::get_evaluations(modelo_maxentP)

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
colnames(TSS_resultadosP) <- c("TSS", "Sensitivity", "Specificity", "Threshold")

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
colnames(AUC_resultadosP) <- c("AUC", "Sensitivity", "Specificity", "Threshold")

# Verificação
head(AUC_resultadosP)

# Salvar os resultados em um arquivo csv
write.csv(AUC_resultadosP, 
          "./Dados/Resultados_E_subsecundum/subsecundum_AUC_tabela.csv")


### ESCOLHA DO MELHOR MODELO A PARTIR DAS MÉTRICAS CALCULADAS (será usado para
### as projeções futuras)

# Escolha de modelos com TSS > 0.4 e AUC > 0.7
# Sensitividade e especificidade mais próximas de 100 são melhores

# Testa e seleciona os modelos:
AUC_resultadosP[which(AUC_resultadosP[,1] > 0.75), ]
TSS_resultadosP[which(AUC_resultadosP[,1] > 0.75), ]

posicao_modelosP <- which(AUC_resultados[,1] > 0.75)

# Seleção do nome dos melhores modelos e do melhor modelo:

# Verificação dos nomes
modelo_maxentP@models.computed

# Nomes dos melhores modelos
melhores_modelosP <- modelo_maxentP@models.computed[posicao_modelosP]

# Nome do melhor modelo
melhor_modeloP <- modelo_maxentP@models.computed[5]



### CÁLCULO DA IMPORTÂNCIA DAS VARIÁVEIS

# Considerando todos os modelos
importancia_varsP <- t(as.data.frame(get_variables_importance(modelo_maxentP)))  ##### TESTAR ESSA PARTE

# Verificação
importancia_varsP

# Salvar os resultados
write.csv(importancia_varsP, 
          './Dados/Resultados_E_subsecundum/subsecundum_mportancia_variaveis.csv')


# Considerar apenas os melhores modelos
melhores_impP <- importancia_varsP[posicao_modelosP, ]

# Cálculo da média da importância 
media_impP <- c(mean(melhores_impP[,1]), mean(melhores_impP[,2]), 
               mean(melhores_impP[,3]), mean(melhores_impP[,4]), 
               mean(melhores_impP[,5]))                                         # NUM DE VARIAVEIS VERIFICAR APOS A RODAGEM DOS MODELOS

# Adicionar uma última linha com as médias de importância
melhores_mediasP <- rbind(melhores_impP, media_impP)

# Verificação
tail(melhores_mediasP)

# Salvar os resultados
write.csv(melhores_mediasP, 
          "./Dados/Resultados/subsecundum_importancia_vars_melhores_modelos.csv")


### GRÁFICOS DA IMPORTÂNCIA DAS VARIÁVEIS

# Gráfico da importância das variáveis para todos os modelos
ggplot2::ggplot(gather(as.data.frame(importancia_varsP)),
       aes(x = reorder(key, value, fun = median,), y = value)) + 
    
    geom_boxplot() + 
    
    scale_x_discrete(name="Variáveis")+
    
    scale_y_continuous(name="Importância (%)")+
    
    theme_bw(base_size = 14)

# Gráfico da importância das variáveis dos melhores modelos
ggplot2::ggplot(gather(as.data.frame(melhores_impP))
       ,aes(x = reorder(key, value, fun = median,), y = value)) + 
    
    geom_boxplot() + 
    
    scale_x_discrete(name="Variáveis")+
    
    scale_y_continuous(name="Importância (%)")+
    
    theme_bw(base_size = 14)



### CURVAS DE RESPOSTAS DAS VARIÁVEIS PARA OS MODELOS

modelosP<-biomod2::BIOMOD_LoadModels(modelo_maxentP, models = "MAXENT.Phillips")

# Verificação dos dados
modelosP


# Construir as curvas de resposta para os melhores modelos
curvas_melhores_modelosP <- biomod2::response.plot2(
        models = modelosP[posicao_modelosP], # Escolher os modelos 
        Data = get_formal_data(modelo_maxentP, 'expl.var') , #RasterStack com as variáveis
        show.variables = get_formal_data(modelo_maxentP, 'expl.var.names'),
        do.bivariate = FALSE,
        fixed.var.metric = "mean",
        save.file = "pdf",                       # Formato do arquivo de imagem
        name = "./Resultados/Resultados_E_subsecundum/Curva_resposta_subsecundum_melhores_modelos", # Nome do modelo
        ImageSize = 480,                         # Resolução da imagem
        col = c("blue", "red", "black", "gray"), # Cores para as curvas de acordo com o número de modelos
        legend = TRUE,
        data_species = get_formal_data(modelo_maxentP, 'resp.var'))

# Curvas de resposta para o melhor modelo
curvas_melhor_modelosP <- biomod2::response.plot2(
        models = modelosP[5],                                                   ##### ESCOLHER QUAL FOI O MELHOR MODELO
        Data = get_formal_data(modelo_maxentP, 'expl.var') , #RasterStack com as variáveis
        show.variables = get_formal_data(modelo_maxentP, 'expl.var.names'),
        do.bivariate = FALSE,
        fixed.var.metric = "mean",
        save.file = "pdf",                       # Formato do arquivo de imagem
        name = "./Resultados/Resultados_E_subsecundum/Curva_resposta_subsecundum_melhores_modelos", # Nome do modelo
        ImageSize = 480,                         # Resolução da imagem
        col = c("blue", "red", "black", "gray"), # Cores para as curvas de acordo com o número de modelos
        legend = TRUE,
        data_species = get_formal_data(modelo_maxentP, 'resp.var'))


################################################################################

#--------- 3. PROJEÇÃO DOS MODELOS GERADOS 
#                  PARA O PRESENTE  --------#

# Carregamento das camadas do presente selecionadas para a planta
camadas_projP <- explP

projec_presenteP <- biomod2::BIOMOD_Projection(modeling.output = modelo_maxentP,
                                 new.env = camadas_projP,
                                 proj.name = 'presente',
                                 selected.models = melhores_modelosP, 
                                 compress = FALSE,
                                 build.clamping.mask = FALSE,
                                 output.format = '.img',
                                 do.stack = TRUE)

# Verificação dos modelos
projec_presenteP

plot(projec_presenteP)

# Transformar as projeções para o tipo raster
rasters_presenteP <- biomod2::get_predictions(projec_presenteP)

plot(rasters_presenteP[[3]])


# Fazer um modelo médio de todas as projeções criadas
raster_medio_presenteP <- calc(rasters_presenteP, fun=mean)

# Salvar o modelo médio
raster::writeRaster(
    raster_medio_presenteP,
    filename="./Dados/Resultados_E_subsecundum/Projecao_presente/subsecundum_modelo_medio_presente.asc", 
    format="ascii")



### CONSTRUÇÃO DO MAPA FINAL A PARTIR DO THRESHOLD

# Construir um mapa binário (presença / ausência) com base em um valor de limiar
# (threshold) e no mapa médio

limiares_presenteP <- as.data.frame(AUC_resultados[which(
    AUC_resultados[,1] > 0.75), ][4])

# Cálculo do threshold médio
limiar_presente_medio <- mean(limiares$Threshold)

# Verificação
limiar_presente_medio


# Criar o mapa binário
mapa_binario_presenteP <- biomod2::BinaryTransformation(raster_medio_presenteP,
                                              limiar_presente_medio)


# Salvar o mapa binário criado
raster::writeRaster(mapa_binario_presenteP, 
            filename="./Resultados_E_subsecundum/Projecao_presente/subsecundum_mapa_binario_presente.asc", 
            format="ascii", overwrite=TRUE)


# Para criar o mapa final basta multiplicar o mapa binário pelo mapa médio dos
# melhores modelos
raster_final_presente <- raster_medio_presenteP * mapa_binario_presenteP

# Verificação
raster_final_presente

# Salvar o mapa final
raster::writeRaster(raster_final_presente, 
                    filename="./Resultados_E_subsecundum/Projecao_presente/subsecundum_mapa_final.asc", 
                    format="ascii")


################################################################################











