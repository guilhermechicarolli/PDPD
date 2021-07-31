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
nomeP <- "modelo_subsecundum"

explP <- camadasP

xyP <- ocorrP@coords

respP <- rep(1, length(ocorrP))


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
                                       PA.dist.min = 20000,	 # Distância mínima para criar as PA
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
                                 models.eval.meth = c("TSS", "ROC"),  # Testes estatísticos
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
TSS_resultadosP<-as.data.frame(cbind(modelos_validacaoP["TSS","Testing.data",,,],
                                      modelos_validacaoP["TSS","Sensitivity",,,],
                                      modelos_validacaoP["TSS","Specificity",,,], 
                                      modelos_validacaoP["TSS","Cutoff",,,] ))

# alterar o nome das colunas
colnames(TSS_resultadosP) <- c("TSS", "Sensitivity", "Specificity", "Threshold")

# Verificação
head(TSS_resultadosP)

# Salvar os resultados em um arquivo csv
write.csv(TSS_resultadosP, 
          "./Dados/Resultados_Modelagem_E_subsecundum/subsecundum_TSS_tabela.csv")


# Organização dos resultados do ROC/AUC
modelos_validacaoP["ROC","Testing.data",,,]
modelos_validacaoP["ROC","Sensitivity",,,]
modelos_validacaoP["ROC","Specificity",,,]
modelos_validacaoP["ROC","Cutoff",,,]

# Armazenamento dos resultados em uma tabela
AUC_resultadosP<-as.data.frame(cbind(modelos_validacaoP["ROC","Testing.data",,,],
                                     modelos_validacaoP["ROC","Sensitivity",,,],
                                     modelos_validacaoP["ROC","Specificity",,,], 
                                     modelos_validacaoP["ROC","Cutoff",,,] ))

# alterar o nome das colunas
colnames(AUC_resultadosP) <- c("AUC", "Sensitivity", "Specificity", "Threshold")

# Verificação
head(AUC_resultadosP)

# Salvar os resultados em um arquivo csv
write.csv(AUC_resultadosP, 
          "./Dados/Resultados_modelagem_E_subsecundum/subsecundum_AUC_tabela.csv")


### ESCOLHA DO MELHOR MODELO A PARTIR DAS MÉTRICAS CALCULADAS (será usado para
### as projeções futuras)

# Escolha de modelos com TSS > 0.4 e AUC > 0.7
# Sensitividade e especificidade mais próximas de 100 são melhores

# Consensus Forecasting of SpeciesDistributions: The Effects of Niche 
# ModelPerformance and Niche Properties (Lei Zhang et al. 2015)

# Testa e seleciona os modelos:
AUC_resultadosP[which(AUC_resultadosP[,1] > 0.75), ]
TSS_resultadosP[which(AUC_resultadosP[,1] > 0.75), ]

posicao_modelosP <- which(AUC_resultadosP[,1] > 0.75)

# Seleção do nome dos melhores modelos e do melhor modelo:

# Verificação dos nomes
modelo_maxentP@models.computed

# Nomes dos melhores modelos
melhores_modelosP <- modelo_maxentP@models.computed[posicao_modelosP]

# Nome do melhor modelo
melhor_modeloP <- modelo_maxentP@models.computed[5]

melhor_modeloP  # "subsecundum_PA1_RUN5_MAXENT.Phillips"


### CÁLCULO DA IMPORTÂNCIA DAS VARIÁVEIS

# Considerando todos os modelos
importancia_varsP <- t(as.data.frame(biomod2::get_variables_importance(
    modelo_maxentP)))                                                           ##### TESTAR ESSA PARTE

# Verificação
importancia_varsP

# Salvar os resultados
write.csv(importancia_varsP, 
          './Dados/Resultados_modelagem_E_subsecundum/subsecundum_importancia_variaveis.csv')


# Considerar apenas os melhores modelos
melhores_impP <- importancia_varsP[posicao_modelosP, ]

# Cálculo da média da importância 
media_impP <- c(mean(melhores_impP[,1]), mean(melhores_impP[,2]), 
               mean(melhores_impP[,3]))                                        # NUM DE VARIAVEIS VERIFICAR APOS A RODAGEM DOS MODELOS

# Adicionar uma última linha com as médias de importância
melhores_mediasP <- rbind(melhores_impP, media_impP)

# Verificação
tail(melhores_mediasP)

# Salvar os resultados
write.csv(melhores_mediasP, 
          "./Dados/Resultados_modelagem_E_subsecundum/subsecundum_importancia_vars_melhores_modelos.csv")


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
        name = "./Dados/Resultados_modelagem_E_subsecundum/Curva_resposta_subsecundum_melhores_modelos", # Nome do modelo
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
        name = "./Dados/Resultados_modelagem_E_subsecundum/Curva_resposta_subsecundum_melhores_modelos", # Nome do modelo
        ImageSize = 480,                         # Resolução da imagem
        col = c("blue", "red", "black", "gray"), # Cores para as curvas de acordo com o número de modelos
        legend = TRUE,
        data_species = get_formal_data(modelo_maxentP, 'resp.var'))


################################################################################

#--------- 3. PROJEÇÃO DOS MODELOS GERADOS 
#                  PARA O PRESENTE  --------#

# Carregamento das camadas do presente selecionadas para a planta
camadas_projP <- explP

# Projeção
projec_presenteP <- biomod2::BIOMOD_Projection(modeling.output = modelo_maxentP,
                                 new.env = camadas_projP,
                                 proj.name = 'Presente',
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

# Verificação de uma única projeção
plot(rasters_presenteP[[2]])


# Fazer um modelo médio de todas as projeções criadas
raster_medio_presenteP <- calc(rasters_presenteP, fun=mean)

# Salvar o modelo médio
raster::writeRaster(
    raster_medio_presenteP,
    filename="./Dados/Resultados_modelagem_E_subsecundum/Projecao_presente/subsecundum_modelo_medio_presente.asc", 
    format="ascii")



### CONSTRUÇÃO DO MAPA FINAL A PARTIR DO THRESHOLD

# Construir um mapa binário (presença / ausência) com base em um valor de limiar
# (threshold) e no mapa médio

limiares_presenteP <- as.data.frame(AUC_resultadosP[which(
    AUC_resultadosP[,1] > 0.75), ][4])

# Cálculo do threshold médio
limiar_presente_medio <- mean(limiares_presenteP$Threshold)

# Verificação
limiar_presente_medio


# Criar o mapa binário
mapa_binario_presenteP <- biomod2::BinaryTransformation(raster_medio_presenteP,
                                              limiar_presente_medio)


# Salvar o mapa binário criado
raster::writeRaster(mapa_binario_presenteP, 
            filename="./Dados/Resultados_modelagem_E_subsecundum/Projecao_presente/subsecundum_mapa_binario_presente.asc", 
            format="ascii", overwrite=TRUE)


# Para criar o mapa final basta multiplicar o mapa binário pelo mapa médio dos
# melhores modelos
raster_final_presenteP <- raster_medio_presenteP * mapa_binario_presenteP

# Verificação
raster_final_presenteP

# Salvar o mapa final
raster::writeRaster(raster_final_presenteP, 
                    filename="./Dados/Resultados_modelagem_E_subsecundum/Projecao_presente/subsecundum_mapa_final_presente.asc", 
                    format="ascii")


### RECLASSIFICAÇÃO DO MAPA FINAL E ESTIMATIVA DA ÁREA ADEQUADA

# Definir a área em quilômetros quadrados dos pixels (0.5 km^2)
celulaP <- 0.5

# Estimativa da área adequada ***(com qualquer grau de adequabilidade)***
area_adequada_presenteP <- as.data.frame(tapply(area(mapa_binario_presenteP), 
                                                mapa_binario_presenteP[], sum)*
                                             celulaP)

rownames(area_adequada_presenteP) <- c("Não-adequada", "Adequada")
colnames(area_adequada_presenteP) <- c("Área (Km²)")

# Verificação
area_adequada_presenteP  # Adequada = 86829.85 Km²
                         # Não adequada = 4161191.30 Km²
# Salvar os resultados
write.csv(area_adequada_presenteP, 
          "./Dados/Resultados_modelagem_E_subsecundum/Projecao_presente/area_adequada_subsecundum_presente")



# Estimativa da área adequada por classes de adequabilidade

# Valores de 0 à 1000
raster_final_presenteP 

# Propôr uma divisão de classes
# 0 ao limiar médio = Classe 0 = Inadequada
# limiar médio ao 750 = Classe 1 = Média
# 750 ao 900 = Classe 2 = Alta
# 900 ao 1000 = Classe 3 = Muito alta

# 1) Criar data frame com a reclassificação a partir da divisão 
df_reclass_presente <- c(0, limiar_presente_medio, 0,
               limiar_presente_medio, 750, 1,
               750, 900, 2,
               900, 1000, 3)
# Verificação
df_reclass_presente

# 2) Converter o data frame a uma matriz
matriz_reclass_presente <- matrix(df_reclass_presente,
                   ncol = 3,
                   byrow = TRUE)
# Verificação
matriz_reclass_presente


# 3) Criação do raster reclassificado:
raster_classificado_presente <- reclassify(raster_final_presenteP,
                                          matriz_reclass_presente)

# 4) Estimativa da área adequada por classes:
area_adequada_classes_presente <- as.data.frame(tapply(area(
    raster_classificado_presente), raster_classificado_presente[], sum)*celulaP)

rownames(area_adequada_classes_presente)  <- c("Não-adequada", "Média", "Alta", 
                                             "Muito Alta")

colnames(area_adequada_classes_presente) <- c("Área (Km²)")

# Verificação
area_adequada_classes_presente   # Não adequada = 4161191.301 Km²
                                 # Média = 71795.532 Km²
                                 # Alta =  7215.328 Km²
                                 # Muito Alta = 7818.995 Km²

# Salvar os resultados
write.csv(area_adequada_classes_presente, 
          "./Dados/Resultados_modelagem_E_subsecundum/Projecao_presente/subsecundum_area_adequada_presente_classes.csv")


# Salvar o raster reclassificado
writeRaster(raster_classificado_presente, filename=
                "./Dados/Resultados_modelagem_E_subsecundum/Projecao_presente/subsecundum_mapa_final_presente_reclassificado.asc", 
            format="ascii")


################################################################################

#--------- 4. PROJEÇÃO DOS MODELOS GERADOS PARA O
#               CENÁRIO FUTURO DE 2070 RCP 4.5  --------#

# Projeção do modelo criado para o cenário futuro de 2070, RCP 4.5, com 
# resolução de 0.5 arcsegundos

# Carregamento das camadas de RCP 4.5 selecionadas para a planta
camadas45P <- list.files(path='./Dados/Camadas_selecionadas_PCA/E_subsecundum/RCP45/',
                       pattern = '.asc', full.names = TRUE)

camadas45P <- raster::stack(camadas45P)

# Adicionar a projeção geográfica
raster::crs(camadas45P) <- proj_WGS

# Verificação dos dados
camadas45P 


# Projeção 
projec_RCP45P <- biomod2::BIOMOD_Projection(modeling.output = modelo_maxentP,
                                               new.env = camadas45P,
                                               proj.name = 'Futuro_RCP_45',
                                               selected.models = 'all', 
                                               compress = FALSE,
                                               build.clamping.mask = FALSE,
                                               output.format = '.img',
                                               do.stack = TRUE)

# Verificação dos modelos
projec_RCP45P

plot(projec_RCP45P)


# Transformar as projeções para o tipo raster
rasters_RCP45P <- biomod2::get_predictions(projec_RCP45P)

plot(rasters_RCP45P[[3]])


# Fazer um modelo médio de todas as projeções criadas
raster_medio_RCP45P <- raster::calc(rasters_RCP45P, fun=mean)

# Salvar o modelo médio
raster::writeRaster(
    raster_medio_RCP45P,
    filename="./Dados/Resultados_modelagem_E_subsecundum/Projecao_RCP45/subsecundum_modelo_medio_RCP45.asc", 
    format="ascii")


### CONSTRUÇÃO DO MAPA FINAL A PARTIR DO THRESHOLD

# Construir um mapa binário (presença / ausência) com base em um valor de limiar
# (threshold) e no mapa médio

limiares_RCP45P <- as.data.frame(AUC_resultadosP[which(
    AUC_resultadosP[,1] > 0.75), ][4])

# Cálculo do threshold médio
limiar_RCP45_medio <- mean(limiares_RCP45P$Threshold)

# Verificação
limiar_RCP45_medio


# Criar o mapa binário
mapa_binario_RCP45P <- biomod2::BinaryTransformation(raster_medio_RCP45P,
                                                        limiar_RCP45_medio)


# Salvar o mapa binário criado
raster::writeRaster(mapa_binario_RCP45P, 
                    filename="./Dados/Resultados_modelagem_E_subsecundum/Projecao_RCP45/subsecundum_mapa_binario_RCP45.asc", 
                    format="ascii", overwrite=TRUE)


# Para criar o mapa final basta multiplicar o mapa binário pelo mapa médio dos
# melhores modelos
raster_final_RCP45P <- raster_medio_RCP45P * mapa_binario_RCP45P

# Verificação
raster_final_RCP45P

# Salvar o mapa final
raster::writeRaster(raster_final_RCP45P, 
                    filename="./Dados/Resultados_modelagem_E_subsecundum/Projecao_RCP45/subsecundum_mapa_final_RCP45.asc", 
                    format="ascii")


### RECLASSIFICAÇÃO DO MAPA FINAL E ESTIMATIVA DA ÁREA ADEQUADA

# Definir a área em quilômetros quadrados dos pixels (0.5 km^2)
celulaP = 0.5

# Estimativa da área adequada ***(com qualquer grau de adequabilidade)***
area_adequada_RCP45P <- as.data.frame(tapply(area(mapa_binario_RCP45P), 
                                                mapa_binario_RCP45P[], sum)*
                                             celulaP)

rownames(area_adequada_RCP45P) <- c("Não-adequada", "Adequada")
colnames(area_adequada_RCP45P) <- c("Área (Km²)")

# Verificação
area_adequada_RCP45P

# Salvar os resultados
write.csv(area_adequada_RCP45P, 
          "./Dados/Resultados_modelagem_E_subsecundum/Projecao_RCP45/area_adequada_subsecundum_RCP45")



# Estimativa da área adequada por classes de adequabilidade

# Valores de 0 à 1000
raster_final_RCP45P 

# Propôr uma divisão de classes
# 0 ao limiar médio = Classe 0 = Inadequada
# limiar médio ao 750 = Classe 1 = Média
# 750 ao 900 = Classe 2 = Alta
# 900 ao 1000 = Classe 3 = Muito alta

# 1) Criar data frame com a reclassificação a partir da divisão 
df_reclass_RCP45 <- c(0, limiar_RCP45_medio, 0,
                         limiar_RCP45_medio, 750, 1,
                         750, 900, 2,
                         900, 1000, 3)
# Verificação
df_reclass_RCP45

# 2) Converter o data frame a uma matriz
matriz_reclass_RCP45 <- matrix(df_reclass_RCP45,
                                 ncol = 3,
                                 byrow = TRUE)
# Verificação
matriz_reclass_RCP45


# 3) Criação do raster reclassificado:
raster_classificado_RCP45 <- raster::reclassify(raster_final_RCP45P, 
                                        matriz_reclass_RCP45)

# 4) Estimativa da área adequada por classes:
area_adequada_classes_RCP45 <- as.data.frame(tapply(area(
    raster_classificado_RCP45), raster_classificado_RCP45[], sum)*celulaP)

rownames(area_adequada_classes_RCP45) <- c("Não-adequada", "Média", "Alta", 
                                             "Muito Alta")

colnames(area_adequada_classes_RCP45) <- c("Área (Km²)")

# Verificação
area_adequada_classes_RCP45


# Salvar os resultados
write.csv(area_adequada_classes_RCP45, 
          "./Dados/Resultados_modelagem_E_subsecundum/Projecao_RCP45/subsecundum_area_adequada_RCP45_classes.csv")


# Salvar o raster reclassificado
raster::writeRaster(raster_classificado_RCP45, filename=
                "./Dados/Resultados_modelagem_E_subsecundum/Projecao_RCP45/subsecundum_mapa_final_RCP45_reclassificado.asc", 
            format="ascii")


################################################################################

#--------- 5. PROJEÇÃO DOS MODELOS GERADOS PARA O
#               CENÁRIO FUTURO DE 2070 RCP 8.5  --------#

# Projeção do modelo criado para o cenário futuro de 2070, RCP 8.5, com 
# resolução de 0.5 arcsegundos

# Carregamento das camadas de RCP 8.5 selecionadas para a planta
camadas85P <- list.files(path='./Dados/Camadas_selecionadas_PCA/E_subsecundum/RCP85/',
                          pattern = '.asc', full.names = TRUE)

camadas85P <- raster::stack(camadas85P)

# Adicionar a projeção geográfica
raster::crs(camadas85P) <- proj_WGS

# Verificação dos dados
camadas85P 


# Projeção 
projec_RCP85P <- biomod2::BIOMOD_Projection(modeling.output = modelo_maxentP,
                                            new.env = camadas85P,
                                            proj.name = 'Futuro_RCP_85',
                                            selected.models = melhores_modelosP, 
                                            compress = FALSE,
                                            build.clamping.mask = FALSE,
                                            output.format = '.img',
                                            do.stack = TRUE)

# Verificação dos modelos
projec_RCP85P

plot(projec_RCP85P)


# Transformar as projeções para o tipo raster
rasters_RCP85P <- biomod2::get_predictions(projec_RCP85P)

plot(rasters_RCP85P[[3]])


# Fazer um modelo médio de todas as projeções criadas
raster_medio_RCP85P <- raster::calc(rasters_RCP85P, fun=mean)

# Salvar o modelo médio
raster::writeRaster(
    raster_medio_RCP85P,
    filename="./Dados/Resultados_modelagem_E_subsecundum/Projecao_RCP85/subsecundum_modelo_medio_RCP85.asc", 
    format="ascii")


### CONSTRUÇÃO DO MAPA FINAL A PARTIR DO THRESHOLD

# Construir um mapa binário (presença / ausência) com base em um valor de limiar
# (threshold) e no mapa médio

limiares_RCP85P <- as.data.frame(AUC_resultadosP[which(
    AUC_resultadosP[,1] > 0.75), ][4])

# Cálculo do threshold médio
limiar_RCP85_medio <- mean(limiares_RCP85P$Threshold)

# Verificação
limiar_RCP85_medio


# Criar o mapa binário
mapa_binario_RCP85P <- biomod2::BinaryTransformation(raster_medio_RCP85P,
                                                     limiar_RCP85_medio)


# Salvar o mapa binário criado
raster::writeRaster(mapa_binario_RCP85P, 
                    filename="./Dados/Resultados_modelagem_E_subsecundum/Projecao_RCP85/subsecundum_mapa_binario_RCP85.asc", 
                    format="ascii", overwrite=TRUE)


# Para criar o mapa final basta multiplicar o mapa binário pelo mapa médio dos
# melhores modelos
raster_final_RCP85P <- raster_medio_RCP85P * mapa_binario_RCP85P

# Verificação
raster_final_RCP85P

# Salvar o mapa final
raster::writeRaster(raster_final_RCP85P, 
                    filename="./Dados/Resultados_modelagem_E_subsecundum/Projecao_RCP85/subsecundum_mapa_final_RCP85.asc", 
                    format="ascii")


### RECLASSIFICAÇÃO DO MAPA FINAL E ESTIMATIVA DA ÁREA ADEQUADA

# Definir a área em quilômetros quadrados dos pixels (0.5 km^2)
celulaP = 0.5

# Estimativa da área adequada ***(com qualquer grau de adequabilidade)***
area_adequada_RCP85P <- as.data.frame(tapply(area(mapa_binario_RCP85P), 
                                             mapa_binario_RCP85P[], sum)*
                                          celulaP)

rownames(area_adequada_RCP85P) <- c("Não-adequada", "Adequada")
colnames(area_adequada_RCP85P) <- c("Área (Km²)")

# Verificação
area_adequada_RCP85P

# Salvar os resultados
write.csv(area_adequada_RCP85P, 
          "./Dados/Resultados_modelagem_E_subsecundum/Projecao_RCP85/area_adequada_subsecundum_RCP85")



# Estimativa da área adequada por classes de adequabilidade

# Valores de 0 à 1000
raster_final_RCP85P 

# Propôr uma divisão de classes
# 0 ao limiar médio = Classe 0 = Inadequada
# limiar médio ao 750 = Classe 1 = Média
# 750 ao 900 = Classe 2 = Alta
# 900 ao 1000 = Classe 3 = Muito alta

# 1) Criar data frame com a reclassificação a partir da divisão 
df_reclass_RCP85 <- c(0, limiar_RCP85_medio, 0,
                      limiar_RCP85_medio, 750, 1,
                      750, 900, 2,
                      900, 1000, 3)
# Verificação
df_reclass_RCP85

# 2) Converter o data frame a uma matriz
matriz_reclass_RCP85 <- matrix(df_reclass_RCP85,
                               ncol = 3,
                               byrow = TRUE)
# Verificação
matriz_reclass_RCP85


# 3) Criação do raster reclassificado:
raster_classificado_RCP85 <- raster::reclassify(raster_final_RCP85P, 
                                        matriz_reclass_RCP85)

# 4) Estimativa da área adequada por classes:
area_adequada_classes_RCP85 <- as.data.frame(tapply(area(
    raster_classificado_RCP85), raster_classificado_RCP85[], sum)*celulaP)

rownames(area_adequada_classes_RCP85) <- c("Não-adequada", "Média", "Alta", 
                                           "Muito Alta")

colnames(area_adequada_classes_RCP85) <- c("Área (Km²)")

# Verificação
area_adequada_classes_RCP85


# Salvar os resultados
write.csv(area_adequada_classes_RCP85, 
          "./Dados/Resultados_modelagem_E_subsecundum/Projecao_RCP85/subsecundum_area_adequada_RCP85_classes.csv")


# Salvar o raster reclassificado
raster::writeRaster(raster_classificado_RCP85, filename=
                "./Dados/Resultados_modelagem_E_subsecundum/Projecao_RCP45/subsecundum_mapa_final_RCP85_reclassificado.asc", 
            format="ascii")




################################ FIM ###########################################