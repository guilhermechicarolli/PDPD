if (!require(dismo)) install.packages('dismo')
if (!require(biomod2)) install.packages('biomod2')
if (!require(raster)) install.packages('raster')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(rnaturalearth)) install.packages('rnaturalearth')

data(acaule)

summary(acaule)

head(acaule)

View(acaule)

sum(is.na(acaule$lat))
sum(is.na(acaule$lon))

df <- acaule %>% drop_na(lat, lon)

View(df)
sum(is.na(df$lon))

unique(df$species)

sum((df$lat)==0)
sum((df$lon)==0)

df[df$lon == 0]
df[,1:7][df$lon == 0]


filter(df, lon==0)

df <- df %>% filter(!lon == 0)

summary(df)

df <- df %>% filter(lon < -50)


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

g1 <- ggplot(data = world) +
    geom_sf(colour = "white", fill = "#d3d3d3") +
    coord_sf(xlim = c(-121, -30), ylim = c(-50,5), expand = FALSE) +
    theme_bw() + 
    # Plot the sites
    geom_point(data = df, aes(x = lon, y = lat, 
                              colour = species))
g1
        
sum(df$species == 'Solanum acaule Bitter')

lonlat <- select(df, species, lon, lat)

lonlat <- lonlat %>% rename(y=lat) %>%
    rename(x=lon)



smp_size <- floor(0.75 * nrow(lonlat))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(lonlat)), size = smp_size)

train <- lonlat[train_ind, ]
test <- lonlat[-train_ind, ]

write.csv(test,'./Dados/TESTE_MODELAGEM/occ_teste_acaule.csv', row.names = FALSE)
write.csv(train,'./Dados/TESTE_MODELAGEM/occ_treino_acaule.csv', row.names=FALSE)



################################################################################

# CORTE DAS CAMADAS DO PRESENTE

mascara <- raster::shapefile('Dados/Mascaras/South_America.shx')
raster::crs(mascara) <- proj_WGS

# Verificar dados
mascara
plot(mascara)

# Carregamento de uma camada representante, escolhida a camada 'bio1'
camada_rep <- raster::raster(
    './Dados/Camadas_brutas_res_2.5/Presente_res_2.5/wc2.1_2.5m_bio_1.tif')

# Adicionar a projeção
raster::crs(camada_rep) <- proj_WGS

# Verificação
plot(camada_rep)


# Carregamento de todas as variáveis ambientais raster 
camadas <- list.files(path='./Dados/Camadas_brutas_res_2.5/Presente_res_2.5/', pattern='.tif', 
                      full.names = TRUE)

camadas <- raster::stack(camadas)

# Adicionar a projeção às camadas
crs(camadas) <- proj_WGS

# Verificar dados
camadas


# Reduzir o tamanho da camada representante para um retângulo, que será depois
# cortado a partir da máscara
corte_cam <- raster::crop(camada_rep, extent(mascara))

# Verificação
plot(corte_cam)


# Reduzir o tamanho de todas as camadas ambientais do presente
cortes_final <- raster::resample(camadas, corte_cam, method="bilinear", 
                                 snap='out', bylayer=TRUE, progress='text')

# Verificação
cortes_final
plot(cortes_final)

# Cortar as camadas ambientais e cortar a partir da máscara
camadas_final <- raster::mask(cortes_final, mascara, bylayer=TRUE)

# Verificação
plot(camadas_final)


# Salvar as camadas na pasta "Camadas_presente" no formato ".asc"
raster::writeRaster(camadas_final, paste0("Dados/Camadas_res_2.5_cortadas/Presente_teste_acaule/", 
                                          paste0(names(camadas_final),".asc")), 
                    driver='ascii', bylayer=TRUE)


####### CORTE DAS CAMADAS RCP45

# Carregamento de uma camada representante, escolhida a camada 'bio1'
camada_rep <- raster::raster(
    './Dados/Camadas_brutas_res_2.5/RCP45_res_2.5/ac45bi701.tif')

# Adicionar a projeção
raster::crs(camada_rep) <- proj_WGS

# Verificação
plot(camada_rep)


# Carregamento de todas as variáveis ambientais raster 
camadas <- list.files(path='./Dados/Camadas_brutas_res_2.5/RCP45_res_2.5/', pattern='.tif', 
                      full.names = TRUE)

camadas <- raster::stack(camadas)

# Adicionar a projeção às camadas
crs(camadas) <- proj_WGS

# Verificar dados
camadas


# Reduzir o tamanho da camada representante para um retângulo, que será depois
# cortado a partir da máscara
corte_cam <- raster::crop(camada_rep, extent(mascara))

# Verificação
plot(corte_cam)


# Reduzir o tamanho de todas as camadas ambientais do presente
cortes_final <- raster::resample(camadas, corte_cam, method="bilinear", 
                                 snap='out', bylayer=TRUE, progress='text')

# Verificação
cortes_final
plot(cortes_final)

# Cortar as camadas ambientais e cortar a partir da máscara
camadas_final <- raster::mask(cortes_final, mascara, bylayer=TRUE)

# Verificação
plot(camadas_final)


# Salvar as camadas na pasta "Camadas_presente" no formato ".asc"
raster::writeRaster(camadas_final, paste0("Dados/Camadas_res_2.5_cortadas/RCP45_teste_acaule/", 
                                          paste0(names(camadas_final),".asc")), 
                    driver='ascii', bylayer=TRUE)



################################################################################


camadasP <- list.files(path='./Dados/Camadas_res_2.5_cortadas/teste_acaule_pres_selec/',
                       pattern = '.asc', full.names = TRUE)

camadasP <- raster::stack(camadasP)

# Adicionar a projeção geográfica
raster::crs(camadasP) <- proj_WGS

# Verificação dos dados
camadasP

mascaraP <- raster::raster(camadasP[1])

mascaraP


lonlat

sp::coordinates(lonlat) <- ~x+y

raster::crs(lonlat) <- proj_WGS

# Verificação dos dados
lonlat


# Formatação dos dados para usar no biomod2
nomeP <- "modelo_acaule_teste"

explP <- camadasP

xyP <- lonlat@coords

respP <- rep(1, length(lonlat))


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

head(pontos_backgP@data.env.var)


opcoes_maxentP <- biomod2::BIOMOD_ModelingOptions(
    MAXENT.Phillips = list(
        path_to_maxent.jar = "~/R/win-library/4.0/dismo/java",
        memory_allocated = NULL, 
        maximumiterations = 500,    # quantidade de interações
        # Features:
        linear = TRUE, 
        quadratic = TRUE, 
        product = TRUE,            
        threshold = TRUE,          
        hinge = TRUE,              
        lq2lqptthreshold = 80,
        l2lqthreshold = 10,
        hingethreshold = 15,
        beta_threshold = -1,
        beta_categorical = -1,
        beta_lqp = -1,
        beta_hinge = -1,
        betamultiplier = 0.5,        # valor de beta/regularização
        defaultprevalence =.5))

modelo_maxentP <- biomod2::BIOMOD_Modeling(pontos_backgP,
                                           models=c("MAXENT.Phillips"), 
                                           models.options = opcoes_maxentP, 
                                           NbRunEval = 3,	 # quantidade de replicações
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
          "./Dados/TESTE_MODELAGEM_2/acaule_TSS_tabela.csv")



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
          "./Dados/TESTE_MODELAGEM_2/acaule_AUC_tabela.csv")

AUC_resultadosP[which(AUC_resultadosP[,1] > 0.75), ]
TSS_resultadosP[which(AUC_resultadosP[,1] > 0.75), ]

posicao_modelosP <- which(AUC_resultadosP[,1] > 0.75)

# Seleção do nome dos melhores modelos e do melhor modelo:

# Verificação dos nomes
modelo_maxentP@models.computed

# Nomes dos melhores modelos
melhores_modelosP <- modelo_maxentP@models.computed[posicao_modelosP]

# Nome do melhor modelo
melhor_modeloP <- modelo_maxentP@models.computed[3]

melhor_modeloP  # "subsecundum_PA1_RUN5_MAXENT.Phillips"


### CÁLCULO DA IMPORTÂNCIA DAS VARIÁVEIS

# Considerando todos os modelos
importancia_varsP <- t(as.data.frame(biomod2::get_variables_importance(
    modelo_maxentP)))                                                           ##### TESTAR ESSA PARTE

# Verificação
importancia_varsP

# Salvar os resultados
write.csv(importancia_varsP, 
          './Dados/TESTE_MODELAGEM_2/acaule_importancia_variaveis.csv')


# Considerar apenas os melhores modelos
melhores_impP <- importancia_varsP[posicao_modelosP, ]

media_impP <- c()
for (i in 1:4){
    media_impP <- append(media_impP, mean(melhores_impP[,1]))
}

# Adicionar uma última linha com as médias de importância
melhores_mediasP <- rbind(melhores_impP, media_impP)

# Verificação
tail(melhores_mediasP)

# Salvar os resul
write.csv(melhores_mediasP, 
          "./Dados/TESTE_MODELAGEM_2/acaule_importancia_vars_melhores_modelos.csv")


### GRÁFICOS DA IMPORTÂNCIA DAS VARIÁVEIS

# Gráfico da importância das variáveis para todos os modelos
ggplot2::ggplot(gather(as.data.frame(importancia_varsP)),
                aes(x = reorder(key, value, fun = median,), y = value)) + 
    
    geom_boxplot() + 
    
    scale_x_discrete(name="Variáveis")+
    
    scale_y_continuous(name="Importância (%)")


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
    name = "./Dados/TESTE_MODELAGEM_2/Curva_resposta_acaule_melhores_modelos", # Nome do modelo
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
    name = "./Dados/TESTE_MODELAGEM_2/Curva_resposta_acaule_melhores_modelos", # Nome do modelo
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
    filename="./Dados/TESTE_MODELAGEM_2/Projecao_presente/acaule_modelo_medio_presente.asc", 
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
                    filename="./Dados/TESTE_MODELAGEM_2/Projecao_presente/acaule_mapa_binario_presente.asc", 
                    format="ascii", overwrite=TRUE)


# Para criar o mapa final basta multiplicar o mapa binário pelo mapa médio dos
# melhores modelos
raster_final_presenteP <- raster_medio_presenteP * mapa_binario_presenteP

# Verificação
raster_final_presenteP

# Salvar o mapa final
raster::writeRaster(raster_final_presenteP, 
                    filename="./Dados/TESTE_MODELAGEM_2/Projecao_presente/acaule_mapa_final_presente.asc", 
                    format="ascii")



# Definir a área em quilômetros quadrados dos pixels (0.5 km^2)
celulaP <- 4.5

# Estimativa da área adequada ***(com qualquer grau de adequabilidade)***
area_adequada_presenteP <- as.data.frame(tapply(area(mapa_binario_presenteP), 
                                                mapa_binario_presenteP[], sum)*
                                             celulaP)

rownames(area_adequada_presenteP) <- c("Não-adequada", "Adequada")
colnames(area_adequada_presenteP) <- c("Área (Km²)")

# Verificação
area_adequada_presenteP  # Adequada = 6377959 Km²
# Não adequada = 73217145 Km²
# Salvar os resultados
write.csv(area_adequada_presenteP, 
          "./Dados/TESTE_MODELAGEM_2/Projecao_presente/area_adequada_subsecundum_presente")



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
area_adequada_classes_presente   # Não adequada = 73217144.5 Km²
# Média = 5642918.4 Km²
# Alta =  267412.4 Km²
# Muito Alta = 467628.5 Km²

# Salvar os resultados
write.csv(area_adequada_classes_presente, 
          "./Dados/TESTE_MODELAGEM_2/Projecao_presente/subsecundum_area_adequada_presente_classes.csv")


# Salvar o raster reclassificado
writeRaster(raster_classificado_presente, filename=
                "./Dados/TESTE_MODELAGEM_2/Projecao_presente/subsecundum_mapa_final_presente_reclassificado.asc", 
            format="ascii")

################################################################################


#--------- 4. PROJEÇÃO DOS MODELOS GERADOS PARA O
#               CENÁRIO FUTURO DE 2070 RCP 4.5  --------#

# Projeção do modelo criado para o cenário futuro de 2070, RCP 4.5, com 
# resolução de 0.5 arcsegundos

# Carregamento das camadas de RCP 4.5 selecionadas para a planta
camadas45P <- list.files(path='./Dados/Camadas_res_2.5_cortadas/teste_acaule_45_selec/',
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

plot(rasters_RCP45P)


# Fazer um modelo médio de todas as projeções criadas
raster_medio_RCP45P <- raster::calc(rasters_RCP45P, fun=mean)

# Salvar o modelo médio
raster::writeRaster(
    raster_medio_RCP45P,
    filename="./Dados/TESTE_MODELAGEM/Projecao_RCP45/acaule_modelo_medio_RCP45.asc", 
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
                    filename="./Dados/TESTE_MODELAGEM/Projecao_RCP45/subsecundum_mapa_binario_RCP45.asc", 
                    format="ascii", overwrite=TRUE)


# Para criar o mapa final basta multiplicar o mapa binário pelo mapa médio dos
# melhores modelos
raster_final_RCP45P <- raster_medio_RCP45P * mapa_binario_RCP45P

# Verificação
raster_final_RCP45P

# Salvar o mapa final
raster::writeRaster(raster_final_RCP45P, 
                    filename="./Dados/TESTE_MODELAGEM/Projecao_RCP45/subsecundum_mapa_final_RCP45.asc", 
                    format="ascii")


### RECLASSIFICAÇÃO DO MAPA FINAL E ESTIMATIVA DA ÁREA ADEQUADA

# Definir a área em quilômetros quadrados dos pixels (0.5 km^2)
celulaP = 4.5

# Estimativa da área adequada ***(com qualquer grau de adequabilidade)***
area_adequada_RCP45P <- as.data.frame(tapply(area(mapa_binario_RCP45P), 
                                             mapa_binario_RCP45P[], sum)*
                                          celulaP)

rownames(area_adequada_RCP45P) <- c("Não-adequada", "Adequada")
colnames(area_adequada_RCP45P) <- c("Área (Km²)")

# Verificação
area_adequada_RCP45P 

# Adequada = 348756.8 Km²
# Não adequada = 79247028.3 Km²


# Salvar os resultados
write.csv(area_adequada_RCP45P, 
          "./Dados/TESTE_MODELAGEM/Projecao_RCP45/area_adequada_subsecundum_RCP45")



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

rownames(area_adequada_classes_RCP45) <- c("Não-adequada", "Média", "Alta")

colnames(area_adequada_classes_RCP45) <- c("Área (Km²)")

# Verificação
area_adequada_classes_RCP45

# Não adequada = 79247028.342 Km²
# Média = 4055117.8 Km²
# Alta =  347258.243 Km²
# Muito Alta = 1498.543 Km²



# Salvar os resultados
write.csv(area_adequada_classes_RCP45, 
          "./Dados/TESTE_MODELAGEM/Projecao_RCP45/acaule_area_adequada_RCP45_classes.csv")


# Salvar o raster reclassificado
raster::writeRaster(raster_classificado_RCP45, filename=
                        "./Dados/TESTE_MODELAGEM/Projecao_RCP45/acaule_mapa_final_RCP45_reclassificado.asc", 
                    format="ascii")


################################################################################


