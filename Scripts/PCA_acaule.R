############## SCRIPTS DO PROJETO DE PDPD ##############

# 1. Seleção das variáveis mais adequadas para a modelagem
#    através da Análise de Componentes Principais (PCA) para
#    a espécie de Solanum acaule


################################################################################

##### Carregamento das bibliotecas necessárias

if (!require(dismo)) install.packages('dismo')
if (!require(rgdal)) install.packages('rgdal')
if (!require(raster)) install.packages('raster')
if (!require(sp)) install.packages('sp')
if (!require(usdm)) install.packages('usdm')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(psych)) install.packages('psych')



# Permite que dados espaciais sejam associados com o sistema de coordenadas,
# criando uma projeção que pode ser utilizada nos rasters
proj_WGS <- sp::CRS(
    "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

################################################################################

#--------- 1. SELEÇÃO DE VARIÁVEIS USANDO PCA PARA A
#                ESPÉCIE DE PLANTA MIMOSA LEWISII  ---------#

### CARREGAMENTO DOS DADOS

# Carregamento dos pontos 
pontos_planta <- read.csv('./Dados/TESTE_MODELAGEM/occ_treino_acaule.csv')

pontos_planta$species <- NULL

# Dropar NAs
pontos_planta %>% drop_na()

# Dropas duplicados
pontos_planta <- unique( pontos_planta[,1:2] )



# Converção dos pontos geográficos para SpatialPoints
sp::coordinates(pontos_planta) <- ~x+y

# Adicionar a projeção
raster::crs(pontos_planta) <- proj_WGS

# Verificação dos dados
pontos_planta


# Carregamento das camadas ambientais raster cortadas no script 02 
camadas <- list.files(path='./Dados/Camadas_res_2.5_2050_cortadas/Presente_teste_acaule/', pattern='.asc', 
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



### ANÁLISE DE COMPONENTES PRINCIPAIS (PCA)    # TESTE 1

valores_planta<-na.omit(valores_planta)


pcaP <- psych::pca(valores_planta, nfactors = 3)
pcaP


pcaP$loadings


###   TESTE 2

# PCA com os valores extraidos:
variaveis_PCA_planta <- stats::prcomp(valores_planta, center=TRUE, scale=TRUE, rank.=3)                      

# Verificação
summary(variaveis_PCA_planta)  # 4 componentes explicam 93.9% da variância total dos dados


# Seleção do número de PCs a partir do Broken Stick Model:
stats::screeplot(variaveis_PCA_planta, bstick=TRUE, type="lines")
stats::screeplot(variaveis_PCA_planta, bstick=TRUE, type="barplot")

# Verificação 
summary(variaveis_PCA_planta)

# Definição do número de PCs e eixos principais
pc = 3

PCA_rotacaoP <- abs(variaveis_PCA_planta$rotation)
PCA_rotacaoP


lista <- list()
for (i in 1:pc) {posicao = PCA_rotacaoP[PCA_rotacaoP[, i] > 0.32, ]
lista[[i]]<-row.names(posicao)}

lista

# Variáveis selecionadas
variaveis_selecP <- unlist(lista)

variaveis_selecP <- unique(variaveis_selecP)


# Verificação das variáveis selecionadas
variaveis_selecP


# Salvar os resultados da análise em um arquivo csv
write.csv(as.data.frame(variaveis_selecP), 
          file = "./Dados/Resultados_PCA/PCA_variaveis_planta.csv")


# Selecionar as variáves com a maior contribução em cada PC selecionado
PC1_var <- names(which.max(abs(variaveis_PCA_planta$rotation[,1])))

# Verificação
PC1_var

PC2_var <- names(which.max(abs(variaveis_PCA_planta$rotation[,2])))

# Verificação
PC2_var

PC3_var <- names(which.max(abs(variaveis_PCA_planta$rotation[,3])))

# Verificação
PC3_var


#salvar os resultados:
as.data.frame(rbind(PC1_var, PC2_var, PC3_var))

write.csv(as.data.frame(rbind(PC1_var, PC2_var, PC3_var)), 
          file = "./Dados/Resultados_PCA/Variaveis_max_contribuicao_planta.csv")



################################################################################