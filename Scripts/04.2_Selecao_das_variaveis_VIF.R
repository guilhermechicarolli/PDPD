install.packages("corrplot")


corrplot::corrplot(cor(valores_planta))

psych::pca(valores_planta, nfactors = 3)$loadings

v <- usdm::vifstep(valores_planta)

x <- usdm::exclude(valores_planta, v)
cor(x)
