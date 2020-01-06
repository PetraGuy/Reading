
setwd("C:/dev/code/Woods2019/code")

library(factoextra)
library(FactoMineR)
library(corrplot)

woods = read.csv("../data/woodprops.csv", stringsAsFactors = TRUE, header = TRUE)
#add ndep to woods

ndep  = readRDS("../data/ndepsepbysite.RDS")
woods  = cbind(woods,ndep[,4:6])
#do pca with each categorical var, get separate df

woods_soil = woods[c(5,6,10,11,12,13,16,17,18)]
#woods_sp = woods[c(3,5:12)]
#woods_grazed = woods[c(2,5:12)]

#do separate PCA
woods_soilPCA = PCA(woods_soil[-1],graph = FALSE)
woods_grazedPCA = PCA(woods_grazed[-1], graph = FALSE)
woods_spPCA = PCA(woods_sp[-1], graph  = FALSE)

#do PCA plots 

fviz_pca_biplot(woods_soilPCA,
                col.ind = woods_soil$type, 
                palette = c("red", "green", "purple", "blue", 
                            "black", "orange"),
                fill.ind = woods_soil$type,
                col.var = "black", repel = TRUE,
                legend.title = "soil type")+
                scale_shape_manual(values = c(15,16,17,18,19,25))


#  fviz_pca_biplot(woods_grazedPCA,
#                 col.ind = woods_grazed$grazed, 
#                 palette = c("red", "green"),
#                 #label = "var",
#                 col.var = "black", repel = TRUE,
#                 legend.title = "grazed")
# 
# fviz_pca_biplot(woods_spPCA,
#                 col.ind = woods_sp$species, 
#                 palette = c("red", "green", "yellow"),
#                 #label = "var",
#                 col.var = "black", repel = TRUE,
#                 legend.title = "species")

var = get_pca_var(woods_soilPCA)
corrplot(var$cos2)

# woodscontvars = cor(as.matrix(woods[,5:12]))
# corrplot(woodscontvars,type = "lower", order = "hclust", diag = FALSE)





