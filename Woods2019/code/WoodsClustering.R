
setwd("C:/dev/code/Woods2019/code")
#clustering for woodland properties, also did PCA

library(cluster) #for gower distance
library(dplyr)
library(Rtsne)
library(ggplot2)
library(factoextra)
library(Gifi)
library(ggfortify)
library(FactoMineR)

woods = read.csv("../data/woodprops.csv", stringsAsFactors = TRUE, header = TRUE)


#compute gower distance
gower_dist = daisy(woods[,-c(1:5)],
                   type = list(nominal = c(1,2,3,4)),
                   metric = "gower",
                   stand = TRUE)

gower_mat = as.matrix(gower_dist) 

#add rownames so plat has labels
rownames(gower_mat) = woods$site

# most similar pair
woods[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
              arr.ind = TRUE)[1, ], ]

#most dissimilar
woods[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

#choose the number of clusters
sil_width = c(NA)
 for (i in 2:19) {
   pam_fit = pam(gower_dist,
                 diss = TRUE,
                 k=i)
   sil_width[i] = pam_fit$silinfo$avg.width
 }
plot(2:19, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(2:19, sil_width)
axis(side = 1,at = c(2:19))

#choose  clusters and look at detail of each cluster using a pam method
# pam_fit = pam(gower_dist,diss = TRUE,k=13)
# 
# pam_results <- woods[-1]%>%
#   mutate(cluster = pam_fit$clustering) %>%
#   group_by(cluster) %>%
#   do(the_summary = summary(.))
# 
# pam_results$the_summary



#cluster using any other method and plot results
woods.clust =eclust(gower_mat,"pam",k = 13,stand = TRUE)



#plot the groups
#fviz_cluster(woods.clust, repel = TRUE,geom = "text")
fviz_silhouette(woods.clust)

#do a categorical PCA ising Gifi
ordvec = c(TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE)
woods.catpca = princals(woods[-1],ndim = 2,ordinal = ordvec)
plot(woods.catpca)
      