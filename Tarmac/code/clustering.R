# look at different cluststering techniques and see how it divides the siets up
#using euclidean distance
setwd("C:/dev/code/Reading/Tarmac/code")
library(dendextend)
library(tidyverse)
soils = read.csv("../data/SoilsOrLayer.csv", stringsAsFactors = TRUE, header = TRUE)

#change layout for dend so i can get lables
#remove unwated cols and turn sampelsnames into rownames
soil_reduced = soils[-c(2,3,4,6,9,10)]
soilsdata = data.frame(soil_reduced, row.names = 1)

#distance matrix
dist = dist(scale(soilsdata)) #euclidean distance on scaled matrix

#do the clustering###########

##k = 5, complete
hclustsoils = hclust(dist, method = 'ward.D')
labels = hclustsoils[4]$soils

#get the clusters if take k = 
#clusters_k2 <- cutree(hclustsoils, k=3)  

# Create a new data frame storing these results
#soils_k3_complete <- mutate(soils, cluster = clusters_k3)

#how many samples in each cluster
#count(soils_k2_complete, cluster)

#plot the dendogram
dend = as.dendrogram(hclustsoils)

dend %>% set("labels_col", c("red","orange","black","blue","purple","brown"), k =4) %>%
  set("labels_cex", 0.6) %>%  
  plot(main = "Hierachical clustering, eucludian distance,method= ward.D, k = 4")
