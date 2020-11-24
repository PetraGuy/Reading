

#trying out  various cluster/PCA for soils data

setwd("C:/dev/code/Reading/Tarmac/code")


library(factoextra)
library(purrr)
library(ggplot2)
library(cluster)


soils = read.csv("../data/SoilsOrLayer.csv", stringsAsFactors = TRUE, header = TRUE)

#tidy the data, MSs soil wts taken as mean of the 3 MS values
#BDs soil weights as mean of the 5 BD values
# 5 of MSs deleted - need to make this a random selection of 5
# all of BDs deleted because have BD
# other missing data mean interpolated
#PS - did this by hand on ALlSoilsData csv

#scale the data



# k means ######################################################
#find sum of sqs for diffferent k values to determine num of clusters
scaled =scale(soils[-c(1:4)]) #some functions do this anyway

tot_withinss = map_dbl(1:10, function(k){
   model = kmeans(scaled, centers = k)
   model$tot.withinss
})

elbow = data.frame(K=1:10, tot_withinss = tot_withinss) 

#plot elbow plot to see how many clusters
ggplot(elbow, aes(x = K, y = tot_withinss)) +
   geom_line() +
   scale_x_continuous(breaks = 1:10)

#silhouette widths
sil_width = map_dbl(2:10, function(k){
   model = pam(scaled, k = k)
   model$silinfo$avg.width
})

sil = data.frame(k = 2:10, sil_width = sil_width)

ggplot(sil, aes(x = k, y = sil_width))+
   geom_line() + 
   scale_x_continuous(breaks = 2:10)
   
#looks like there are 2 clusters'
#elbow plots shows leveling off after 2
#silhouete width most at 2

# Extract the vector of cluster assignments from the model
model <- kmeans(scaled, centers = 2)
clust_soils <- model$cluster

# Build the soils data frame with these clusters aded
soil_cluster <- mutate(soils, cluster = clust_soils)

# Calculate the size of each cluster
count(soil_cluster, cluster)

###plot clusters
fviz_cluster(model, data =scaled,
             palette = c("#2E9FDF", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

############################################



 #######################################  
#compute gower distance and a pam cluster plot
gower_dist = daisy(soils[,-c(1:4)],
                   type = list(nominal = c(1,2,3,4)),
                   metric = "gower",
                   stand = TRUE)

gower_mat = as.matrix(gower_dist) 

###plot clusters, pam clusters - partition around medoids
soils.clust =eclust(gower_mat,"pam",k = 2,stand = TRUE)


##############################################
#look at most similar and dissimalr soils, not sure why
#add rownames so plat has labels
rownames(gower_mat) = soils$site

# most similar pair
soils[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
              arr.ind = TRUE)[1, ], ]

#most dissimilar
soils[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

###################################


#PCA


library(FactoMineR)
library(corrplot)

# Dimension reduction using PCA
soilspca <- prcomp(soils[-c(1:4)],  scale = TRUE)
# Coordinates of individuals
ind.coord <- as.data.frame(get_pca_ind(soilspca)$coord)
# Add clusters obtained using the K-means algorithm
ind.coord$cluster <- factor(model$cluster)
# Add Species groups from the original data sett
ind.coord$site <- soils$site
eigenvalue <- round(get_eigenvalue(soilspca), 1)
variance.percent <- eigenvalue$variance.percent

ggscatter(
   ind.coord, x = "Dim.1", y = "Dim.2", 
   color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
   shape = "site", size = 1.5,  legend = "right", ggtheme = theme_bw(),
   xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
   ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
) +
   stat_mean(aes(color = cluster), size = 4)  

######################### Another PCA this way


soilPCA = PCA(soils[-c(1,2,3,4,6,9,10)],graph = FALSE)

#do PCA plots 
#this is the traditional PCA plot
fviz_pca_biplot(soilPCA,
                col.ind = soils$site, 
                fill.ind = soils$site,
                col.var = "black", repel = TRUE,
                legend.title = "site")

# this plots is a mess
fviz_pca_biplot(soilPCA,label = "var", habillage = soils$site,
                addEllipses = TRUE, ellipse.level = 0.95) 
######################
#corrplot of domensions
var = get_pca_var(soilPCA)
corrplot(var$cos2)


########################################

#hierarchical clustering
#distance matrix,this is a gower, not sure if need this without categorical vars
gower_dist = daisy(soils[,-c(1:4)],
                   type = list(nominal = c(1,2,3,4)),
                   metric = "gower",
                   stand = TRUE)

gower_mat = as.matrix(gower_dist) 

#or euclidean dist
dist = dist(scale(soils[-c(1:4)])) #euclidean distance on scaled matrix

#do the clustering
hclustsoils = hclust(dist, method = 'complete')

#look at dendogram
dend_soils = as.dendrogram(hclustsoils)
dend_col = dendextend::color_branches(dend_soils, k = 2) 
plot(dend_col, xlab = site)

#get the clusters if take k = 
clusters_k2 <- cutree(hclustsoils, k=2)  

# Create a new data frame storing these results
soils_k2_complete <- mutate(soils, cluster = clusters_k2)

#how many samples in each cluster
count(soils_k2_complete, cluster)
