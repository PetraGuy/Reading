#looked at richness and adundance across am and em plots in year 2
setwd("C:/dev/code/Reading/AMEMtrees/Code")
library(dplyr)
library(stringr)
library(tidyverse)
#Visualising the data

#get data
data = read.csv('../data/year2data.csv')

#get rid of non woodland plots - anything that doesnt syart with W
woodsonly = data%>%filter(str_detect(nvc,"W"))

#replace nas with 0s in lba, allelos,amtrees,a,cover
woodsonlynona = woodsonly%>%mutate_at(vars(cover,allelos,lba,amtrees,amcover),
                      ~replace_na(.,0))

#separate am and em woods
emwoods = woodsonlynona%>%filter(ms=='em')
amwoods = woodsonlynona%>%filter(ms=='am')

#select the required cols from each to make things a bit easier
#to get longdf for ggplot
#first amcover
emcols = emwoods[c('amtrees','amcover')]
emcols['wood'] = rep('em',790)

amcols = amwoods[c('amtrees','amcover')]
amcols['wood'] = rep('am',407)

df = rbind(emcols,amcols)

ggplot(df,aes(x = wood, y = amtrees))+geom_boxplot(aes(fill = wood))

ggplot(df,aes(x = wood, y = amcover))+geom_boxplot(aes(fill = wood))

#what about richness between the two types

emcols = emwoods[c('alpha','cover')]
emcols['wood'] = rep('em',790)

amcols = amwoods[c('alpha','cover')]
amcols['wood'] = rep('am',407)
df = rbind(emcols,amcols)


ggplot(df,aes(x = wood, y = alpha))+geom_boxplot(aes(fill = wood))

ggplot(df,aes(x = wood, y = cover))+geom_boxplot(aes(fill = wood))

#what about SOM

emcols = emwoods[c('som','pH')]
emcols['wood'] = rep('em',790)

amcols = amwoods[c('som','pH')]
amcols['wood'] = rep('am',407)
df = rbind(emcols,amcols)


ggplot(df,aes(x = wood, y = som))+geom_boxplot(aes(fill = wood))

ggplot(df,aes(x = wood, y = pH))+geom_boxplot(aes(fill = wood))

#look at some correlations
source('../../utilities/corrplot.R')
df = woodsonlynona[c("alpha","cover","allelos","lba","som","pH","amtrees", "amcover")]

corrplot(df,'spearman','Spearman')

pairs(df, lower.panel = NULL)

#############

library("PerformanceAnalytics")
my_data <- df
chart.Correlation(my_data, histogram=TRUE, pch=19)

###################

#some clustering
library(factoextra)
#use woods without nvc,ms and flag
df = woodsonlynona[c("alpha","cover","allelos","lba","som","pH","amtrees", "amcover")]
df = df%>%na.omit()%>%scale()

fviz_nbclust(df, kmeans, method = "gap_stat")
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

fviz_nbclust(df, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")


set.seed(123)
km.res <- kmeans(df, 2, nstart = 25)
# Visualize
library("factoextra")
fviz_cluster(km.res, data = df,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())


###################

####################
source('../../utilities/normdf.R')
df = woodsonlynona[c("alpha","cover","allelos","lba","som","pH","amtrees", "amcover")]
normalize <- function(x) {
  norm = ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  
  return(norm)
}

normdf = normalize(df)

#combine amtrees and amcover

#nortmalised dfs for richness and abundace separately, where am inoc is sum
#of normalised amcover and amtrees
normdf['aminoc'] = normdf['amtrees'] + normdf['amcover']
normdf = normdf[-c(7,8)]
nortmdfrich = normdf[,-2]
normdfabund = normdf[,-1]

#now look at PCA, remove response var
xvars = normdf[-c(1,2)]
res.pca <- prcomp(na.omit(xvars))

fviz_eig(res.pca)

fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
var_coord_func <- function(loadings, comp.sdev){
  loadings*comp.sdev
}
# Compute Coordinates
#::::::::::::::::::::::::::::::::::::::::
loadings <- res.pca$rotation
sdev <- res.pca$sdev
var.coord <- t(apply(loadings, 1, var_coord_func, sdev)) 
head(var.coord[, 1:4])
#######

library(visreg)
fit = lm(alpha ~ lba+som+pH+aminoc,data = nortmdfrich)
par(mfrow=c(1,4))
visreg(fit)

fit = lm(cover ~ lba+som+pH+aminoc,data = normdfabund)
par(mfrow=c(1,4))
visreg(fit)

####
