
library(ggplot2)
library(dplyr)
library(stringr)
library(gridExtra)
library(corrplot)
library(cluster)
library(factoextra)
library(FactoMineR)

data = read.csv("AllPlotsVarsRichness.csv")
datacomplete = read.csv("CompleteSiteLevelVars.csv")
dataplot = data = read.csv("PlotVars.csv")
aw = read.csv("Site_designations.csv")

ggplot(data = data, aes(x = ShortNVC, y = SOMYr2))+
  geom_boxplot()


OV = data%>%filter(str_detect(ShortNVC,"^OV"))
MG = data%>%filter(str_detect(ShortNVC,"^MG"))
H = data%>%filter(str_detect(ShortNVC,"^H"))
W = data%>%filter(str_detect(ShortNVC,"^W"))
U = data%>%filter(str_detect(ShortNVC,"^U"))
#########################################
#SOM in all open versus woodland codes
g1= ggplot(data = W, aes(x = ShortNVC, y = SOMYr2))+
  geom_boxplot()
g2 = ggplot(data = OV, aes(x = ShortNVC, y = SOMYr2))+
  geom_boxplot()

grid.arrange(g1,g2)

##################################
# take range across all W and all OV
WSOM = W%>%select((SOMYr2))
OVSOM = OV%>%select((SOMYr2))


g3 = ggplot(data = WSOM, aes(x = "", y = SOMYr2))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0,100))+
  ggtitle("W codes SOM")

g4 = ggplot(data = OVSOM, aes(x = "", y = SOMYr2))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0,100))+
  ggtitle("Open NVC codes SOM")+
  ylab("Soil Organic Matter")

grid.arrange(g3,g4, ncol =2)
##############################
# some woodlands are scrub - remove
WHi = W%>%filter(SOMYr2>20)
WHicodes = WHi%>%group_by(ShortNVC)

g5 = ggplot(data = WHicodes, aes(x = ShortNVC, y = SOMYr2))+
  geom_boxplot()

grid.arrange(g1,g5)
##############################
# non scrub woodlands and open codes
WHiSOM = WHi%>%select(SOMYr2)

g6 = ggplot(data = WHiSOM, aes(x = "", y = SOMYr2))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0,100))+
  ggtitle("Woodlands NVC codes SOM")+
  ylab("Soil Organic Matter")

grid.arrange(g6,g4, ncol = 2)



##############################
# look at pH in W verus open
OVpH = OV%>%select((pHYr2))
WHipH = WHi%>%select(pHYr2)

g7 = ggplot(data = OVpH, aes(x = "", y = pHYr2))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0,10))+
  ggtitle("Open NVC codes pH")+
  ylab("soil pH")



g8 = ggplot(data = WHipH, aes(x = "", y = pHYr2))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0,10))+
  ggtitle("Woodland NVC codes pH")+
  ylab("soil pH")

grid.arrange(g8,g7, ncol = 2)
###############################
# look for coreelations with dbh etc
subset = data%>%select(pHYr2,SOMYr2,tree.density,LiveBasalAreaYr2)

cor(subset)

mcor = round(cor(subset, method = "spearman", use = "na.or.complete"),2)

corrplot(mcor, type = "upper", tl.pos = "td",
         method = "number", tl.cex = 0.5, tl.col = 'black',tl.srt=45,
         order = "hclust", diag = FALSE,
         title = "pearson correlation",
         mar=c(0,0,1,0))

data$NVCFactor = as.numeric((data$ShortNVC))
subset = data%>%select(pHYr2,SOMYr2,tree.density,LiveBasalAreaYr2,NVCFactor,MSG,mean_dbh)
scaled = scale(subset(na.omit(subset))

mcor = round(cor(scaled, method = "spearman", use = "na.or.complete"),2)
corrplot(mcor, type = "upper", tl.pos = "td",
         method = "number", tl.cex = 0.5, tl.col = 'black',tl.srt=45,
        order = "hclust", diag = FALSE,
         title = "pearson correlation",
         mar=c(0,0,1,0))           


wss <- (nrow(scaled)-1)*sum(apply(scaled,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(scaled, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

fit <- kmeans(scaled, 3) # 5 cluster solution
# get cluster means 
aggregate(scaled,by=list(fit$cluster),FUN=mean)
# append cluster assignment 
scaleddf <- data.frame(scaled, fit$cluster)

clusplot(scaleddf, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

################

scalePCA = PCA(scaleddf[,-8], scale.unit = TRUE, ncp = 11, graph = FALSE)
var = get_pca_var(scalePCA)


eig.val_physical = get_eigenvalue(scalePCA)
eig.val_physical
fviz_eig(scalePCA, addlabels = TRUE, ylim = c(0,30))
fviz_pca_var(scalePCA, col.var = "cos2", repel = TRUE)
fviz_cos2(scalePCA, choice = "var")
```


fviz_contrib(scalePCA, choice = "var", axes = 1, top = 10)

fviz_contrib(scalePCA, choice = "var", axes = 2, top = 10)
#######################