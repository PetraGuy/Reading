setwd("C:/dev/code/Reading/AMEMtrees/Code")
library(dplyr)
library(stringr)
library(tidyverse)
library(gridExtra)
library(GGally)


corrplot = function(data,method,title){
  corr = corr_sp <- cor(data, method = method, use = 'pairwise.complete.obs')
  plot = ggcorrplot(corr, hc.order = TRUE, 
                    type = "lower", 
                    lab = TRUE, 
                    lab_size = 2, 
                    method="circle", 
                    colors = c("tomato2", "white", "springgreen3"), 
                    title=title, 
                    ggtheme=theme_bw)
  return(plot)
}
normalize <- function(x) {
  norm = ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  
  return(norm)
}

#get data # year 2 data with dual do not contribute, ctataegus = em ilex = am
data = read.csv('../data/year2dataA.csv')

#get rid of non woodland plots - anything that doesnt syart with W
woodsonly = data%>%filter(str_detect(nvc,"W"))


# Look at pH over am and em plots

am = c('W6','W7','W8','W9','W12','W13')
em = c('W3','W4','W10','W11','W14','W15','W16','W17')

amplots = filter(woodsonly, nvc %in% am)
emplots = filter(woodsonly, nvc %in% em)

#remove nvc codes 
df1 = amplots[-c(1,2,12,13,14)]

#normalize the whole thing and add inoc columns
df1norm = as.data.frame(apply(df1,2, FUN =normalize))
df1norm = df1norm%>%mutate(aminoc = amtrees+amcover,
                  allelo = shrub_allelo+tree_allelo)  
df1norm = df1norm[-c(3,4,8,9)]

g1 = ggpairs(df1norm, columns = c('lba','som','pH','aminoc','allelo'))+
  ggtitle('am plots')
####### em
df1 = emplots[-c(1,2,12,13,14)]
df1norm = as.data.frame(apply(df1,2, FUN =normalize))
df1norm = df1norm%>%mutate(aminoc = amtrees+amcover,
                           allelo = shrub_allelo+tree_allelo)  
df1norm = df1norm[-c(3,4,8,9)]

g2 = ggcorr(df1norm, label = TRUE)

grid.arrange(g1,g2)

g2 = ggpairs(df1norm, columns = c('lba','som','pH','aminoc','allelo'))+
  ggtitle('em plots')
  
grid.arrange(g1,g2)

