setwd("C:/dev/code/Reading/AMEMtrees/Code")
library(dplyr)
library(stringr)
library(tidyverse)
library(gridExtra)
library(GGally)
library(tidyr)
library(MuMIn)
library(reshape)

alldata = read.csv('../data/year2dataA.csv')

#get rid of non woodland plots - anything that doesnt syart with W
woodsonly = alldata%>%filter(str_detect(nvc,"W"))

am = c('W6','W7','W8','W9','W12','W13')
em = c('W3','W4','W10','W11','W14','W15','W16','W17')

amplots = filter(woodsonly, nvc %in% am)
emplots = filter(woodsonly, nvc %in% em)

# format the data - first col for y, other cols for x vars
data = amplots[c(3,4,5,6,7)]

#######################################
# this is to normalze the data
normalize <- function(x) {
  norm = ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  
  return(norm)
}
####################################
 # used in the ggplot of the model avg
fun_mean <- function(x){
  data = data.frame(y=mean(x),label=mean(x,na.rm=T))
  data = round(data,2)
  return(data)}

###############

#create the lm on which the model avg is done
linearmod = function(data,n){
  data = na.omit(data)
  data = as.data.frame(apply(data,2, FUN =normalize)) # normalize
  y = colnames(data)[1]
  x = colnames(data[-1])
  xvect = ""
  i=1
  while (i<n){
    xvect = paste(xvect,x[[i]],"+")
    i = i+1
  }
  xvect  = paste(xvect,x[[n]])
  
  mod = lm(reformulate(xvect, y), data = data,na.action = 'na.fail')
  return(mod)
 }
#############################################

#this does the model average, data  is df first col = y others are x
#n is number of x vars

modelavg = function(data,n){
  model_avg  = model.avg(get.models(dredge(linearmod(data,n)),subset = delta < 2),subset)
  summary = summary(model_avg)
  coefs =  model_avg$coefficients
  importance =  as.vector(importance(model_avg))
  coef_matrix = summary$coefmat.full
  coefs = coef_matrix[,1][2:(n+1)]
  adj_se = coef_matrix[,3][2:(n+1)]
  CI_lower =  coefs - 1.96*adj_se
  CI_upper = coefs + 1.96*adj_se
  output = round(as.data.frame(cbind(coefs,adj_se, CI_lower, CI_upper, importance)),2) 
  melted = melt(as.data.frame(t(output[,c(1:4)])))
  melted$variable = as.factor(melted$variable)
  
  ggplot(melted,aes_string(x = 'variable', y ='value' ) )+
    geom_boxplot(na.rm = TRUE, width = 0)+
    stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
    stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7)+
    stat_summary(geom = "text", label = output$importance,fun.y = max, hjust = 1, colour = "red")+
    geom_abline(intercept = 0, slope = 0)+
    labs(y = "Parameter estimate and 95%CI",x = "Parameter")+ 
    labs(title = "Model averaged results for delta <2",
         subtitle = "Numbers in red are importance")
}

