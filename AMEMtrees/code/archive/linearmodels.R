
#looking at linear models and model averaging. 

setwd("C:/dev/code/Reading/AMEMtrees/Code")
library(dplyr)
library(stringr)
library(tidyverse)
library(gridExtra)
library(GGally)
library(tidyr)
library(MuMIn)

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
dfam = amplots[-c(1,2,12,13,14)]
dfamnorm = as.data.frame(apply(dfam,2, FUN =normalize))
dfamnorm = dfamnorm%>%mutate(aminoc = amtrees+amcover,
                           allelo = shrub_allelo+tree_allelo)  
dfamnorm = dfamnorm[-c(3,4,8,9)]

dfem = emplots[-c(1,2,12,13,14)]
dfemnorm = as.data.frame(apply(dfem,2, FUN =normalize))
dfemnorm = dfemnorm%>%mutate(aminoc = amtrees+amcover,
                             allelo = shrub_allelo+tree_allelo)  
dfemnorm = dfemnorm[-c(3,4,8,9)]

################
#create the linear models for am and em plots
data = na.omit(dfamnorm)
lmamalpha = lm(alpha ~ lba + som + pH + aminoc + allelo, data =data, na.action = "na.fail")
lmamabund = lm(cover ~ lba + som + pH + aminoc + allelo, data = data, na.action = 'na.fail')
data = na.omit(dfemnorm)
lmemalpha = lm(alpha ~ lba + som + pH + aminoc + allelo, data = data, na.action = 'na.fail')
lmemabund = lm(cover ~ lba + som + pH + aminoc + allelo, data = data, na.action = 'na.fail')

#pick out the r2 
r2amalpha = round(summary(lmamalpha)$r.squared,2)
r2amabund = round(summary(lmamabund)$r.squared,2)
r2emalpha = round(summary(lmemalpha)$r.squared,2)
r2emabund = round(summary(lmemabund)$r.squared,2)
###########################

# drop insignificant variables

lmamalpha2 = lm(alpha ~ som + pH + aminoc, data = dfamnorm)
lmamabund2 = lm(cover ~ aminoc + pH, data = dfamnorm)

lmemabund2 = lm(cover ~ lba + som + aminoc + allelo, data = dfemnorm)

#doesnt make much difference

#do some model averaging############################

#get top models, use dredge on the model set of interest
#lmamalpha
models = dredge(lmamabund)
model_set = get.models(models, subset = delta<2)


#do model averaging, subset means zero method
amalpha_avg_models = model.avg(model_set, subset)

summary = summary(amalpha_avg_models)
coefs =  amalpha_avg_models$coefficients
importance =  as.vector(importance(amalpha_avg_models))

coef_matrix = summary$coefmat.full
coefs = coef_matrix[,1]
coefs = coefs[2:6]
adj_se = coef_matrix[,3]
adj_se = adj_se[2:6]
CI_lower =  coefs - 1.96*adj_se
CI_upper = coefs + 1.96*adj_se

output = round(as.data.frame(cbind(coefs,adj_se, CI_lower, CI_upper, importance)),2)

data = output[,c(1:4)]
#data = data[,c(1,3,4)]
data = t(data)
data = as.data.frame(data)
melted = melt(data)

fun_mean <- function(x){
  data = data.frame(y=mean(x),label=mean(x,na.rm=T))
  data = round(data,2)
  return(data)}

#(aes_string(x = 'variable', y='value', na.rm = TRUE)

melted$variable = as.factor(melted$variable)# ps, wont plot as separate plots if x continous

ggplot(melted,aes_string(x = 'variable', y ='value' ) )+
  geom_boxplot(na.rm = TRUE, width = 0)+
  stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7)+
  stat_summary(geom = "text", label = output$importance,fun.y = max, hjust = 1, colour = "red")+
  geom_abline(intercept = 0, slope = 0)+
  labs(y = "Parameter estimate and 95%CI",x = "Parameter")+ 
  labs(title = "Model averaged results for delta <2, am plots, alpha diversity",
       subtitle = "Numbers in red are importance")
#######################
# repeat for am plots abundance
models = dredge(lmamabund)
model_set = get.models(models, subset = delta<2)


#do model averaging, subset means zero method
amalpha_avg_models = model.avg(model_set, subset)

summary = summary(amalpha_avg_models)
coefs =  amalpha_avg_models$coefficients
importance =  as.vector(importance(amalpha_avg_models))

coef_matrix = summary$coefmat.full
coefs = coef_matrix[,1]
coefs = coefs[2:6]
adj_se = coef_matrix[,3]
adj_se = adj_se[2:6]
CI_lower =  coefs - 1.96*adj_se
CI_upper = coefs + 1.96*adj_se

output = round(as.data.frame(cbind(coefs,adj_se, CI_lower, CI_upper, importance)),2)

data = output[,c(1:4)]
#data = data[,c(1,3,4)]
data = t(data)
data = as.data.frame(data)
melted = melt(data)

fun_mean <- function(x){
  data = data.frame(y=mean(x),label=mean(x,na.rm=T))
  data = round(data,2)
  return(data)}

#(aes_string(x = 'variable', y='value', na.rm = TRUE)

melted$variable = as.factor(melted$variable)# ps, wont plot as separate plots if x continous

ggplot(melted,aes_string(x = 'variable', y ='value' ) )+
  geom_boxplot(na.rm = TRUE, width = 0)+
  stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7)+
  stat_summary(geom = "text", label = output$importance,fun.y = max, hjust = 1, colour = "red")+
  geom_abline(intercept = 0, slope = 0)+
  labs(y = "Parameter estimate and 95%CI",x = "Parameter")+ 
  labs(title = "Model averaged results for delta <2, am plots, cover",
       subtitle = "Numbers in red are importance")
####################################################################################
#em alpha

fun_mean <- function(x){
  data = data.frame(y=mean(x),label=mean(x,na.rm=T))
  data = round(data,2)
  return(data)}

models = dredge(lmemalpha)
model_set = get.models(models, subset = delta<5)


#do model averaging, subset means zero method
amalpha_avg_models = model.avg(model_set, subset)

summary = summary(amalpha_avg_models)
coefs =  amalpha_avg_models$coefficients
importance =  as.vector(importance(amalpha_avg_models))

coef_matrix = summary$coefmat.full
coefs = coef_matrix[,1]
coefs = coefs[2:6]
adj_se = coef_matrix[,3]
adj_se = adj_se[2:6]
CI_lower =  coefs - 1.96*adj_se
CI_upper = coefs + 1.96*adj_se

output = round(as.data.frame(cbind(coefs,adj_se, CI_lower, CI_upper, importance)),2)

data = output[,c(1:4)]
#data = data[,c(1,3,4)]
data = t(data)
data = as.data.frame(data)
melted = melt(data)

fun_mean <- function(x){
  data = data.frame(y=mean(x),label=mean(x,na.rm=T))
  data = round(data,2)
  return(data)}

#(aes_string(x = 'variable', y='value', na.rm = TRUE)

melted$variable = as.factor(melted$variable)# ps, wont plot as separate plots if x continous

ggplot(melted,aes_string(x = 'variable', y ='value' ) )+
  geom_boxplot(na.rm = TRUE, width = 0)+
  stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7)+
  stat_summary(geom = "text", label = output$importance,fun.y = max, hjust = 1, colour = "red")+
  geom_abline(intercept = 0, slope = 0)+
  labs(y = "Parameter estimate and 95%CI",x = "Parameter")+ 
  labs(title = "Model averaged results for delta <5, em plots, alpha diversity",
       subtitle = "Numbers in red are importance")

###########################################

#em abundance

#em alpha

fun_mean <- function(x){
  data = data.frame(y=mean(x),label=mean(x,na.rm=T))
  data = round(data,2)
  return(data)}

models = dredge(lmemabund)
model_set = get.models(models, subset = delta<5)


#do model averaging, subset means zero method
amalpha_avg_models = model.avg(model_set, subset)

summary = summary(amalpha_avg_models)
coefs =  amalpha_avg_models$coefficients
importance =  as.vector(importance(amalpha_avg_models))

coef_matrix = summary$coefmat.full
coefs = coef_matrix[,1]
coefs = coefs[2:6]
adj_se = coef_matrix[,3]
adj_se = adj_se[2:6]
CI_lower =  coefs - 1.96*adj_se
CI_upper = coefs + 1.96*adj_se

output = round(as.data.frame(cbind(coefs,adj_se, CI_lower, CI_upper, importance)),2)

data = output[,c(1:4)]
#data = data[,c(1,3,4)]
data = t(data)
data = as.data.frame(data)
melted = melt(data)

fun_mean <- function(x){
  data = data.frame(y=mean(x),label=mean(x,na.rm=T))
  data = round(data,2)
  return(data)}

#(aes_string(x = 'variable', y='value', na.rm = TRUE)

melted$variable = as.factor(melted$variable)# ps, wont plot as separate plots if x continous

ggplot(melted,aes_string(x = 'variable', y ='value' ) )+
  geom_boxplot(na.rm = TRUE, width = 0)+
  stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7)+
  stat_summary(geom = "text", label = output$importance,fun.y = max, hjust = 1, colour = "red")+
  geom_abline(intercept = 0, slope = 0)+
  labs(y = "Parameter estimate and 95%CI",x = "Parameter")+ 
  labs(title = "Model averaged results for delta <5, em plots, abundance",
       subtitle = "Numbers in red are importance")
