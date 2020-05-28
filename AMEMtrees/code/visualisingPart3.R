setwd("C:/dev/code/Reading/AMEMtrees/Code")
library(dplyr)
library(jtools)
library(devtools)
library(nlme)
library(lme4)
library(Metrics) #rmse
library(stringr)
library(arm) #standardize

source('../../utilities/modelavg.R')
#######################################

'%!in%' = function(x,y)!('%in%'(x,y))

########################################

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

###################################
devtools::install_github("jacob-long/jtools")
#get data
alldata = read.csv('../data/alldata.csv')

#response to pH is quadratic, therefore transform pH to pH2
alldata['pH2'] = alldata['pH']*alldata['pH']


#get rid of non woodland plots - anything that doesnt start with W
woodsonly = alldata%>%filter(str_detect(shortcodes,"W"))

#take out the scrub codes -W19 - W25 inclusive
scrub = c('W19','W20','W21','W22','W23','W24','W25')

woods = filter(woodsonly, shortcodes %!in% scrub) # all wood plots, not scrub

# 4 plots in pH and SOM are na - mean impute these
woods%>%group_by(shortcodes)%>%summarise( mean = mean(som, na.rm = TRUE))
impute <- function(x) {
  replace(x, is.na(x), mean(x, na.rm = TRUE))}
woods = woods%>%group_by(shortcodes)%>%mutate_at(c('pH','som'), impute)


# all remaining NAs are 0 because they are where no am trees , no am cover occur etc, but delete
woods[is.na(woods)] = 0


#these are am and em codes for non-scrub plots
#nb - W1, dual damp willows, in both
am = c('W6','W7','W8','W9','W12','W13','W1')
em = c('W3','W4','W10','W11','W14','W15','W16','W17','W1')

amplots = filter(woods, shortcodes %in% am) #am plots not scrub
emplots = filter(woods, shortcodes %in% em) #em plots not scrub

###################################
# all data
#data = alldata

#remove the open plots
#data = woodsonly

#without scrub plots
#data = woods

#all wood plots without scrub but uninvaded
#data = woods%>%filter(percentinvcover<0.5)

#split into am and em plots , am first
#data = amplots

#take uninvaded am plots
data = amplots%>%filter(percentinvcover<0.1)

#em plots
#data = emplots

#em plots uninvaded
#data = emplots%>%filter(percentinvcover < 0.8)



#alpha

  yr1 = data%>%filter(Yr == 1)
  data1 = yr1[-c(1,2,3,5,11,10)]
  scaled1 =  apply(data1[,-1],2, rescale)
  scaled1 = as.data.frame(cbind(data1$alpha,scaled1))
  scaled1 = scaled1%>%rename(alpha = V1)
  fit1 = lm(alpha ~ . ,scaled1, na.action = na.exclude)
  
  yr2 = data%>%filter(Yr == 2)
  data2 = yr2[-c(1,2,3,5,11,10)]
  scaled2 =  apply(data2[,-1],2, rescale)
  scaled2 = as.data.frame(cbind(data2$alpha,scaled2))
  scaled2 = scaled2%>%rename(alpha = V1)
  fit2 = lm(alpha~ . ,scaled2, na.action = na.exclude)
  
  x =summ(fit1,vifs = TRUE, confint = TRUE, ci.width = 0.5)
  x
  x$coeftable[2:8]
  y = summ(fit2,vifs = TRUE, confint = TRUE, ci.width = 0.5)
  y 
  y$coeftable[2:8]
  
  plot_summs(fit1,fit2,ci_level = 0.5)
  pred1 = predict(fit1)
  rmse1 = rmse(actual = yr1$alpha,predicted = pred1)
  pred2 = predict(fit2)
  rmse2 = rmse(actual = yr2$alpha,predicted = pred2)
#######################################
  
  
  #data = amplots

  alpha = function(data){
   # browser()
  df = data[-c(1,2,5,11,10)]
  scaled =  apply(df[,-c(1,2)],2, rescale)
  scaled = as.data.frame(cbind(data$alpha,data$Yr, scaled))
  scaled = scaled%>%rename(alpha = V1,Yr = V2)
  output = list()
  for (yr in 1:2){
    thisyr = scaled%>%filter(Yr == yr)
    thisyr = thisyr[,-2]
    parameters = data.frame(matrix(ncol = 7,nrow = 0))
    cols = c('pamtree','pinvtree','lba','som','pamcov','pinvcov','pH2')
    colnames(parameters) = cols
    for (i in  1:10) {
      n = i/10
      thisdata = thisyr%>%filter(percentinvcover < n)
      fit = lm(alpha ~ . ,thisdata, na.action = na.exclude)
      x =summ(fit,vifs = TRUE, confint = TRUE, ci.width = 0.5)
      thisrow= as.vector(x$coeftable[2:8])
      parameters = rbind(parameters,thisrow)
    }
    output[[yr]]=parameters
  }
  output = lapply(output,setNames, nm =cols)
  return(output)
}  

  amrichness = alpha(amplots)
  
  
  ##################################
#plot the above
 library(reshape2) 
library(ggplot2)
x = seq(0.1:1, by = 0.1)
xvar = rep(x,7)
melted = melt(amrichness[[2]])
melted = cbind(melted,xvar)

ggplot(melted, aes(x = xvar, y = value, colour = variable))+geom_point()+
  geom_smooth(method = lm)+
  xlab("proportion of plot invaded")+
  ggtitle("emplots, 2000, change standardised parameter effect sizes as
          invaded area of plot increases")
  
#try mixed effects ############################################

fit = function(data){
  yr1 = data%>%filter(Yr == 1)
  yr2 = data%>%filter(Yr == 2)
  fit1 = lmer(alpha ~ . +(1|shortcodes) ,data = yr1[-c(1,2,3,5)], na.action = na.omit)
  fit2 = lmer(alpha ~ . +1|shortcodes ,data = yr2[-c(1,2,3,5)], na.action = na.omit)
  summ(fit1)
  summ(fit2)
  plot_summs(fit1,fit2)
  pred = predict(fit1)
  rmse(actual = data$alpha,predicted = pred)
}  


coef_df = data.frame()
  data = woods
  model = lme(alpha~ amtreeprop + invtreeprop+lba+som+percentamcover+percentinvcover+ pH2,
              random = ~1|shortcodes, data = woods, na.action = na.omit) 
  #get coefficients
  int = round(model$coefficients$fixed[[1]],digits = 2)
  slope = round(model$coefficients$fixed[[2]],digits = 2)
  p_slope = round(summary(model)$tTable[2,5],digits=4)
  sd = round(as.numeric(VarCorr(model)[1,2]),digits = 2)
  row = as.numeric(c(int,slope, p_slope,sd))
  coef_df = as.data.frame(rbind(coef_df,row))
}
colnames(coef_df)

#but how do I know if this is any good?
pred = predict(model)
rmse(actual = data$alpha,predicted = pred)
plot.lme(model) # this seems to show that the model isn't good. 




