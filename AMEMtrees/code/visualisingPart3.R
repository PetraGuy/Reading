#pH across NVC
#alpha acros pH
#effect sizes by year at plot level
#mixed effects
#distribution of alpha across different plot types


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
#look at different assignment of am and em
#am = c('W6','W7','W8','W12','W13')
#int = c('W10','W11','W14')
#em = c('W1','W3','W4','W15','W16','W17')

amplots = filter(woods, shortcodes %in% am) #am plots not scrub
#intplots = filter(woods,shortcodes %in% int) 
emplots = filter(woods, shortcodes %in% em) #em plots not scrub
##############################################################################

##look at pH across the above NVC allocations
#can include intermediate codes or not

pHam = amplots['pH']
index = rep('am',length(pHam))
df1 = cbind(pHam,index)

#pHint = intplots['pH']
#index = rep('int', length(pHint))
#df2 = cbind(pHint,index)

pHem = emplots['pH']
index = rep('em',length(pHem))
df3 = cbind(pHem,index)

data = rbind(df1, df3)


ggplot(data,aes(x = index, y = pH))+geom_violin(aes(fill = index))
#####################################################################

#look at alpha diversity and cover across pH
#produces boxplots of alpha across pH and violin plots of abundance across pH

dfalpha = woodsonly[c(4,10)]
dfalpha2 <- dfalpha %>% mutate_at(c('alpha'), ~(normalize(.) %>% as.vector))
dfabund = woodsonly[c(5,10)]
dfabund2 <- dfabund %>% mutate_at(c('herbcover'), ~(normalize(.) %>% as.vector))

index = rep('rich', nrow(dfalpha2))
df1 = cbind(dfalpha2,index)
colnames(df1) = c('value','pH','index')
df1 = df1%>%mutate(bin = cut_width(pH,width = 0.5, boundary = 0))
g1 = ggplot(data = na.omit(df1), aes(x=bin, y=value) ) +
  geom_boxplot(fill="#69b3a2") +
  xlab("pH")+
  ylab('alpha diversity')


index = rep('abund',nrow(dfabund2))
df2 = cbind(dfabund2,index)
colnames(df2) = c('value','pH','index')
df2 = df2%>%mutate(bin = cut_width(pH,width = 0.5, boundary = 0))
g2 = ggplot(data = na.omit(df2), aes(x=bin, y=value) ) +
  geom_violin(fill="#69b3a2") +
  xlab("pH")+
  ylab('abundance')


grid.arrange(g1,g2)

##################################################################################
#linear models by year for whatever plots you put in
#use woods, or am or em etc
#filter by percentinv for more or less invaded plots etc
data = emplots #%>%filter(percentinvcover<0.1)

  yr1 = data%>%filter(Yr == 1)
  data1 = yr1[-c(1,2,3,5,11,10)]
  scaled1 =  apply(data1[,-1],2, rescale)
  scaled1 = as.data.frame(cbind(data1$alpha,scaled1))
  scaled1 = scaled1%>%dplyr::rename(alpha = V1)
  fit1 = lm(alpha ~ . ,scaled1, na.action = na.exclude)
  rsquared1 = paste("R2 = ",round(summary(fit2)$r.squared,2))
  pred1 = predict(fit1)
  rmse1 = paste("rmse = ",rmse(actual = yr1$alpha,predicted = pred1))
  
  
  yr2 = data%>%filter(Yr == 2)
  data2 = yr2[-c(1,2,3,5,11,10)]
  scaled2 =  apply(data2[,-1],2, rescale)
  scaled2 = as.data.frame(cbind(data2$alpha,scaled2))
  scaled2 = scaled2%>%dplyr::rename(alpha = V1)
  fit2 = lm(alpha~ . ,scaled2, na.action = na.exclude)
  rsquared2 = paste("R2 = ",round(summary(fit2)$r.squared,2))
  pred2 = predict(fit2)
  rmse2 = paste("rmse = ",rmse(actual = yr2$alpha,predicted = pred2))
  

#plot_summs is the output with CI using dev tools package  
plot_summs(fit1,fit2,ci_level = 0.9, model.names = c("1971","2000"))+ggtitle("All plots")

########################################################################################

#look at coefs using summ function if you want, this gives the actual coefficients
summ(fit1,vifs = TRUE, confint = TRUE, ci.width = 0.5)
x$coeftable[2:8]
summ(fit2,vifs = TRUE, confint = TRUE, ci.width = 0.5)
y$coeftable[2:8]

####################################################################################
  
#data = amplots #put in whatever data you want to use here
#repeat the plots for 10 different invasion levels, didnt use this in the end

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

#use above funtion to get richness at different levels of invasion
amrichness = alpha(amplots)
  
  
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
  
 ####################################################################################

#a mixed effects model, not sure what to do with this


#mixed effects model on all NVC W NVCs, NVC is random
data = woods
model = lme(alpha~ amtreeprop + invtreeprop+lba+som+percentamcover+percentinvcover+ pH2,
              random = ~1|shortcodes, data = woods, na.action = na.omit) 
#get coefficients
coef_df = data.frame()
int = round(model$coefficients$fixed[[1]],digits = 2)
slope = round(model$coefficients$fixed[[2]],digits = 2)
p_slope = round(summary(model)$tTable[2,5],digits=4)
sd = round(as.numeric(VarCorr(model)[1,2]),digits = 2)
row = as.numeric(c(int,slope, p_slope,sd))
coef_df = as.data.frame(rbind(coef_df,row))
  
colnames(coef_df)

#but how do I know if this is any good?
pred = predict(model)
rmse(actual = data$alpha,predicted = pred)
plot.lme(model) # this seems to show that the model isn't good. 

#####################################################################################

#looking at spread in richness to see if noise means crap model
library(gridExtra)
#amplots
data1yr1 = amplots%>%filter(Yr==1)
g1 = ggplot(data1yr1, aes(x=alpha))+geom_density(aes(y = ..count..), fill = "palegreen1")+
  ggtitle("AM invaded 1971")+ylim(0,60)
data1yr2 = amplots%>%filter(Yr==2)
g2 = ggplot(data1yr2, aes(x=alpha))+geom_density(aes(y = ..count..),fill = "palegreen1")+
  ggtitle("AM invaded 2000")+ylim(0,60)

data2 = amplots%>%filter(percentinvcover<0.2)
data2yr1 = data2%>%filter(Yr==1)
g3 = ggplot(data2yr1, aes(x=alpha))+geom_density(aes(y = ..count..),fill = "palegreen1")+
  ggtitle("AM uninvaded 1971")+ylim(0,60)
data2yr2 = data2%>%filter(Yr==2)
g4 = ggplot(data2yr2, aes(x=alpha))+geom_density(aes(y = ..count..),fill = "palegreen1")+
  ggtitle("AM uninvaded 2000")+ylim(0,60)

# emplots
data3yr1 = emplots%>%filter(Yr==1)
g5 = ggplot(data3yr1, aes(x=alpha))+geom_density(aes(y = ..count..),fill = "skyblue1")+
  ggtitle("EM invaded 1971")+ylim(0,60)
data3yr2 = emplots%>%filter(Yr==2)
g6 = ggplot(data3yr2, aes(x=alpha))+geom_density(aes(y = ..count..),fill = "skyblue1")+
  ggtitle("EM invaded 2000")+ylim(0,60)

data4 = emplots%>%filter(percentinvcover<0.2)
data4yr1 = data4%>%filter(Yr==1)
g7 = ggplot(data4yr1, aes(x=alpha))+geom_density(aes(y = ..count..),fill = "skyblue1")+
  ggtitle("EM uninvaded 1971")+ylim(0,60)
data4yr2 = data4%>%filter(Yr==2)
g8 = ggplot(data4yr2, aes(x=alpha))+geom_density(aes(y = ..count..),fill = "skyblue1")+
  ggtitle("EM uninvaded 2000")+ylim(0,60)

#library(gridExtra)
grid.arrange(g1,g2,g5,g6,g3,g4,g7,g8, ncol = 2)


###########################################################################################
