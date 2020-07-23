#moving things up to site level, but need tolook at EM plots only,
#coz AM shrub cover not imprtoant in AM plots
#and consider how many  plots in each wood - wont be the same


setwd("C:/dev/code/Reading/AMEMtrees/Code")
library(dplyr)
library(jtools)
library(devtools)
#library(nlme)
#library(lme4)
library(Metrics) #rmse
library(stringr)
library(arm) #standardize
library(ggplot2)


#######################################

'%!in%' = function(x,y)!('%in%'(x,y))

########################################


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

#am = c('W6','W7','W8','W9','W12','W13')
em = c('W3','W4','W10','W11','W14','W15','W16','W17','W1')

#amplots = filter(woods, shortcodes %in% am)
emplots = filter(woods, shortcodes %in% em)



#######################################################################################
#if we're interested in looking at em plots, because in prev models
#am shrub cover NOT important in AM plots, need to see how many there are
#in each wood - different nums of plots is an issue for models at site level
#split by year and see how many plots in each wood

#remove NVC and herb cover
emplotssht = emplots[-c(5,11)]
emwoodsyr1 = emplotssht%>%filter(Yr == 1)
emwoodsyr2 = emplotssht%>%filter(Yr==2)

#get group totals
sitedata1 = emwoodsyr1%>%group_by(Yr,Site)%>%summarise_all(sum)
numplots1  = emwoodsyr1%>%group_by(Yr,Site)%>%summarise(length(Site))
sitedatayr1 = as.data.frame(full_join(sitedata1,numplots1, by = "Site"))
sitedatayr1 = sitedatayr1[-c(1,2,3,13)]
sitedatayr1=sitedatayr1%>%dplyr::rename(numplots = "length(Site)")

numplots2 = emwoodsyr2%>%group_by(Yr,Site)%>%summarise(length(Site))
sitedata2 = emwoodsyr2%>%group_by(Yr,Site)%>%summarise_all(sum)
sitedatayr2 = as.data.frame(full_join(sitedata2,numplots2, by = "Site"))
sitedatayr2 = sitedatayr2[-c(1,2,3,13)]
sitedatayr2=sitedatayr2%>%dplyr::rename(numplots = "length(Site)")

#check out spread of num of plots in the woods over the two years
data = sitedatayr2
title = ("EM plots 2000")
ggplot(data, aes(x=numplots))+
  geom_histogram(aes(y = ..count..))+
  ggtitle(title)+
  scale_y_continuous(name = "number of woods", breaks = seq(0,20,2))+
  scale_x_continuous(name = "number of plots",breaks = seq(0,16,1))

#################################################

#start from emplots above and see how many plots there are in each year when inv removed or not

yr1inv = emplotssht%>%filter(Yr == 1)%>%group_by(Yr,Site)%>%summarise(length(Site))
yr1uninv = emplotssht%>%filter(Yr == 1)%>%filter(percentinvcover<0.2)%>%
  group_by(Yr,Site)%>%summarise(length(Site))

yr2inv = emplotssht%>%filter(Yr == 2)%>%group_by(Yr,Site)%>%summarise(length(Site))
yr2uninv = emplotssht%>%filter(Yr == 2)%>%filter(percentinvcover<0.2)%>%
  group_by(Yr,Site)%>%summarise(length(Site))

#rename the cols
yr1inv = yr1inv%>%dplyr::rename(numplots = "length(Site)")
yr2inv = yr2inv%>%dplyr::rename(numplots = "length(Site)")
yr1uninv = yr1uninv%>%dplyr::rename(numplots = "length(Site)")
yr2uninv = yr2uninv%>%dplyr::rename(numplots = "length(Site)")

#add id col
yr1inv$id = "yr1inv"
yr1uninv$id = "yr1uninv"
yr2inv$id = "yr2inv"
yr2uninv$id = "yr2uninv"

df = rbind(yr1inv,yr1uninv,yr2inv,yr2uninv)
num1 = nrow(yr1inv)
num2 = nrow(yr1uninv)
num3 = nrow(yr2inv)
num4 = nrow(yr2uninv)

ggplot(df, aes(x = id, y = numplots)) + 
  geom_boxplot() +
  annotate("text", x = 1, y = 15, label = num1)+
  annotate("text", x = 2, y = 15, label = num2)+
  annotate("text", x = 3, y = 15, label = num3)+
  annotate("text", x = 4, y = 15, label = num4)

############################################################################################

#plot everything - this has all am/em plots etc, not sure why I'm interested in this
#maybe want to see whether the stratification EM/AM is necessary.

yr1 = woods%>%filter(Yr == 1)
data1 = yr1[-c(1,2,3,5,11,10)]
scaled1 =  apply(data1[,-1],2, rescale)
scaled1 = as.data.frame(cbind(data1$alpha,scaled1))
scaled1 = scaled1%>%dplyr::rename(alpha = V1)
fit1 = lm(alpha ~ . ,scaled1, na.action = na.exclude)
rsquared1 = paste("R2 = ",round(summary(fit1)$r.squared,2))
pred1 = predict(fit1)
rmse1 = paste("rmse = ",rmse(actual = yr1$alpha,predicted = pred1))


yr2 = woods%>%filter(Yr == 2)
data2 = yr2[-c(1,2,3,5,11,10)]
scaled2 =  apply(data2[,-1],2, rescale)
scaled2 = as.data.frame(cbind(data2$alpha,scaled2))
scaled2 = scaled2%>%dplyr::rename(alpha = V1)
fit2 = lm(alpha~ . ,scaled2, na.action = na.exclude)
rsquared2 = paste("R2 = ",round(summary(fit2)$r.squared,2))
pred2 = predict(fit2)
rmse2 = paste("rmse = ",rmse(actual = yr2$alpha,predicted = pred2))


#plot_summs is the output with CI using dev tools package  
plot_summs(fit1,fit2,ci_level = 0.5, model.names = c("1971","2000"))+ggtitle("All plots")

#####################################################################################


#add a column with a flag for plots above pH5.5 and plots below pH5.5
#maybe this better than grouping by am/em? That was self imposed, but ecological stratum 

woodsnew = woods%>%mutate(pHgrp = ifelse((pH< 5.5), yes = "low",no = "high"))

lowwoods = woodsnew%>%filter(pHgrp=="low")%>%filter(percentinvcover<0.2)

yr1 = lowwoods%>%filter(Yr == 1)
data1 = yr1[-c(1,2,3,5,11,10,15)]
scaled1 =  apply(data1[,-1],2, rescale)
scaled1 = as.data.frame(cbind(data1$alpha,scaled1))
scaled1 = scaled1%>%dplyr::rename(alpha = V1)
fit1 = lm(alpha ~ . ,scaled1, na.action = na.exclude)
rsquared1 = paste("R2 = ",round(summary(fit1)$r.squared,2))
pred1 = predict(fit1)
rmse1 = paste("rmse = ",rmse(actual = yr1$alpha,predicted = pred1))


yr2 = lowwoods%>%filter(Yr == 2)
data2 = yr2[-c(1,2,3,5,11,10,15)]
scaled2 =  apply(data2[,-1],2, rescale)
scaled2 = as.data.frame(cbind(data2$alpha,scaled2))
scaled2 = scaled2%>%dplyr::rename(alpha = V1)
fit2 = lm(alpha~ . ,scaled2, na.action = na.exclude)
rsquared2 = paste("R2 = ",round(summary(fit2)$r.squared,2))
pred2 = predict(fit2)
rmse2 = paste("rmse = ",rmse(actual = yr2$alpha,predicted = pred2))


plot_summs(fit1,fit2,ci_level = 0.5, model.names = c("1971","2000"))+ggtitle("")

#########################################################################

# how many of each type of NVC code are there?

#remove NVC and herb cover
#emplotssht = emplots[-c(5,11)]
emwoodsyr1 = emplots%>%filter(Yr == 1)
emwoodsyr2 = emplots%>%filter(Yr==2)

#get group totals
numplots1  = emwoodsyr1%>%group_by(Yr,shortcodes)%>%summarise(length(shortcodes))
numplots1=numplots1%>%dplyr::rename(numplots = "length(shortcodes)")
numplots2  = emwoodsyr2%>%group_by(Yr,shortcodes)%>%summarise(length(shortcodes))
numplots2=numplots2%>%dplyr::rename(numplots = "length(shortcodes)")


data = as.data.frame(rbind(numplots1,numplots2))
ggplot(data=data,aes(x = shortcodes,y = numplots,colour = factor(Yr)))+
    geom_point(size = 4,alpha = 0.5)
#most of the plots are W10 in both years, next is W16
  