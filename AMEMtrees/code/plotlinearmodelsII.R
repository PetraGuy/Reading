#redoing linear model at different strata but with corrected data that includes inv herbs!!

#pH across NVC
#alpha acros pH
#effect sizes by year at plot level
#mixed effects
#distribution of alpha across different plot types

# this repeats the whole code for different stratawith richness and abunance, very messy
#but you can run sections individually


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
alldata = read.csv('../data/all2data.csv')

#response to pH is quadratic, therefore transform pH to pH2
alldata['pH2'] = alldata['pH']*alldata['pH']


#get rid of non woodland plots - anything that doesnt start with W
woodsonly = alldata%>%filter(str_detect(shortcodes,"W"))

#take out the scrub codes -W19 - W25 inclusive
scrub = c('W19','W20','W21','W22','W23','W24','W25')
am = c('W6','W7','W8','W9','W12','W13','W1')
em = c('W3','W4','W10','W11','W14','W15','W16','W17','W1')

woods = filter(woodsonly, shortcodes %!in% scrub) # all wood plots, not scrub

# 4 plots in pH and SOM are na - mean impute these
woods%>%group_by(shortcodes)%>%summarise( mean = mean(som, na.rm = TRUE))
impute <- function(x) {
  replace(x, is.na(x), mean(x, na.rm = TRUE))}
woods = woods%>%group_by(shortcodes)%>%mutate_at(c('pH','som'), impute)


# all remaining NAs are 0 because they are where no am trees , no am cover occur etc, but delete
woods[is.na(woods)] = 0

woods = woods%>%mutate(NVCtype = if_else (shortcodes %in% am, -1,1))


woods['am'] = woods[5]+woods[8]
woods['inv'] = woods[6]+woods[9]+woods[10]
######################################################

#just do richness and abundance because we know the rest
fitrich = lm(alpha ~ am + inv , woods, na.action = na.exclude)
fitabund = lm(herbcover ~  am + inv, woods, na.action = na.exclude) 
rsquaredrich = summary(fitrich)$r.squared
rsquaredabund = summary(fitabund)$r.squared
pred = predict(fit)
rmse = paste("rmse = ",rmse(actual = scaledalpha$scaledalpha, predicted = pred))


#plot_summs is the output with CI using dev tools package  
plot_summs(fitabund,ci_level = 0.9, model.names = c("all data"))+ggtitle("Plot abundance")+
  theme_grey(base_size = 22)
plot_summs(fitrich,ci_level = 0.9, model.names = c("all data"))+ggtitle("Plot richness")+
  theme_grey(base_size = 22)
################################################

#stratify the data into am and em plots


amplots = woods%>%filter(NVCtype == -1)
emplots = woods%>%filter(NVCtype == 1)

fitam = lm(alpha ~ inv + am,amplots, na.action = na.exclude)
fitem= lm(alpha ~ inv + am,emplots, na.action = na.exclude)


rsquared2 = summary(fitem)$r.squared
pred = predict(fit2)
rmse = rmse(actual = scaledalphaem$alpha, predicted = pred)

plot_summs(fitam,fitem,ci_level = 0.9, model.names = c("am","em"))+
  ggtitle("Plot richness, stratified by plot type")+
  theme_grey(base_size = 22)

#############

#as above for abundance                                #2 am/em abund


fitam = lm(herbcover ~ inv + am,amplots, na.action = na.exclude)
fitem = lm(herbcover ~ inv + am ,emplots , na.action = na.exclude)

rsquared2 = summary(fitem)$r.squared
pred = predict(fit2)
rmse = rmse(actual = scaledalphaem$abundance, predicted = pred)

plot_summs(fitam,fitem,ci_level = 0.9, model.names = c("am","em"))+
  ggtitle("Plot abundance, stratified by plot type")+
  theme_grey(base_size = 22)
######################################################################
# clean by stratifying years                          #3 am/em/yr


amplotsyr1 = woods%>%filter(NVCtype == -1 & Yr ==1) 
amplotsyr2 = woods%>%filter(NVCtype == -1 & Yr ==2)
emplotsyr1 = woods%>%filter(NVCtype == 1 & Yr ==1) 
emplotsyr2 = woods%>%filter(NVCtype == 1 & Yr ==2) 


amyr1 = lm(alpha~ inv+am,amplotsyr1, na.action = na.exclude)
amyr2 = lm(alpha~ inv+am,amplotsyr2, na.action = na.exclude)
emyr1= lm(alpha~ inv+am,emplotsyr1, na.action = na.exclude)
emyr2 = lm(alpha~ inv+am,emplotsyr2, na.action = na.exclude)

plot_summs(amyr1,amyr2,emyr1,emyr2, ci_level = 0.8, model.names = c("AM Yr1","AM Yr2","EM Yr 1","EM Yr 2"))+
  ggtitle("Plot richness, stratified by plot type and year")+
  theme_grey(base_size = 22)

##############
#same as above for abundance


amyr1 = lm(herbcover~ inv+am,amplotsyr1, na.action = na.exclude)
amyr2 = lm(herbcover~ inv+am,amplotsyr2, na.action = na.exclude)
emyr1= lm(herbcover~ inv+am,emplotsyr1, na.action = na.exclude)
emyr2 = lm(herbcover~ inv+am,emplotsyr2, na.action = na.exclude)

plot_summs(amyr1,amyr2,emyr1,emyr2, ci_level = 0.8, model.names = c("AM Yr1","AM Yr2","EM Yr 1","EM Yr 2"))+
  ggtitle("Plot abundance, stratified by plot type and year")+
  theme_grey(base_size = 22)

######################################################################

#just uninvaded plots

uninvaded = woods%>%filter(alpha>1)  #4am/em/yr - univaded richness


amplotsyr1 = uninvaded%>%filter(NVCtype == -1 & Yr ==1) 
amplotsyr2 = uninvaded%>%filter(NVCtype == -1 & Yr ==2)
emplotsyr1 = uninvaded%>%filter(NVCtype == 1 & Yr ==1) 
emplotsyr2 = uninvaded%>%filter(NVCtype == 1 & Yr ==2) 

amyr1 = lm(alpha~ inv+am,amplotsyr1, na.action = na.exclude)
amyr2 = lm(alpha~ inv+am,amplotsyr2, na.action = na.exclude)
emyr1= lm(alpha~ inv+am,emplotsyr1, na.action = na.exclude)
emyr2 = lm(alpha~ inv+am,emplotsyr2, na.action = na.exclude)

plot_summs(amyr1,amyr2,emyr1,emyr2, ci_level = 0.8, model.names = c("AM Yr1","AM Yr2","EM Yr 1","EM Yr 2"))+
  ggtitle("Plot richness, stratified by plot type and year")+
  theme_grey(base_size = 18)

summary(emyr2)$r.squared

#####

#same as above for abundance

amplotsyr1 = uninvaded%>%filter(NVCtype == -1 & Yr ==1) 
amplotsyr2 = uninvaded%>%filter(NVCtype == -1 & Yr ==2)
emplotsyr1 = uninvaded%>%filter(NVCtype == 1 & Yr ==1) 
emplotsyr2 = uninvaded%>%filter(NVCtype == 1 & Yr ==2) 

amyr1 = lm(herbcover~ inv+am,amplotsyr1, na.action = na.exclude)
amyr2 = lm(herbcover~ inv+am,amplotsyr2, na.action = na.exclude)
emyr1= lm(herbcover~ inv+am,emplotsyr1, na.action = na.exclude)
emyr2 = lm(herbcover~ inv+am,emplotsyr2, na.action = na.exclude)

plot_summs(amyr1,amyr2,emyr1,emyr2, ci_level = 0.8, model.names = c("AM Yr1","AM Yr2","EM Yr 1","EM Yr 2"))+
  ggtitle("Plot abundance, stratified by plot type and year, plots less then 20% invaded")+
  theme_grey(base_size = 18)

summary(emyr2)$r.squared

#what is pH range of uninvaded/invaded plots?
data = uninvaded   
phdata = as.data.frame(cbind(data$pH,data$NVCtype))
phdata=phdata%>%dplyr::rename(pH = V1, plottype = V2)
ggplot(phdata,aes(x = as.factor(plottype), y = pH))+geom_violin(aes(fill = as.factor(plottype)))+
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10,1) )+
  labs(title ="pH across am  and em plot types, invasion < 0.2", x = "plot type")+
  theme_grey(base_size = 22)+
  theme(legend.position = "none")
 #as you'd expect, no diff for uninvaded

################################################################

#finally, just take out em plots and inv and amount am combined
#compare invaded/uninvaded
uninvaded = woods%>%filter(alpha > 21 ) #4am/em/yr - univaded richness

amplotsyr1 = uninvaded%>%filter(NVCtype == -1 & Yr ==1) 
amplotsyr2 = uninvaded%>%filter(NVCtype == -1 & Yr ==2)
emplotsyr1 = uninvaded%>%filter(NVCtype == 1 & Yr ==1) 
emplotsyr2 = uninvaded%>%filter(NVCtype == 1 & Yr ==2) 

uamplotsyr1 = woods%>%filter(NVCtype == -1 & Yr ==1) 
uamplotsyr2 = woods%>%filter(NVCtype == -1 & Yr ==2)
uemplotsyr1 = woods%>%filter(NVCtype == 1 & Yr ==1) 
uemplotsyr2 = woods%>%filter(NVCtype == 1 & Yr ==2) 

fit1 = lm(alpha~ am,emplotsyr1, na.action = na.exclude)
fit2 = lm(alpha~ am,emplotsyr2, na.action = na.exclude)
fit3 = lm(alpha~ am,amplotsyr1, na.action = na.exclude)
fit4 = lm(alpha~ am,amplotsyr2, na.action = na.exclude)
fit5 = lm(alpha~ am,uemplotsyr1, na.action = na.exclude)
fit6 = lm(alpha~ am,uemplotsyr2, na.action = na.exclude)
fit7 = lm(alpha~ am,uamplotsyr1, na.action = na.exclude)
fit8 = lm(alpha~ am,uamplotsyr2, na.action = na.exclude)


plot_summs(fit1,fit2,fit3,fit4, fit5,fit6,fit7,fit8, ci_level = 0.8,
           colors = 'Rainbow',
           model.names = c("Uninvaded Yr1 EM", "Uninvaded Yr2 EM", 
                           "Uninvaded Yr1 AM", "Uninvaded Yr2 AM","Invaded Yr1 EM", "Invaded Yr2 EM", 
                           "Invaded Yr1 AM", "Invaded Yr2 AM"))+
  labs(title = "Effect of amount of AM trees and shrubs on herb species richness",
       subtitle = "alpha > uq")+
  theme_grey(base_size = 18)+
  theme(legend.position = "bottom")
  
  

summary(fit8)$r.squared
######################
#just do stuff of interest

#finally, just take out em plots and inv and amount am combined
#compare invaded/uninvaded
uninvaded1 = woods%>%filter(percentinvherb < 0.85 )#4am/em/yr - univaded richness
uninvaded2 = woods%>%filter(percentinvshrub<0.51)
uninvaded3 = woods%>%filter(percentinvtree<0.13)
uninvaded4 = woods%>%filter(inv<1.34)
uninvaded5 = woods%>%filter(inv<2.07)


emplotsyr1herb = uninvaded1%>%filter(NVCtype == 1 & Yr ==1) 
emplotsyr2herb = uninvaded1%>%filter(NVCtype == 1 & Yr ==2)
emplotsyr1shrub = uninvaded2%>%filter(NVCtype == 1 & Yr ==1) 
emplotsyr2tree = uninvaded3%>%filter(NVCtype == 1 & Yr ==2)
emplotsyr1all = uninvaded4%>%filter(NVCtype == 1 & Yr ==1) 
emplotsyr2all = uninvaded5%>%filter(NVCtype == 1 & Yr ==1)

####

fit1 = lm(alpha~ am,emplotsyr1herb, na.action = na.exclude)
fit2 = lm(alpha~ am,emplotsyr2herb, na.action = na.exclude)
fit3 = lm(alpha~ am,emplotsyr1shrub, na.action = na.exclude)
fit4 = lm(alpha~ am,emplotsyr2tree, na.action = na.exclude)
fit5 = lm(alpha~ am,emplotsyr1all, na.action = na.exclude)
fit6 = lm(alpha~ am,emplotsyr2all, na.action = na.exclude)



plot_summs(fit1,fit2,fit3,fit4,fit5,fit6, ci_level = 0.8,
           colors = 'Rainbow',
           model.names = c('invherb<uq, yr1','invherb<uq yr2','invshrub<lq yr1','invtree<lq yr2','allinv<lq yr1','allinv<uq yr1'))+
  labs(title = "Effect of amount of AM trees and shrubs on herb species richness",
       subtitle = "Emplots only")+
    theme_grey(base_size = 18)+
  theme(legend.position = "bottom")


##########################################
uninvaded1 = woods%>%filter(percentinvherb < 0.85 )#4am/em/yr - univaded richness
uninvaded2 = woods%>%filter(percentinvshrub<0.51)
uninvaded3 = woods%>%filter(percentinvtree<0.13)
uninvaded4 = woods%>%filter(inv<1.34)
uninvaded5 = woods%>%filter(inv<2.07)

amplotsyr1herb = uninvaded1%>%filter(NVCtype == -1 & Yr ==1) 
amplotsyr2herb = uninvaded1%>%filter(NVCtype == -1 & Yr ==2)
amplotsyr1shrub = uninvaded2%>%filter(NVCtype == -1 & Yr ==1) 
amplotsyr2tree = uninvaded3%>%filter(NVCtype == -1 & Yr ==1)
amplotsyr1all = uninvaded4%>%filter(NVCtype == -1 & Yr ==2) 
amplotsyr2all = uninvaded5%>%filter(NVCtype == -1 & Yr ==1)

####

fit1 = lm(alpha~ am,amplotsyr1herb, na.action = na.exclude)
fit2 = lm(alpha~ am,amplotsyr2herb, na.action = na.exclude)
fit3 = lm(alpha~ am,amplotsyr1shrub, na.action = na.exclude)
fit4 = lm(alpha~ am,amplotsyr2tree, na.action = na.exclude)
fit5 = lm(alpha~ am,amplotsyr1all, na.action = na.exclude)
fit6 = lm(alpha~ am,amplotsyr2all, na.action = na.exclude)



plot_summs(fit1,fit2,fit3,fit4,fit5,fit6, ci_level = 0.8,
           colors = 'Rainbow',
           model.names = c('invherb<uq, yr1','invherb<uq yr2','invshrub<lq yr1','invtree<lq yr1','allinv<lq yr2','allinv<uq yr1'))+
  labs(title = "Effect of amount of AM trees and shrubs on herb species richness",
       subtitle = "AM plots only")+
  theme_grey(base_size = 18)+
  theme(legend.position = "bottom")

###################################################

#as above for abundance
#stratify the data into am and em plots, start from scaled  
data = woods[-c(1,2,10,11)]                       #2am/em richness

scaled =  as.data.frame(apply(data[,-c(1,2,3,11)],2, rescale))
scaled$inv = scaled$invtreeprop+scaled$percentinvcover
scaled$am = scaled$amtreeprop+scaled$percentamcover
scaled = scaled[-c(1,2,5,6)]


scaledabund =  as.data.frame(cbind(data$herbcover,data$Yr,data$NVCtype, scaled))


scaledabund = scaledabund%>%dplyr::rename(abundance = "data$herbcover",
                                          type="data$NVCtype",
                                          yr = "data$Yr")

scaledabundemyr1 = scaledabund%>%filter(type == 1 & yr ==1)
scaledabundemyr1 = scaledabundemyr1[-c(2,3)]
scaledabundemyr2 = scaledabund%>%filter(type == 1 & yr == 2)
scaledabundemyr2 = scaledabundemyr2[-c(2,3)]

scaledabundemyr1uninv = scaledalphaemyr1%>%filter(inv<0.2)
scaledabundemyr2uninv = scaledalphaemyr2%>%filter(inv<0.2)

fit1 = lm(abundance~ inv+am,scaledabundemyr1, na.action = na.exclude)
fit2 = lm(abundance~ inv+am,scaledabundemyr2, na.action = na.exclude)
fit3 = lm(abundance~ inv+am,scaledabundemyr1uninv, na.action = na.exclude)
fit4 = lm(abundance~ inv+am,scaledabundemyr2uninv, na.action = na.exclude)


plot_summs(fit1,fit2,fit3,fit4, ci_level = 0.8, 
           model.names = c("Yr1 invaded", "Yr2 invaded", "Yr1 uninvaded", "Yr2 uninvaded"))+
  ggtitle("Plot abundance, EM plots only")+
  theme_grey(base_size = 22)
