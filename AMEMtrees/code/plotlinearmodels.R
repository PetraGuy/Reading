#redoing linear model at different strata

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
alldata = read.csv('../data/alldata.csv')

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
#############################################
#remove site plot year NVC pH
#combine inv and amshrub/tree cols

data = woods[-c(1,2,10,11)]

scaled =  as.data.frame(apply(data[,-c(1,2,3,11)],2, rescale))
scaled$inv = scaled$invtreeprop+scaled$percentinvcover
scaled$am = scaled$amtreeprop+scaled$percentamcover
scaled = scaled[-c(1,2,5,6)]

scaledalpha =  as.data.frame(cbind(data$alpha,data$Yr,data$NVCtype, scaled))
scaledabund =  as.data.frame(cbind(data$herbcover,data$Yr,data$NVCtype, scaled))

scaledalpha = scaledalpha%>%dplyr::rename(alpha = "data$alpha",
                                          type="data$NVCtype",
                                          yr = "data$Yr")
scaledabund = scaledabund%>%dplyr::rename(abundance = "data$herbcover",
                                          type="data$NVCtype",
                                          yr = "data$Yr")


#with year and type and all interactions
#fit = lm(alpha~ .+ inv:yr+inv:type+am:yr+am:type,scaledalpha, na.action = na.exclude)

#without interactions but with yr and type as vars
fitrich = lm(alpha~ .,scaledalpha, na.action = na.exclude) # 1 everything
fitabund = lm(abundance~., scaledabund, na.action = na.exclude) 
rsquaredrich = summary(fitrich)$r.squared
rsquaredabund = summary(fitabund)$r.squared
pred = predict(fit)
rmse = paste("rmse = ",rmse(actual = scaledalpha$scaledalpha, predicted = pred))


#plot_summs is the output with CI using dev tools package  
plot_summs(fitabund,ci_level = 0.9, model.names = c("all data"))+ggtitle("Plot abundance")+
  theme_grey(base_size = 22)

################################################

#stratify the data into am and em plots, start from scaled
data = woods[-c(1,2,10,11)]                       #2am/em richness

scaled =  as.data.frame(apply(data[,-c(1,2,3,11)],2, rescale))
scaled$inv = scaled$invtreeprop+scaled$percentinvcover
scaled$am = scaled$amtreeprop+scaled$percentamcover
scaled = scaled[-c(1,2,5,6)]

scaledalpha =  as.data.frame(cbind(data$alpha,data$Yr,data$NVCtype, scaled))
scaledabund =  as.data.frame(cbind(data$herbcover,data$Yr,data$NVCtype, scaled))

scaledalpha = scaledalpha%>%dplyr::rename(alpha = "data$alpha",
                                          type="data$NVCtype",
                                          yr = "data$Yr")
scaledabund = scaledabund%>%dplyr::rename(abundance = "data$herbcover",
                                          type="data$NVCtype",
                                          yr = "data$Yr")

scaledalphaam = scaledalpha%>%filter(type == -1)           
scaledalphaam = scaledalphaam[-3]
scaledalphaem = scaledalpha%>%filter(type == 1)
scaledalphaem = scaledalphaem[-3]

fit1 = lm(alpha~ .,scaledalphaam, na.action = na.exclude)
fit2 = lm(alpha~ .,scaledalphaem, na.action = na.exclude)


rsquared2 = summary(fit2)$r.squared
pred = predict(fit2)
rmse = rmse(actual = scaledalphaem$alpha, predicted = pred)

plot_summs(fit1,fit2,ci_level = 0.9, model.names = c("am","em"))+
  ggtitle("Plot richness, stratified by plot type")+
  theme_grey(base_size = 22)

#############

#as above for abundance                                #2 am/em abund
scaledabundam = scaledabund%>%filter(type == -1)
scaledalphaam = scaledabundam[-3]
scaledabundem = scaledabund%>%filter(type == 1)
scaledabundem = scaledabundem[-3]

fit1 = lm(abundance~ .,scaledalphaam, na.action = na.exclude)
fit2 = lm(abundance~ .,scaledabundem, na.action = na.exclude)

rsquared2 = summary(fit2)$r.squared
pred = predict(fit2)
rmse = rmse(actual = scaledalphaem$abundance, predicted = pred)

plot_summs(fit1,fit2,ci_level = 0.9, model.names = c("am","em"))+
  ggtitle("Plot abundance, stratified by plot type")+
  theme_grey(base_size = 22)
######################################################################
# clean by stratifying years                          #3 am/em/yr
data = woods[-c(1,2,10,11)]


scaled =  as.data.frame(apply(data[,-c(1,2,3,11)],2, rescale))
scaled$inv = scaled$invtreeprop+scaled$percentinvcover
scaled$am = scaled$amtreeprop+scaled$percentamcover
scaled = scaled[-c(1,2,5,6)]

scaledalpha =  as.data.frame(cbind(data$alpha,data$Yr,data$NVCtype, scaled))

scaledalpha = scaledalpha%>%dplyr::rename(alpha = "data$alpha",
                                          type="data$NVCtype",
                                          yr = "data$Yr")

#stratify am.em plots
scaledalphaamyr1 = scaledalpha%>%filter(type == -1 & yr ==1)           
scaledalphaamyr1 = scaledalphaamyr1[-c(2,3)]
scaledalphaamyr2 = scaledalpha%>%filter(type == -1 & yr ==2)
scaledalphaamyr2 = scaledalphaamyr2[-c(2,3)]
scaledalphaemyr1 = scaledalpha%>%filter(type == 1 & yr ==1)
scaledalphaemyr1 = scaledalphaemyr1[-c(2,3)]
scaledalphaemyr2 = scaledalpha%>%filter(type == 1 & yr == 2)
scaledalphaemyr2 = scaledalphaemyr2[-c(2,3)]



fit1 = lm(alpha~ .,scaledalphaamyr1, na.action = na.exclude)
fit2 = lm(alpha~ .,scaledalphaamyr2, na.action = na.exclude)
fit3 = lm(alpha~ .,scaledalphaemyr1, na.action = na.exclude)
fit4 = lm(alpha~ .,scaledalphaemyr2, na.action = na.exclude)

plot_summs(fit1,fit2,fit3,fit4, ci_level = 0.9, model.names = c("AM Yr1","AM Yr2","EM Yr 1","EM Yr 2"))+
  ggtitle("Plot richness, stratified by plot type and year")+
  theme_grey(base_size = 22)

##############
#same as above for abundance
data = woods[-c(1,2,10,11)]


scaled =  as.data.frame(apply(data[,-c(1,2,3,11)],2, rescale))
scaled$inv = scaled$invtreeprop+scaled$percentinvcover
scaled$am = scaled$amtreeprop+scaled$percentamcover
scaled = scaled[-c(1,2,5,6)]

scaledabund =  as.data.frame(cbind(data$herbcover,data$Yr,data$NVCtype, scaled))

scaledabund = scaledabund%>%dplyr::rename(abundance = "data$herbcover",
                                          type="data$NVCtype",
                                          yr = "data$Yr")
#stratify am.em plots
scaledabundamyr1 = scaledabund%>%filter(type == -1 & yr ==1)           
scaledabundamyr1 = scaledabundamyr1[-c(2,3)]

scaledabundamyr2 = scaledabund%>%filter(type == -1 & yr ==2)
scaledabundamyr2 = scaledabundamyr2[-c(2,3)]

scaledabundemyr1 = scaledabund%>%filter(type == 1 & yr ==1)
scaledabundemyr1 = scaledabundemyr1[-c(2,3)]
scaledabundemyr2 = scaledabund%>%filter(type == 1 & yr == 2)
scaledabundemyr2 = scaledabundemyr2[-c(2,3)]



fit1 = lm(abundance~ .,scaledabundamyr1, na.action = na.exclude)
fit2 = lm(abundance~ .,scaledabundamyr2, na.action = na.exclude)
fit3 = lm(abundance~ .,scaledabundemyr1, na.action = na.exclude)
fit4 = lm(abundance~ .,scaledabundemyr2, na.action = na.exclude)

plot_summs(fit1,fit2,fit3,fit4, ci_level = 0.9, model.names = c("AM Yr1","AM Yr2","EM Yr 1","EM Yr 2"))+
  ggtitle("Plot abundance, stratified by plot type and year")+
  theme_grey(base_size = 22)

######################################################################

#clean up the signal by removing the invaded plots, what would effect of
#amshrub be in an uninvaded woodland

data = woods[-c(1,2,10,11)]%>%filter(percentinvcover<0.2)  #4am/em/yr - univaded richness


scaled =  as.data.frame(apply(data[,-c(1,2,3,11)],2, rescale))
scaled$inv = scaled$invtreeprop+scaled$percentinvcover
scaled$am = scaled$amtreeprop+scaled$percentamcover
scaled = scaled[-c(1,2,5,6)]

scaledalpha =  as.data.frame(cbind(data$alpha,data$Yr,data$NVCtype, scaled))

scaledalpha = scaledalpha%>%dplyr::rename(alpha = "data$alpha",
                                          type="data$NVCtype",
                                          yr = "data$Yr")

#stratify am.em plots
scaledalphaamyr1 = scaledalpha%>%filter(type == -1 & yr ==1)           
scaledalphaamyr1 = scaledalphaamyr1[-c(2,3)]
scaledalphaamyr2 = scaledalpha%>%filter(type == -1 & yr ==2)
scaledalphaamyr2 = scaledalphaamyr2[-c(2,3)]
scaledalphaemyr1 = scaledalpha%>%filter(type == 1 & yr ==1)
scaledalphaemyr1 = scaledalphaemyr1[-c(2,3)]
scaledalphaemyr2 = scaledalpha%>%filter(type == 1 & yr == 2)
scaledalphaemyr2 = scaledalphaemyr2[-c(2,3)]



fit1 = lm(alpha~ .,scaledalphaamyr1, na.action = na.exclude)
fit2 = lm(alpha~ .,scaledalphaamyr2, na.action = na.exclude)
fit3 = lm(alpha~ .,scaledalphaemyr1, na.action = na.exclude)
fit4 = lm(alpha~ .,scaledalphaemyr2, na.action = na.exclude)

plot_summs(fit1,fit2,fit3,fit4, ci_level = 0.9, 
           model.names = c("AM Yr1","AM Yr2","EM Yr 1","EM Yr 2"))+
  ggtitle("Plot richness, stratified by plot type and year, plots less then 20% invaded")

#####

#same as above for abundance
data = woods[-c(1,2,10,11)]%>%filter(percentinvcover<0.2)                #4am/em/yr - univaded abundance


scaled =  as.data.frame(apply(data[,-c(1,2,3,11)],2, rescale))
scaled$inv = scaled$invtreeprop+scaled$percentinvcover
scaled$am = scaled$amtreeprop+scaled$percentamcover
scaled = scaled[-c(1,2,5,6)]

scaledabund =  as.data.frame(cbind(data$herbcover,data$Yr,data$NVCtype, scaled))

scaledabund = scaledabund%>%dplyr::rename(abundance = "data$herbcover",
                                          type="data$NVCtype",
                                          yr = "data$Yr")
#stratify am.em plots
scaledabundamyr1 = scaledabund%>%filter(type == -1 & yr ==1)           
scaledabundamyr1 = scaledabundamyr1[-c(2,3)]

scaledabundamyr2 = scaledabund%>%filter(type == -1 & yr ==2)
scaledabundamyr2 = scaledabundamyr2[-c(2,3)]

scaledabundemyr1 = scaledabund%>%filter(type == 1 & yr ==1)
scaledabundemyr1 = scaledabundemyr1[-c(2,3)]
scaledabundemyr2 = scaledabund%>%filter(type == 1 & yr == 2)
scaledabundemyr2 = scaledabundemyr2[-c(2,3)]



fit1 = lm(abundance~ .,scaledabundamyr1, na.action = na.exclude)
fit2 = lm(abundance~ .,scaledabundamyr2, na.action = na.exclude)
fit3 = lm(abundance~ .,scaledabundemyr1, na.action = na.exclude)
fit4 = lm(abundance~ .,scaledabundemyr2, na.action = na.exclude)

plot_summs(fit1,fit2,fit3,fit4, ci_level = 0.9, model.names = c("AM Yr1","AM Yr2","EM Yr 1","EM Yr 2"))+
  ggtitle("Parameters effect sizes for abundance, stratified by plot type and year")

#what is pH range of uninvaded/invaded plots?
data = woods[-c(1,2,10,11)]%>%filter(percentinvcover<0.2)   
phdata = as.data.frame(cbind(sqrt(data$pH2),data$NVCtype))
phdata=phdata%>%dplyr::rename(pH = V1, plottype = V2)
ggplot(phdata,aes(x = as.factor(plottype), y = pH))+geom_violin(aes(fill = as.factor(plottype)))+
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10,1) )+
  labs(title ="pH across am  and em plot types, invasion < 0.2", x = "plot type")+
  theme_grey(base_size = 22)+
  theme(legend.position = "none")
  

################################################################

#finally, just take out em plots and inv and amount am combined
data = woods[-c(1,2,10,11)]%>%filter(percentinvcover<0.2)  #4am/em/yr - univaded richness


scaled =  as.data.frame(apply(data[,-c(1,2,3,11)],2, rescale))
scaled$inv = scaled$invtreeprop+scaled$percentinvcover
scaled$am = scaled$amtreeprop+scaled$percentamcover
scaled = scaled[-c(1,2,5,6)]

scaledalpha =  as.data.frame(cbind(data$alpha,data$Yr,data$NVCtype, scaled))

scaledalpha = scaledalpha%>%dplyr::rename(alpha = "data$alpha",
                                          type="data$NVCtype",
                                          yr = "data$Yr")
#just look at em plots
scaledalphaemyr1 = scaledalpha%>%filter(type == 1 & yr ==1)
scaledalphaemyr1 = scaledalphaemyr1[-c(2,3)]
scaledalphaemyr2 = scaledalpha%>%filter(type == 1 & yr == 2)
scaledalphaemyr2 = scaledalphaemyr2[-c(2,3)]

scaledalphaemyr1uninv = scaledalphaemyr1%>%filter(inv<0.2)
scaledalphaemyr2uninv = scaledalphaemyr2%>%filter(inv<0.2)

fit1 = lm(alpha~ inv+am,scaledalphaemyr1, na.action = na.exclude)
fit2 = lm(alpha~ inv+am,scaledalphaemyr2, na.action = na.exclude)
fit3 = lm(alpha~ inv+am,scaledalphaemyr1uninv, na.action = na.exclude)
fit4 = lm(alpha~ inv+am,scaledalphaemyr2uninv, na.action = na.exclude)


plot_summs(fit1,fit2,fit3,fit4, ci_level = 0.9, 
           model.names = c("Yr1 invaded", "Yr2 invaded", "Yr1 uninvaded", "Yr2 uninvaded"))+
  ggtitle("Plot richness, EM plots only")+
  theme_grey(base_size = 22)
######################

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


plot_summs(fit1,fit2,fit3,fit4, ci_level = 0.9, 
           model.names = c("Yr1 invaded", "Yr2 invaded", "Yr1 uninvaded", "Yr2 uninvaded"))+
  ggtitle("Plot abundance, EM plots only")+
  theme_grey(base_size = 22)
