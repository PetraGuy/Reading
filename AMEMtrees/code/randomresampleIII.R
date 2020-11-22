# try random resampling to get richness and other vars for sites which have
#different number plots in
#because I have deleted lost of plots - am, open etc, sites now no longer have
#same number. Pick a number of plots to integrate over - say 5? then sum the cover, richness
#etc over these 5. But for sites with more than 5 - rendomly resample and average

#moving things up to site level, but need tolook at EM plots only,
#coz AM shrub cover not imprtoant in AM plots
#and consider how many  plots in each wood - wont be the same

#THIS IS THE SCRIPT I AM USING FOR THE PLOTS - NOT RANDOMSAMPLE 3 OR 4
#this for alpha, repeated randomsampleabund for abundance


#redoing this because when you get the richness over the 5 plots, then richness is unique plants,
#lba is sum, pH is ave, SOM is ave, am is sum
#This script collects LBA SOM etc and AMand EM plots if you change getsites function.
#if inv level = 1 
#This starts as duplicate of IV, then reoving expl vars and just using AMplot

setwd("C:/dev/code/Reading/AMEMtrees/Code")
library(dplyr)
#library(rlang)
library(jtools)
library(devtools)
#library(nlme)
#library(lme4)
library(Metrics) #rmse
library(stringr)
library(arm) #standardize
library(ggplot2)
library(purrr) # for compact
devtools::install_github("jacob-long/jtools")


#######################################

'%!in%' = function(x,y)!('%in%'(x,y))

########################################



#get data
alldata = read.csv('../data/all2datasens3.csv') #from collatingdata.py
herbs = read.csv('../data/herbs.csv')

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


woods['am'] = woods[5]+woods[8]
woods['inv'] = woods[6]+woods[9]+woods[10]

em = c('W3','W4','W10','W11','W14','W15','W16','W17','W1')
amcodes = c('W6','W7','W8','W9','W12','W13','W1')

emplots = filter(woods, shortcodes %in% em)

woods = woods%>%mutate(NVCtype = if_else (shortcodes %in% amcodes, -1,1))


amplots = woods%>%filter(NVCtype == -1)
emplots = woods%>%filter(NVCtype == 1)

################################

deletesites = function(site){  #used in get plots
  if (nrow(site) < 5) {
    site = NULL
  } else {
    site = site
  }
  return(site)
}

#####################################################

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

##########################################
#retrns list of all sites for yr, invlevel where numplots per site >=5
#change to am/emplots depending what you want
#change to alpha or inv level filter depending what you want
#with invlevel = 1 and alpha > invlevel, it means select by richness not invasion and alpha>1 means just take all sites. 
getsites = function(yr, invlevel){
  sites = emplots%>%filter(alpha >invlevel)%>%filter(Yr==yr)%>%group_by(Site)%>%group_split() 
  reducedsites = compact(lapply(sites, deletesites))
  return(reducedsites)
  
}
#####################################################
#return the richness and the sum of other vars for a set of plots
getrichness = function(plots,site,yr){
  #browser()
  richplots = herbs%>%filter(Yr == yr)%>%filter(Site==site)
  richplots = filter(richplots, Plot%in% plots)
  numherbs = (count(distinct(richplots, Amalgams)))[[1]] #site richness
  varplots = woods%>%filter(Yr==yr)%>%filter(Site==site)
  varplots = filter(varplots, Plot%in% plots)            # the other vars for the site
  reducedvars = varplots[c(11,12,15,16)]
  varsums = apply(reducedvars[c(1,4)],2,sum)
  varaves = apply(reducedvars[c(2,3)],2,mean)
  row = c(numherbs,varsums,varaves)
  return(row)
}

##############################################

getaves = function(asite,yr){
  testdf = data.frame(matrix(ncol = 5,nrow=100))
  colnames(testdf) = c("alpha","lba","som","pH2","am")
  for (i in 1:100){
    fiverows = sample_n(asite,5,replace = F) #randomly select n rows
    site = asite[[1]][[1]]
    plots = fiverows[[2]]
    thisrow = getrichness(plots,site,yr) #get the richness of the 5 rows
    testdf[i,] = thisrow
  }
  #average that data, i.e. average the sums across the 5 rows over 100 times
  finalrow = colMeans(testdf)
  
  return(finalrow)
}

####################
#input all the sites for a yr and inv level and get the richness and other vars
getsamples = function(siteslist,yr){
  #browser()
  df = data.frame(matrix(ncol = 5,nrow = length(siteslist)))
  colnames(df)= c("alpha","lba","som","pH2","am")
  for (n in 1:length(siteslist)){
    site = siteslist[[n]]
    if (nrow(site)>5){
      thisrow = getaves(site,yr) # get data for random resample of 5 rows
      df[n,] = thisrow
    } else if (nrow(site)==5){
      site = siteslist[[n]][[1]][[1]]
      plots = siteslist[[n]][[2]]
      thisrow = getrichness(plots,site,yr)
      df[n,] = thisrow
    }
  }
  df = df[!apply(df, 1, function(x) all(x == 0)), ]
  return(df)
}
#############################################
#notice, not scaled for just richness with prop am because propam already relative and between 0-1

getfits = function(data){
  trimmed = data[c(1,5)] #this leaves richnes, in and am
  #scaled =  as.data.frame(apply(trimmed[-1],2, rescale))         #do we need to scale when is%cover?
  #modeldata = as.data.frame(cbind(data$alpha,scaled))
  #names(modeldata)[names(modeldata) == "data$alpha"] <- "alpha" # had to revert to base R for rename 
  fit = lm(alpha ~ am ,trimmed, na.action = na.exclude)      #because dplyr rename stopped working
  summary(fit)$r.squared
  return(fit)
}
###############################################

runall = function(yr,invlevel){
  sitelist = getsites(yr,invlevel)
  samples = getsamples(sitelist,yr)
  fits = getfits(samples)
  return(fits)
}
#############################################
#to change between em and am plots, change getsites
#quantiles for am 1,11,17,24,61. But only 2 sites at >24 so cant model
#quantiles for em: 1,7,12,19,65
model1 = runall(1,1) # input year and invasion level, this can be alpha if you change getsites
model2 = runall(2,1)
model3 = runall(1,1)
model4 = runall(2,1)


model5 = runall(2,7)
model6 = runall(2,12)

plot_summs(model1, model3,model2,model4, ci_level = 0.9, 
           model.names = c('Yr1 EMplots','Yr1 Amplots','Yr2 EMplots','Yr2 AMplots'))+
  ggtitle("Site richness in AM and EM plots for both survey years")+
  theme_grey(base_size = 18)+
  #theme(axis.text.y = element_blank(),
  #axis.ticks.y = element_blank())+
  xlab('standardised regression coefficient')+
  ylab('')
################################################
#just doing this part now, run sitelisnt and samples 
#change to yr 2, rerun, # change to emplots etc..
#to plot linear models of data
am1data = as.data.frame(samples[,c(1,5)])
am2data = as.data.frame(samples[,c(1,5)])
em1data = as.data.frame(samples[,c(1,5)])
em2data = as.data.frame(samples[,c(1,5)])

#redo fit so can put data on graph


ggplotRegression <- function (fit) {
  
    ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("R2 = ",signif(summary(fit)$r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(round(summary(fit)$coef[2,4], 5),2)))+
    xlab('proportion AM trees and shrubs')+
    ylab('herb richness')+
    annotate(geom="text", x=35, y=10, label="AM plots yr 1",
             color="black")
}

data = am1data
fit =  lm(alpha ~ am ,data, na.action = na.exclude) 
gam1 = ggplotRegression(fit)

data = am2data
fit =  lm(alpha ~ am ,data, na.action = na.exclude) 
gam2 = ggplotRegression(fit)

data = em1data
fit =  lm(alpha ~ am ,data, na.action = na.exclude) 
gem1 = ggplotRegression(fit)

data = em2data
fit =  lm(alpha ~ am ,data, na.action = na.exclude) 
gem2 = ggplotRegression(fit)

library(gridExtra)
grid.arrange(gam1,gam2,gem1,gem2,ncol = 2)
