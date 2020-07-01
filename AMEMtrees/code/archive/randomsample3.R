#this is rewrite of randomsample2 but with functions - so this can be easily repeated later
#plots the parameter effect sizes for ph,som.shading,amshrub and invshrub
#inputs are am or em or both, year and invasion level
#see randomsample4 for when you want to reduce the number of variables to just am/em
#and remove pH etc - because there are less data points in the model

#probably not good to use this as there may be few datapoints - so cant have 5 vars

#ACTUALLY USED FIRST RANDOMSAMPLE SCRIPT, NOT THIS

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
devtools::install_github("jacob-long/jtools")


#######################################

'%!in%' = function(x,y)!('%in%'(x,y))

########################################


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
########################################################

#select pot groups required
amem = c('W6','W7','W8','W9','W12','W13','W3','W4','W10','W11','W14','W15','W16','W17','W1')
am = c('W6','W7','W8','W9','W12','W13')
em = c('W3','W4','W10','W11','W14','W15','W16','W17','W1')

amplots = filter(woods, shortcodes %in% am)
emplots = filter(woods, shortcodes %in% em)
amemplots = filter(woods, shortcodes %in% amem)

###########################################################
getplots = function(type,yr,inv){
  if (type == "am"){
    plots = filter(woods, shortcodes %in% am)
  } else if (type == "em"){
    plots = filter(woods, shortcodes %in% em)
  } else {
    plots = filter(woods, shortcodes %in% amem)
  }
  #plots = plots[-c(2,5,10,11)]
  plots = plots%>%filter((percentinvcover+invtreeprop)<inv)
  plots = plots%>%filter(Yr==yr)
  plots = plots[-c(2,3,5,10,11)]
  plots = plots%>%group_by(Site)%>%group_split()
  return(plots)
}
##########################################################################

getaves = function(asite){
  # browser()
  testdf = data.frame(matrix(ncol = 8,nrow=100))
  colnames(testdf) = c("alpha","amtreeprop","invtreeprop","lba","som","percentamcover","percentinvcover", "pH2")
  
  for (i in 1:100){
    fiverows = sample_n(asite,5) #randomly select 5 rows
    meanph = mean(fiverows[[9]])
    thisrow = apply(fiverows[-c(1,9)], 2, sum)
    thisrow = c(thisrow,meanph)
    testdf[i,] = thisrow
  }
  #average that data
  finalrow = colMeans(testdf)
  return(finalrow)
}
########################################################################
#go through all sites and do the above
getsamples = function(siteslist){
  #browser()
  df = data.frame(matrix(ncol = 8,nrow = length(siteslist)))
  colnames(df)= c("alpha","amtreeprop","invtreeprop","lba","som","percentamcover","percentinvcover", "pH2")
  for (n in 1:length(siteslist)){
    site = siteslist[[n]]
    if (nrow(site)>5){
      thisrow = getaves(site)
      df[n,] = thisrow
    } else if (nrow(site)==5){
      thisrow = apply(site[-c(1)], 2, sum)
      df[n,] = thisrow
    } else {
      thisrow = vector(mode = 'numeric', length = 8)
      df[n,] = thisrow
    }
    
  }
  #remove rows where all values are zero - they were site with < 5 plots
  df = df[!apply(df, 1, function(x) all(x == 0)), ]
  return(df)
}
######################################################################

getfits = function(data){
  scaled =  apply(data[,-1],2, rescale)
  scaled = as.data.frame(cbind(data$alpha,scaled))
  inv = scaled$invtreeprop+scaled$percentinvcover
  am = scaled$amtreeprop+scaled$percentamcover
  modeldata = as.data.frame(cbind(data$alpha,inv,am,scaled$lba,scaled$pH2))
  modeldata = modeldata%>%dplyr::rename(alpha = V1,
                                        shading = V4,
                                        pH2 = V5)
  fit = lm(alpha ~ . ,modeldata, na.action = na.exclude) 
  return(fit)
}

######################################################################

#run the whole model
fit = function(type,yr,inv){
  plots = getplots(type,yr,inv)
  aved = getsamples(plots)
  fitted = getfits(aved)
  return(fitted)
}
###################################################################


  

fit71eminv = fit("em",1,1)
fit00eminv = fit("em",2,1)
fit71emuninv = fit("em",1,0.2)
fit00emuninv = fit("em",2,0.2)

plot_summs(fit71eminv,fit00eminv,fit71emuninv,fit00emuninv,ci_level = 0.9,
           model.names = c("EM Yr1 invaded","EM Yr 2 invaded","EM Yr 1 uninvaded","EM Yr2 uninvaded"))+
  ggtitle("Parameter effect sizes for EM plots, stritified by year and invasion level")

plot_summs(fitted)
summary(fit)$r.squared
