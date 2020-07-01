
#for 2 vars linear mod with random resampling
#inv and am are combined at the beginning here, so inv < 0.2
#means inv trees + inv shrub, so results will be different to randomsample.R

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

#am = c('W6','W7','W8','W9','W12','W13')
em = c('W3','W4','W10','W11','W14','W15','W16','W17','W1')

#amplots = filter(woods, shortcodes %in% am)
emplots = filter(woods, shortcodes %in% em)

#remove vars I dont need
emplots = emplots[-c(2,5,8,9,10,11,14)]

#lets add the percentam+amshrub etc now
emplots$am = emplots$amtreeprop+emplots$percentamcover
emplots$inv = emplots$invtreeprop + emplots$percentinvcover

emplots = emplots[-c(4,5,6,7)]


###################################
getplots = function(yr,invlevel){
  plots = emplots%>%filter(inv<invlevel)
  plots = plots%>%filter(Yr==yr)
  plots = plots%>%group_by(Site)%>%group_split()
  return(plots)
}
#############

getaves = function(asite){
  # browser()
  testdf = data.frame(matrix(ncol = 3,nrow=100))
  colnames(testdf) = c("alpha","am","inv")
  
  for (i in 1:100){
    fiverows = sample_n(asite,5) #randomly select 5 rows
    thisrow = apply(fiverows[-c(1,2)], 2, sum)
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
  df = data.frame(matrix(ncol = 3,nrow = length(siteslist)))
  colnames(df)= c("alpha","am","inv")
  for (n in 1:length(siteslist)){
    site = siteslist[[n]]
    if (nrow(site)>5){
      thisrow = getaves(site)
      df[n,] = thisrow
    } else if (nrow(site)==5){
      thisrow = apply(site[-c(1,2)], 2, sum)
      df[n,] = thisrow
    } else {
      thisrow = vector(mode = 'numeric', length = 3)
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
  scaled = scaled%>%dplyr::rename(alpha = V1)
  fit = lm(alpha ~ . ,data, na.action = na.exclude) 
  return(fit)
}

######################################################################

#run the whole model
fit = function(yr,invlevel){
  plots = getplots(yr,invlevel)
  aved = getsamples(plots)
  fitted = getfits(aved)
  return(fitted)
}
###############################################################


fit1 = fit(1,1)
fit2 = fit(1,0.2)
fit3 = fit(2,1)
fit4 = fit(2,0.2)

plot_summs(fit1,fit2,fit3,fit4,inner_ci_level = 0.8,
           model.names = c("1971 invaded","2000 invaded","1971 uninvaded","2000 uninvaded"))
