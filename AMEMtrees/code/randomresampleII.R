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
library(purrr)
devtools::install_github("jacob-long/jtools")


#######################################

'%!in%' = function(x,y)!('%in%'(x,y))

########################################



#get data
alldata = read.csv('../data/all2data.csv')
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
  if (nrow(site) < 6) {
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
getsites = function(yr, invlevel){
  sites = amplots%>%filter(alpha>invlevel)%>%filter(Yr==yr)%>%group_by(Site)%>%group_split()
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
  varsums = apply(varplots[-c(1,2,3,5,6,7,8,9,10,13,14)],2,sum)
  row = c(numherbs,varsums)
  return(row)
}

##############################################


getaves = function(asite,yr){
  testdf = data.frame(matrix(ncol = 6,nrow=100))
  colnames(testdf) = c("alpha","lba","som","pH2","am","inv")
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
  df = data.frame(matrix(ncol = 6,nrow = length(siteslist)))
  colnames(df)= c("alpha","lba","som","pH2","am","inv")
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


getfits = function(data){
  #lose most of the columns, change here for how many vars required
  trimmed = data[-c(2,3,4,6)] #this leaves richnes, in and am
   #scaled =  apply(data[,-1],2, rescale)         #do we need to scale when is%cover?
   #scaled = as.data.frame(cbind(data$alpha,scaled))
  #inv = scaled$Propinvtree+scaled$propInvcover
  #am = scaled$propAMtree+scaled$propAMcover
  #modeldata = as.data.frame(cbind(data$alpha,inv,am))
 # modeldata = modeldata%>%dplyr::rename(alpha = V1)
  fit = lm(alpha ~ am ,trimmed, na.action = na.exclude) 
  #summary(fit)$r.squared
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

model1 = runall(1,4)
model2 = runall(1,1)
model3 = runall(2,4)
model4 = runall(2,1)



plot_summs(model1,model2,model3,model4, ci_level = 0.8, model.names = c('Yr1, inv1','Yr1 inv2','Yr2 inv1','Yr2 inv2'))+
  ggtitle("Site richness AM plots only")+
  theme_grey(base_size = 22)
