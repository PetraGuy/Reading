
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
alldata = read.csv('../data/alldata.csv')
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

em = c('W3','W4','W10','W11','W14','W15','W16','W17','W1')

emplots = filter(woods, shortcodes %in% em)

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
  sites = emplots%>%filter(percentinvcover<invlevel)%>%filter(Yr==yr)%>%group_by(Site)%>%group_split()
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
  varsums = apply(varplots[-c(1,2,3,4,10,11)],2,sum)
  row = c(numherbs,varsums)
  row = 
  return(row)
}

##############################################


getaves = function(asite,yr){
  testdf = data.frame(matrix(ncol = 9,nrow=500))
  colnames(testdf) = c("alpha","abundance","propAMtree", "Propinvtree",
                       "lba","som","propAMcover","propInvcover","pH2")
  for (i in 1:500){
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
  df = data.frame(matrix(ncol = 9,nrow = length(siteslist)))
  colnames(df)= c("alpha","abundance","propAMtree", "Propinvtree",
                  "lba","som","propAMcover","propInvcover","pH2")
    
  df = df[!apply(df, 1, function(x) all(x == 0)), ]
  return(df)
}
#############################################


getfits = function(data){
  #lose most of the columns, change here for how many vars required
  trimmed = data[-c(1,5,6,9)] #this leaves richnes, in and am
  scaled =  apply(trimmed[,-1],2, rescale)         #do we need to scale when is%cover?
  scaled = as.data.frame(cbind(data$alpha,scaled))
  inv = scaled$Propinvtree+scaled$propInvcover
  am = scaled$propAMtree+scaled$propAMcover
  modeldata = as.data.frame(cbind(data$abundance,inv,am))
  modeldata = modeldata%>%dplyr::rename(abund = V1)
  fit = lm(abund ~ . ,modeldata, na.action = na.exclude) 
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

model1 = runall(1,0.1)
model2 = runall(1,0.2)
model3 = runall(1,0.3)
model4 = runall(2,0.1)
model5 = runall(2,0.2)
model6 = runall(2,0.3)


plot_summs(model1,model2,model3,model4,model5,model6,ci_level = 0.8,
           model.names = c("yr1,inv0.1","yr1,inv0.2","yr1,inv0.3","yr2,inv0.1","yr2,inv0.2","yr3,inv0.3"))+
  ggtitle("Site abundance EM plots only")+
  theme_grey(base_size = 22)

###################################
#just look at richness/abund vs amtree/shrubs


trimmed = data[-c(2,5,6,9)] #this leaves richnes, in and am
# scaled =  apply(trimmed[,-1],2, rescale)         #do we need to scale when is%cover?
# scaled = as.data.frame(cbind(data$alpha,scaled))
inv = trimmed$Propinvtree+trimmed$propInvcover
am = trimmed$propAMtree+trimmed$propAMcover
modeldata = as.data.frame(cbind(data$alpha,inv,am))
modeldata = modeldata%>%dplyr::rename(alpha = V1)
fit = lm(alpha ~am,modeldata, na.action = na.exclude) 
ggplot(modeldata, aes(x = am, y = alpha))+
  geom_point()+
  geom_smooth(method = lm, se = F)
  


trimmed = data[-c(1,5,6,9)] #this leaves abund, in and am
# scaled =  apply(trimmed[,-1],2, rescale)         #do we need to scale when is%cover?
# scaled = as.data.frame(cbind(data$alpha,scaled))
inv = trimmed$Propinvtree+trimmed$propInvcover
am = trimmed$propAMtree+trimmed$propAMcover
modeldata = as.data.frame(cbind(data$abundance,inv,am))
modeldata = modeldata%>%dplyr::rename(abund = V1)
fit = lm(abund ~ am ,modeldata, na.action = na.exclude) 
summary(fit)$r.squared
ggplot(modeldata, aes(x = am, y = abund))+
  geom_point()+
  geom_smooth(method = lm, se = F)


