
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
getsites = function(yr, invlevel,mycor){
  mycor = get(mycor)
  sites = mycor%>%filter(alpha >invlevel)%>%filter(Yr==yr)%>%group_by(Site)%>%group_split() 
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


getdata = function(yr,invlevel,plotype){
  sitelist = getsites(yr,invlevel,plotype)
  samples = getsamples(sitelist,yr)
  return(samples)
}



ggplotRegression <- function (fit,label) {
  
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("R2 = ",signif(summary(fit)$r.squared, 2),
                       "Intercept =",signif(fit$coef[[1]],2 ),
                       " Slope =",signif(fit$coef[[2]], 2),
                       " P =",signif(round(summary(fit)$coef[2,4], 3),2)),
         subtitle = label) +
         xlab ("proportion AM trees and shrubs" )+
         ylab("herb richness") +
    theme(plot.title = element_text(size = 14))
        
}


#run each of these to get the data for each plot type and each yea  r
am1data = getdata(1,1,"amplots")
am2data = getdata(2,1,"amplots")
em1data = getdata(1,2,"emplots")
em2data = getdata(2,1,"emplots")


#run these to do linear plots of richness in EM/AM plots year 1 and 2
data = as.data.frame(am1data[,c(1,5)])
fit =  lm(alpha ~ am ,data, na.action = na.exclude) 
gam1 = ggplotRegression(fit,"AM plots year 1")

data = as.data.frame(am2data[,c(1,5)])
fit =  lm(alpha ~ am ,data, na.action = na.exclude) 
gam2 = ggplotRegression(fit,"AM plots year 2")

data = as.data.frame(em1data[,c(1,5)])
fit =  lm(alpha ~ am ,data, na.action = na.exclude) 
gem1 = ggplotRegression(fit,"EM plots year 1")

data = as.data.frame(em2data[,c(1,5)])
fit =  lm(alpha ~ am ,data, na.action = na.exclude) 
gem2 = ggplotRegression(fit,"EM plots year 2")

library(gridExtra)
grid.arrange(gam1,gam2,gem1,gem2,ncol = 2)
