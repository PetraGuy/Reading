
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



#get data, #change this to all data, alldata2sens1/2/3 for sensitivity analysis
alldata = read.csv('../data/all2data.csv') #from collatingdata.py
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

#separate em and am designated plot types
amplots = woods%>%filter(NVCtype == -1)
emplots = woods%>%filter(NVCtype == 1)

################################
#following functions used to generate multivatriate linear model

#delete sites from analysis if they contain less than 5 plots
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
#returns list of all sites for yr, invlevel where numplots per site >=5
#yr = 1 or 2, inv level - 1 means all plots, mycor = "emplots" or "amplots"
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

#run the above functions
getdata = function(yr,invlevel,plotype){
  sitelist = getsites(yr,invlevel,plotype)
  samples = getsamples(sitelist,yr)
  return(samples)
}


#plot the linear regressions for richness/prop AM trees
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
######################################################################

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

############################################################################
# plots of SOM with pH and shading

ggplot(alldata, aes(y = som, x = pH))+
  geom_point()+
  geom_smooth(method = lm)+
  theme(text = element_text(size = 16))

ggplot(alldata, aes(y = som, x = lba))+
  geom_point()+
  geom_smooth(method = lm)+
  theme(text = element_text(size = 16))
###########################################################################

#plots of most common tree types

trees = read.csv("../data/Trees.csv") #get trees data
shrubs = read.csv("../data/shrubstotals.csv") #same for shrubs

#look at trees by count (looked same by cover)
#this to get counts
treescounts = as.data.frame(trees%>%dplyr::select(species, Count,Yr)%>%group_by(Yr,species)%>%tally())
#take only the top quartile
treescountstop = treescounts%>%top_n((25))
#turn Yr to factors to plot correctly
treescountstop$Yr = as.factor(treescountstop$Yr)
#normalise to look neater
treescountstop$n = treescountstop$n/max(treescountstop$n)
#remove salix forneatnesscoz not in both years so graph messy
treescountstop = treescountstop%>%filter(species != 'Salix seedling/sp')



#this to sort shrubs for plot
shrubs$Yr = as.factor(shrubs$Yr)
shrubstop = shrubs%>%top_n(40)
#normalise tolook neater
shrubstop$cover = shrubstop$cover/max(shrubstop$cover)
#tidy by removing tree saplings
saps = c('Acer pseudoplatanus','Betula seedling/sp','Castanea sativa','Quercus seedling/sp',
         'Sorbus aucuparia','Ligustrum vulgare','Sambucus nigra','Acer campestre',
         'Betula pubescens','Viburnum opulus')
shrubstop = filter(shrubstop, species %!in% saps)

#plot trees by count
ggplot(treescountstop, aes(reorder(species,-n),n))+
  geom_bar(aes(fill = Yr),position = "dodge",stat = "identity")+
  theme(axis.text.x = element_text(angle = 90))+
  ylab("relative frequency of tree counts")+
  xlab('tree species')+
  theme(text = element_text(size = 16))+
  ggtitle('Relative fequency for most common trees by counts')
             

#shrubs by cover
ggplot(shrubstop, aes(reorder(species,-cover),cover))+
  geom_bar(aes(fill = Yr),position = "dodge",stat = "identity")+
  theme(axis.text.x = element_text(angle = 90))+
  ylab("relative frequency of shrub cover")+
  xlab('shrub species')+
  theme(text = element_text(size = 16))+
  ggtitle('Relative fequency for most common shrubs by cover')

################################################################

#box plots of diversity with pH

df = woodsonly[c(4,13)]
#dfalpha2 <- dfalpha %>% mutate_at(c('alpha'), ~(normalize(.) %>% as.vector))

index = rep('rich', nrow(df))
df = cbind(df,index)
colnames(df) = c('value','pH','index')
data = df%>%mutate(bin = cut_width(pH,width = 0.5, boundary = 0))
g1 = ggplot(data = na.omit(data), aes(x=bin, y=alpha) ) +
  geom_boxplot(fill="#69b3a2") +
  xlab("pH")+
  ylab('alpha diversity')

##############################################################
#violin plots of pH on AM and Em plot types

pHam = amplots['pH']
index = rep('am',length(pHam))
df1 = cbind(pHam,index)

pHem = emplots['pH']
index = rep('em',length(pHem))
df2 = cbind(pHem,index)
thedata = rbind(df1,df2)


ggplot(thedata,aes(x = index, y = pH))+geom_violin(aes(fill = index))

##########################################################
#a correaltion between variables

library(corrplot)

#combine am and emplots into 1 df

data = rbind(amplots,emplots)
m = cor(data[,c(4,11,12,15,16)])
corrplot(m, method = "color", title = "All plot types")


#just look at emplots
m = cor(emplots[,c(4,11,12,15,16)])
corrplot(m, method = "color", title = "EM plots")

#just look at am plots
m = cor(amplots[,c(4,11,12,15,16)])

corrplot(m, method="color", title = "AM plots", type = 'lower')
         
         
######################################################

#how many am/em plot type in each woodland - are woods generally em dominant?
data = rbind(amplots,emplots)
df = data%>%group_by(Yr,Site,NVCtype)%>%tally()









