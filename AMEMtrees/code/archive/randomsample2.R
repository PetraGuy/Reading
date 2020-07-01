#messy code see random samples 3 and 4

# try random resampling to get richness and other vars for sites which have
#different number plots in
#because I have deleted lost of plots - am, open etc, sites now no longer have
#same number. Pick a number of plots to integrate over - say 5? then sum the cover, richness
#etc over these 5. But for sites with more than 5 - rendomly resample and average

#moving things up to site level, but need tolook at EM plots only,
#coz AM shrub cover not imprtoant in AM plots
#and consider how many  plots in each wood - wont be the same

#this is same as previous, but I kept other vars in - incase they change the effects of interest

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
plots = amemplots

#remove vars I dont need
plots = plots[-c(2,5,10,11)]
###########################################################



#################################################################

#check ho many plots you will end up with if you want
#count the number of plots so those sites with <5 can be removed
countuninvaded = emplotsuninvaded%>%group_by(Yr,Site)%>%summarise(length(Site))
countuninvaded = countuninvaded%>%dplyr::rename(numplots = "length(Site)")

#check how many sites each year will fit these criteria
validsites = countuninvaded%>%filter(numplots>5)%>%filter(Yr==1)
###############################################################


#remove very invaded plots
plotsuninvaded = plots%>%filter(percentinvcover<0.2)

#split into years again, reduce number of vars as few rows
plotsyr1 = plotsuninvaded%>%filter(Yr==1)
plotsyr1 = plotsyr1[-c(2)]
plotsyr2 = plotsuninvaded%>%filter(Yr==2)
plotsyr2 = plotsyr2[-c(2)]


#make the sites into a list and loop thorugh list
plotsyr1list = plotsyr1%>%group_by(Site)%>%group_split()
plotsyr2list = plotsyr2%>%group_by(Site)%>%group_split()


###########################################################################
getplots = function(type,yr,inv){
  if (type == "am"){
    plots = filter(woods, shortcodes %in% am)
  } else if (type == "em"){
    plots = filter(woods, shortcodes %in% em)
  } else {
    plots = filter(woods, shortcodes %in% amem)
  }
  plots = plots[-c(2,5,10,11)]
  plots = plots%>%filter(percentinvcover<inv)
  plots = plots%>%filter(Yr==yr)
  
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
###########################################################################

#go through all sites and do the above
getsamples = function(siteslist){
  #browser()
  df = data.frame(matrix(ncol = 8,nrow = length(siteslist)))
  colnames(df)= c("alpha","amtreeprop","invtreeprop","lba","som","percentamcover","percentinvcover", "pH2")
  for (n in 1:length(siteslist)){
    site = siteslist[[n]]
    if (nrow(site)>7){
      thisrow = getaves(site)
      df[n,] = thisrow
    } else if (nrow(site)==7){
      thisrow = apply(site[-c(1)], 2, sum)
      df[n,] = thisrow
    } else {
      thisrow = vector(mode = 'numeric', length = 8)
      df[n,] = thisrow
    }
  }
  return(df)
}
######################################################################

randomavesyr1 = getsamples(plotsyr1list)
randomavesyr2 = getsamples(plotsyr2list)

#remove rows where all values are zero - they were site with < 5 plots
randomavesyr1complete = randomavesyr1[!apply(randomavesyr1, 1, function(x) all(x == 0)), ]
randomavesyr2complete = randomavesyr2[!apply(randomavesyr2, 1, function(x) all(x == 0)), ]

########################################################################

#linear model for these large site-level data
data = randomavesyr1complete

scaled =  apply(data[,-1],2, rescale)
scaled = as.data.frame(cbind(data$alpha,scaled))

#modeldata = scaled%>%dplyr::rename(alpha = V1) #this for 4 vars

#to add inv and am vars together and have 2 vars only,omit this for 4 vars
inv = scaled$invtreeprop+scaled$percentinvcover
am = scaled$amtreeprop+scaled$percentamcover
modeldata = as.data.frame(cbind(data$alpha,inv,am,scaled$lba,scaled$som,scaled$pH2))
modeldata = modeldata%>%dplyr::rename(alpha = V1,
                                      shading = V4,
                                      som = V5,
                                      pH2 = V6)

fit = lm(alpha ~ . ,modeldata, na.action = na.exclude) 

rsquared = summary(fit)$r.squared
pred = predict(fit)
rmse = rmse(actual = data$alpha,predicted = pred)


plot_summs(fit1971inv,ci_level = 0.25, model.names = c("1971"))+ggtitle("2000 invaded")

############################################

getfits = function(data){
  scaled =  apply(data[,-1],2, rescale)
  scaled = as.data.frame(cbind(data$alpha,scaled))
  inv = scaled$invtreeprop+scaled$percentinvcover
  am = scaled$amtreeprop+scaled$percentamcover
  modeldata = as.data.frame(cbind(data$alpha,inv,am,scaled$lba,scaled$som,scaled$pH2))
  modeldata = modeldata%>%dplyr::rename(alpha = V1,
                                        lba = V4,
                                        som = V5,
                                        pH2 = V6)
  fit = lm(alpha ~ . ,modeldata, na.action = na.exclude) 
  return(fit)
}
##################################################

fit1971inv = getfits(randomavesyr1complete)
fit2000inv = getfits(randomavesyr2complete)
fit1971uninv = getfits(randomavesyr1complete)
fit2000uninv = getfits(randomavesyr2complete)

plot_summs(fitted,ci_level = 0.5,
           model.names = c("1971 uninvaded","2000 uninvaded"))
