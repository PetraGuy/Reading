

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

em = c('W3','W4','W10','W11','W14','W15','W16','W17','W1')

emplots = filter(woods, shortcodes %in% em)

#remove vars I dont need
emplots = emplots[-c(2,8,9,10,11,14)]

#remove invaded plot
emplotsuninv = emplots%>%filter(percentinvcover<0.2)

#scale
scaled =  as.data.frame(apply(emplotsuninv[,-c(1,2,3,4)],2, rescale))
inv = scaled$invtreeprop+scaled$percentinvcover
am = scaled$amtreeprop+scaled$percentamcover

scaledalpha = as.data.frame(cbind(emplotsuninv$Site,emplotsuninv$Yr,emplotsuninv$alpha,inv,am))
scaledabund = as.data.frame(cbind(emplotsuninv$Site,emplotsuninv$Yr,emplotsuninv$herbcover,inv,am))

#rename cols
scaledalpha = scaledalpha%>%dplyr::rename(site = "V1",
                                          yr="V2",
                                          alpha = "V3")
scaledabund = scaledabund%>%dplyr::rename(site = "V1",
                                          yr="V2",
                                          abundance = "V3")
#split by year
scaledalpha1 = scaledalpha%>%filter(yr==1)
scaledalpha2 == scaledalpha%>%filter(yr==2)

scaledalpha1list = scaledalpha1%>%group_by(site)%>%group_split

#look at distribution of number of plots per wood
n = sapply(scaledalpha1list,nrow)
ggplot()+geom_histogram(aes(n))+scale_y_discrete(lims(0,16))


