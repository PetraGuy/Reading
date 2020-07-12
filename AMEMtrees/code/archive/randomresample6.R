# try random resampling to get richness and other vars for sites which have
#different number plots in
#because I have deleted lost of plots - am, open etc, sites now no longer have
#same number. Pick a number of plots to integrate over - say 5? then sum the cover, richness
#etc over these 5. But for sites with more than 5 - rendomly resample and average

#moving things up to site level, but need tolook at EM plots only,
#coz AM shrub cover not imprtoant in AM plots
#and consider how many  plots in each wood - wont be the same

#redid this to try a solve problem of samping across 7 so different to 5.
#keeping inv, pH and am


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
emplots = emplots[-c(2,5,8,9,10,11)] #inv still here for now as neet it to filter

#####################################################

getplots = function(yr, invlevel){
  plots = emplots%>%filter(percentinvcover<invlevel)
  plots = plots%>%filter(Yr==yr)
  plots = plots[-c(2)]
  plots = plots%>%group_by(Site)%>%group_split()
}
#####################################################


getaves = function(asite){
  # browser()
  testdf = data.frame(matrix(ncol = 6,nrow=100))
  colnames(testdf) = c("alpha","propAMtree", "Propinvtree","propAMcover","propInvcover","pH2")
  
  for (i in 1:100){
    fiverows = sample_n(asite,5,replace = T) #randomly select n rows
    thisrow = apply(fiverows[-c(1)], 2, sum)
    testdf[i,] = thisrow
  }
  #average that data, i.e. average the sums across the 5 rows over 100 times
  finalrow = colMeans(testdf)
  
  return(finalrow)
}
###########################################################################

#go through all sites and do the above
getsamples = function(siteslist){
  #browser()
  df = data.frame(matrix(ncol = 6,nrow = length(siteslist)))
  colnames(df)=c("alpha","propAMtree", "Propinvtree","propAMcover","propInvcover","pH2")
  for (n in 1:length(siteslist)){
    site = siteslist[[n]]
    if (nrow(site)>7){
      thisrow = getaves(site)
      df[n,] = thisrow
    } else if (nrow(site)==7){
      thisrow = apply(site[-c(1)], 2, sum)
      df[n,] = thisrow
    } else {
      thisrow = vector(mode = 'numeric', length = 6)
      df[n,] = thisrow
    }
  }
  df = df[!apply(df, 1, function(x) all(x == 0)), ]
  return(df)
}
######################################################################

getfits = function(data){
  scaled =  apply(data[,-1],2, rescale)
  scaled = as.data.frame(cbind(data$alpha,scaled))
  inv = scaled$Propinvtree+scaled$propInvcover
  am = scaled$propAMtree+scaled$propAMcover
  modeldata = as.data.frame(cbind(data$alpha,scaled$pH2,am,inv))
  modeldata = modeldata%>%dplyr::rename(alpha = V1,
                                        pH2 = V2)
  fit = lm(alpha ~ . ,modeldata, na.action = na.exclude) 
  return(fit)
}
###############################################

runall = function(yr,invlevel){
  data = getplots(yr,invlevel)
  samples = getsamples(data)
  fits = getfits(samples)
  return(fits)
}


model1 = runall(1,0.2)
model2 = runall(1,1)
model3 = runall(2,0.2)
model4 = runall(2,1)


plot_summs(model1,model2,model3,model4,ci_level = 0.9,
           model.names = c("1971 uninvaded","1971 invaded","2000 uninvaded","2000 invaded"))+
  ggtitle("Site richness EM plots only")+
  theme_grey(base_size = 22)
