
#double checks
#show there are >5 am or em plots in every wood



library(data.table)
library(dplyr)
library(ggplot2)
setwd("C:/dev/code/Reading/AMEMtrees/Code")

###################################################
#repeat code from randomvars etc to split int am and em plots
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
############################################################

#not sure i need this now, was cerating nice dfs to get plot
joindf = function(data){
  sites = as.data.frame(seq(from = 1, to =103, by = 1))
  colnames(sites) = "Site"
  joined = left_join(sites,data,by ='Site')
  joined[is.na(joined)]=0
  return(joined)
}

#split the data by am,em and yr
getcounts = function(data,yr){
  df = data%>% filter(Yr == yr)
  df <- data.table(df)
  df = df[, .N, by = list(Site)]
  df = joindf(df)
  df = as.data.frame(df[,-1])
  colnames(df) = 'count'
  return(df)
}


#get the number of sites with > numplots per site
getnum = function(data,num){
  num = data%>%filter(count>num)
  n = nrow(num)
  return(n)
}

am1 = getcounts(amplots,1)
getnum(am1,4)
##################################################

#which are the most common trees/shrubs

shrubstotals = read.csv('../data/shrubstotals.csv') #from invasiveanalysis.py
totaltrees = read.csv('../data/totaltrees.csv')

#shrubs#####################################

#plot everything above upper quartile
uq = quantile(shrubstotals$cover)[4]
df = shrubstotals%>%filter(cover>uq)

#normalise to max val
df$cover = df$cover/max(df$cover)

#separate yr1 and yr 2 and take top 15
df1 = df%>%filter(Yr==1)%>%top_n(15)
df2 = df%>%filter(Yr==2)%>%top_n(15)


#make longdf
data = rbind(df1,df2)
data$Yr =  as.factor(data$Yr)

ggplot(data = data, aes(x = reorder(species, -cover), y = cover, fill = Yr))+
 geom_bar(stat = "identity", position = 'dodge')+
  xlab('shrub and sapling species')+
  ylab('nromalised cover across all sites')+
  theme_grey(base_size = 15)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle('Top 15 most dominant shrubs')

#trees###############################################
#plot everything above upper quartile
#shrubs
uq = quantile(totaltrees$sumdbh)[4]
df = totaltrees%>%filter(sumdbh>uq)

#normalise to max val
df$sumdbh = df$sumdbh/max(df$sumdbh)

#separate yr1 and yr 2 and take top 15
df1 = df%>%filter(Yr==1)%>%top_n(20)
df2 = df%>%filter(Yr==2)%>%top_n(20)


#make longdf
data = rbind(df1,df2)
data$Yr =  as.factor(data$Yr)


ggplot(data = data, aes(x = reorder(BRC.names, -sumdbh), y = sumdbh, fill = Yr))+
  geom_bar(stat = "identity", position = 'dodge')+
  xlab('Tree species')+
  ylab('nromalised cover across all sites')+
  theme_grey(base_size = 15)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle('Top 15 most dominant trees')

##################
#look at SOM vs pH

alldata = read.csv('../../../CMEECourseWork/CMEEMainProject/Data/AllPlotsVarsRichness.csv')

SOMpH = as.data.frame(cbind(alldata$pHYr2,alldata$SOMYr2))
colnames(SOMpH)=c('pH','SOM')

g1= ggplot(SOMpH, aes(x = pH,y =SOM))+geom_point(na.rm = TRUE)+geom_smooth(method='lm')+
  theme_grey(base_size = 15)+
  ggtitle((''))

SOMLBA= as.data.frame(cbind(alldata$SOMYr2,alldata$LiveBasalAreaYr2))
colnames(SOMLBA)=c('SOM','shading')

g2 = ggplot(SOMLBA, aes(x = shading,y = SOM))+geom_point(na.rm = TRUE)+geom_smooth(method='lm')
library(gridExtra)

grid.arrange(g1,g2,ncol = 2)
############################

#try 1 3d plot of ph ~SOM,LBA
library(plot3D)

df = alldata[,c(6,7,9)]
df = df[complete.cases(df),] # remove rows with NAs

som = df$SOMYr2
shading = df$LiveBasalAreaYr2
ph = df$pHYr2


scatter3D(som,shading,ph, bty = "b2", colkey = FALSE, main ="bty= 'b2'" )



