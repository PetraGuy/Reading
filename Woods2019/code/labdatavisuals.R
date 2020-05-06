setwd("C:/dev/code/Reading/Woods2019/code")

#messing about for some quick plots
#here is boxplots of grazed/ungrazed and mixed/single species with all lab vars
#also corr plot - also done in python


library(ggplot2)
library(ggcorrplot)
library(dplyr)
library(reshape2)


data =  read.csv('../data/woodprops.csv')
source('../../utilities/normdf.R')


#look at correlations of continuous variables - remove nas
source('../../utilities/contorcatdata.R')
cont = continous(data)
cat = categorical(data)
cat = cat%>%select(1,3,5,6)

#even smaller subset
cont_smaller = cont%>%select(1,3,6,10,21:32)

#stick site bcak on so can merge etc
site = data%>%select(1)


#normalise continuous data
norm = apply(cont_small, 2, normalize)
norm = cbind(site,norm)



#########################
source('../../utilities/corrplot.R')
corrplot(cont_smaller,'spearman','Spearman')



#################################
#what about correaltions of categorical data like mixed/sigle species. grazed etc
# look at grazing and mixed/single species
df = data%>%select('type2',"mean_lsOR","meanrateOR",
          "meancgOR", "morphsOR", "propoccOR", "mean_lsA",
          "meancgA","morphsA", "propoccA")

#look at grazed - just make df of grazed with other cont vars 
#and look at box plots

grazed = cat%>%select('grazed')
df = cbind(norm,grazed)

#get rid of na rows
df = df[complete.cases(df),]


#get rid of all the weather vars, just keep lab data
df = df[-c(1:9,24)]

melted = melt(df)

grazed = ggplot(data = melted, aes(x = variable, y = value, fill = grazed))+
  geom_boxplot()+
  theme(legend.position = c(0.85,0.2))

#above with mixed or single species
type = cat%>%select('type2')
df = cbind(norm,type)

#get rid of na rows
df = df[complete.cases(df),]

#get rid of all the weather vars, just keep lab data
df = df[-c(1:9,24)]

melted = melt(df)

type = ggplot(data = melted, aes(x = variable, y = value, fill = type2))+
  geom_boxplot()+
  theme(legend.position = c(0.85,0.2))

