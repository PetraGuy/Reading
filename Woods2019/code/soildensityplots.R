#look at soil density for soil ring density readings

setwd("C:/dev/code/Reading/Woods2019/Code")
#get data
library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)
data = read.csv("../data/soil_density.csv", header = TRUE, stringsAsFactors = FALSE)
                

#grouped by Site or soil
boxplot = ggplot(data, aes(x = Site, y = normalised_wt))+
  geom_boxplot(aes(fill = Site))+
  facet_grid(Site~soil)

boxplot = ggplot(data, aes(x = Site, y = normalised_wt))+
  geom_boxplot(aes(fill = soil))+
  facet_wrap(~type)

boxplot = ggplot(data, aes(x = Site, y = normalised_wt))+
  geom_boxplot(aes(fill = type))+
  facet_wrap(~soil)

#look at some clustering, but use a gower distance for categoricals - type and soil
library(cluster)
gower_distance = daisy(woods[,c])

df = data%>%select('site_total','soil')
df = df[complete.cases(df),]

ggplot(df,aes(x = soil, y = site_total))+
  geom_violin()


