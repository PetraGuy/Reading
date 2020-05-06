#look at mycors in Or and A from lab observations
#this is the density plots for all the lab vars together

setwd("C:/dev/code/Reading/Woods2019/Code")
#get data
library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)
library(reshape)

data = read.csv("../data/labdatalong.csv", header = TRUE, stringsAsFactors = FALSE)

#coming in with weird colnames, no idea why
colnames(data) = c("site","meanls","meanrate","meancg", "morphs","propoc","horizon")

#may as well remove bd,ll and lw
removed = data[-c(2,12,13,21,31,32),]

#normalise so can compare in boxplots
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}
 

normdata = as.data.frame(apply(removed[c(2:6)],2,normalize))

#STICK SITE AND HORIZON BACK ON
normdata[c('horizon','site')] = removed[c('horizon','site')]

melted = melt(normdata)

ggplot(data = melted, aes(x = value, group = horizon, fill = horizon))+
  geom_density(adjust = 1.5, alpha = 0.4)+
  facet_wrap(~variable) +
  ylab("density")+
  xlab("normalised value")+
  theme(legend.position = c(0.85,0.2))
  )




