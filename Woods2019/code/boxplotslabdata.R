#look at mycors in Or and A from lab observations
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
ggplot(melted, aes(x = site, y = value))+
  geom_boxplot(aes(fill = horizon))+
  facet_grid(rows = vars(site))

ggplot(melted, aes(x = variable, y = value))+
  geom_boxplot(aes(fill = horizon))+
  facet_wrap(vars(site))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


