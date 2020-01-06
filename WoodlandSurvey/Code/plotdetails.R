

setwd("C:/dev/code/Reading/WoodlandSurvey/Code")
library(dplyr)

Oaksites = readRDS("../Data/oaksitesdf.RDS")
plotvars = read.csv("../Data/AllPlotsVarsRichness.csv")
sitevars = read.csv("../Data/CompleteSiteLevelVars.csv")

#select oak plots from a site

#list plot data for a site

getplotdetials =  function(sitenum){
  sitedata = plotvars%>%filter(Site==sitenum)
  return(sitedata)
}



