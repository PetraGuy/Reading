#using CEH data see if plots with AM trees are more diverse
library(dplyr)
setwd("C:/dev/code/Reading/WoodlandSurvey/Code")

#BRC codes and whether am/em tree
plants = read.csv("../Data/vegetation_codes.csv")

#just take out trees from tis
trees_mych = plants%>%filter(Type == "t")%>%select(BRC.number, Sym)

#BRC has .0 on end of number - need to remove this
trees_mych$BRC.number = gsub("\\..*","", trees_mych$BRC.number)
#sort out col types 
trees_mych$brc = as.numeric(trees_mych$brc)

#complete data, remove richness site and plot from these
plotrichness = read.csv("../Data/AllPlotsVarsRichness.csv")
siterichness = read.csv("../Data/CompleteSiteLevelVars.csv")


#need to get data showing which plots have am/em trees by using this data
#this is just the tree data, not herbs
#might remove small rees as they represent AM pool
trees = read.csv("../Data/table_DBH_live_counts_71-03.csv")


#remove small trees - take DBH class >2
#just try year 2 for DBH class > 2
trees_yr2 = trees%>%filter(DBH_class>2 & Yr == 2)%>%select(SITE,PLOT,BRC_number)

#tidy column names
colnames(trees_mych) = c("brc","mychor")
colnames(trees_yr2) = c("site","plot","brc")


#some codes are not trees, remove
not_trees = c(9205472,9206,9209,920180,920840,9201771,9202046,9202205,
              9201104,9201241,9201103,8201254,921640)
"%w/o%" <- function(x, y) !x %in% y
trees_yr2 = trees_yr2%>%filter(brc %w/o% not_trees)



trees_with_mych = semi_join(trees_yr2,trees_mych, by = "brc", all.x = TRUE)
#ps -there is 1 record difference here, dont know why

