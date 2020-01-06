# select data from dfs according to matches in long lat
#I want to select, eg, Ndep data from large file where it corresponds to my site long lat
#used this nearest data table function as there werent exact macthed 
#therefore dplr filter methods didnt work


library(data.table)

setwd("C:/dev/code/Woods2019/code")

#locations of my sites
mysites = data.table(read.csv("../data/woodslocs.csv", stringsAsFactors = TRUE, header = TRUE))

#df of data to be extracted
extractdata = data.table(readRDS("../data/ndepseplonglat.RDS"))

#a data table thing
setkey(mysites,lat)
setkey(extractdata,lat)


extracted <- extractdata[mysites, roll = "nearest"]

#save ndep data

saveRDS(extracted, "../data/ndepsepbysite.RDS")
