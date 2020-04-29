
# take the metoffice climate data for all the stations and work out 5 year averages
setwd("C:/dev/code/Reading/Woods2019/code")
library(readxl)


sheets = c("dunstaffnage","braemar","leuchars","paisley",
           "newtonrigg","whitby","aberporth","suttonbon",
           "cardiff","rossonwye","chivenor","yeovilton","heathrow",
           "cambs")

#create empty list
elm = matrix(NA,6,69)
metdata = rep(list(elm),14)

#read met data in
for (l in 1:length(sheets)) {
  metdata[[l]] = read_excel("../data/metofficeclimate.xlsx",  sheet = sheets[l])
  }


#calculate 5 year aves
aves = lapply(metdata,function(x) colMeans(x[,3:6], na.rm = TRUE) )

saveRDS(aves, "../data/5yearaves.RDS")
