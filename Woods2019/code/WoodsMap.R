# plot a map
setwd("C:/dev/code/Reading/Woods2019/code")

#create map
library(dplyr)
 library(ggrepel)
 library(ggplot2)
 library(maps)
 library(mapproj)
 
mysites = read.csv("../data/woodslocs.csv", stringsAsFactors = TRUE, header = TRUE)
colnames(mysites)[1] = "site"
UKmap <- map_data(map = "world", region = "UK") # changed map to "world"
weather = read.csv("../data/metofficelocations.csv", header = TRUE,stringsAsFactors = TRUE)

ggplot(data = UKmap, aes(x = long, y = lat, group = group)) + 
  geom_polygon(alpha = 0.5) +
  scale_x_continuous(breaks = c(-6,-5,-4,-3,-2,-1,0,1),limits = c(-7,2))+
  scale_y_continuous(breaks = c(50,51,52,53,54,55,56,57,58),limits = c(50,59))+
  coord_map()+
  geom_point(data = mysites, mapping = aes(x = long, y = lat), # this data is file of long and lats
             inherit.aes = FALSE,color = "red")+
  geom_text_repel(data = mysites, aes(x = long, y = lat,label = site), 
                  inherit.aes = FALSE, size = 4, hjust = 0.5, vjust = 0.5,color = "red")+
  theme(plot.margin=unit(c(0,0,0,0),"mm"))+
  geom_point(data = weather, mapping = aes(x = long, y = lat), # this data is file of long and lats
           inherit.aes = FALSE)+
  geom_text_repel(data = weather, aes(x = long, y = lat,label = name), 
                  inherit.aes = FALSE, size = 2, hjust = 0.5, vjust = 0.5)+
  theme(plot.margin=unit(c(0,0,0,0),"mm"))



#dont know what this is for
#weather = data.frame(name = character(),long = numeric(),lat = numeric())

#polot weather stations on their own
ggplot(data = UKmap, aes(x = long, y = lat, group = group)) + 
  geom_polygon(alpha = 0.5) +
  scale_x_continuous(breaks = c(-6,-5,-4,-3,-2,-1,0,1),limits = c(-7,2))+
  scale_y_continuous(breaks = c(50,51,52,53,54,55,56,57,58),limits = c(50,59))+
  coord_map()+
  geom_point(data = weather, mapping = aes(x = long, y = lat), # this data is file of long and lats
             inherit.aes = FALSE)+
  geom_text_repel(data = weather, aes(x = long, y = lat,label = name), 
                  inherit.aes = FALSE, size = 4, hjust = 0.5, vjust = 0.5)+
  theme(plot.margin=unit(c(0,0,0,0),"mm"))

