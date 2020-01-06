# plot a map
setwd("C:/dev/code/Woods2019/code")

#create map
library(dplyr)
 library(ggrepel)
 library(ggplot2)
 library(maps)
 library(mapproj)


#read in the data you want plotted 
mapdata = readRDS("../data/ndepseplonglat.RDS")


#get average ndep for map colour
mid = mean(mapdata$NH3_NH4)

#get a map of the UK
UKmap <- map_data(map = "world", region = "UK") 

ggplot(data = UKmap, aes(x = long, y = lat, group = group)) + 
  geom_polygon(alpha = 0.5) +
  scale_x_continuous(breaks = c(-6,-5,-4,-3,-2,-1,0,1),limits = c(-7,2))+
  scale_y_continuous(breaks = c(50,51,52,53,54,55,56,57,58),limits = c(50,59))+
  coord_map()+
  #put in datafile containg colnames long lat and give solor the colname of varible to map
  geom_point(data = mapdata, mapping = aes(x = long, y = lat, color = NH3_NH4), inherit.aes = FALSE)+
  theme(plot.margin=unit(c(0,0,0,0),"mm"))+
  #mid calculated above is mean of the data you are plotting. remove this for points
  scale_color_gradient2(midpoint=mid, low="blue", mid="white",
                        high="red", space ="Lab" )


#for a small amount of data where you want each point labelled. label is the col name for the labels
#geom_text_repel(data = mapdata, aes(x = long, y = lat,label = plotvalue), 
#               inherit.aes = FALSE, size = 4, hjust = 0.5, vjust = 0.5)+

