# plot a map
 setwd("C:/dev/code/Reading/WoodlandSurvey/Code")

#create map
library(dplyr)
mysites = c(74,56,57,46,73,78,33,32,30,79)
locations = read.csv("../Data/EastingNorthing.csv", stringsAsFactors = FALSE)
sitedata = locations%>%filter(SITE_NUM %in% mysites)%>%select(SITE_NUM,Lon, Lat)
colnames(sitedata) = c("site","long","lat")

UK <- map_data(map = "world", region = "UK") # changed map to "world"
ggplot(data = UK, aes(x = long, y = lat, group = group)) + 
  geom_polygon(alpha = 0.5) +
  scale_x_continuous(breaks = c(-6,-5,-4,-3,-2,-1,0,1))+
  scale_y_continuous(breaks = c(50,51,52,53,54,55,56,57,58),limits = c(50,58))+
  coord_map()+
  geom_point(data = sitedata, mapping = aes(x = lat, y = long),           # this data is file of long and lats
             inherit.aes = FALSE)+
  geom_text_repel(data = sitedata, aes(x = lat, y = long,label = site), 
                  inherit.aes = FALSE, size = 2, hjust = 0.5, vjust = 0.5)+
  theme(plot.margin=unit(c(0,0,0,0),"mm"))
