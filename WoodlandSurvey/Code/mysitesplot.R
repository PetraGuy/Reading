
#plot my sites

setwd("C:/dev/code/Reading/WoodlandSurvey/Code")

library(ggplot2)
library(ggrepel)

UK <- map_data(map = "world", region = "UK") # changed map to "world"
ggplot(data = UK, aes(x = long, y = lat, group = group)) + 
  geom_polygon(alpha = 0.5) +
  scale_x_continuous(breaks = c(-6,-5,-4,-3,-2,-1,0,1))+
  scale_y_continuous(breaks = c(50,51,52,53,54,55,56,57,58),limits = c(50,58))+
  coord_map()+
  geom_point(data = mysites, mapping = aes(x = Lat, y = Long),
             inherit.aes = FALSE)+
  geom_text_repel(data = mysites, aes(x = Lat, y = Long,label = Site), 
                  inherit.aes = FALSE, size = 3, hjust = 0.5, vjust = 0.5)+
  theme(plot.margin=unit(c(0,0,0,0),"mm"))

mysites = read.csv("../Data/mysites.csv")
