

setwd("C:/dev/code/Reading/WoodlandSurvey/Code")
library(dplyr)
library(gridExtra)
library(ggrepel)
library(ggplot2)
library(reshape2)

plotdata = read.csv("../Data/AllPlotsVarsRichness.csv", stringsAsFactors = FALSE)
sitedata = read.csv("../Data/CompleteSiteLevelVars.csv")
sitevars = read.csv("../Data/SiteVars.csv")
sitenames = readRDS(("../Data/SiteNames.RDS"))
locations = read.csv("../Data/EastingNorthing.csv", stringsAsFactors = FALSE)
#get a df of long lats for each site

longlat = locations%>%select(SITE_NUM,Lon,Lat)
colnames(longlat) = c("site","long","lat")
longlat$site = as.factor(longlat$site)

w10 = plotdata%>%filter(ShortNVC == "W10")
w11 = plotdata%>%filter(ShortNVC == "W11")#none
w16 = plotdata%>%filter(ShortNVC == "W16")
w17 = plotdata%>%filter(ShortNVC == "W17") #none
w13 = plotdata%>%filter(ShortNVC == "W13")

# vector of all sites which contain w10 or w16 plots.
W10sites =  unique(w10$Site)
W16sites =  unique(w16$Site)


#make longdf of w10 and w16

w10label = rep("w10", length(W10sites))
w16label = rep("w16",length(W16sites))

w10df = as.data.frame(cbind(W10sites,w10label))
w16df = as.data.frame(cbind(W16sites,w16label))
#w10df$site = as.integer(w10df$site)
#w16df$site = as.integer(w16df$site)

colnames(w10df) = c("site","NVC")
colnames(w16df) = c("site","NVC")

w10w16long = as.data.frame(rbind(w10df,w16df), stringsAsFactors = FALSE)
#w10w16long$site = as.integer(w10w16long$site)

#no merge with long and lats

w10w16locs = inner_join(w10w16long,longlat, by  = "site")
#make this wide

w10w16wide = dcast(w10w16long,site~NVC)
w10w16wide$site =  as.numeric(as.character(w10w16wide$site))

#now select sites which have both w10 and w16

#bothNVC =  w10w16wide %>% filter(!is.na(w10) & !is.na(w16))


###########################################

#add other details to wide, long lat, aw site name, alt, area


#add locs to this 
alloaklocs = inner_join(w10w16wide,longlat, by  = "site")

# add whether any AW, altitude, Richness, PHI, buffers

other1 = sitevars%>%select(Site,AW, Area_ha, Alt_m)
colnames(other1) = c("site","aw","Area","Alt")

other2 = sitedata%>%select(Site,Richness,Pos_Hetero_Index)
other2$PHINorm = round(other2$Pos_Hetero_Index/(max(other2$Pos_Hetero_Index, na.rm = TRUE)),2)
colnames(other2) = c("site","Richness","PHI","PHINorm")
other2required = other2%>%select(site,Richness,PHINorm)

buffers = sitedata%>%select(Site,Buffer1,Buffer2,Buffer3)
buffers$totalbuffer = rowSums(buffers[2:4])
buffers$connectivity = round(buffers$totalbuffer/max(buffers$totalbuffer),2)
totalbuffer = as.data.frame(buffers[,c(1,6)])
colnames(totalbuffer) = c("site","connectivity")

#get site names
colnames(sitenames) = c("name","site")

#combine bothNVClocs with other1, other2 and names

w10w16wide1 = merge(alloaklocs,other1)
w10w16wide2 = merge(w10w16wide1,other2required)
w10w16wide3 = merge(w10w16wide2,totalbuffer)
w10w16wide4 = merge(w10w16wide3,sitenames)

saveRDS(w10w16wide4,"../Data/oaksitesdf.RDS") ############# start from here !!!!!!!!!!!!!!!!!!
write.csv(w10w16wide4,"../Data/oakplots.csv")

oaksites= readRDS("../Data/oaksitesdf.RDS")
#create map

scot = Oaksites%>%filter(long>56)

UK <- map_data(map = "world", region = "UK") # changed map to "world"
ggplot(data = UK, aes(x = long, y = lat, group = group)) + 
  geom_polygon(alpha = 0.5) +
  scale_x_continuous(breaks = c(-6,-5,-4,-3,-2,-1,0,1))+
  scale_y_continuous(breaks = c(50,51,52,53,54,55,56,57,58),limits = c(50,58))+
  coord_map()+
  geom_point(data = scot, mapping = aes(x = lat, y = long),
             inherit.aes = FALSE)+
  geom_text_repel(data = scot, aes(x = lat, y = long,label = site), 
            inherit.aes = FALSE, size = 2, hjust = 0.5, vjust = 0.5)+
  theme(plot.margin=unit(c(0,0,0,0),"mm"))
  


###### read in oak plots ##  restart from here, this is now the prepared data start point

oaksites =  readRDS("../Data/oaksitesdf.RDS")
 oaksites = oaksites[,-c(2,3)]
#######################################################

#look at box plots with outliers
 
data = oaksites
#replot without outliers because easier to read graphs
#whats distribution of PHI, take out outlier and NA, sites 23,25
#take out outlier in area, site74
data = oaksites%>%filter(!site %in% (c(23,25,74))) #witho

#####plotting for all data

plist <- lapply(data[c(5,6,7,8,9)], function (j)  ggplot(data, aes(x='',y=j))+
                   geom_boxplot()+
                   stat_summary(geom = "text", fun.y = quantile,
                                aes(label=sprintf("%1.2f", ..y..)),
                                position=position_nudge(x=0.4), size=3.5) )
 



grid.arrange(plist[[1]],plist[[2]],plist[[3]],plist[[4]],plist[[5]], ncol=3)


#get the quantiles cols 5:9 inc

quantdf = lapply(oaksites[c(5,6,7,8,9)], quantile, probs = c(0.25,0.75), na.rm=TRUE)


###### forget this, take out the 8 areas then select by hand

ScotW = oaksites %>% filter(long >= 55 & lat < -4)
write.csv(ScotW, "../Data/ScotW.csv")

ScotE =  oaksites %>% filter(long >= 55 & lat > -4)
write.csv(ScotE, "../Data/ScotE.csv")


EnglandNW =  oaksites %>% filter(long >= 53.5 & long <55 & lat < -2)
write.csv(EnglandNW, "../Data/EnglandNW.csv")

EnglandNE = oaksites %>% filter(long >= 53.5 & long < 55 & lat > -2)
write.csv(EnglandNE, "../Data/EnglandNE.csv")

West = oaksites %>% filter(long >= 51.5 & long <53.5 & lat < -2)
write.csv(West, "../Data/West.csv")


East = oaksites %>% filter(long > 51.5 & long <=53.5 & lat > -2)
write.csv(East, "../Data/East.csv")

SW = oaksites %>% filter(long < 51.5  & lat < -2)
write.csv(SW, "../Data/SW.csv")


SE = oaksites %>% filter(long < 51.5  & lat >-2)
write.csv(SE, "../Data/SE.csv")

Alldata = rbind(ScotW,ScotE,EnglandNW,EnglandNE,West,East,SW,SE)
write.csv(Alldata,"../Data/Oakssitesordered.csv")

Oaksperms = read.csv("../Data/OakSitesPerms.csv", stringsAsFactors = FALSE)
oaksperms2 =Oaksperms%>%filter(phoneemail == "y")


UK <- map_data(map = "world", region = "UK") # changed map to "world"
ggplot(data = UK, aes(x = long, y = lat, group = group)) + 
  geom_polygon(alpha = 0.5) +
  scale_x_continuous(breaks = c(-6,-5,-4,-3,-2,-1,0,1))+
  scale_y_continuous(breaks = c(50,51,52,53,54,55,56,57,58),limits = c(50,58))+
  coord_map()+
  geom_point(data = oaksperms2, mapping = aes(x = lat, y = long),
             inherit.aes = FALSE)+
  geom_text_repel(data = oaksperms2, aes(x = lat, y = long,label = site), 
                  inherit.aes = FALSE, size = 2, hjust = 0.5, vjust = 0.5)+
  theme(plot.margin=unit(c(0,0,0,0),"mm"))


