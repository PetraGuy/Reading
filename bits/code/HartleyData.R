
library(dplyr)
library(ggplot2)
library(reshape2)

rawdata = read.csv("steve.csv")
data = rawdata%>%select(isolate,nitrogen.source,Radial.GR.mm.d)
SourceG = data%>%filter(nitrogen.source=="G")%>%select(isolate,Radial.GR.mm.d)
SourceN = data%>%filter(nitrogen.source=="N")%>%select(isolate,Radial.GR.mm.d)

colnames(SourceG)=c("isolate","growthrate")
colnames(SourceN)=c("isolate","growthrate")
Nsource = rep("N",60)
Gsource = rep("G",60)
SourceG = cbind(Gsource,SourceG)
SourceN = cbind(Nsource,SourceN)
colnames(SourceG)=c("nsource","isolate","growthrate")
colnames(SourceN)=c("nsource","isolate","growthrate")



g1 = ggplot(data = SourceG,aes(x = isolate, y = growthrate))+
  geom_boxplot()
g2 = ggplot(data = SourceN,aes(x = isolate, y = growthrate))+
  geom_boxplot()

alldata = rbind(SourceG,SourceN)

ggplot(data = alldata, aes(x=reorder(isolate,growthrate,FUN = median), y=growthrate)) + 
  geom_boxplot(aes(fill=nsource))+
  xlab("isolate")+
  ylab("growth rate (mm/d)")
