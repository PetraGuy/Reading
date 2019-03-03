
# modelling replicator equations for SUV vs compact car
library(reshape2)
library(ggplot2)
Ps = vector()
Pc = vector()

PiS = c(2,2)
PiC = c(0,3)
Ps[1] = 0.1 
Pc[1] = 0.9

for (i in 1:20){
 Ps[i+1] = sum(Ps[i]*PiS)/(sum(Ps[i]*PiS) + sum(Pc[i]*PiC))
 Pc[i+1] = sum(Pc[i]*PiC)/(sum(Ps[i]*PiS) + sum(Pc[i]*PiC))
}

t = c(1:21)
S = Ps
C = Pc
datatoplot = as.data.frame(cbind(t,S,C))

melted = melt(datatoplot, id ="t")
colnames(melted = )

ggplot(melted, aes(x = t, value, colour = variable))+
geom_line(size = 2)+
  theme(axis.text = element_text(size = 15))+
  theme(axis.title = element_text(size = 25))+
  ylab("proportions for each strategy")
  
