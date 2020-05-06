
library(ggplot2)
library(GGally)
library(gridExtra)

normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}
normdata = as.data.frame(apply(data,2,normalize))
normdata = normdata[complete.cases(normdata),]


lm_eqn = function(data, col1,col2){
  df = cbind(data[col1],data[col2])
  colnames(df) = c("y","x")
  r2 = round(summary(lm(y ~ x, df))$r.squared,2)
  text = paste("R2 = ", r2) 
  plot = ggplot(df, aes(x = x))+
    geom_point(aes(y = y), colour = "black")+
    geom_smooth(method = "lm",aes(y = y), colour = "black")+
    annotate("text",x = 0.2, y = 0.9,label =text)+
    ylim(0,round(max(data[col1]),2))+
    ylab(col1) + xlab(col2)
  
  return(plot)
}

#do a few selected scatter plots, ggpairs is too much info
#get data tow ork on

data =  read.csv('../data/woodprops.csv')
################################################


#look at correlations of continuous variables - remove nas
source('../../utilities/contorcatdata.R')
cont = continous(data)

#might need to normalise so can see together
normdata = as.data.frame(apply(cont,2,normalize))


g1 = lm_eqn(normdata,"meancgOR","NdepTotal")

g2 = lm_eqn(normdata,"meancgA","rainfall.month.previous.to.sampling")

g3 = lm_eqn(normdata,"mean_lsA","elevation")

g4 = lm_eqn(normdata,"mean_lsA", "continuity")

g5 = lm_eqn(normdata,"morphsA","NdepTotal")

g6 = lm_eqn(normdata,"morphsA","continuity")

g7 = lm_eqn(normdata,"morphsOR","elevation")



#do some of soil density with lab data
g8 = lm_eqn(normdata,"propoccOR" ,"soil_density")

grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8, ncol = 4 )
