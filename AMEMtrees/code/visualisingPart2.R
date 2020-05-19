setwd("C:/dev/code/Reading/AMEMtrees/Code")
library(dplyr)
library(stringr)
library(tidyverse)
library(gridExtra)

normalize <- function(x) {
  norm = ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  
  return(norm)
}


#get data
data = read.csv('../data/year2dataA.csv')

#get rid of non woodland plots - anything that doesnt syart with W
woodsonly = data%>%filter(str_detect(nvc,"W"))


# Look at pH over am and em plots

am = c('W6','W7','W8','W9','W12','W13')
em = c('W3','W4','W10','W11','W14','W15','W16','W17')

amplots = filter(woodsonly, nvc %in% am)
emplots = filter(woodsonly, nvc %in% em)
#####

#pH across plots
pHam = amplots['pH']
index = rep('am',length(pHam))
df1 = cbind(pHam,index)

pHem = emplots['pH']
index = rep('em',length(pHem))
df2 = cbind(pHem,index)


data = rbind(df1,df2)


ggplot(data,aes(x = index, y = pH))+geom_violin(aes(fill = index))

#look at richness and abundance of em and am plots
amcover = amplots['cover']
index = rep('am',length(amcover))
df1 = cbind(amcover,index)

emcover = emplots['cover']
index = rep('em',length(emcover))
df2 = cbind(emcover,index)


data = rbind(df1,df2)

g1 = ggplot(data,aes(x = index, y = cover))+geom_violin(aes(fill = index))

#abundance

amalpha = amplots['alpha']
index = rep('am',length(amalpha))
df1 = cbind(amalpha,index)

emalpha = emplots['alpha']
index = rep('em',length(emalpha))
df2 = cbind(emalpha,index)


data = rbind(df1,df2)

g2 = ggplot(data,aes(x = index, y = alpha))+geom_violin(aes(fill = index))

grid.arrange(g1,g2, ncol = 1)

# look at richness and abundance with pH##############

dfalpha = woodsonly[c(3,9)]
dfalpha2 <- dfalpha %>% mutate_at(c('alpha'), ~(normalize(.) %>% as.vector))
dfabund = woodsonly[c(4,9)]
dfabund2 <- dfabund %>% mutate_at(c('cover'), ~(normalize(.) %>% as.vector))

index = rep('rich', nrow(dfalpha2))
df1 = cbind(dfalpha2,index)
colnames(df1) = c('value','pH','index')
df1 = df1%>%mutate(bin = cut_width(pH,width = 0.5, boundary = 0))
g1 = ggplot(data = na.omit(df1), aes(x=bin, y=value) ) +
  geom_violin(fill="#69b3a2") +
  xlab("pH")+
 ylab('alpha diversity')


index = rep('abund',nrow(dfabund2))
df2 = cbind(dfabund2,index)
colnames(df2) = c('value','pH','index')
df2 = df2%>%mutate(bin = cut_width(pH,width = 0.5, boundary = 0))
g2 = ggplot(data = na.omit(df2), aes(x=bin, y=value) ) +
  geom_violin(fill="#69b3a2") +
  xlab("pH")+
 ylab('abundance')


grid.arrange(g1,g2)

##########################

#is %am inoculum correlated with pH?

dfam = amplots[c(3,9,10,11)]
dfam = dfam%>%mutate(normshrub= normalize(amcover),
                 normtree = normalize(amtrees))
dfam = dfam%>%mutate(aminoc = (normshrub+normtree)*100)

dfem = emplots[c(3,9,10,11)]
dfem = dfem%>%mutate(normshrub= normalize(amcover),
                 normtree = normalize(amtrees))
dfem = dfem%>%mutate(aminoc = (normshrub+normtree)*100)

df1 = dfam['aminoc']
index = rep('am',nrow(df1))
df1 = cbind(dfam,index)

df2 = dfem['aminoc']
index = rep('em',nrow(df2))
df2 = cbind(dfem,index)


data = rbind(df1,df2)

ggplot(data,aes(x = index, y = aminoc))+geom_boxplot(aes(fill = index))

#is the amount of inoculum correlated with pH

r2am = round(summary(lm(aminoc ~ pH, df1))$r.squared,2)

text = paste("R2 = ", r2am) 
  g1 = ggplot(df1, aes(x = pH))+
    geom_point(aes(y = aminoc), colour = "red")+
    geom_smooth(method = "lm",aes(y = aminoc), colour = "black")+
    annotate("text",x = 9, y = 100,label =text)+
    ylab('aminoc') + xlab('pH')+
    xlim(3,10)+
    annotate('text',x=3,y=100,label="am plots")
#####
r2em = round(summary(lm(aminoc ~ pH, df2))$r.squared,2)

text = paste("R2 = ", r2em) 
g2 = ggplot(df2, aes(x = pH))+
  geom_point(aes(y = aminoc), colour = "blue")+
  geom_smooth(method = "lm",aes(y = aminoc), colour = "black")+
  annotate("text",x = 9, y = 100,label =text)+
  ylab('aminoc') + xlab('pH')+
  xlim(3,10)+
 annotate('text',x=3,y=100,label="em plots")
grid.arrange(g1,g2)
