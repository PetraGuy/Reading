#plots of pH across am and em
#richness and abundance across pH

setwd("C:/dev/code/Reading/AMEMtrees/Code")
library(dplyr)
library(stringr)
library(tidyverse)
library(gridExtra)

normalize <- function(x) {
  norm = ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  
  return(norm)
}
'%!in%' = function(x,y)!('%in%'(x,y))


#get data
data = read.csv('../data/all2data.csv')

#get rid of non woodland plots - anything that doesnt syart with W
woodsonly = data%>%filter(str_detect(shortcodes,"W"))

#take out the scrub codes -W19 - W25 inclusive
scrub = c('W19','W20','W21','W22','W23','W24','W25')

woods = filter(woodsonly, shortcodes %!in% scrub)
#these are am and em codes for non-scrub plots
amcodes = c('W6','W7','W8','W9','W12','W13')
emcodes = c('W3','W4','W10','W11','W14','W15','W16','W17')



woods%>%group_by(shortcodes)%>%summarise( mean = mean(som, na.rm = TRUE))
impute <- function(x) {
  replace(x, is.na(x), mean(x, na.rm = TRUE))}
woods = woods%>%group_by(shortcodes)%>%mutate_at(c('pH','som'), impute)

#now make NA in other columns = 0
woods[is.na(woods)] <- 0

woods['am'] = woods[5]+woods[8]
woods['inv'] = woods[6]+woods[9]+woods[10]

woods = woods%>%mutate(NVCtype = if_else (shortcodes %in% amcodes, -1,1))


amplots = woods%>%filter(NVCtype == -1)
emplots = woods%>%filter(NVCtype == 1)
############################################################################

#pH across plots - violin plot of pH for AM and EM plots
#produces violin plot of ph across am and em plots
pHam = amplots['pH']
index = rep('am',length(pHam))
df1 = cbind(pHam,index)

pHem = emplots['pH']
index = rep('em',length(pHem))
df2 = cbind(pHem,index)
thedata = rbind(df1,df2)


ggplot(thedata,aes(x = index, y = pH))+geom_violin(aes(fill = index))

#look at richness and abundance of em and am plots
#herb cover
amcover = amplots['herbcover']
index = rep('am',length(amcover))
df1 = cbind(amcover,index)

emcover = emplots['herbcover']
index = rep('em',length(emcover))
df2 = cbind(emcover,index)


thedata = rbind(df1,df2)

g1 = ggplot(thedata,aes(x = index, y = herbcover))+geom_violin(aes(fill = index))
#############################################################################
#produces violin plots of richness and herb cover across am and em plots
amalpha = amplots['alpha']
index = rep('am',length(amalpha))
df1 = cbind(amalpha,index)

emalpha = emplots['alpha']
index = rep('em',length(emalpha))
df2 = cbind(emalpha,index)


thedata = rbind(df1,df2)

g2 = ggplot(thedata,aes(x = index, y = alpha))+geom_violin(aes(fill = index))

grid.arrange(g1,g2, ncol = 1)
#############################################################################
# look at richness and abundance with pH
#produces violin plot of alpha and cover with pH

dfalpha = woods[c(4,13)]
dfalpha2 <- dfalpha %>% mutate_at(c('alpha'), ~(normalize(.) %>% as.vector))
dfabund = woods[c(7,13)]
dfabund2 <- dfabund %>% mutate_at(c('herbcover'), ~(normalize(.) %>% as.vector))

index = rep('rich', nrow(dfalpha2))
df1 = cbind(dfalpha2,index)
colnames(df1) = c('value','pH','index')
df1 = df1%>%mutate(bin = cut_width(pH,width = 0.5, boundary = 0))
g1 = ggplot(data = na.omit(df1), aes(x=bin, y=value) ) +
  geom_boxplot(fill="#69b3a2") +
  xlab("pH")+
 ylab('alpha diversity')


index = rep('abund',nrow(dfabund2))
df2 = cbind(dfabund2,index)
colnames(df2) = c('value','pH','index')
df2 = df2%>%mutate(bin = cut_width(pH,width = 0.5, boundary = 0))
g2 = ggplot(data = na.omit(df2), aes(x=bin, y=value) ) +
  geom_boxplot(fill="#69b3a2") +
  xlab("pH")+
 ylab('abundance')


grid.arrange(g1,g2)

############################################################################

#boxplots of how invaded plots are by year and type

aminvyr1 = amplots%>%filter(Yr == 1)%>%data.frame()%>%dplyr::select(inv)
index = rep('am',length(aminvyr1))
df1 = cbind(aminvyr1,index)

eminvyr1 = emplots%>%filter(Yr == 1)%>%data.frame()%>%dplyr::select(inv)
index = rep('em',length(eminvyr1))
df2 = cbind(eminvyr1,index)


thedatayr1 = rbind(df1,df2)

aminvyr2 = amplots%>%filter(Yr == 2)%>%data.frame()%>%dplyr::select(inv)
index = rep('am',length(aminvyr1))
df1 = cbind(aminvyr2,index)

eminvyr2 = emplots%>%filter(Yr == 2)%>%data.frame()%>%dplyr::select(inv)
index = rep('em',length(eminvyr2))
df2 = cbind(aminvyr2,index)


thedatayr2 = rbind(df1,df2)

g1 = ggplot(thedatayr1,aes(x = index, y = inv))+geom_boxplot(aes(fill = index))
g2 = ggplot(thedatayr2,aes(x = index, y = inv))+geom_boxplot(aes(fill = index))

grid.arrange(g1,g2, ncol = 1)

##########################################################################

#boxplots of richness 


aminvyr1 = amplots%>%filter(Yr == 1)%>%data.frame()%>%dplyr::select(alpha)
index = rep('am',length(aminvyr1))
df1 = cbind(aminvyr1,index)

eminvyr1 = emplots%>%filter(Yr == 1)%>%data.frame()%>%dplyr::select(alpha)
index = rep('em',length(eminvyr1))
df2 = cbind(eminvyr1,index)


thedatayr1 = rbind(df1,df2)

aminvyr2 = amplots%>%filter(Yr == 2)%>%data.frame()%>%dplyr::select(alpha)
index = rep('am',length(aminvyr1))
df1 = cbind(aminvyr2,index)

eminvyr2 = emplots%>%filter(Yr == 2)%>%data.frame()%>%dplyr::select(alpha)
index = rep('em',length(eminvyr2))
df2 = cbind(aminvyr2,index)


thedatayr2 = rbind(df1,df2)

g1 = ggplot(thedatayr1,aes(x = index, y = alpha))+geom_boxplot(aes(fill = index))
g2 = ggplot(thedatayr2,aes(x = index, y = alpha))+geom_boxplot(aes(fill = index))

grid.arrange(g1,g2, ncol = 1)

######################################################################

#boxplots of abundance
aminvyr1 = amplots%>%filter(Yr == 1)%>%data.frame()%>%dplyr::select(herbcover)
index = rep('am',length(aminvyr1))
df1 = cbind(aminvyr1,index)

eminvyr1 = emplots%>%filter(Yr == 1)%>%data.frame()%>%dplyr::select(herbcover)
index = rep('em',length(eminvyr1))
df2 = cbind(eminvyr1,index)


thedatayr1 = rbind(df1,df2)

aminvyr2 = amplots%>%filter(Yr == 2)%>%data.frame()%>%dplyr::select(herbcover)
index = rep('am',length(aminvyr1))
df1 = cbind(aminvyr2,index)

eminvyr2 = emplots%>%filter(Yr == 2)%>%data.frame()%>%dplyr::select(herbcover)
index = rep('em',length(eminvyr2))
df2 = cbind(aminvyr2,index)


thedatayr2 = rbind(df1,df2)

g1 = ggplot(thedatayr1,aes(x = index, y = herbcover))+geom_boxplot(aes(fill = index))
g2 = ggplot(thedatayr2,aes(x = index, y = herbcover))+geom_boxplot(aes(fill = index))

grid.arrange(g1,g2, ncol = 1)