#looking at invasives


setwd("C:/dev/code/Reading/AMEMtrees/Code")
allflora = read.csv('../data/allfloracomp.csv')

#take out just plots where cover of a species >50%

invaded = allflora%>%filter(cover_x > 50)

#look at effect size per species for richness vs cover
data = invaded%>%dplyr::select(c(species_x,cover_x,alpha))

#get rid of na
data = data%>%drop_na(alpha)

models = data%>%group_by(species_x)%>%do(model = lm(alpha ~ cover_x, data = ., na.action = na.exclude))

#get the coefficients
coefs = vector()
for (i in 1:46) {
  coefs[i] = models[[2]][[i]]$coefficients[[2]]
}

#get the species
species = as.data.frame(models[[1]])

df = as.data.frame(cbind(species,coefs))
df = df%>%drop_na(coefs)
df = df%>%arrange(coefs)
index = seq.int(from = 1, to = 32)
df['index'] = index
df2 <- data.frame(df[,-1], row.names=df[,1])

#to plot, add x values and use species as labels?

ggplot(df2, aes(x = index, y = coefs, label = rownames(df2)))+
  geom_point()+
  geom_text(angle = 90)+
  ylim(-3,+3)

#how many plots are affected by each species though?
df = df%>%dplyr::rename( species_x = 'models[[1]]')

numplots = invaded%>%group_by(species_x)%>%summarise(count=n())
combined = merge(df,numplots, by = 'species_x', type = 'left')

combined['overall'] = combined['coefs']*combined['count']

#now look at graph again
combined = combined%>%arrange(overall)
index = seq.int(from = 1, to = 32)
combined['index'] = index
combinedherb <- data.frame(combined[,-1], row.names=combined[,1])


ggplot(combined2, aes(x = index, y = overall, label = rownames(combined2)))+
  geom_point()+
  geom_text(angle = 90)+
  ylim(-120,+50)
  

 ####################################
#repeat for shrubs invasives
invaded = allflora%>%filter(cover_y > 50)
data = invaded%>%dplyr::select(c(species_y,cover_y,alpha))

#get rid of na
data = data%>%drop_na(alpha)%>%drop_na(cover_y)

models = data%>%group_by(species_y)%>%do(model = lm(alpha ~ cover_y, data = ., na.action = na.exclude))

#get the coefficients
coefs = vector()
for (i in 1:11) {
  coefs[i] = models[[2]][[i]]$coefficients[[2]]
}

#get the species
species = as.data.frame(models[[1]])

df = as.data.frame(cbind(species,coefs))
df = df%>%drop_na(coefs)
df = df%>%arrange(coefs)

index = seq.int(from = 1, to = 6)
df['index'] = index
df2 <- data.frame(df[,-1], row.names=df[,1])

#to plot, add x values and use species as labels?

ggplot(df2, aes(x = index, y = coefs, label = rownames(df2)))+
  geom_point()+
  geom_text(angle = 90)+
  ylim(-3,+3)

#how many plots are affected by each species though?
df = df%>%dplyr::rename( species_y = 'models[[1]]')

numplots = invaded%>%group_by(species_y)%>%summarise(count=n())
combined = merge(df,numplots, by = 'species_y', type = 'left')

combined['overall'] = combined['coefs']*combined['count']

#now look at graph again
combined = combined%>%arrange(overall)
index = seq.int(from = 1, to = 6)
combined['index'] = index
combinedshrub <- data.frame(combined[,-1], row.names=combined[,1])


ggplot(combinedshrub, aes(x = index, y = overall, label = rownames(combinedshrub)))+
  geom_point()+
  geom_text(angle = 90)+
  ylim(-120,+50)


#now I wnat to combine the shrub and herb invasive data in one plot

combinedinv = as.data.frame(rbind(combinedherb,combinedshrub))

#remove +ve stuff as not invasive!
neginvasives = combinedinv%>%rownames_to_column('species')%>%
  filter(overall < -5)%>%arrange(overall)%>%column_to_rownames('species')

index = seq.int(from = 1, to = 18)
neginvasives['index'] = index

ggplot(neginvasives, aes(x = index, y = overall, label = rownames(neginvasives)))+
  geom_point()+
  geom_text(angle = 90,check_overlap = T,position = position_nudge(x = -0.2))+
  ylim(-250,+50)+
  ylab('effect size x num plots')+
  xlab('')+
  theme(axis.text.x = element_blank())

  
