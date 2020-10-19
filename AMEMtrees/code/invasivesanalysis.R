#looking at invasives
library(dplyr)
library(ggplot2)
library(tidyr)

setwd("C:/dev/code/Reading/AMEMtrees/Code")


#allflora = read.csv('../data/allfloracomp.csv')
herbs = read.csv('../data/herb_cover_richness.csv')
shrubs = read.csv('../data/shrub_cover_richness.csv')

#take out just plots where cover of a species >50%
#invaded = herbs_and_shrubs%>%filter(cover > 50)

#select required cols from invaded df
herbdata = herbs%>%dplyr::select(c(species,cover,alpha))

#create lm for each species of richness~cover
#models = data%>%group_by(species_x)%>%do(model = lm(alpha ~ cover_x, data = ., na.action = na.exclude))
#you need several occurrances to plot a line. Two values for cover not very convincing
#so delete those with less than 5 points
tally = herbdata%>%group_by(species)%>%tally()
suff_points = tally%>%filter(n > 100)

#take just these species from herbdata and do correlations with them
req_species = suff_points$species
reduced_herbs = filter(herbdata, species %in% req_species)

#correlations on the species where there are many occurance
cors = reduced_herbs%>%group_by(species)%>%summarise(spearman = cor(cover,alpha, method = "spearman"))

#remove the ones where correlation is small
#just take top and bottom 8 
#top = top_n(cors,8)
bottom = cors%>%filter(spearman < 0)
#largestcors = rbind(top,bottom)


#get these species from the herbdata and look at graphs
largestcors_species = largestcors$species
largestcors_data = filter(herbdata,species %in% largestcors_species)
largestcors_data = merge(largestcors_data,largestcors)

#plot these
ggplot(largestcors_data, aes(cover,alpha))+
  geom_point()+
  geom_smooth(method = 'lm')+
  facet_wrap(~species)+
  stat_cor()



################################

#examine some plots
allrhodis = shrubs%>%filter(species == 'Rhododendron ponticum')
ggplot(allrhodis,aes(cover,alpha))+
  geom_point()+
  geom_smooth(method='lm')

allbracken = herbs%>%filter(species == 'Pteridium aquilinum')
ggplot(allbracken,aes(cover,alpha))+
  geom_point()+
  geom_smooth(method='lm')

#create lm for each species of richness~cover
models = herbdata%>%group_by(species)%>%do(model = lm(alpha ~ cover, data = ., na.action = na.exclude))

#get the coefficients in the lms
coefs = vector()
for (i in 1:46) {
  coefs[i] = models[[2]][[i]]$coefficients[[2]]
}

ggplot(herbdata, aes(cover,alpha, group = species))+
  geom_point()+
  geom_smooth(method = lm)

#get the species and combine with coefs to get df of a coef for each species
species = as.data.frame(models[[1]])
df = as.data.frame(cbind(species,coefs))
df = df%>%drop_na(coefs)
df = df%>%arrange(coefs)
index = seq.int(from = 1, to = 32)
df['index'] = index

#in this format can make a nice plot
df2 <- data.frame(df[,-1], row.names=df[,1])

#to plot, add x values and use species as labels?
#plot is change in richness for 1% change in cover
ggplot(df2, aes(x = index, y = coefs, label = rownames(df2)))+
  geom_point(colour = 'red', size = 5)+
  geom_text(angle = 90)+
  ylim(-3,+3)+
  theme(axis.text.x = element_blank())+
  ylab('effect of cover on herb richness')+
  geom_hline(yintercept = 0)+
  theme(axis.text.x = element_blank())+
  xlab('')

#how many plots are affected by each species though?
#get num of plots for each species
df = df%>%dplyr::rename( species_x = 'models[[1]]')
numplots = invaded%>%group_by(species_x)%>%summarise(count=n())
combined = merge(df,numplots, by = 'species_x', type = 'left')

#multiply numplots by coef
combined['overall'] = combined['coefs']*combined['count']

#now look at graph again
combined = combined%>%arrange(overall)
index = seq.int(from = 1, to = 32)
combined['index'] = index
combinedherb <- data.frame(combined[,-1], row.names=combined[,1])

#the response is now change in richness for 1% chnage in cover over all plots
#that contain that species. Its a cumulative change in richness, some species occur
#in many plots. so we are counting the same species multiple times in this richness change
ggplot(combinedherb, aes(x = index, y = overall, label = rownames(combinedherb)))+
  geom_point(colour = 'red', size = 5)+
  geom_text(angle = 90)+
  ylim(-120,+120)+
  theme(axis.text.x = element_blank())+
  ylab('effect of cover on herb richness over all plots in Bunce')+
  geom_hline(yintercept = 0)+
  theme(axis.text.x = element_blank())+
  xlab('')
  

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
  geom_point(colour = 'red', size = 5)+
  geom_text(angle = 90)+
  ylim(-0.5,+1)+
  theme(axis.text.x = element_blank())+
  ylab('effect of cover on herb richness ')+
  geom_hline(yintercept = 0)+
  theme(axis.text.x = element_blank())+
  xlab('')

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
  geom_point(colour = 'red', size = 5)+
  geom_text(angle = 90)+
  ylim(-250,+30)+
  theme(axis.text.x = element_blank())+
  ylab('effect of cover on herb richness over all plots in Bunce')+
  geom_hline(yintercept = 0)+
  theme(axis.text.x = element_blank())+
  xlab('')

########################

#repeat for trees

trees = read.csv('../data/trees.csv')

#take out just plots where cover of a species >50%
#but tree cover not listed - so use count * dbh to work something out
#just use count for now

invaded = trees%>%filter(Count > 10)%>%dplyr::select(site,plot,Yr,species,Count)

#merge with flora data to add richness of the plots
alphadata = allflora%>%dplyr::select(Site,Plot,Yr,alpha)
alphadata = alphadata%>%rename(site = Site, plot = Plot)

#repeats coz allflora has all the species rows, so take unique
alphadata = unique(alphadata)
treesinvasives = merge(invaded,alphadata, by = c('site','plot','Yr'))


models = treesinvasives%>%group_by(species)%>%do(model = lm(alpha ~ Count, data = ., na.action = na.exclude))

#get the coefficients
coefs = vector()
for (i in 1:69) {
  coefs[i] = models[[2]][[i]]$coefficients[[2]]
}

#get the species
species = as.data.frame(models[[1]])

df = as.data.frame(cbind(species,coefs))
df = df%>%drop_na(coefs)
df = df%>%arrange(coefs)
index = seq.int(from = 1, to = 54)
df['index'] = index
df2 <- data.frame(df[,-1], row.names=df[,1])

#to plot, add x values and use species as labels?

ggplot(df2, aes(x = index, y = coefs, label = rownames(df2)))+
  geom_point(colour = 'red', size = 5)+
  geom_text(angle = 90)+
  ylim(-3,+3)+
  theme(axis.text.x = element_blank())+
  ylab('effect of cover on herb richness')+
  geom_hline(yintercept = 0)+
  theme(axis.text.x = element_blank())+
  xlab('')

#how many plots are affected by each species though?
df = df%>%dplyr::rename( species = 'models[[1]]')

numplots = treesinvasives%>%group_by(species)%>%summarise(count=n())
combined = merge(df,numplots, by = 'species', type = 'left')

combined['overall'] = combined['coefs']*combined['count']

#now look at graph again
combined = combined%>%arrange(overall)
index = seq.int(from = 1, to = 54)
combined['index'] = index
combinedtree <- data.frame(combined[,-1], row.names=combined[,1])


ggplot(combinedtree, aes(x = index, y = overall, label = rownames(combinedherb)))+
  geom_point(colour = 'red', size = 5)+
  geom_text(angle = 90)+
  ylim(-10,+12)+
  theme(axis.text.x = element_blank())+
  ylab('effect of cover on herb richness over all plots in Bunce')+
  geom_hline(yintercept = 0)+
  theme(axis.text.x = element_blank())+
  xlab('')




##########################
#now I wnat to combine the shrub tree and herb invasive data in one plot

combinedinv = as.data.frame(rbind(combinedherb,combinedshrub, combinedtree))


#remove +ve stuff as not invasive!
neginvasives = combinedinv%>%tibble::rownames_to_column('species')%>%
  filter(coefs < -0.05)%>%arrange(coefs)%>%tibble::column_to_rownames('species')

index = seq.int(from = 1, to = 37)
neginvasives['index'] = index

ggplot(neginvasives, aes(x = index, y = coefs, label = rownames(neginvasives)))+
  geom_point(colour = 'red', size = 5)+
  geom_text(angle = 90)+
  ylim(-2.5,0.5)+
  theme(axis.text.x = element_blank())+
  ylab('effect of cover on herb richness')+
  geom_hline(yintercept = 0)+
  theme(axis.text.x = element_blank())+
  xlab('')


#remove +ve stuff as not invasive!
neginvasives = combinedinv%>%tibble::rownames_to_column('species')%>%
  filter(overall < -5)%>%arrange(overall)%>%tibble::column_to_rownames('species')

index = seq.int(from = 1, to = 21)
neginvasives['index'] = index

ggplot(neginvasives, aes(x = index, y = overall, label = rownames(neginvasives)))+
  geom_point(colour = 'red', size = 5)+
  geom_text(angle = 90)+
  ylim(-250,15)+
  theme(axis.text.x = element_blank())+
  ylab('effect of cover on herb richness over all plots in Bunce')+
  geom_hline(yintercept = 0)+
  theme(axis.text.x = element_blank())+
  xlab('')

####what about the +ve stuff???######################

#remove +ve stuff as not invasive!
posinvasives = combinedinv%>%tibble::rownames_to_column('species')%>%
  filter(coefs > 0.1)%>%arrange(coefs)%>%tibble::column_to_rownames('species')

#just do effect of coef first, then over entire bunce
#need to reindex each time you reorganise rows
index = seq.int(from = 1, to = 20)
posinvasives['index'] = index

ggplot(posinvasives, aes(x = index, y = coefs, label = rownames(posinvasives)))+
  geom_point(colour = 'red', size = 5)+
  geom_text(angle = 90)+
  ylim(-0.5,+3)+
  theme(axis.text.x = element_blank())+
  ylab('effect of cover on herb richness')+
  geom_hline(yintercept = 0)+
  theme(axis.text.x = element_blank())+
  xlab('')

#reorder by overall
posinvasives = combinedinv%>%tibble::rownames_to_column('species')%>%
  filter(overall > 5)%>%arrange(overall)%>%tibble::column_to_rownames('species')
index = seq.int(from = 1, to = 13)
posinvasives['index'] = index

ggplot(posinvasives, aes(x = index, y = overall, label = rownames(posinvasives)))+
  geom_point(colour = 'red', size = 5)+
  geom_text(angle = 90)+
  ylim(0,20)+
  theme(axis.text.x = element_blank())+
  ylab('effect of cover on herb richness over all plots in Bunce')+
  geom_hline(yintercept = 0)+
  theme(axis.text.x = element_blank())+
  xlab('')

###########################

#create file with flag that can be exported to redo modelling

combinedinv['flag'] = 2
write.csv(combinedinv,'../data/combinedinv.csv')
