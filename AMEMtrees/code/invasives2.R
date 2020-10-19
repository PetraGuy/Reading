library(dplyr)
library(ggplot2)
library(tidyr)

setwd("C:/dev/code/Reading/AMEMtrees/Code")


#allflora = read.csv('../data/allfloracomp.csv')
herbs = read.csv('../data/herb_cover_richness.csv')
shrubs = read.csv('../data/shrub_cover_richness.csv')

#take out just plots where cover of a species >50%
#invaded = herbs_and_shrubs%>%filter(cover > 50)

viewcorrelations = function(data){
#select required cols from invaded df
data= data%>%dplyr::select(c(species,cover,alpha))

#create lm for each species of richness~cover
#models = data%>%group_by(species_x)%>%do(model = lm(alpha ~ cover_x, data = ., na.action = na.exclude))
#you need several occurrances to plot a line. Two values for cover not very convincing
#so delete those with less than 5 points
tally = data%>%group_by(species)%>%tally()
suff_points = tally%>%filter(n > 200)

#take just these species from herbdata and do correlations with them
req_species = suff_points$species
reduced_herbs = filter(data, species %in% req_species)

#correlations on the species where there are many occurance
cors = reduced_herbs%>%group_by(species)%>%summarise(spearman = cor(cover,alpha, method = "pearson"))

#remove the ones where correlation is small
#just take top and bottom 8 
top = top_n(cors,-25)
#bottom = cors%>%filter(spearman > 0)
#largestcors = rbind(top,bottom)


#get these species from the herbdata and look at graphs
largestcors_species = top$species
largestcors_data = filter(data,species %in% largestcors_species)

#plot these
ggplot(largestcors_data, aes(cover,alpha))+
  geom_point()+
  geom_smooth(method = 'lm')+
  facet_wrap(~species)+
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
    label.x = 3
  )
}

viewcorrelations(herbs)

################################

#repeat with shrubs
