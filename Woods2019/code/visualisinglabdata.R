setwd("C:/dev/code/Reading/Woods2019/code")

#visualisations on woodprops with ndep added

library(ggplot2)
library(ggcorrplot)

data =  read.csv('../data/woodprops.csv')

#select some of this data to visualise, too much to look at it all
continuous = data%>%select(7,8,9,11,12,13,14,15,16,33:42,44)

#remove rows with NA
continuous = continuous[complete.cases(continuous),]

corr_sp <- cor(continuous, method = 'spearman', use = 'pairwise.complete.obs')

# Plot
corr_sp = ggcorrplot(corr_sp, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 2, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of all continuous variables", 
           ggtheme=theme_bw)


corr_pear <- cor(continuous, method = 'pearson', use = 'pairwise.complete.obs')

# Plot
corr_sp = ggcorrplot(corr_pear, hc.order = TRUE, 
                     type = "lower", 
                     lab = TRUE, 
                     lab_size = 2, 
                     method="circle", 
                     colors = c("tomato2", "white", "springgreen3"), 
                     title="Correlogram of all continuous variables", 
                     ggtheme=theme_bw)
