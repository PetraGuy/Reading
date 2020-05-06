setwd("C:/dev/code/Reading/Woods2019/code")

#visualisations on woodprops with ndep added

library(ggplot2)
library(ggcorrplot)


 corrplot = function(data,method){
  corr = corr_sp <- cor(cont_small, method = method, use = 'pairwise.complete.obs')
  plot = ggcorrplot(corr, hc.order = TRUE, 
                       type = "lower", 
                       lab = TRUE, 
                       lab_size = 2, 
                       method="circle", 
                       colors = c("tomato2", "white", "springgreen3"), 
                       title="Correlogram of all continuous variables", 
                       ggtheme=theme_bw)
  return(plot)
 }
  