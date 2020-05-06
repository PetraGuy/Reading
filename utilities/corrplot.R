
#visualisations on woodprops with ndep added

library(ggplot2)
library(ggcorrplot)
library(dplyr)

 corrplot = function(data,method,title){
  corr = corr_sp <- cor(data, method = method, use = 'pairwise.complete.obs')
  plot = ggcorrplot(corr, hc.order = TRUE, 
                       type = "lower", 
                       lab = TRUE, 
                       lab_size = 2, 
                       method="circle", 
                       colors = c("tomato2", "white", "springgreen3"), 
                       title=title, 
                       ggtheme=theme_bw)
  return(plot)
 }
  