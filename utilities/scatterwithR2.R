
#create scatter plot with R2
#data = df, col1, col2 are y and x values to plot



lm_eqn = function(data, col1,col2){
  df = cbind(data[col1],data[col2])
  colnames(df) = c("y","x")
  r2 = round(summary(lm(y ~ x, df))$r.squared,2)
  text = paste("R2 = ", r2) 
  plot = ggplot(df, aes(x = x))+
  geom_point(aes(y = y), colour = "black")+
  geom_smooth(method = "lm",aes(y = y), colour = "black")+
  annotate("text",x = 0.2, y = 0.9,label =text)
  ylab(col1) + xlab(col2)
  return(plot)
}
  
#with limits to rescale y axis
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

p = lm_eqn(normdata,"meancgA","rainfall.month.previous.to.sampling")

data = normdata
col1  = "meancgA"
col2 = "rainfall.month.previous.to.sampling"

