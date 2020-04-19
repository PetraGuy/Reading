
library(ggplot2)
library(reshape2)
# plotting x,y in terms of vars in game theory matrix

#m = (d-b), n = (a-c)

m = seq(from = 1, to = 20, by = 1)
n = seq(from = 1, to = 20, by = 1)

x = data.frame(matrix(nrow = 20, ncol = 20))

for (i in m){
  #browser()
  num = m[i]
  col = c()
  for (j in n){
    val = num/(n[j] + num)
    col = c(col,val)
  }
  x[,i] = col
}

colnames(x) = m
rownames(x) = n

x[,21] = m
colnames(x) = c(m,"X")

melted  = melt(x,id.vars = "X")

g = ggplot(melted, aes(x=X, y = value, col= variable)) +
  geom_line(show.legend = FALSE)+
  geom_point(show.legend = FALSE)+
  ylab("X")+
  xlab("(a-c)")+
  labs(colour = "(d-b)")
  
  
  
