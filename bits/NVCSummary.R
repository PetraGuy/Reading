## distribution of NVCs
library(dplyr)
library(ggplot2)

data = read.csv("AllPlotsVarsRichness.csv")


codes = as.data.frame(data%>%select(ShortNVC))


counts = table(data$ShortNVC)
bigcounts = table(counts)

ggplot(data, aes(ShortNVC)) + 
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


nvc = as.data.frame(table(data$ShortNVC))
colnames(nvc) = c("NVC","Freq")             
nvc[1,1] = ("Missing")

ggplot(nvc, aes(x = reorder(NVC, -Freq), y = Freq)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))    


nvclong = as.data.frame(table(data$Yr2NVC))
colnames(nvclong) = c("NVC","Freq")             
nvc[1,1] = (NA)
nvclong = nvclong%>%mutate(prop = round((Freq/(sum(nvclong$Freq))*100),2))
df = nvclong%>%filter(prop>2)

ggplot(df, aes(x = reorder(NVC, -Freq), y = Freq)) + 
  geom_bar(stat = "identity", fill = "red") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("NVC code")+
  ggtitle("Frequencey of various NVC codes")+
  geom_label(aes(label = prop), angle = 90)+
  annotate("text", x = 10, y = 120, label = "labels show NVC codes as % of total plots")






