
#select continous or categorical complete cases
#note - you might want to keep the incomplete cases for imputation
# same for cols
#decide whether you want these

library(dplyr)
library(purrr)

continous = function(data){
cont = (select_if(data,is.numeric))
#cont = cont[complete.cases(cont),]
return(cont)
}

categorical = function(data){
cat = data%>%select_if(~!is.numeric(.x))
#discard cols if all na
cat = cat%>%discard(~all(is.na(.x)))
#discard rows if all na
#cat = cat[complete.cases(cat),]
return(cat)
}


