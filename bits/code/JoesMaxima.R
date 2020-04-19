#test to fix dplyr

library(dplyr)
library(data.table)
#creating a toy df to play with
df = data.frame(rep(c(1:5),5),rep(c(letters[1:5]),5), stringsAsFactors = FALSE)
colnames(df) = c("col1","col2")

#initialize empty df to save the extracted rows in
maxdf <- data.frame(matrix(ncol = 2, nrow = 5))
colnames(maxdf) = c("col1","col2")

#we know repeats are every 5 so..
for (i in 1:5){
  startrow = (1+(i-1)*5)
  endrow = (5*i)
  subdf = df[startrow:endrow,]
  maxrow = as.vector(df[which.max(subdf$col1),])
  maxdf[i,] = maxrow
}

#now you have the pulled out max rows.

##########################

#read in your data
getdata = function(filename){
  tides = read.csv(filename)
  return(tides)
}

#get the max row
getmaxrow = function(somerows){
  maxrow = requiredday[which.max(requiredday$high),]
  return(maxrow)
}

#what row number is the max row
getmaxrownum = function(somerows){
  row_number = as.integer(row.names(maxrow))
  return(rownumber)
}

#how many days are there in this data file
getnumdays = function(filename){
  numdays = nrow(data)/96
  return(numdays)
}

#get the start and end rows - the slices
getday = function(i,data){
  startrow =  startrow = (1+(i-1)*96)
  endrow = (96*i)
  subdf = data[startrow:endrow,]
  return(subdf)
}

getthemaxrow = function(day){
  maxrow = day[which.max(day$high),]
  return(maxrow)
}

getrownum = function(arow){
  rownum = as.integer(row.names(arow))
  return(rownum)
}


#get last half of the day
getendday = function(aday){
  endday = slice(aday,49:96)
  return(endday)
}

#get first half of the day
getbegday = function(aday){
  begday = slice(aday,1:48)
  return(begday)
}

#if max row is in the second half of the day, get the first half
#and vice versa
getrestofday = function(r,day, i){
  midpoint = 96*(i-1) + 48
  if (r<=midpoint){
    halfofday = getendday(day)
  }else{
    halfofday = getbegday(day)
  }
  return(halfofday)
}
#############


data = getdata("JoeTestTides.csv")
howmanydays = getnumdays(data)

maxdf = data.frame()
                   
N = howmanydays-1
for (i in (1:N)){

  singleday = getday(i,data) #get each day
  
  maxrow1 = getthemaxrow(singleday) # find the maxrow
  
  row_number = getrownum(maxrow1)  # what row is it
  
  restofday = getrestofday(row_number, singleday,i) #if max row is in first half of day, 
                                                  #get the last half, or vice versa
  
  maxrow2 = getthemaxrow(restofday) # now get the other maximum
  
  rows = rbind(maxrow1,maxrow2)
  orderedrows = rows[order(rows$time),]
  
  maxdf = rbind(maxdf,orderedrows)
}              

##############################



    
      