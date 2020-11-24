#a bit of script for Tomos showing how to make a random number selection 
#for his bias estimator

#create empy df to store this stuff
data = data.frame(matrix(NA,nrow = 200, ncol = 5))

#and name the columns
colnames(data) = c('dates','age','maxcorrection','randadj','newdate')

#create  a test column of dates
data$dates  = floor(runif(200, min = 1500, max = 2005))

#how old is the date, because adjustment for bias depends on age
data$age = 2005 - data$dates

#want to adjust dates to a maximum value of 100 for oldest and 0 for most recent
#giving a maximum adjustment value for each date
data$maxcorrection = floor((100/505)*data$age)

#but want to select random value from 0 to that maximum value
#new column is random selection from between 0 and that maximum correction value
data$randadj = unlist(lapply(data$maxcorrection,function(x) sample(0:x,1)))

#newdate is the original date - the randomly altered value
data$newdate = data$dates - data$randadj


###############################################
# forget the aboce step through - do it like this
# wrap it all up in a single function
adjustdates = function(date){
  age = 2005 - date
  maxcorrection = floor((100/505)*age)
  rand = sample(0:maxcorrection,1)
  newdate = date - rand
  return(newdate)
}
  
#how to run that on a date column of a dataframe
data$newdate = unlist(lapply(data$dates, adjustdates))

################################################
#make some data to play with
#create a vector of dates - that would have come from df by df$dates
dates = sample(seq(from = 1500, to = 2005, by = 1),500, replace = TRUE)

#create test df
data = data.frame(matrix(NA,nrow = 500, ncol = 5))
data$dates = dates
###########################################


#take out just unique values
years = unique(data$dates)

#create empty dataframe which will become subset of original dataframe
#with proportion of some rows deleted
#dont know how big it will be, so just make it as big as original
newdf <- data[FALSE,]

for (year in years) {
  subset = data%>%filter(dates == year)  # filter out just the rows which equal the first data
  offset = year - 1500
  max_num = exp(0.007786*offset)-1 # this gives value between and 50, depending on date
  rand = 1 - (sample(0:max_num,1))/100 # percentage of rows to keep
  keep = subset%>%sample_frac(rand) # keeps that percentage of the subset
  newdf = rbind(newdf,keep)
 }



