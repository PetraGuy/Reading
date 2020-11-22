#a bit of script for Tomos showing how to make a random number selection 
#for his bias estimator

#create empy df to store this stuff
data = data.frame(matrix(NA,nrow = 50, ncol = 5))

#and name the columns
colnames(data) = c('dates','age','maxcorrection','randadj','newdate')

#create  a test column of dates
data$dates  = floor(runif(50, min = 1500, max = 2005))

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

