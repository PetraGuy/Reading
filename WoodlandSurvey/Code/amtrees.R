#using CEH data see if plots with AM trees are more diverse

setwd("C:/dev/code/Reading/WoodlandSurvey/Code")
data = read.csv("../Data/table_DBH_live_counts_71-03.csv", header = TRUE, stringsAsFactors = FALSE)
plants = read.csv("../Data/vegetation_codes.csv")

