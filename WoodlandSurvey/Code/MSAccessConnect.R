
# get the site names from the Bunce Access database

setwd("C:/dev/code/Reading/WoodlandSurvey/Code")

library(RODBC)

db <- odbcConnectAccess("Data/OriginalData")

#list all the tables

sqlTables(db)
df = sqlFetch(db, "Sites_List",colnames = TRUE)

query = "SELECT [Site Name],SITE from Sites_List"

df = sqlQuery(db,query = query)
saveRDS(df, "../Data/SiteNames.RDS")


odbcClose(db)

