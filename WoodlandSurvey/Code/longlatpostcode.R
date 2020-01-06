# get OS grid refs and post codes from long lat

## libraries
library(httr)


postcode_lookup <- function(postcode) {
  r <- GET(paste0("https://api.postcodes.io/postcodes/", postcode))
  warn_for_status(r)
  content(r)
}
# returns a list
pc_content <- postcode_lookup("GU7 1SJ")

# transform into a data frame
pc_result <- pc_content[[2]]
take_names <- setdiff(names(pc_result), 'codes')
pc_result[sapply(pc_result, is.null)] = list(NA)
pc_df <- cbind(as.data.frame(pc_result[take_names]),
               as.data.frame(pc_result$codes))

# create an interactive map
library(dplyr) # load pipes
library(leaflet) # load maps

leaflet(pc_df) %>%
  addTiles() %>%
  addCircles(lng = ~longitude, lat = ~latitude, popup = ~postcode)

#################################################################

#need to get post codes from long lat
#long lat table imported - postcodes

rawpostcodes = read.csv("../Data/postcodes.csv")

removechar = function(column){
  newcol = gsub("'","",column)
}


processedpostcodes = as.data.frame(apply(rawpostcodes,2,removechar),stringsAsFactors = FALSE)
colnames(processedpostcodes) = c("postcode","lat","long")

                                  

colnames(processedpostcodes) = c("postcode","lat","long")

processedpostcodes[2:3] =  sapply(processedpostcodes[2:3],as.numeric)




        
         