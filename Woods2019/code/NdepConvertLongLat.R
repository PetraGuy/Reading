# takes ndeptoal/ndep sep
#gives ndeptota/ndepsep longlat
# took Ndep file from ceh and turned easting/northing to long/lat
#

#convert Ndep data to long lat and extratc relevant data
setwd("C:/dev/code/Reading/Woods2019/code")
library(rgdal)

#this file is sinle year values for total Ndep
ndeptotal = read.csv("../data/NdepTotal.csv", 
                stringsAsFactors = TRUE, 
                header = TRUE)

#this file has 3 year aves for No2-No3. NH3-NH4 and SO2-SO3
ndepsep = read.csv("../data/NdepSep.csv", 
                   stringsAsFactors = TRUE, 
                   header = TRUE)
##########################################################
#convert to long lat
#takes df with col head of Easting and Northing and creates new cols of long lat
converttolonglat = function(df){
  coords <- cbind(Easting = as.numeric(as.character(df$Easting)),
                  Northing = as.numeric(as.character(df$Northing)))
  df_ID =  1:nrow(df)
  df = cbind(df_ID, df)
  
  #create spatial points dataframe
  dfSP = SpatialPointsDataFrame(coords, 
                                data = data.frame(df[1:3]),
                                proj4string = CRS("+init=epsg:27700"))
  #convert to long lat
  dflonglat = spTransform(dfSP,  CRS("+proj=longlat +datum=WGS84"))
  
  #take out the converted longlats and bind to end of orginal df
  df$long = dflonglat@coords[,"Easting"]
  df$lat = dflonglat@coords[,"Northing"]
  return(df)                              
}

####################################################
ndepseplonglat = converttolonglat(ndepsep)

saveRDS(ndepseplonglat,"../data/ndepseplonglat.RDS")
 
ndeptotallonglat = converttolonglat(ndeptotal)

saveRDS(ndeptotallonglat,"../data/ndeptotallonglat.RDS")
