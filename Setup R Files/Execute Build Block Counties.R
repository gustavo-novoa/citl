#Execute Build Block Counties
library(cvap)
library(tidycensus)
library(censable)
library(dplyr)
library(sf)
options(tigris_use_cache = TRUE)

#set year value 
year<-2000

setwd(paste0("/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/Time Series/City Council Maps/",year))

filenames<-list.files(pattern="*.shp", full.names=FALSE) #Generate list of shapefiles

filenames<-str_sub(filenames, 1, str_length(filenames)-4) #Remove file extension from names

filenames<-gsub("_", " ", filenames) #Replace underscores with spaces 

counties<-read.csv("/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/county bank.csv") #read in coutny bank

setwd(paste0("/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/RR/Complete Shapefiles/county blocks/",year)) #change wd to save location for county blocks

if(year==2000){
  filenames<-tolower(filenames)
  filenames<-gsub('\\.', '', filenames)
  
}


for(i in 1:length(filenames)){ 

  #IF statements below are specifically for 2010 cycle
  
  # if(i == 54)
  #   next  #Houston county Fort Bend not accepted for some reason
  # 
   x<-grep(filenames[i], counties$city)
   
   if(i==83&year==2020) #distinguish between richmond and augusta-richmond 2020
     x<-117

  # if(i==58) #distinguish between jackson and jacksonville
  #   x=65

  # if(i==105) #distinguish between richmond and augusta-richmond
  #   x=116
  # if(i==110) #distinguish between salem and winston-salem
  #   x=124
  
  ifelse(!as.numeric(x), print("not found!"), print(x))


  
  
  ifelse( grepl("," ,counties$county[x]),  c<-save_blocks(counties$state[x], lapply(strsplit(counties$county[x], ", ")[[1]], trimws), year), c<-save_blocks(counties$state[x], counties$county[x], year))

  
  # if(i==38 & year==2020) #Houston's Fort Bend unrecognized for some reason, do it manually
  #   c<-save_blocks(counties$state[x], c(201, 339, 157), year)
  # 
  
  if(year>2000){
  state_est <- cvap_distribute_censable(counties$state[x], year=year,include_implied = TRUE)
  
  county_est<-subset(state_est, state_est$GEOID%in%c$GEOID)
  
  county_est<-county_est%>%dplyr::select("cvap", "cvap_white", "cvap_black", "cvap_hisp",
                                       "cvap_asian", "impl_cvap", "GEOID")
  
  c<-c%>%left_join(county_est, by="GEOID")
  }
  
  print(filenames[i])
 st_write(c, driver='ESRI Shapefile', paste0(filenames[i], "_", "blocks_", year), append=FALSE)
    
}
