library(sf)
library(dplyr)
wd<-"~/Documents/GitHub/citl"
setwd(wd)
year<-2010
aggs_wd<-paste0("./Complete Shapefiles/block data aggregated to districts/2010")
filenames_aggs <- list.files(path=aggs_wd, pattern="*.shp", full.names=FALSE) #Generate list of aggregated cities
setwd(aggs_wd)
districts<-data.frame()

for(i in 1:length(filenames_aggs)){
  
  city<-st_read(filenames_aggs[i])
  
  city$city<- sub("_d.*", "", filenames_aggs[i]) 
  
  df<-data.frame(st_drop_geometry(city))
  
  
  df<-df[colSums(!is.na(df)) > 0]
  
  districts<-rbind(df, districts)
  
}
setwd(wd)
city_st<-read.csv("./External Data/city_state_extended.csv")
districts<-left_join(districts, city_st, by='city')

#remove leading zeroes
districts$distrct <- sub("^0+", "", districts$distrct)

#fix weird input error 
districts$distrct[nchar(districts$distrct)>3]<- substr(districts$distrct[nchar(districts$distrct)>3],0,2)
#convert to numeric
districts$distrct<-as.numeric(districts$distrct)
districts$district<-districts$distrct             
dbk<-read.csv("./External Data/ledb_candidatelevel.csv")
dbk<-dbk%>%filter(office_consolidated=='City Council')
dbk<-dbk%>%filter(year<2021 & year>2009)
dbk$city<-dbk$geo_name
dbk$state<-dbk$state_abb
dbk$city<-gsub("\\.", "", dbk$city)
dbk<-dbk[grep("large|chair|superward|president|wide", dbk$district, invert=TRUE, ignore.case = TRUE) ,]
dbk$district <- ifelse(grepl("[0-9]", dbk$district), gsub("\\D", "", dbk$district), dbk$district)
dbk<-dbk[grepl("[0-9]", dbk$district),]
dbk$district<-as.double(dbk$district)
merged<-left_join(districts, dbk, by=c('city', 'state', 'district'))
merged<-merged[!is.na(merged$winner),]

write.csv(merged, file='./Compiled Results/districtswDBK.csv')
