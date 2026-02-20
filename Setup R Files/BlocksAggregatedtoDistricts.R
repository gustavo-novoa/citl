#District Summaries
library(dplyr)
library(sf)
library(stringr)

bank<-read.csv("/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/county bank.csv") #read in coutny bank

#set year value 
year=2000

setwd(paste0("/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/Time Series/City Council Maps/",year))

filenames_city<-list.files(pattern="*.shp", full.names=FALSE) #Generate list of shapefiles

filenames_city<-str_sub(filenames_city, 1, str_length(filenames_city)-4) #Remove file extension from names

filenames_city<-gsub("_", " ", filenames_city) #Replace underscores with spaces 
filenames_city<-tolower(filenames_city)

if(year==2010)
filenames_city<-filenames_city[-c(139, 141)] #Remove Washington for L2 only

setwd(paste0("/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/RR/Complete Shapefiles/county block with L2 data/", year))

filenames_county<-list.files(pattern="*.shp", full.names=FALSE) #Generate list of shapefiles


if(year>2000)
filenames_county<-str_sub(filenames_county, 1, str_length(filenames_county)-19) #Remove file extension from names
if(year==2000)
  filenames_county<-str_sub(filenames_county, 1, str_length(filenames_county)-16) #Remove file extension from names


filenames_county<-gsub("_", " ", filenames_county) #Replace underscores with spaces 



for(i in 1:length(filenames_city)){


  
setwd(paste0("/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/Time Series/City Council Maps/",year))
  
city<-st_read(paste0(stringr::str_replace_all(filenames_city[i], " ","_"),'.shp'))
print("city read")

setwd(paste0("/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/RR/Complete Shapefiles/county block with L2 data/",year))
l2<-''
if(year>2000)
l2<-"_L2"
county<-st_read(paste0(filenames_county[i],'_blocks_', year, l2, '.shp'))
print("county read")

if(is.na(st_crs(city)[[1]]))
  city<-sf::st_set_crs(city, sf::st_crs(county))

if(sf::st_crs(county)!=sf::st_crs(city))
  city<- sf::st_transform(city, sf::st_crs(county))

sf::sf_use_s2(FALSE)

city<- sf::st_make_valid(city)

city_districts<-sf::st_interpolate_aw(county%>% dplyr::select(where(is.numeric)), city%>%dplyr::select(district), extensive=TRUE)



city_districts$district<-city$district

city_districts<-city_districts%>%group_by(district)%>%mutate('h_pct_vap'= sum(vap_hsp)/sum(vap))

city_districts<-city_districts%>%group_by(district)%>%mutate('b_pct_vap'= sum(vp_blck)/sum(vap))

city_districts<-city_districts%>%group_by(district)%>%mutate('w_pct_vap'= sum(vap_wht)/sum(vap))

city_districts<-city_districts%>%group_by(district)%>%mutate('a_pct_vap'= sum(vap_asn)/sum(vap))

city_districts<-city_districts%>%group_by(district)%>%mutate('nw_pct_vap'= sum(vap_asn+vap_hsp+vp_blck)/sum(vap))

if(year>2000){

city_districts<-city_districts%>%group_by(district)%>%mutate('h_pct_cvap'= sum(cvp_hsp)/sum(cvap))

city_districts<-city_districts%>%group_by(district)%>%mutate('b_pct_cvap'= sum(cvp_blc)/sum(cvap))

city_districts<-city_districts%>%group_by(district)%>%mutate('w_pct_cvap'= sum(cvp_wht)/sum(cvap))

city_districts<-city_districts%>%group_by(district)%>%mutate('a_pct_cvap'= sum(cvap_sn)/sum(cvap))

city_districts<-city_districts%>%group_by(district)%>%mutate('nw_pct_cvap'= sum(cvap_sn+cvp_blc+cvp_hsp)/sum(impl_cv))
}

setwd(paste0("/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/RR/Complete Shapefiles/block data aggregated to districts L2/",year))

st_write(city_districts, driver='ESRI Shapefile', paste0(filenames_city[i], "_", "districts_summary_", year, "_L2"), append=FALSE)

}

