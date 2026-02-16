library(sf)
library(dplyr)
sf_use_s2(FALSE)

#set year 
year<-2000
block_path<-paste0("/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/RR/Complete Shapefiles/county block with L2 data/",year)
council_path<-paste0("/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/Time Series/City Council Maps/",year)

county_block_names<-list.files(pattern='.shp', path=block_path)
if(year==2010)
#county_block_names<-county_block_names[-141] #Remove winston=salem, city mistakenly defined 12 districts instead of 8

council_map_names<-list.files(pattern='.shp', path=council_path)
if(year==2010)
council_map_names<-council_map_names[-c(139,141)] #Remove winston=salem, city mistakenly defined 12 districts instead of 8


for(i in 1:length(council_map_names)){
  if(year>2000){
  if(i==grep("lubbock", council_map_names)|i==grep("mobile", council_map_names)) #issue w lubbock and mobile files 
    next
}
  setwd(block_path)
  county_block<-st_read(county_block_names[i])
  #county blocks read
  
  setwd(council_path)
  council_map<-st_read(council_map_names[i])
  #city map read
  
  
  if(sf::st_crs(county_block)!=sf::st_crs(council_map)){
    council_map<- sf::st_transform(council_map, sf::st_crs(county_block))
    print("CRS SET")
  }
  
  if(!sf::st_is_valid(council_map)[1])
    council_map<- sf::st_make_valid(council_map)
  
  if(!sf::st_is_valid(county_block)[1])
    county_block<- sf::st_make_valid(county_block)
  
  # Compute the intersection of the two shapefiles
  intersection <- geomander::geo_trim(from=county_block, to=council_map)
  print("Trimmed")
  print(sum(intersection$pop))
  setwd(paste0("/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/RR/Complete Shapefiles/blocks clipped to cities L2 data/", year))
  st_write(intersection, driver='ESRI Shapefile', paste0(substr(council_map_names[i], 0, nchar(council_map_names[i])-4), "_L2"),
           append=FALSE)

  
}

