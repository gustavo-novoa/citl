
library(dplyr)
library(redist)
library(sf)

#Adjust as needed
year<-2010

set.seed(2026)
#Set directory to files with blocks
block_wd<-("~/Princeton Dropbox/Gustavo Novoa/Replication Folder CITL/Complete Shapefiles/blocks clipped to cities L2 data/2010")
setwd(block_wd)
filenames_city<-list.files(pattern="*.shp", full.names=FALSE) #Generate list of shapefiles

#Set directory to summarized districts 

agg_wd<-("~/Princeton Dropbox/Gustavo Novoa/Replication Folder CITL/Complete Shapefiles/block data aggregated to districts L2/2010")
setwd(agg_wd)
agg_city <- list.files(pattern="*.shp", full.names=FALSE) #Generate list of aggregated cities


for(i in 1:length(filenames_city)){
  
  
  setwd(block_wd)
  city<-st_read(filenames_city[i]) #Read in city blocks 
  city<-city%>%dplyr::select(pop, GEOID, NAME, geometry)
  
  setwd(agg_wd) #Read in district summaries 
  agg_dists<-st_read(agg_city[i])
  
  #Calculate existing population deviation from actual map
  pop_tol<-max(agg_dists$pop/(sum(agg_dists$pop)/(n_distinct(agg_dists$distrct)))-1)
  
  #Get number of districts in existing map
  ndists=n_distinct(agg_dists$distrct)
  
  #Create redist object
  city_map<- redist::redist_map(city, pop_tol=pop_tol, ndists=ndists, total_pop=pop)
  city_map$adj<-redist.adjacency(city_map)
  
  
  #Run simulations. Use 2+ runs to ensure convergence acheived; Increase sims as needed
  rb_plans<-redist_smc(city_map, nsims=10000, ncores=parallel::detectCores(), runs=2)
  print("sim complete")
  setwd("~/Princeton Dropbox/Gustavo Novoa/Replication Folder CITL/RB Simulated Plans Seeded")
  #Save resulting plans
  saveRDS(rb_plans, file=paste0(substr(filenames_city[i], 0, nchar(filenames_city[i])-7), "_plans.rds"))
  
  
  
}

