
library(dplyr)
library(redist)
library(sf)
set.seed(2026)

wd<-"~/Documents/GitHub/citl"
setwd(wd)

blocks_wd<- paste0(wd,"/Complete Shapefiles/blocks clipped to cities/2010")
filenames_blocks<-list.files(path=blocks_wd, pattern="*.shp", full.names=FALSE) #Generate list of shapefiles

aggs_wd<-paste0(wd, "/Complete Shapefiles/block data aggregated to districts/2010")
agg_city <- list.files(path=aggs_wd, pattern="*.shp", full.names=FALSE) #Generate list of aggregated cities


for(i in 1:length(filenames_city)){
  setwd(blocks_wd)
  city<-st_read(filenames_blocks[i]) #Read in city blocks 
  city<-city%>%dplyr::select(pop, GEOID, NAME, geometry)
  
  setwd(aggs_wd) #Read in district summaries 
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
  setwd(paste0(wd,"/RB Seeded Sims/" ))
  #Save resulting plans
  saveRDS(rb_plans, file=paste0(substr(filenames_blocks[i], 0, nchar(filenames_blocks[i])-7), "_plans.rds"))
  
  
  
}

