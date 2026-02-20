library(dplyr)
library(redist)
library(sf)
library(geomander)
library(ggplot2)

wd<-"/Users/gnovoa/Documents/GitHub/citl"
setwd(wd)


# Define paths
blocks_wd<- paste0(wd,"/Complete Shapefiles/blocks clipped to cities/2010")

aggs_wd<-paste0(wd, "/Complete Shapefiles/block data aggregated to districts/2010")


agg_city <-   list.files(path = aggs_wd, pattern = ".shp", full.names = FALSE)

filenames_city<-  list.files(path = blocks_wd, pattern = ".shp", full.names = FALSE)


bank<-read.csv("./External Data/county_bank_rpv_plus.csv")
rpv<-read.csv("./External Data/national_summary_2020.csv")
rpv$county<-gsub("city", "City" ,rpv$county) #to match bank
rpv<-rpv[grepl("pre_16_r", rpv$cand),]


rpv <- rpv %>%
  group_by( county, state) %>%
  mutate(
    wdiff = mean - mean[race == "vap_white"]
  ) %>%
  ungroup()

#set to just run once as an example. Group demographics depend on city 

for(i in 1){

  #remove extension 
  city<-   gsub("_"," ", substr(filenames_city[i], 0, nchar(filenames_city[i])-7))
  cityx<-paste0("^",city, "\\b") #Make sure first word is fully captured
  
  
  bankid<-grep(cityx, bank$city)
  
  state<-bank$state[bankid]
  
  single<-TRUE
  
  
  
  countys<-paste0("\\<",bank$rpv_county[bankid],"\\>")
  

  countys<-grep(countys, unique(rpv$county), value=TRUE, ignore.case=TRUE)[1]
  
  if(countys=="Cameron Parish")
    countys<-"Cameron County"
  
  black_rpv<-rpv$mean[rpv$race=="vap_black"& rpv$county==countys & rpv$state==state]
  hisp_rpv<-rpv$mean[rpv$race=="vap_hisp"& rpv$county==countys & rpv$state==state]
  white_rpv<-rpv$mean[rpv$race=="vap_white"& rpv$county==countys & rpv$state==state]
  
  setwd(aggs_wd)
  agg_dists<-st_read(paste0(city,"_districts_summary_2010_L2.shp"), quiet = TRUE)
  ndists<-n_distinct(agg_dists$distrct)
  
  setwd(blocks_wd)
  city_blocks<-st_read(filenames_city[i], quiet = TRUE)
  
  hisp_viable=FALSE
  black_viable=FALSE
  
  hisp_vra=FALSE
  black_vra=FALSE
  
  if(sum(city_blocks$pp_hspn)/sum(city_blocks$pop)>(1/ndists))
    hisp_viable=TRUE
  
  if(sum(city_blocks$pp_blck)/sum(city_blocks$pop)>(1/ndists))
    black_viable=TRUE
  
  
  if(white_rpv<.5 & hisp_rpv>.5 & hisp_viable | white_rpv>.5 & hisp_rpv<.5 &hisp_viable)
    hisp_vra=TRUE
  
  if(white_rpv<.5 & black_rpv>.5 & black_viable | white_rpv>.5 & black_rpv<.5 &black_viable)
    black_vra=TRUE
  
  if(hisp_vra & black_vra){
    
    
    constr <- redist_constr(city_blocks) %>%
      add_constr_grp_hinge(
        5,
        vp_blck,
        total_pop = vap,
        tgts_group = c(0.45)
      ) %>%
      add_constr_grp_hinge(-5,
                           vp_blck,
                           vap,
                           0.3) %>%
      add_constr_grp_inv_hinge(5,
                               vp_blck,
                               vap,
                               0.70) %>%
      add_constr_grp_hinge(
        5,
        vap_hsp,
        total_pop = vap,
        tgts_group = c(0.45)
      ) %>%
      add_constr_grp_hinge(-5,
                           vap_hsp,
                           vap,
                           0.3) %>%
      add_constr_grp_inv_hinge(5,
                               vap_hsp,
                               vap,
                               0.70)
    
    
    
  }
  
  if(black_vra & !hisp_vra){
    
    constr <- redist_constr(city_blocks) %>%
      add_constr_grp_hinge(
        5,
        vp_blck,
        total_pop = vap,
        tgts_group = c(0.45)
      ) %>%
      add_constr_grp_hinge(-5,
                           vp_blck,
                           vap,
                           0.3) %>%
      add_constr_grp_inv_hinge(5,
                               vp_blck,
                               vap,
                               0.70)
    
    
    
    
  }
  
  if(hisp_vra & !black_vra){
    
    
    constr <- redist_constr(city_blocks) %>%
      #########################################################
    # HISPANIC
    add_constr_grp_hinge(
      5,
      vap_hsp,
      total_pop = vap,
      tgts_group = c(0.45)
    ) %>%
      add_constr_grp_hinge(-5,
                           vap_hsp,
                           vap,
                           0.3) %>%
      add_constr_grp_inv_hinge(5,
                               vap_hsp,
                               vap,
                               0.70)
  }
  
  city_blocks<-city_blocks%>%dplyr::select(pop, GEOID, NAME, geometry)
  
  pop_tol<-max(agg_dists$pop/(sum(agg_dists$pop)/(n_distinct(agg_dists$distrct)))-1)
  
  ndists=n_distinct(agg_dists$distrct)
  
  city_blocks<- redist::redist_map(city_blocks, pop_tol=pop_tol, ndists=2, total_pop=pop)
  city_blocks$adj<-redist.adjacency(city_blocks)
  
  
}
print(constr)
plot(constr)
setwd(wd)
ggsave(filename='Figure_A10.tiff', path='./Figures')
