library(dplyr)
library(redist)


aggregated_path <- "/scratch/network/gnovoa/Big City Maps/Big City Aggregated/2010"

block_path <-  "/scratch/network/gnovoa/Big City Maps/Big City Blocks/2010"

agg_city <-   list.files(path = aggregated_path, pattern = ".shp", full.names = FALSE)

filenames_city<-  list.files(path = block_path, pattern = ".shp", full.names = FALSE)

hisp_rpvs<-list()
black_rpvs<-list()
hisp_viables<-list()
black_viables<-list()
cities<-list()

hisp_viables_v<-list()
black_viables_v<-list()

hisp_viables_c<-list()
black_viables_c<-list()

# setwd("/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/RR/Complete Shapefiles/blocks clipped to cities L2 data/2010")
# filenames_city<-list.files(pattern="*.shp", full.names=FALSE) #Generate list of shapefiles
# 
# setwd("/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/RR/Complete Shapefiles/block data aggregated to districts L2/2010")
# agg_city <- list.files(pattern="*.shp", full.names=FALSE) #Generate list of aggregated cities

bank<-read.csv("/scratch/network/gnovoa/county_bank_rpv_plus.csv")
rpv<-read.csv("/scratch/network/gnovoa/rpv_national_summary_2020.csv")
rpv$county<-gsub("city", "City" ,rpv$county) #to match bank
rpv<-rpv[grepl("pre_16_r", rpv$cand),]


rpv <- rpv %>%
  group_by( county, state) %>%
  mutate(
    wdiff = mean - mean[race == "vap_white"]
  ) %>%
  ungroup()


#rpv<-rpv[rpv$race!='vap_white',]
for(i in 1:length(filenames_city)){
  
  
  #remove extension 
  city<-   gsub("_"," ", substr(filenames_city[i], 0, nchar(filenames_city[i])-7))
  cityx<-paste0("^",city, "\\b") #Make sure first word is fully captured
  
  
  bankid<-grep(cityx, bank$city)
  
  state<-bank$state[bankid]
  
  single<-TRUE
  
  # if(bank$props[bankid]!="1")
  #   next
  
  
  countys<-paste0("\\<",bank$rpv_county[bankid],"\\>")
  
  #countys<-ifelse(grepl("county", countys, ignore.case=TRUE),paste0("\\<",countys," County"  ,"\\>"),countys  )
  
  countys<-grep(countys, unique(rpv$county), value=TRUE, ignore.case=TRUE)[1]
  
  if(countys=="Cameron Parish")
    countys<-"Cameron County"
  
  black_rpv<-rpv$mean[rpv$race=="vap_black"& rpv$county==countys & rpv$state==state]
  hisp_rpv<-rpv$mean[rpv$race=="vap_hisp"& rpv$county==countys & rpv$state==state]
  white_rpv<-rpv$mean[rpv$race=="vap_white"& rpv$county==countys & rpv$state==state]
  
  setwd(aggregated_path)
  agg_dists<-st_read(paste0(city,"_districts_summary_2010_L2.shp"), quiet = TRUE)
  ndists<-n_distinct(agg_dists$distrct)
  
  setwd(block_path)
  city_blocks<-st_read(filenames_city[i], quiet = TRUE)
  
  hisp_viable=FALSE
  black_viable=FALSE
  hisp_viable_v=FALSE
  black_viable_v=FALSE
  hisp_viable_c=FALSE
  black_viable_c=FALSE
  
  hisp_vra=FALSE
  black_vra=FALSE
  
  if(sum(city_blocks$pp_hspn)/sum(city_blocks$pop)>(.45/ndists))
    hisp_viable=TRUE
  if(sum(city_blocks$vap_hsp)/sum(city_blocks$vap)>(.45/ndists))
    hisp_viable_v=TRUE
  if(sum(city_blocks$cvp_hsp)/sum(city_blocks$cvap)>(.45/ndists))
    hisp_viable_c=TRUE
  
  
  
  if(sum(city_blocks$pp_blck)/sum(city_blocks$pop)>(.45/ndists))
    black_viable=TRUE
  if(sum(city_blocks$vp_blck)/sum(city_blocks$vap)>(.45/ndists))
    black_viable_v=TRUE
  if(sum(city_blocks$cvp_blc)/sum(city_blocks$cvap)>(.45/ndists))
    black_viable_c=TRUE
  
  if(white_rpv<.5 & hisp_rpv>.5 &hisp_viable)
    hisp_vra=TRUE
  if(white_rpv>.5 & hisp_rpv<.5 &hisp_viable)
    hisp_vra=TRUE
  
  if(white_rpv<.5 & black_rpv>.5 &black_viable)
    black_vra=TRUE
  
  if(white_rpv>.5 & black_rpv<.5 &black_viable)
    black_vra=TRUE
  
  hisp_rpvs[i]<-hisp_vra
  black_rpvs[i]<-black_vra
  hisp_viables[i]<-hisp_viable
  black_viables[i]<-black_viable
  cities[i]<-filenames_city[i]
  
  hisp_viables_v[i]<-hisp_viable_v
  black_viables_v[i]<-black_viable_v
  
  hisp_viables_c[i]<-hisp_viable_c
  black_viables_c[i]<-black_viable_c
  
  
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
  
  print(hisp_viables)
  
}

hisp_rpvs<-unlist(hisp_rpvs)
black_rpvs<-unlist(black_rpvs)
cities<-unlist(cities)
hisp_viables<-unlist(hisp_viables)
black_viables<-unlist(black_viables)
hisp_viables_v<-unlist(hisp_viables_v)
black_viables_v<-unlist(black_viables_v)
hisp_viables_c<-unlist(hisp_viables_c)
black_viables_c<-unlist(black_viables_c)

data.frame(hisp_rpvs, black_rpvs, cities, black_viables,hisp_viables)
data.frame(hisp_rpvs, black_rpvs, substr(cities,1,  nchar(cities)-7), black_viables,hisp_viables)
cities<-substr(cities,1,  nchar(cities)-7)
cities<-gsub("_", " ", cities)
cities<-stringr::str_to_title(cities)


df<-data.frame(hisp_rpvs, black_rpvs, cities, black_viables,hisp_viables, black_viables_v,hisp_viables_v, black_viables_c,hisp_viables_c)
df$hisp_gingles_c<-df$hisp_viables_c & df$hisp_rpvs
df$black_gingles_c<-df$black_viables_c & df$black_rpvs
df$hisp_gingles_v<-df$hisp_viables_v & df$hisp_rpvs
df$black_gingles_v<-df$black_viables_v & df$black_rpvs
write.csv(df, file='rpv_viable_list.csv')


