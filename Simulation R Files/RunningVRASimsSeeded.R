library(dplyr)
library(redist)
library(sf)

set.seed(2026)

aggregated_path <- "/scratch/network/gnovoa/Big City Maps/Big City Aggregated/2010"

block_path <-  "/scratch/network/gnovoa/Big City Maps/Big City Blocks/2010"

agg_city <-   list.files(path = aggregated_path, pattern = ".shp", full.names = FALSE)

filenames_city<-  list.files(path = block_path, pattern = ".shp", full.names = FALSE)

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


for(i in 1:length(filenames_city)){
  
  if(exists('constr'))
    rm(constr)
  
  cli::cli_alert(paste0("Now on ", filenames_city[i]))
  
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
  
  setwd(aggregated_path)
  agg_dists<-st_read(paste0(city,"_districts_summary_2010_L2.shp"), quiet = TRUE)
  ndists<-n_distinct(agg_dists$distrct)
  
  setwd(block_path)
  city_blocks<-st_read(filenames_city[i], quiet = TRUE)
  
  hisp_viable=FALSE
  black_viable=FALSE
  
  hisp_vra=FALSE
  black_vra=FALSE
  
  
  if(sum(city_blocks$pp_hspn)/sum(city_blocks$pop)>(.5/ndists) )
    hisp_viable=TRUE
  
  if(sum(city_blocks$pp_blck)/sum(city_blocks$pop)>(.5/ndists) )
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
  
  city_blocks<- redist::redist_map(city_blocks, pop_tol=pop_tol, ndists=ndists, total_pop=pop)
  city_blocks$adj<-redist.adjacency(city_blocks)
  
  
  
  if(exists('constr')){
    setwd("/scratch/network/gnovoa/VRA Plans/2010_seeded")
    if (file.exists(paste0(substr(filenames_city[i], 1, nchar(filenames_city[i])-7), "_plans.rds"))) {
      cli::cli_alert("Simulations for this city already produced.")
      next
    }
    cli::cli_alert(paste0("Simulating ", filenames_city[i]))
    
    
    vra_plans<-redist_smc(city_blocks, nsims=25000, ncores=parallel::detectCores(), runs=2, constraints = constr)
    print(paste0("#",i, " ", filenames_city[i], " sim complete"))
    
    
    saveRDS(vra_plans, file=paste0(substr(filenames_city[i], 0, nchar(filenames_city[i])-7), "_plans.rds"))
    rm(constr)
    
  }
  else{
    cli::cli_alert(paste0(filenames_city[i], "not eligible."))
  }
}
