
library(redist)
library(tidyverse)
library(forcats)

wd<-"~/Documents/GitHub/citl"
setwd(wd)
year<-2010

vra_ftp <- function(dists_simd, additional_vector, cities, xl) {
  
  xl <- xl
  
  stats_df <- do.call(rbind, lapply(dists_simd, function(vec) {
    data.frame(
      fifth = quantile(vec, .05),
      median = median(vec),
      ninetyfifth = quantile(vec, .95)
    )
  }))
  
  stats_df$cities <- cities
  
  plot_data <- stats_df %>%
    mutate(actual = additional_vector) %>%
    filter(!(fifth == 0 & actual == 0)) %>%
    mutate(gap = (actual - median)) %>%
    arrange(desc(actual))  # Sort cities by gap in descending order
  
  num_cities <- nrow(plot_data)
  
  if (num_cities > 40) {
    split_index <- ceiling(num_cities / 2)
    plot_data1 <- plot_data[1:split_index, ]
    plot_data2 <- plot_data[(split_index + 1):num_cities, ]
    
    p1 <- ggplot(plot_data1, aes(y = fct_reorder(cities, actual))) +
      geom_segment(aes(x = fifth, xend = ninetyfifth, yend = cities), color = "black") +
      geom_point(aes(x = median), color = "darkgreen", size = 5, shape = 18) +
      geom_point(aes(x = actual), color = "red", size = 2) +
      labs( y = NULL, x = xl) +
      xlim(0,1)+
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8), plot.title = element_text(size = 12, face = "bold"))
    
    p2 <- ggplot(plot_data2, aes(y = fct_reorder(cities, actual))) +
      geom_segment(aes(x = fifth, xend = ninetyfifth, yend = cities), color = "black") +
      geom_point(aes(x = median), color = "darkgreen", size = 5, shape = 18) +
      geom_point(aes(x = actual), color = "red", size = 2) +
      labs(y = NULL, x = xl) +
      xlim(0,1)+
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8))
    
    print(p1 + p2)
  } else {
    p <- ggplot(plot_data, aes(y = fct_reorder(cities, actual))) +
      geom_segment(aes(x = fifth, xend = ninetyfifth, yend = cities), color = "black") +
      geom_point(aes(x = median), color = "darkgreen", size = 5, shape = 18) +
      geom_point(aes(x = actual), color = "red", size = 2) +
      labs(y = NULL, x=xl) +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8), plot.title = element_text(size = 12, face = "bold"))
    
    print(p)

  }
}



blocks_wd<- paste0(wd,"/Complete Shapefiles/blocks clipped to cities/2010")
filenames_blocks<-list.files(path=blocks_wd, pattern="*.shp", full.names=FALSE) #Generate list of shapefiles


aggs_wd<-paste0(wd, "/Complete Shapefiles/block data aggregated to districts/2010")
filenames_aggs <- list.files(path=aggs_wd, pattern="*.shp", full.names=FALSE) #Generate list of aggregated cities

vra_plans_wd<- paste0(wd,"/VRA Seeded Sims")
filenames_plans<-list.files(path=vra_plans_wd, pattern="*.rds", full.names=FALSE) #Generate list of aggregated cities


cities<-character()
simulated_cities<-list()
majority_white <-list()
majority_white_v <-list()
majority_white_c <-list()
majority_black <-list()
majority_black_v <-list()
majority_black_c <-list()
majority_hisp <-list()
majority_hisp_v <-list()
majority_hisp_c<-list()
majority_asian <-list()
majority_asian_v <-list()
majority_asian_c <-list()
majority_nonwhite <-list()
majority_nonwhite_v <-list()
majority_nonwhite_c <-list()
total_dists<-numeric()


#Actual counts Initialize
actual_maj_hisp<-numeric()
actual_maj_hisp_v<-numeric()
actual_maj_hisp_c<-numeric()
actual_maj_wht<-numeric()
actual_maj_wht_v<-numeric()
actual_maj_wht_c<-numeric()
actual_maj_blk<-numeric()
actual_maj_blk_v<-numeric()
actual_maj_blk_c<-numeric()
actual_maj_asn<-numeric()
actual_maj_asn_v<-numeric()
actual_maj_asn_c<-numeric()

for(i in 1:length(filenames_plans)){
  
  
  name<-substr(filenames_plans[i], 0, nchar(filenames_plans[i])-10)
  name<-gsub("_", " ", name)
  cities<-c(cities, name)
  id<-(grep(name, filenames_aggs))
  if(name=='jackson')
    id<-id[1] # differentiate between jackson and jacksonville
  setwd(blocks_wd)
  blocks<-st_read(filenames_blocks[id])
  simulated_cities[i]<-cities
  setwd(aggs_wd)
  agg_dists<-st_read(filenames_aggs[id])
  #Actual Counts Assign
  actual_maj_hisp<-c(actual_maj_hisp,sum(agg_dists$pp_hspn/agg_dists$pop>.45)/n_distinct(agg_dists$distrct))
  actual_maj_hisp_v<-c(actual_maj_hisp_v,sum(agg_dists$vap_hsp/agg_dists$vap>.45)/n_distinct(agg_dists$distrct))
  actual_maj_hisp_c<-c(actual_maj_hisp_c,sum(agg_dists$cvp_hsp/agg_dists$cvap>.45)/n_distinct(agg_dists$distrct))
  
  actual_maj_wht<-c(actual_maj_wht,sum(agg_dists$pop_wht/agg_dists$pop>.45)/n_distinct(agg_dists$distrct))
  actual_maj_wht_v<-c(actual_maj_wht_v,sum(agg_dists$vap_wht/agg_dists$vap>.45)/n_distinct(agg_dists$distrct))
  actual_maj_wht_c<-c(actual_maj_wht_c,sum(agg_dists$cvp_wht/agg_dists$cvap>.45)/n_distinct(agg_dists$distrct))
  
  actual_maj_blk<-c(actual_maj_blk,sum(agg_dists$pp_blck/agg_dists$pop>.45)/n_distinct(agg_dists$distrct))
  actual_maj_blk_v<-c(actual_maj_blk_v,sum(agg_dists$vp_blck/agg_dists$vap>.45)/n_distinct(agg_dists$distrct))
  actual_maj_blk_c<-c(actual_maj_blk_c,sum(agg_dists$cvp_blc/agg_dists$cvap>.45)/n_distinct(agg_dists$distrct))
  
  actual_maj_asn<-c(actual_maj_asn,sum(agg_dists$pop_asn/agg_dists$pop>.45)/n_distinct(agg_dists$distrct))
  actual_maj_asn_v<-c(actual_maj_asn_v,sum(agg_dists$vap_asn/agg_dists$vap>.45)/n_distinct(agg_dists$distrct))
  actual_maj_asn_c<-c(actual_maj_asn_c,sum(agg_dists$cvap_sn/agg_dists$cvap>.45)/n_distinct(agg_dists$distrct))
  
  
  
  setwd(vra_plans_wd)
  
  pop_tol<-max(agg_dists$pop/(sum(agg_dists$pop)/(n_distinct(agg_dists$distrct)))-1)
  ndists<-n_distinct(agg_dists$distrct)
  
  city_map<- redist::redist_map(blocks, pop_tol=pop_tol, ndists=ndists, total_pop=pop)
  city_map$adj<-redist.adjacency(city_map)
  
  plans<-readRDS(filenames_plans[i])  
  
  
  if(year==2020){
    plans = plans %>%
      mutate(pop_dev = abs(total_pop / get_target(city_map) - 1),
             pct_white   = group_frac(city_map, pop_wht, pop),
             pct_white_v = group_frac(city_map, vap_wht, vap),
             pct_white_c = group_frac(city_map, cvp_wht, cvap),
             pct_black   = group_frac(city_map, pp_blck, pop),
             pct_black_v = group_frac(city_map, vp_blck, vap),
             pct_black_c = group_frac(city_map, cvp_blc, cvap),
             pct_hisp    = group_frac(city_map, pp_hspn, pop),
             pct_hisp_v  = group_frac(city_map, vap_hsp, vap),
             pct_hisp_c  = group_frac(city_map, cvp_hsp, cvap),
             pct_asian   = group_frac(city_map, pop_asn, pop),
             pct_asian_v   = group_frac(city_map, vap_asn, vap),
             pct_asian_c   = group_frac(city_map, cvap_sn, cvap),
             pct_nonwhite  = group_frac(city_map, pop-pop_wht, pop),
             pct_nonwhite_v= group_frac(city_map, pop-pop_wht, vap),
             pct_nonwhite_c= group_frac(city_map, cvap-cvp_wht, cvap),
             pct_reg_blk_c =group_frac(city_map, reg_aa, cvp_blc),
             pct_reg_wht_c =group_frac(city_map, reg_eur, cvp_wht),
             pct_reg_hisp_c =group_frac(city_map, reg_hisp, cvp_hsp),         
             pct_reg_asn_c =group_frac(city_map, reg_esa, cvap_sn),   
             pct_vtd_reg_eur   =group_frac(city_map, vtd_eur, reg_eur),
             pct_vtd_reg_aa    =group_frac(city_map, vtd_aa, reg_aa),
             pct_vtd_reg_esa   =group_frac(city_map, vtd_esa, reg_esa),
             pct_vtd_reg_hisp  =group_frac(city_map, vtd_hisp, reg_hisp),
             pct_vtd_reg_eur   =group_frac(city_map, vtd_eur, cvp_wht),
             pct_vtd_reg_aa    =group_frac(city_map, vtd_aa, cvp_blc),
             pct_vtd_reg_esa   =group_frac(city_map, vtd_esa, cvap_sn),
             pct_vtd_reg_hisp  =group_frac(city_map, vtd_hisp, cvp_hsp)
      )}
  
  if(year==2010){
    
    plans = plans %>%
      mutate(pop_dev = abs(total_pop / get_target(city_map) - 1),
             pct_white   = group_frac(city_map, pop_wht, pop),
             pct_white_v = group_frac(city_map, vap_wht, vap),
             pct_white_c = group_frac(city_map, cvp_wht, cvap),
             pct_black   = group_frac(city_map, pp_blck, pop),
             pct_black_v = group_frac(city_map, vp_blck, vap),
             pct_black_c = group_frac(city_map, cvp_blc, cvap),
             pct_hisp    = group_frac(city_map, pp_hspn, pop),
             pct_hisp_v  = group_frac(city_map, vap_hsp, vap),
             pct_hisp_c  = group_frac(city_map, cvp_hsp, cvap),
             pct_asian   = group_frac(city_map, pop_asn, pop),
             pct_asian_v   = group_frac(city_map, vap_asn, vap),
             pct_asian_c   = group_frac(city_map, cvap_sn, cvap),
             pct_nonwhite  = group_frac(city_map, pop-pop_wht, pop),
             pct_nonwhite_v= group_frac(city_map, pop-pop_wht, vap),
             pct_nonwhite_c= group_frac(city_map, cvap-cvp_wht, cvap)
 
      )
    
    
    
    
  }
  
  plans <- plans %>%
    group_by(district) %>%
    mutate(
      majority_white = if_else(pct_white >= 0.45, 1, 0),
      majority_white_v = if_else(pct_white_v >= 0.45, 1, 0),
      majority_white_c = if_else(pct_white_c >= 0.45, 1, 0),
      
      majority_black = if_else(pct_black >= 0.45, 1, 0),
      majority_black_v = if_else(pct_black_v >= 0.45, 1, 0),
      majority_black_c = if_else(pct_black_c >= 0.45, 1, 0),
      
      majority_hisp = if_else(pct_hisp >= 0.45, 1, 0),
      majority_hisp_v = if_else(pct_hisp_v >= 0.45, 1, 0),
      majority_hisp_c = if_else(pct_hisp_c >= 0.45, 1, 0),
      
      majority_asian = if_else(pct_asian >= 0.45, 1, 0),
      majority_asian_v = if_else(pct_asian_v >= 0.45, 1, 0),
      majority_asian_c = if_else(pct_asian_c >= 0.45, 1, 0),
      
      majority_nonwhite = if_else(pct_nonwhite >= 0.45, 1, 0),
      majority_nonwhite_v = if_else(pct_nonwhite_v >= 0.45, 1, 0),
      majority_nonwhite_c = if_else(pct_nonwhite_c >= 0.45, 1, 0)
    ) %>%
    ungroup()
  
  plans <- plans %>%
    group_by(draw) %>%
    mutate(across(
      .cols = starts_with("pct_") & !contains("reg") & !contains("vtd"),
      .fns = ~ sum(. > 0.45),
      .names = "plan_maj_{.col %>% sub('^pct_', '', .)}"
    )) %>%
    ungroup()
  if(i!=24)
  print(summary(plans))
  total_dists<-c(total_dists,n_distinct(agg_dists$distrct))
  
  majority_white[[i]] <-plans%>%group_by(draw)%>%summarize(sum(majority_white))%>%pull(2)/total_dists[i]
  majority_white_v[[i]] <-plans%>%group_by(draw)%>%summarize(sum(majority_white_v))%>%pull(2)/total_dists[i]
  majority_white_c[[i]] <-plans%>%group_by(draw)%>%summarize(sum(majority_white_c))%>%pull(2)/total_dists[i]
  majority_black[[i]] <-plans%>%group_by(draw)%>%summarize(sum(majority_black))%>%pull(2)/total_dists[i]
  majority_black_v[[i]] <-plans%>%group_by(draw)%>%summarize(sum(majority_black_v))%>%pull(2)/total_dists[i]
  majority_black_c[[i]] <-plans%>%group_by(draw)%>%summarize(sum(majority_black_c))%>%pull(2)/total_dists[i]
  majority_hisp[[i]] <-plans%>%group_by(draw)%>%summarize(sum(majority_hisp))%>%pull(2)/total_dists[i]
  majority_hisp_v[[i]] <-plans%>%group_by(draw)%>%summarize(sum(majority_hisp_v))%>%pull(2)/total_dists[i]
  majority_hisp_c[[i]]<-plans%>%group_by(draw)%>%summarize(sum(majority_hisp_c))%>%pull(2)/total_dists[i]
  majority_asian[[i]] <-plans%>%group_by(draw)%>%summarize(sum(majority_asian))%>%pull(2)/total_dists[i]
  majority_asian_v[[i]] <-plans%>%group_by(draw)%>%summarize(sum(majority_asian_v))%>%pull(2)/total_dists[i]
  majority_asian_c[[i]] <-plans%>%group_by(draw)%>%summarize(sum(majority_asian_c))%>%pull(2)/total_dists[i]
  majority_nonwhite[[i]] <-plans%>%group_by(draw)%>%summarize(sum(majority_nonwhite))%>%pull(2)
  majority_nonwhite_v[[i]] <-plans%>%group_by(draw)%>%summarize(sum(majority_nonwhite_v))%>%pull(2)/total_dists[i]
  majority_nonwhite_c[[i]] <-plans%>%group_by(draw)%>%summarize(sum(majority_nonwhite_c))%>%pull(2)/total_dists[i]
  
}


vra_wp<-vra_ftp(majority_white, actual_maj_wht,stringr::str_to_title(cities), xl="Proportion Majority-White Districts (Pop.)")
vra_wv<-vra_ftp(majority_white_v, actual_maj_wht_v,stringr::str_to_title(cities), xl="Proportion Majority-White Districts (VAP)")
vra_wc<-vra_ftp(majority_white_c, actual_maj_wht_c,stringr::str_to_title(cities), xl="Proportion Majority-White Districts (CVAP)")

vra_hp<-vra_ftp(majority_hisp, actual_maj_hisp,stringr::str_to_title(cities), xl="Proportion Majority-Hispanic Districts (Pop.)") 
vra_hv<-vra_ftp(majority_hisp_v, actual_maj_hisp_v,stringr::str_to_title(cities), xl="Proportion Majority-Hispanic Districts (VAP)") 
vra_hc<-vra_ftp(majority_hisp_c, actual_maj_hisp_c,stringr::str_to_title(cities), xl="Proportion Majority-Hispanic Districts") 

vra_bp<-vra_ftp(majority_black, actual_maj_blk,stringr::str_to_title(cities), xl="Proportion Majority-Black Districts (Pop.)") 
vra_bv<-vra_ftp(majority_black_v, actual_maj_blk_v,stringr::str_to_title(cities), xl="Proportion Majority-Black Districts (VAP)") 
vra_bc<-vra_ftp(majority_black_c, actual_maj_blk_c,stringr::str_to_title(cities), xl="Proportion Majority-Black Districts") 

vra_ap<-vra_ftp(majority_asian, actual_maj_asn,stringr::str_to_title(cities), xl="Proportion Majority-Asian Districts (Pop.)") 
vra_av<-vra_ftp(majority_asian_v, actual_maj_asn_v,stringr::str_to_title(cities), xl="Proportion Majority-Asian Districts (VAP)") 
vra_ac<-vra_ftp(majority_asian_c, actual_maj_asn_c,stringr::str_to_title(cities), xl="Proportion Majority-Asian Districts (CVAP)") 

vra_lists<-list(majority_white=majority_white,
               majority_white_v=majority_white_v,
               majority_white_c=majority_white_c,
               actual_maj_wht = actual_maj_wht, 
               actual_maj_wht_v=actual_maj_wht_v,
               actual_maj_wht_c=actual_maj_wht_c,
               majority_hisp = majority_hisp,
               majority_hisp_v=majority_hisp_v,
               majority_hisp_c=majority_hisp_c,
               actual_maj_hisp=actual_maj_hisp,
               actual_maj_hisp_v= actual_maj_hisp_v,
               actual_maj_hisp_c= actual_maj_hisp_c,
               majority_black=majority_black,
               majority_black_v=majority_black_v,
               majority_black_c=majority_black_c,
               actual_maj_blk=actual_maj_blk,
               actual_maj_blk_v=actual_maj_blk_v,
               actual_maj_blk_c=actual_maj_blk_c,
               majority_asian=majority_asian,
               majority_asian_v=majority_asian_v,
               majority_asian_c=majority_asian_c,
               actual_maj_asn=actual_maj_asn,
               actual_maj_asn_v=actual_maj_asn_v,
               actual_maj_asn_c=actual_maj_asn_c,
               cities=cities
)

save(vra_lists, file='/Users/gnovoa/Documents/GitHub/citl/Compiled Results/vra_ft_data.RData')


