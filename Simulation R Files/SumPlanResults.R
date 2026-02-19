
library(dplyr)
library(redist)
year<-2010

wd<-"~/Documents/GitHub/citl"
setwd(wd)

blocks_wd<- paste0(wd,"/Complete Shapefiles/blocks clipped to cities/2010")
filenames_blocks<-list.files(path=blocks_wd, pattern="*.shp", full.names=FALSE) #Generate list of shapefiles

aggs_wd<-paste0(wd, "/Complete Shapefiles/block data aggregated to districts/2010")
filenames_aggs <- list.files(path=aggs_wd, pattern="*.shp", full.names=FALSE) #Generate list of aggregated cities

rb_plans_wd<- paste0(wd,"/RB Seeded Sims")
filenames_plans<-list.files(path=rb_plans_wd, pattern="*.rds", full.names=FALSE) #Generate list of aggregated cities


cities<-character()
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

for(i in 1:length(filenames_plans)){


name<-substr(filenames_plans[i], 0, nchar(filenames_plans[i])-10)
name<-gsub("_", " ", name)
cities<-c(cities, name)
id<- grep(paste0("^", name, "_"), filenames_aggs)
setwd(blocks_wd)
blocks<-st_read(filenames_blocks[id])
setwd(aggs_wd)
agg_dists<-st_read(filenames_aggs[id])
setwd(rb_plans_wd)

pop_tol<-max(agg_dists$pop/(sum(agg_dists$pop)/(n_distinct(agg_dists$distrct)))-1)
ndists<-n_distinct(agg_dists$distrct)

city_map<- redist::redist_map(blocks, pop_tol=pop_tol, ndists=ndists, total_pop=pop)
city_map$adj<-redist.adjacency(city_map)

plans<-readRDS(filenames_plans[i])  



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


print(summary(plans))

majority_white[[i]] <-plans%>%group_by(draw)%>%summarize(sum(majority_white))%>%pull(2)
majority_white_v[[i]] <-plans%>%group_by(draw)%>%summarize(sum(majority_white_v))%>%pull(2)
majority_white_c[[i]] <-plans%>%group_by(draw)%>%summarize(sum(majority_white_c))%>%pull(2)
majority_black[[i]] <-plans%>%group_by(draw)%>%summarize(sum(majority_black))%>%pull(2)
majority_black_v[[i]] <-plans%>%group_by(draw)%>%summarize(sum(majority_black_v))%>%pull(2)
majority_black_c[[i]] <-plans%>%group_by(draw)%>%summarize(sum(majority_black_c))%>%pull(2)
majority_hisp[[i]] <-plans%>%group_by(draw)%>%summarize(sum(majority_hisp))%>%pull(2)
majority_hisp_v[[i]] <-plans%>%group_by(draw)%>%summarize(sum(majority_hisp_v))%>%pull(2)
majority_hisp_c[[i]]<-plans%>%group_by(draw)%>%summarize(sum(majority_hisp_c))%>%pull(2)
majority_asian[[i]] <-plans%>%group_by(draw)%>%summarize(sum(majority_asian))%>%pull(2)
majority_asian_v[[i]] <-plans%>%group_by(draw)%>%summarize(sum(majority_asian_v))%>%pull(2)
majority_asian_c[[i]] <-plans%>%group_by(draw)%>%summarize(sum(majority_asian_c))%>%pull(2)
majority_nonwhite[[i]] <-plans%>%group_by(draw)%>%summarize(sum(majority_nonwhite))%>%pull(2)
majority_nonwhite_v[[i]] <-plans%>%group_by(draw)%>%summarize(sum(majority_nonwhite_v))%>%pull(2)
majority_nonwhite_c[[i]] <-plans%>%group_by(draw)%>%summarize(sum(majority_nonwhite_c))%>%pull(2)

total_dists<-c(total_dists,n_distinct(agg_dists$distrct))
}
  

save.image(file = "Compiled Results/districts_rb.RData")
