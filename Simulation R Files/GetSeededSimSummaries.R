library(dplyr)
library(sf)
library(redist)
library(tibble)
year<-2010

blocks_wd<-paste0("/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/RR/Complete Shapefiles/blocks clipped to cities L2 data/", year)
filenames_blocks<-list.files(path=blocks_wd, pattern="*.shp", full.names=FALSE) #Generate list of shapefiles


aggs_wd<-paste0("/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/RR/Complete Shapefiles/block data aggregated to districts L2/",year)
filenames_aggs <- list.files(path=aggs_wd, pattern="*.shp", full.names=FALSE) #Generate list of aggregated cities

#vra_plans_wd<-paste0("/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/RR/VRA Plans/", year)
#vra_filenames_plans<-list.files(path=plans_wd, pattern="*.rds", full.names=FALSE) #Generate list of aggregated cities
# plans_wd<-vra_plans_wd
# filenames_plans<-vra_filenames_plans

rb_plans_wd<-paste0("~/Princeton Dropbox/Gustavo Novoa/Replication Folder CITL/RB Simulated Plans Seeded")
rb_filenames_plans<-list.files(path=rb_plans_wd, pattern="*.rds", full.names=FALSE) #Generate list of aggregated cities
plans_wd<-rb_plans_wd
filenames_plans<-rb_filenames_plans

#Simulations
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

#Initialize group props 
prop_black<-numeric()
prop_black_v<-numeric()
prop_black_c<-numeric()
prop_white<-numeric()
prop_white_v<-numeric()
prop_white_c<-numeric()
prop_hisp<-numeric()
prop_hisp_v<-numeric()
prop_hisp_c<-numeric()
prop_asian<-numeric()
prop_asian_v<-numeric()
prop_asian_c<-numeric()






for(i in 1:length(filenames_plans)){
  #for(i in 1:5){
  
  
  name<-substr(filenames_plans[i], 0, nchar(filenames_plans[i])-10)
  name<-gsub("_", " ", name)
  simulated_cities[i]<-name
  cities<-c(cities, name)
  id<-(grep(name, filenames_aggs))
  if(name=='jackson')
    id<-id[1] # differentiate between jackson and jacksonville
  setwd(blocks_wd)
  blocks<-st_read(filenames_blocks[id])
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
  
  #Group props assign 
  
  prop_black<-c(prop_black, sum(agg_dists$pp_blck)/sum(agg_dists$pop))
  prop_black_v<-c(prop_black_v,sum(agg_dists$vp_blck)/sum(agg_dists$vap))
  prop_black_c<-c(prop_black_c,sum(agg_dists$cvp_blc)/sum(agg_dists$cvap))
  prop_white<-c(prop_white, sum(agg_dists$pop_wht)/sum(agg_dists$pop))
  prop_white_v<-c(prop_white_v, sum(agg_dists$vap_wht)/sum(agg_dists$vap))
  prop_white_c<-c(prop_white_c, sum(agg_dists$cvp_wht)/sum(agg_dists$cvap))
  prop_hisp<-c(prop_hisp, sum(agg_dists$pp_hspn)/sum(agg_dists$pop))
  prop_hisp_v<-c(prop_hisp_v, sum(agg_dists$vap_hsp)/sum(agg_dists$vap))
  prop_hisp_c<-c(prop_hisp_c, sum(agg_dists$cvp_hsp)/sum(agg_dists$cvap))
  prop_asian<-c(prop_asian, sum(agg_dists$pop_asn)/sum(agg_dists$pop))
  prop_asian_v<-c(prop_asian_v, sum(agg_dists$vap_asn)/sum(agg_dists$vap))
  prop_asian_c<-c(prop_asian_c, sum(agg_dists$cvap_sn)/sum(agg_dists$cvap))
  
  
  setwd(plans_wd)
  
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
           # pct_reg_blk_c =group_frac(city_map[!is.na(city_map$eth1_aa),], eth1_aa, cvp_blc),
           # pct_reg_wht_c =group_frac(city_map, eth1_eur, cvp_wht),
           # pct_reg_hisp_c =group_frac(city_map, eth1_hisp, cvp_hsp),
           # pct_reg_asn_c =group_frac(city_map, eth1_esa, cvap_sn),
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
  
  
  #print(summary(plans))
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


df<-data.frame(cities, 
               "majority_white"=enframe(majority_white), "majority_white_v"=enframe(majority_white_v), "majority_white_c"=enframe(majority_white_c),
               "majority_black"=enframe(majority_black), "majority_black_v"=enframe(majority_black_v), "majority_black_c"=enframe(majority_black_c), 
               "majority_hisp"= enframe(majority_hisp), "majority_hisp_v"= enframe(majority_hisp_v), "majority_hisp_c"=enframe(majority_hisp_c),
               "majority_asian" =enframe(majority_asian), "majority_asian_v"=enframe(majority_asian_v), "majority_asian_c"= enframe(majority_asian_c),
               "majority_nonwhite"= enframe(majority_nonwhite), "majority_nonwhite_v"=enframe(majority_nonwhite_v),"majority_nonwhite_c"=enframe(majority_nonwhite_c))

# actual_maj_asn, actual_maj_asn_c, actual_maj_asn_v, actual_maj_blk, actual_maj_blk_c, actual_maj_blk_v, actual_maj_hisp,actual_maj_hisp_c,
# actual_maj_hisp_v, actual_maj_wht, actual_maj_wht_c, actual_maj_wht_v)


df<-df[, !grepl(pattern='name', colnames(df))]
names(df)<-gsub(".value", "", colnames(df))


# Function to compute 5th percentile, median, and 95th percentile
compute_quantiles <- function(x) {
  q <- quantile(x, probs = c(0.05, 0.5, 0.95), na.rm = TRUE)
  return(c(q5 = q[1], median = q[2], q95 = q[3]))
}

df<-cbind(df, actual_maj_asn, actual_maj_asn_c, actual_maj_asn_v, actual_maj_blk,
          actual_maj_blk_c, actual_maj_blk_v, actual_maj_hisp,actual_maj_hisp_c,
          actual_maj_hisp_v, actual_maj_wht, actual_maj_wht_c, actual_maj_wht_v)


# Apply the function to each column and store results
df_sums <- as.data.frame(do.call(cbind, lapply(df[, 2:ncol(df)], function(col) { #start at 2 to exclude city names
  t(sapply(col, compute_quantiles))
})))

# Rename the columns properly
colnames(df_sums) <- unlist(lapply(names(df[, 2:ncol(df)]), function(name) {
  c(paste0(name, "_5th"), paste0(name, "_median"), paste0(name, "_95th"))
}))

df_sums$city<-cities

df_sums<-cbind(df_sums, actual_maj_asn, actual_maj_asn_c, actual_maj_asn_v, actual_maj_blk,
               actual_maj_blk_c, actual_maj_blk_v, actual_maj_hisp,actual_maj_hisp_c,
               actual_maj_hisp_v, actual_maj_wht, actual_maj_wht_c, actual_maj_wht_v, 
               prop_black, prop_black_v, prop_black_c, prop_white, prop_white_v,prop_white_c,
               prop_hisp, prop_hisp_v, prop_hisp_c, prop_asian, prop_asian_v,prop_asian_c, total_dists)

df_sums<-df_sums%>%rename(ndists=total_dists)


df_seg<-read.csv("/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/RR/city_seg_2010.csv")

df_seg$city <- gsub("_", " ", df_seg$city)

df_sums<-left_join(df_sums, df_seg, by='city')


# Add external data  -------------------- 

# List of Gingles Eligibility
gingles<-read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/RR/gingles_list.csv")
gingles$city<-tolower(gingles$city)
df_sums<-left_join(df_sums, gingles)
# Add state and population columns 
city_state<-read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/RR/city_state_extended.csv")
df_sums<-left_join(df_sums, city_state)

# Add National Election Data 
c1<- foreign::read.dta("/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/warshaw ideology/aip_city_pvote.dta")
c2<-foreign::read.dta("/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/warshaw ideology/aip_consolidated_city_pvote.dta")
c<-rbind(c1,c2)
c<-c%>%filter(year==2008)
c$city[grep("Nashville-Davidson m", c$city)]<-"Nashville"
#remove augusta not in GA and springfield not in MA
c<-c%>%filter(!(grepl("Augusta", c$city) & state!='GA'))
c$city[grepl("consolidated", c$city)]<-"Augusta"
c<-c%>%filter(!(grepl("Springfield", c$city) & state!='MA'))
c<-rename(c, "pres_year" = 'year')
c$city<-tolower(c$city)
c$city[c$city=='columbus' & c$state=='GA'] <- "columbus ga"
c$city[c$city=='springfield' & c$state=='MA'] <- "springfield ma"
c$city[c$city=='lexington-fayette'] <- "lexington"
c$city<-gsub(".", "", c$city, fixed=TRUE)
df_sums<-left_join(df_sums, c, by=c('city', 'state'))


#write.csv(df_sums, 'full_data_for_models.csv')

