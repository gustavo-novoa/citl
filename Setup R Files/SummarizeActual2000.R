library(dplyr)
library(redist)
year<-2000

aggs_wd<-paste0("/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/RR/Complete Shapefiles/block data aggregated to districts L2/",year)
filenames_aggs <- list.files(path=aggs_wd, pattern="*.shp", full.names=FALSE) #Generate list of aggregated cities
setwd(aggs_wd)
#Simulations
cities<-character()
ndists<-numeric()



#Actual counts Initialize
actual_maj_hisp<-numeric()
actual_maj_hisp_v<-numeric()
actual_maj_wht<-numeric()
actual_maj_wht_v<-numeric()
actual_maj_blk<-numeric()
actual_maj_blk_v<-numeric()
actual_maj_asn<-numeric()
actual_maj_asn_v<-numeric()

#Initialize group props 
prop_black<-numeric()
prop_black_v<-numeric()
prop_white<-numeric()
prop_white_v<-numeric()
prop_hisp<-numeric()
prop_hisp_v<-numeric()
prop_asian<-numeric()
prop_asian_v<-numeric()






for(i in 1:length(filenames_aggs)){
  agg_dists<-st_read(filenames_aggs[i])
  name<-substr(filenames_aggs[i],0 , nchar(filenames_aggs[i])-30)
    cities<-c(cities, name)
    ndists<-c(ndists, dim(agg_dists)[1] )
  setwd(aggs_wd)

  #Actual Counts Assign
  actual_maj_hisp<-c(actual_maj_hisp,sum(agg_dists$pp_hspn/agg_dists$pop>.5)/n_distinct(agg_dists$district))
  actual_maj_hisp_v<-c(actual_maj_hisp_v,sum(agg_dists$vap_hsp/agg_dists$vap>.5)/n_distinct(agg_dists$district))

  actual_maj_wht<-c(actual_maj_wht,sum(agg_dists$pop_wht/agg_dists$pop>.5)/n_distinct(agg_dists$district))
  actual_maj_wht_v<-c(actual_maj_wht_v,sum(agg_dists$vap_wht/agg_dists$vap>.5)/n_distinct(agg_dists$district))

  actual_maj_blk<-c(actual_maj_blk,sum(agg_dists$pp_blck/agg_dists$pop>.5)/n_distinct(agg_dists$district))
  actual_maj_blk_v<-c(actual_maj_blk_v,sum(agg_dists$vp_blck/agg_dists$vap>.5)/n_distinct(agg_dists$district))

  actual_maj_asn<-c(actual_maj_asn,sum(agg_dists$pop_asn/agg_dists$pop>.5)/n_distinct(agg_dists$district))
  actual_maj_asn_v<-c(actual_maj_asn_v,sum(agg_dists$vap_asn/agg_dists$vap>.5)/n_distinct(agg_dists$district))

  #Group props assign 
  
  prop_black<-c(prop_black, sum(agg_dists$pp_blck)/sum(agg_dists$pop))
  prop_black_v<-c(prop_black_v,sum(agg_dists$vp_blck)/sum(agg_dists$vap))
  prop_white<-c(prop_white, sum(agg_dists$pop_wht)/sum(agg_dists$pop))
  prop_white_v<-c(prop_white_v, sum(agg_dists$vap_wht)/sum(agg_dists$vap))
  prop_hisp<-c(prop_hisp, sum(agg_dists$pp_hspn)/sum(agg_dists$pop))
  prop_hisp_v<-c(prop_hisp_v, sum(agg_dists$vap_hsp)/sum(agg_dists$vap))
  prop_asian<-c(prop_asian, sum(agg_dists$pop_asn)/sum(agg_dists$pop))
  prop_asian_v<-c(prop_asian_v, sum(agg_dists$vap_asn)/sum(agg_dists$vap))

}

df_sums<-data.frame(cities, ndists,actual_maj_asn,  actual_maj_asn_v, actual_maj_blk,
                actual_maj_blk_v, actual_maj_hisp,
               actual_maj_hisp_v, actual_maj_wht,  actual_maj_wht_v, 
               prop_black, prop_black_v, prop_white, prop_white_v,
               prop_hisp, prop_hisp_v, prop_asian, prop_asian_v)

df_sums$cities<-gsub('\\.', '', df_sums$cities)

names(df_sums)<-paste0(names(df_sums), "_2000")
setwd("/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/RR")
write.csv(df_sums, file='sumactualplans2000.csv')
