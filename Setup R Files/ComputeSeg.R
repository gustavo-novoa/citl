library(sf)
library(redistmetrics)
library(seg)


blocks_wd<-"/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/RR/Complete Shapefiles/blocks clipped to cities L2 data/2010"
filenames_blocks<-list.files(path=blocks_wd, pattern="*.shp", full.names=FALSE) #Generate list of shapefiles

aggs_wd<-"/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/RR/Complete Shapefiles/block data aggregated to districts L2/2010"
filenames_aggs <- list.files(path=aggs_wd, pattern="*.shp", full.names=FALSE) #Generate list of aggregated cities

city_names<-list()
black_segs<-list()
hisp_segs<-list()
asian_segs<-list()
white_segs<-list()
black_segs_v<-list()
hisp_segs_v<-list()
asian_segs_v<-list()
white_segs_v<-list()
black_segs_c<-list()
hisp_segs_c<-list()
asian_segs_c<-list()
white_segs_c<-list()
for(i in 1:length(filenames_blocks)){

setwd(blocks_wd)
city_blocks<-st_read(filenames_blocks[i])


non_black<-city_blocks$pop-city_blocks$pp_blck
non_hisp<-city_blocks$pop-city_blocks$pp_hspn
non_asian<-city_blocks$pop-city_blocks$pop_asn
non_white<-city_blocks$pop-city_blocks$pop_wht

non_black_v<-city_blocks$vap-city_blocks$vp_blck
non_hisp_v<-city_blocks$vap-city_blocks$vap_hsp
non_asian_v<-city_blocks$vap-city_blocks$vap_asn
non_white_v<-city_blocks$vap-city_blocks$vap_wht

non_black_c<-city_blocks$impl_cv-city_blocks$cvp_blc
non_hisp_c<-city_blocks$impl_cv-city_blocks$cvp_hsp
non_asian_c<-city_blocks$impl_cv-city_blocks$cvap_sn
non_white_c<-city_blocks$impl_cv-city_blocks$cvp_wht

black_pop<-city_blocks$pp_blck
hisp_pop<-city_blocks$pp_hspn
asian_pop<-city_blocks$pop_asn
white_pop<-city_blocks$pop_wht


black_vap<-city_blocks$vp_blck
hisp_vap<-city_blocks$vap_hsp
asian_vap<-city_blocks$vap_asn
white_vap<-city_blocks$vap_wht

black_cvap<-city_blocks$cvp_blc
hisp_cvap<-city_blocks$cvp_hsp
asian_cvap<-city_blocks$cvap_sn
white_cvap<-city_blocks$cvp_wht




black_seg<-seg(as.matrix(data.frame(non_black, black_pop)))
hisp_seg<-seg(as.matrix(data.frame(non_hisp, hisp_pop)))
asian_seg<-seg(as.matrix(data.frame(non_asian, asian_pop)))
white_seg<-seg(as.matrix(data.frame(non_white, white_pop)))

black_seg_v<-seg(as.matrix(data.frame(non_black_v, black_vap)))
hisp_seg_v<-seg(as.matrix(data.frame(non_hisp_v, hisp_vap)))
asian_seg_v<-seg(as.matrix(data.frame(non_asian_v, asian_vap)))
white_seg_v<-seg(as.matrix(data.frame(non_white_v, white_vap)))

black_seg_c<-seg(as.matrix(data.frame(non_black_c, black_cvap)))
hisp_seg_c<-seg(as.matrix(data.frame(non_hisp_c, hisp_cvap)))
asian_seg_c<-seg(as.matrix(data.frame(non_asian_c, asian_cvap)))
white_seg_c<-seg(as.matrix(data.frame(non_white_c, white_cvap)))

city_names[i]<-substr(filenames_blocks[i], 0, nchar(filenames_blocks[i])-7)
black_segs[i]<-black_seg
hisp_segs[i]<-hisp_seg
white_segs[i]<-white_seg
asian_segs[i]<-asian_seg

black_segs_v[i]<-black_seg_v
hisp_segs_v[i]<-hisp_seg_v
white_segs_v[i]<-white_seg_v
asian_segs_v[i]<-asian_seg_v

black_segs_c[i]<-black_seg_c
hisp_segs_c[i]<-hisp_seg_c
white_segs_c[i]<-white_seg_c
asian_segs_c[i]<-asian_seg_c


}

city<-unlist(city_names)
black_seg<-unlist(black_segs)
hisp_seg<-unlist(hisp_segs)
asian_seg<-unlist(asian_segs)
white_seg<-unlist(white_segs)
black_seg_v<-unlist(black_segs_v)
hisp_segs_v<-unlist(hisp_segs_v)
asian_seg_v<-unlist(asian_segs_v)
white_seg_v<-unlist(white_segs_v)
black_seg_c<-unlist(black_segs_c)
hisp_seg_c<-unlist(hisp_segs_c)
asian_seg_c<-unlist(asian_segs_c)
white_seg_c<-unlist(white_segs_c)

df_seg<-data.frame(city, black_seg, hisp_seg, asian_seg, white_seg,
                   black_seg_v, asian_seg_v, hisp_seg_v, white_seg_v,
                   black_seg_c, hisp_seg_c, asian_seg_c, white_seg_c)
#write.csv(df_seg, file='city_seg_2010.csv')
