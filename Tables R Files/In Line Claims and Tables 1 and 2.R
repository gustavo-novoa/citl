library(dplyr)
library(gt)
library(sf)
# Set working directory where files are saved
wd<-"/Users/gnovoa/Princeton Dropbox/Gustavo Novoa/Replication Folder CITL/"
setwd(wd)
load("districtFTPVRA.RData")

#dataframe with more election data -----
df2<-read.csv("full_data_for_models_wdbk.csv")
df2$hispanic_mayor<-ifelse(df2$mayor_race=='hispanic',1,0)


# 5.1 Data --------------------------------------------

#Number of cities
nrow(df2) # Page 133 cities 
#Number of states 
n_distinct(df2$state) # 39 states

mean(df2$city_pop) # Population mean 367451
median(df2$city_pop) # Population median 242803
sum(df2$city_pop<150000) # 28 cities <150000
sum(df2$city_pop<100000) # 10 cities <100000

# 5.3 VRA Informed Simulations ------------------------

length(df2$cities[df2$hisp_gingles_c|df2$black_gingles_c])


# 6.1 Implemented Districts  ----------------------------

# At least one minority district viable = 122 
sum(df2$prop_black_c>1/df2$ndists*.45 | df2$prop_hisp_c > 1/df2$ndists *.45 |  df2$prop_asian_c > 1/df2$ndists *.45)
#At least one minority district implemented = 91
sum(df2$actual_maj_hisp_c>0|df2$actual_maj_blk_c>0|df2$actual_maj_asn_c>0)
#At least one Black = 62
sum(df2$actual_maj_blk_c>0)
#At least one Hispanic = 34
sum(df2$actual_maj_hisp_c>0)
# At least one Black & One Hispanic  = 8
sum(df2$actual_maj_hisp_c>0 & df2$actual_maj_blk_c>0)


aggs_wd<-paste0(wd, "/Complete Shapefiles/block data aggregated to districts L2/2010")
filenames_aggs <- list.files(path=aggs_wd, pattern="*.shp", full.names=FALSE) #Generate list of aggregated cities
setwd(aggs_wd)
#Initialize group props 

hisp_dist_concentration <- numeric()
black_dist_concentration<- numeric()
asian_dist_concentration<- numeric()
white_dist_concentration<- numeric()


for(i in 1:length(filenames_aggs)){
  agg_dists<-st_read(filenames_aggs[i])
  name<-substr(filenames_aggs[i],0 , nchar(filenames_aggs[i])-30)
  setwd(aggs_wd)
  
  # Concentration 
  
    hisp_dist_concentration <- c(hisp_dist_concentration, agg_dists$h_pct_c[agg_dists$h_pct_c>=.45])
    black_dist_concentration<- c(black_dist_concentration,agg_dists$b_pct_c[agg_dists$b_pct_c>=.45])
    asian_dist_concentration<- c(asian_dist_concentration,agg_dists$a_pct_c[agg_dists$a_pct_c>=.45])
    white_dist_concentration<- c(white_dist_concentration,agg_dists$w_pct_c[agg_dists$w_pct_c>=.45])

}

# District Concentrations 

mean(black_dist_concentration) # Mean Black district was 68% Black
median(black_dist_concentration) # Median Black district was 65% Black
mean(hisp_dist_concentration) # Mean Hispanic district was 64% Hispanic 
median(hisp_dist_concentration) # Median Hispanic district was 58% Hispanic
mean(asian_dist_concentration) # Mean Asian District was 53% Asian
median(asian_dist_concentration) # Median Asian District was 54% Asian

# Dissimilarity Indices 

mean(df2$black_seg_c[df2$black_viables_c]) # .63 Black dissimialrity 
mean(df2$hisp_seg_c[df2$hisp_viables_c]) # .47 Hispanic dissimilarity
mean(df2$asian_seg_c[df2$prop_asian_c > 1/df2$ndists *.45])

# 6.2 Race-Blind Simulation Results ---------------------------------

length(unique(df2$cities[df2$majority_black_c_median>0 | df2$majority_hisp_c_median>0 | df2$majority_asian_c_median>0]))
#80 cities  

sum(df2$majority_black_c_95th>0 | df2$majority_hisp_c_95th>0 | df2$majority_asian_c_95th>0)
# 93 cities 

setwd(wd)
# Table 1 RB Sims   ------------------------------------
make_table<-function(table_data, title, filename){
  formatted_table<-table_data %>%
    gt() %>%
    tab_header(
      title = title,
      subtitle = "Comparison of Hispanic and Black Districts"
    ) %>%
    cols_label(
      District_Type = "District Type",
      Fewer_than_Median = "Fewer than Median",
      Equal_to_Median = "Equal to Median",
      Greater_than_Median = "Greater than Median",
      Within_Range = "Within Simulated Range",
      Outside_Range = "Outside Simulated Range",
      Total = "Total"
    ) %>%
    tab_spanner(
      label = "Simulation Results",
      columns = c(Fewer_than_Median, Equal_to_Median, Greater_than_Median)
    ) %>%
    tab_spanner(
      label = "Range Analysis",
      columns = c(Within_Range, Outside_Range)
    ) %>%
    cols_align(
      align = "center",
      columns = c(Fewer_than_Median, Equal_to_Median, Greater_than_Median, 
                  Within_Range, Outside_Range, Total)
    ) %>%
    cols_align(
      align = "left",
      columns = District_Type
    ) %>%
    tab_style(
      style = cell_fill(color = "#f0f0f0"),
      locations = cells_body(rows = seq(1, nrow(table_data), 2))
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_spanners()
    ) %>%
    tab_options(
      table.font.size = 12,
      heading.title.font.size = 16,
      heading.subtitle.font.size = 12,
      table.border.top.style = "solid",
      table.border.bottom.style = "solid",
      column_labels.border.top.style = "solid",
      column_labels.border.bottom.style = "solid"
    )
  
  # Display the table
  formatted_table
  

  gtsave(formatted_table, filename=filename)
  
}



#Hisp. Districts 
#Fewer: 4
h1<-sum(df2$actual_maj_hisp_c[df2$hisp_viables_c]<df2$majority_hisp_c_median[df2$hisp_viables_c])
#Equal: 60
h2<-sum(df2$actual_maj_hisp_c[df2$hisp_viables_c]==df2$majority_hisp_c_median[df2$hisp_viables_c])
# Greater: 9
h3<-sum(df2$actual_maj_hisp_c[df2$hisp_viables_c]>df2$majority_hisp_c_median[df2$hisp_viables_c])
# Within Range: 68
h4<-sum(df2$actual_maj_hisp_c[df2$hisp_viables_c]>=df2$majority_hisp_c_5th[df2$hisp_viables_c] & df2$actual_maj_hisp_c[df2$hisp_viables_c]<=df2$majority_hisp_c_95th[df2$hisp_viables_c])
# Outside Range: 5
h5<-sum(df2$actual_maj_hisp_c[df2$hisp_viables_c]<df2$majority_hisp_c_5th[df2$hisp_viables_c] | df2$actual_maj_hisp_c[df2$hisp_viables_c]>df2$majority_hisp_c_95th[df2$hisp_viables_c])
# Total: 73
h6<-length(df2$cities[df2$hisp_viables_c])

#Black Districts 
#Fewer: 0
b1<-sum(df2$actual_maj_blk_c[df2$black_viables_c]<df2$majority_black_c_median[df2$black_viables_c])
#Equal: 65
b2<-sum(df2$actual_maj_blk_c[df2$black_viables_c]==df2$majority_black_c_median[df2$black_viables_c])
# Greater: 29
b3<-sum(df2$actual_maj_blk_c[df2$black_viables_c]>df2$majority_black_c_median[df2$black_viables_c])
# Within Range: 82
b4<-sum(df2$actual_maj_blk_c[df2$black_viables_c]>=df2$majority_black_c_5th[df2$black_viables_c] & df2$actual_maj_blk_c[df2$black_viables_c]<=df2$majority_black_c_95th[df2$black_viables_c])
# Outside Range: 12
b5<-sum(df2$actual_maj_blk_c[df2$black_viables_c]<df2$majority_black_c_5th[df2$black_viables_c] | df2$actual_maj_blk_c[df2$black_viables_c]>df2$majority_black_c_95th[df2$black_viables_c])
# Total: 94
b6<-length(df2$cities[df2$black_viables_c])

rb_table_data <- data.frame(
  District_Type = c("Total Hisp. Districts", "Black Districts"),
  Fewer_than_Median = c(h1, b1),
  Equal_to_Median = c(h2, b2),
  Greater_than_Median = c(h3, b3),
  Within_Range = c(h4, b4),
  Outside_Range = c(h5, b5),
  Total = c(h6, b6)
)

make_table(rb_table_data, "Race Blind Simulation Results", "table_1.html")


# 6.3 VRA Informed Results Table 2   --------------------------------------
#Hisp. Districts 
# Fewer: 1
vra_h1<-sum(vra_hc$data$actual<vra_hc$data$median)
# Equal: 19
vra_h2<-sum(vra_hc$data$actual==vra_hc$data$median)
# Greater: 4
vra_h3<-sum(vra_hc$data$actual>vra_hc$data$median)
# Within Range: 23
vra_h4<-sum(vra_hc$data$actual>=vra_hc$data$fifth & vra_hc$data$actual<=vra_hc$data$ninetyfifth )
# Outside Range: 1
vra_h5<-sum(vra_hc$data$actual<vra_hc$data$fifth | vra_hc$data$actual>vra_hc$data$ninetyfifth )
# Total: 24
vra_h6<-length(vra_hc$data$cities)

#Black Districts 
#combine two datasets 
vra_bc<-rbind(vra_bc[[1]]$data, vra_bc[[2]]$data)
# Fewer: 2
vra_b1<-sum(vra_bc$actual<vra_bc$median)
# Equal: 23
vra_b2<-sum(vra_bc$actual==vra_bc$median)
# Greater: 17
vra_b3<-sum(vra_bc$actual>vra_bc$median)
# Within Range: 35
vra_b4<-sum(vra_bc$actual>=vra_bc$fifth & vra_bc$actual<=vra_bc$ninetyfifth )
# Outside Range: 7
vra_b5<-sum(vra_bc$actual<vra_bc$fifth | vra_bc$actual>vra_bc$ninetyfifth )
# Total: 42
vra_b6<-length(vra_bc$cities)

vra_table_data <- data.frame(
  District_Type = c("Total Hisp. Districts", "Black Districts"),
  Fewer_than_Median = c(vra_h1, vra_b1),
  Equal_to_Median = c(vra_h2, vra_b2),
  Greater_than_Median = c(vra_h3, vra_b3),
  Within_Range = c(vra_h4, vra_b4),
  Outside_Range = c(vra_h5, vra_b5),
  Total = c(vra_h6, vra_b6)
)


# Create formatted table
make_table(vra_table_data, "VRA Simulation Results","table_2.html")
