library(dplyr)
library(sf)
library(redist)
library(tibble)
library(data.table)
library(ggplot2)
library(ggridges)
library(stringr)
# More memory efficient than data.frame
# Set data.table threads to optimize memory usage
setDTthreads(threads = 4)

year <- 2010

# Define paths
blocks_wd <-"~/Documents/GitHub/citl/Complete Shapefiles/blocks clipped to cities/2010"
aggs_wd <- "~/Documents/GitHub/citl/Complete Shapefiles/block data aggregated to districts/2010"
rb_plans_wd <- "~/Documents/GitHub/citl/RB Seeded Sims"

filenames_blocks <- list.files(path = blocks_wd, pattern = "*.shp", full.names = FALSE)
filenames_aggs <- list.files(path = aggs_wd, pattern = "*.shp", full.names = FALSE)
filenames_plans <- list.files(path = rb_plans_wd, pattern = "*.rds", full.names = FALSE)
filenames_blocks<-filenames_blocks[substr(filenames_blocks,0, nchar(filenames_blocks)-7)%in%substr(filenames_plans, 0,nchar(filenames_plans)-10 )]
aggs_cities<-gsub(" ", "_",substr(filenames_aggs,0, nchar(filenames_aggs)-30))
filenames_aggs<-filenames_aggs[aggs_cities%in%substr(filenames_plans, 0,nchar(filenames_plans)-10 )]


# Pre-allocate results with known size
n_cities <-  length(filenames_plans)
results_dt <- data.table(
  city = character(n_cities),
  actual_maj_hisp = numeric(n_cities),
  actual_maj_hisp_v = numeric(n_cities),
  actual_maj_hisp_c = numeric(n_cities),
  actual_maj_wht = numeric(n_cities),
  actual_maj_wht_v = numeric(n_cities),
  actual_maj_wht_c = numeric(n_cities),
  actual_maj_blk = numeric(n_cities),
  actual_maj_blk_v = numeric(n_cities),
  actual_maj_blk_c = numeric(n_cities),
  actual_maj_asn = numeric(n_cities),
  actual_maj_asn_v = numeric(n_cities),
  actual_maj_asn_c = numeric(n_cities),
  prop_black = numeric(n_cities),
  prop_black_v = numeric(n_cities),
  prop_black_c = numeric(n_cities),
  prop_white = numeric(n_cities),
  prop_white_v = numeric(n_cities),
  prop_white_c = numeric(n_cities),
  prop_hisp = numeric(n_cities),
  prop_hisp_v = numeric(n_cities),
  prop_hisp_c = numeric(n_cities),
  prop_asian = numeric(n_cities),
  prop_asian_v = numeric(n_cities),
  prop_asian_c = numeric(n_cities),
  total_dists = numeric(n_cities)
)

# Function to process demographic calculations efficiently
calculate_demographics <- function(agg_dists) {
  n_dists <- n_distinct(agg_dists$distrct)
  
  list(
    actual_maj_hisp = sum(agg_dists$pp_hspn/agg_dists$pop > 0.5) / n_dists,
    actual_maj_hisp_v = sum(agg_dists$vap_hsp/agg_dists$vap > 0.5) / n_dists,
    actual_maj_hisp_c = sum(agg_dists$cvp_hsp/agg_dists$cvap > 0.5) / n_dists,
    
    actual_maj_wht = sum(agg_dists$pop_wht/agg_dists$pop > 0.5) / n_dists,
    actual_maj_wht_v = sum(agg_dists$vap_wht/agg_dists$vap > 0.5) / n_dists,
    actual_maj_wht_c = sum(agg_dists$cvp_wht/agg_dists$cvap > 0.5) / n_dists,
    
    actual_maj_blk = sum(agg_dists$pp_blck/agg_dists$pop > 0.5) / n_dists,
    actual_maj_blk_v = sum(agg_dists$vp_blck/agg_dists$vap > 0.5) / n_dists,
    actual_maj_blk_c = sum(agg_dists$cvp_blc/agg_dists$cvap > 0.5) / n_dists,
    
    actual_maj_asn = sum(agg_dists$pop_asn/agg_dists$pop > 0.5) / n_dists,
    actual_maj_asn_v = sum(agg_dists$vap_asn/agg_dists$vap > 0.5) / n_dists,
    actual_maj_asn_c = sum(agg_dists$cvap_sn/agg_dists$cvap > 0.5) / n_dists,
    
    # Group proportions
    prop_black = sum(agg_dists$pp_blck) / sum(agg_dists$pop),
    prop_black_v = sum(agg_dists$vp_blck) / sum(agg_dists$vap),
    prop_black_c = sum(agg_dists$cvp_blc) / sum(agg_dists$cvap),
    prop_white = sum(agg_dists$pop_wht) / sum(agg_dists$pop),
    prop_white_v = sum(agg_dists$vap_wht) / sum(agg_dists$vap),
    prop_white_c = sum(agg_dists$cvp_wht) / sum(agg_dists$cvap),
    prop_hisp = sum(agg_dists$pp_hspn) / sum(agg_dists$pop),
    prop_hisp_v = sum(agg_dists$vap_hsp) / sum(agg_dists$vap),
    prop_hisp_c = sum(agg_dists$cvp_hsp) / sum(agg_dists$cvap),
    prop_asian = sum(agg_dists$pop_asn) / sum(agg_dists$pop),
    prop_asian_v = sum(agg_dists$vap_asn) / sum(agg_dists$vap),
    prop_asian_c = sum(agg_dists$cvap_sn) / sum(agg_dists$cvap),
    
    total_dists = n_dists
  )
}

# Initialize data.table for median plans (more memory efficient)
all_median_plans <- data.table()

# Process cities in batches to manage memory
batch_size <- 5  # Adjust based on your system memory
n_batches <- ceiling(n_cities / batch_size)

for (batch in 1:n_batches) {
  start_idx <- (batch - 1) * batch_size + 1
  end_idx <- min(batch * batch_size, n_cities)
  
  cat("Processing batch", batch, "of", n_batches, "(cities", start_idx, "to", end_idx, ")\n")
  
  for (i in start_idx:end_idx) {
    if (i > length(filenames_plans) || i > n_cities) break
    
    # Extract city name
    name <- substr(filenames_plans[i], 1, nchar(filenames_plans[i]) - 10)
    name <- gsub("_", " ", name)
    
    # Find corresponding files - be more explicit about matching
    id <- which(grepl(name, filenames_aggs, fixed = TRUE))
    if (length(id) == 0) {
      cat("No matching aggregated file found for", name, "- skipping\n")
      next
    }
    if (name == 'jackson') id <- id[1]
    
    # Load spatial data
    setwd(blocks_wd)
    blocks <- st_read(filenames_blocks[id], quiet = TRUE)
    setwd(aggs_wd)
    agg_dists <- st_read(filenames_aggs[id], quiet = TRUE)
    
    # Calculate demographics efficiently
    demo_results <- calculate_demographics(agg_dists)
    
    # Store results
    results_dt[i, `:=`(
      city = name,
      actual_maj_hisp = demo_results$actual_maj_hisp,
      actual_maj_hisp_v = demo_results$actual_maj_hisp_v,
      actual_maj_hisp_c = demo_results$actual_maj_hisp_c,
      actual_maj_wht = demo_results$actual_maj_wht,
      actual_maj_wht_v = demo_results$actual_maj_wht_v,
      actual_maj_wht_c = demo_results$actual_maj_wht_c,
      actual_maj_blk = demo_results$actual_maj_blk,
      actual_maj_blk_v = demo_results$actual_maj_blk_v,
      actual_maj_blk_c = demo_results$actual_maj_blk_c,
      actual_maj_asn = demo_results$actual_maj_asn,
      actual_maj_asn_v = demo_results$actual_maj_asn_v,
      actual_maj_asn_c = demo_results$actual_maj_asn_c,
      prop_black = demo_results$prop_black,
      prop_black_v = demo_results$prop_black_v,
      prop_black_c = demo_results$prop_black_c,
      prop_white = demo_results$prop_white,
      prop_white_v = demo_results$prop_white_v,
      prop_white_c = demo_results$prop_white_c,
      prop_hisp = demo_results$prop_hisp,
      prop_hisp_v = demo_results$prop_hisp_v,
      prop_hisp_c = demo_results$prop_hisp_c,
      prop_asian = demo_results$prop_asian,
      prop_asian_v = demo_results$prop_asian_v,
      prop_asian_c = demo_results$prop_asian_c,
      total_dists = demo_results$total_dists
    )]
    
    # Process plans efficiently
    setwd(rb_plans_wd)
    pop_tol <- max(agg_dists$pop / (sum(agg_dists$pop) / demo_results$total_dists) - 1)
    
    city_map <- redist::redist_map(blocks, 
                                   pop_tol = pop_tol, 
                                   ndists = demo_results$total_dists, 
                                   total_pop = pop)
    city_map$adj <- redist.adjacency(city_map)
    
    plans <- readRDS(filenames_plans[i])
    
    # Calculate all group fractions at once (must be done on full dataset)
    # This avoids the chunking issue with group_frac
    plans <- plans %>%
      mutate(
        pop_dev = abs(total_pop / get_target(city_map) - 1),
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
        pct_asian_v = group_frac(city_map, vap_asn, vap),
        pct_asian_c = group_frac(city_map, cvap_sn, cvap),
        pct_nonwhite  = group_frac(city_map, pop-pop_wht, pop),
        pct_nonwhite_v= group_frac(city_map, vap-vap_wht, vap),
        pct_nonwhite_c= group_frac(city_map, cvap-cvp_wht, cvap)
      ) %>%
      group_by(district) %>%
      mutate(
        majority_white = as.integer(pct_white >= 0.5),
        majority_white_v = as.integer(pct_white_v >= 0.5),
        majority_white_c = as.integer(pct_white_c >= 0.55),
        
        majority_black = as.integer(pct_black >= 0.5),
        majority_black_v = as.integer(pct_black_v >= 0.5),
        majority_black_c = as.integer(pct_black_c >= 0.5),
        
        majority_hisp = as.integer(pct_hisp >= 0.5),
        majority_hisp_v = as.integer(pct_hisp_v >= 0.5),
        majority_hisp_c = as.integer(pct_hisp_c >= 0.5),
        
        majority_asian = as.integer(pct_asian >= 0.5),
        majority_asian_v = as.integer(pct_asian_v >= 0.5),
        majority_asian_c = as.integer(pct_asian_c >= 0.5),
        
        majority_nonwhite = as.integer(pct_nonwhite >= 0.5),
        majority_nonwhite_v = as.integer(pct_nonwhite_v >= 0.5),
        majority_nonwhite_c = as.integer(pct_nonwhite_c >= 0.5)
      ) %>%
      ungroup() %>%
      group_by(draw) %>%
      mutate(plan_maj_black_c = sum(majority_black_c),
             plan_maj_hisp_c = sum(majority_hisp_c)) %>%
      ungroup()
    
    # Now we can safely convert to data.table for efficient processing
    all_plans <- as.data.table(plans)
    rm(plans)
    gc()
    
    # Calculate median and filter efficiently
    median_maj_black_c <- median(all_plans$plan_maj_black_c, na.rm = TRUE)
    
    # Filter to median plans and sample efficiently - fix data.table filtering
    filtered_plans <- all_plans[all_plans$plan_maj_black_c == median_maj_black_c, ]
    
    if (nrow(filtered_plans) > 100) {
      # Sample 100 draws if we have more than 100
      unique_draws <- unique(filtered_plans$draw)
      if (length(unique_draws) > 100) {
        sampled_draws <- sample(unique_draws, 100)
        filtered_plans <- filtered_plans[filtered_plans$draw %in% sampled_draws, ]
      }
    }
    
    # Add city identifier
    filtered_plans$city <- name
    
    # Combine with previous results
    if (nrow(all_median_plans) == 0) {
      all_median_plans <- as.data.table(filtered_plans)
    } else {
      all_median_plans <- rbind(all_median_plans, as.data.table(filtered_plans), fill = TRUE)
    }
    
    # Clean up large objects
    rm(blocks, agg_dists, city_map, plans, all_plans, filtered_plans)
    gc()
    
    cat("Completed city:", name, "(", i, "of", n_cities, ")\n")
  }
  
  # Force garbage collection between batches
  gc()
}

# Model fitting section - more memory efficient
cat("Loading model data...\n")

# Use fread for faster, more memory-efficient CSV reading
full <- fread("/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/RR/full_data_for_models.csv")
linkedDBK <- fread("/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/RR/Yamil data/districtswDBK.csv")

linkedDBK$yearmonth<-linkedDBK$year + linkedDBK$month*.01

reps1020 <- linkedDBK %>%
  filter(yearmonth > 2011 & year <= 2020 & winner == 'win') %>%
  group_by(city, district) %>%
  slice_min(year, n = 1, with_ties = FALSE) %>%
  ungroup()

# Calculate proportions by city
reps1020 <- reps1020 %>%
  group_by(city) %>%
  summarise(
    proportion_black = sum(race_est == "black", na.rm = TRUE) / n_distinct(district),
    absolute_black= sum(race_est == "black", na.rm = TRUE) ,
    .groups = 'drop'
  )


# Efficient join and filter - fix the data.table join syntax
data<-left_join(linkedDBK, full%>%dplyr::select(-city_pop), by=c('city', 'state'))
data[, city_pop_ms := as.numeric(gsub(",", "", city_pop)) / 1e6]
data[, pct_black_c := b_pct_c]
data[, pct_white_c := w_pct_c]
# Filter for winners only
winner_data <- data[winner == 'win']

# ADD hisp_seg_c to all_median_plans by merging with data
cat("Adding black_seg_c to all_median_plans...\n")
# Create a lookup table for hisp_seg_c values by city
city_black_seg <- data[, .(black_seg_c = unique(black_seg_c[!is.na(black_seg_c)])[1]), by = city]

# Merge black_seg_c into all_median_plans
all_median_plans <- merge(all_median_plans, city_black_seg, by = "city", all.x = TRUE)
all_median_plans<-subset(all_median_plans, !is.na(all_median_plans$black_seg_c))

# Create a lookup table for city_pop_ms values by city
city_pop <- data[, .(city_pop_ms = unique(city_pop_ms[!is.na(city_pop_ms)])[1]), by = city]

# Merge city_pop_ms into all_median_plans
all_median_plans <- merge(all_median_plans, city_pop, by = "city", all.x = TRUE)
all_median_plans<-subset(all_median_plans, !is.na(all_median_plans$city_pop_ms))

# Create a lookup table for city black proportion values by city
black_prop <- data[, .(prop_black_c = unique(prop_black_c[!is.na(prop_black_c)])[1]), by = city]

# Merge city_pop_ms into all_median_plans
all_median_plans <- merge(all_median_plans, black_prop, by = "city", all.x = TRUE)
all_median_plans<-subset(all_median_plans, !is.na(all_median_plans$prop_black_c))

# Create a lookup table for dem  vote share values by city
dvs <- data[, .(demshare_pres = unique(demshare_pres[!is.na(demshare_pres)])[1]), by = city]

# Merge city_pop_ms into all_median_plans
all_median_plans <- merge(all_median_plans, dvs, by = "city", all.x = TRUE)
all_median_plans<-subset(all_median_plans, !is.na(all_median_plans$demshare_pres))

# Check for missing values
missing_seg <- sum(is.na(all_median_plans$black_seg_c))
if (missing_seg > 0) {
  cat("Warning:", missing_seg, "rows have missing black_seg_c values\n")
}

# Fit model with both pct_black_c and pct_white_c
library(rstanarm)
h1 <- stan_glm(data = winner_data%>%filter(black_viables_c), 
               formula = I(race_est == 'black') ~ pct_black_c + pct_white_c +
                 city_pop_ms +prop_black_c +black_seg_c , 
               family = binomial(link = 'logit'), 
               refresh = 0, 
               cores = 4)  # Reduced cores to save memory

# Process predictions in smaller batches
batch_size_pred <- 10000
n_pred_batches <- ceiling(nrow(all_median_plans) / batch_size_pred)

long_sim_list <- list()

for (batch in 1:n_pred_batches) {
  start_idx <- (batch - 1) * batch_size_pred + 1
  end_idx <- min(batch * batch_size_pred, nrow(all_median_plans))
  
  batch_data <- all_median_plans[start_idx:end_idx, ]
  
  # Generate predictions for this batch
  posterior_preds <- posterior_linpred(h1, newdata = batch_data, transform = TRUE)
  
  # Simulate elections
  simulated_elections <- matrix(
    rbinom(length(posterior_preds), 1, as.vector(posterior_preds)),
    nrow = nrow(posterior_preds)
  )
  
  # Process results efficiently
  sim_dt <- as.data.table(simulated_elections)
  sim_dt[, sim_draw := .I]
  
  # Melt to long format
  long_sim_batch <- melt(sim_dt, id.vars = "sim_draw", 
                         variable.name = "row_index", 
                         value.name = "elected")
  
  # Convert row_index to numeric (V1, V2, etc. -> 1, 2, etc.)
  long_sim_batch[, row_index := as.integer(gsub("V", "", row_index))]
  
  # Add metadata - create row indices for the batch data
  batch_data[, row_index := seq_len(.N)]
  
  # Join the simulation results with the batch metadata
  long_sim_batch <- merge(long_sim_batch, batch_data, by = "row_index", all.x = TRUE)
  
  # Summarize by city and draw
  long_sim_summary <- long_sim_batch[, .(total_elected = sum(elected)), 
                                     by = .(city, draw, sim_draw)]
  
  long_sim_list[[batch]] <- long_sim_summary
  
  # Clean up
  rm(batch_data, posterior_preds, simulated_elections, sim_dt, long_sim_batch, long_sim_summary)
  gc()
}

# Combine all batches
long_sim <- rbindlist(long_sim_list)

# Get n_dists for each city from results_dt
long_sim <- merge(long_sim, results_dt[, .(city, total_dists)], by = "city", all.x = TRUE)

# Divide total_elected by n_dists (total_dists) to get proportions
long_sim[, prop_elected := total_elected / total_dists]

# Filter to only include cities that are in the data dataframe
available_cities <- intersect(unique(long_sim$city), unique(data$city[data$black_viables_c]))
long_sim_filtered <- long_sim[city %in% available_cities]

# # Get hisp_cc_prop_2010 values for available cities (assuming unique values per city)
#  hisp_prop_2010 <- data[, .(hisp_cc_prop_2010 = unique(hisp_cc_prop_2010)), by = city]
#  hisp_prop_2010 <- hisp_prop_2010[city %in% available_cities]

# Calculate median for ordering cities
city_medians <- long_sim_filtered[, .(median_prop = median(prop_elected)), by = city]
city_medians <- city_medians[order(median_prop)]

city_medians <- long_sim_filtered[, .(
  median_prop = median(prop_elected),
  median_absolute = median(total_elected)
), by = city]
city_medians <- city_medians[order(median_prop)]


# Order cities by median proportion for better visualization
long_sim_filtered[, city_ordered := factor(city, levels = city_medians$city)]
city_medians[, city_ordered := factor(city, levels = city_medians$city) ]

# Create title case versions consistently across all datasets

city_medians[, city_title := str_to_title(city)]
long_sim_filtered <- data.table(long_sim_filtered)
long_sim_filtered[, city_title := str_to_title(city)]
reps1020 <- data.table(reps1020)
reps1020[, city_title := str_to_title(city)]

#hisp_prop_2010[, city_ordered := factor(city, levels = city_medians$city)]
long_sim_filtered[, city_ordered := factor(city_title, levels = city_medians$city_title)]

reps1020<-data.table(reps1020)
reps1020[, city_ordered := factor(city, levels = city_medians$city)]



density_plot <- ggplot(long_sim_filtered, aes(x = prop_elected, y = city_ordered)) +
  geom_density_ridges(
    aes(fill = city_ordered),
    alpha = 0.7,
    scale = 0.9,
    rel_min_height = 0.01,
    bandwidth = 0.02
  ) + 
  geom_point(
    data = city_medians,
    aes(x = median_prop, xend = median_prop,  
        y = as.numeric(city_ordered) - 0, yend = as.numeric(city_ordered) + .8),
    color = "green",
    size = 1.2,
    alpha = 0.9
  ) +
  geom_segment(
    data = reps1020,
    aes(x = proportion_black, xend = proportion_black, 
        y = as.numeric(city_ordered) - 0, yend = as.numeric(city_ordered) + .8),
    color = "red",
    linetype = "solid",
    size = 1.2,
    alpha = 0.9
  ) +
  labs(
    title = "Projected Distributions of Hisp. Councilmembers",
    x = "Proportion of Districts with Elected Hisp. Candidates", 
    y = "City"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 5),
    axis.text.x=element_text(size=10),
    axis.text.y=element_text(size=8),
    panel.grid.major.y = element_line(color = "gray90", size = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray95", size = 0.3),
    legend.position = "none" 
  ) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_viridis_d(option = "plasma", alpha = 0.8)# Remove legend since colors are redundant with y-axislibrary(dplyr)


print(density_plot)
setwd("~/Documents/GitHub/citl/Figures")
ggsave("Figure_8.tiff")

sum(reps1020$absolute_black[!is.na(reps1020$city_ordered)])
sum(city_medians$median_absolute[!is.na(city_medians$city_ordered)])


#Plot Difference in Elected Hisp. Concilors Across Hisp. Proportion----