# Check convergence 

check_convergence<-function(plans, name){
  
  # Replicate exactly what summary.redist_plans does for SMC
  object <- subset_sampled(plans)
  district <- 1L  # change if needed
  
  cols <- names(object)
  addl_cols <- setdiff(cols, c("chain", "draw", "district", "total_pop"))
  
  n_samp <- ncol(redist::get_plans_matrix(object))
  n_distr <- attr(object, "ndists")
  idx <- as.integer(district) + (seq_len(n_samp) - 1) * n_distr
  
  # Remove constant columns (same filter as in summary)
  const_cols <- vapply(addl_cols, function(col) {
    x <- object[[col]][idx]
    all(is.na(x)) || all(x == x[1]) || any(tapply(x, object[["chain"]][idx], 
                                                  FUN = function(z) length(unique(z))) == 1)
  }, numeric(1))
  addl_cols <- addl_cols[!const_cols]
  
  # Compute R-hats
  rhats <- vapply(addl_cols, function(col) {
    x <- object[[col]][idx]
    na_omit <- !is.na(x)
    redist:::diag_rhat(x[na_omit], object$chain[idx][na_omit])
  }, numeric(1))
  names(rhats) <- addl_cols
  
  # As a tidy tibble
  rhat_df <- tibble::tibble(stat = addl_cols, rhat = rhats)
  answer<-(sum(unname(rhat_df$rhat>1.02))>0)
  
  ifelse(answer, cli::cli_alert(paste0(name, " has ***NOT*** converged")),cli::cli_alert(paste0(name, " has converged")))
  return(answer)
  
}

converged<-list()
city<-list()

#for(i in 1:length(filenames_plans)){
  for(i in 1:5){
    
  name<-substr(filenames_plans[i], 0, nchar(filenames_plans[i])-10)
  name<-gsub("_", " ", name)
  cities<-c(cities, name)
  id<- grep(paste0("^", name, "_"), filenames_aggs)
  setwd(blocks_wd)
  blocks<-st_read(filenames_blocks[id], quiet=TRUE)
  setwd(aggs_wd)
  agg_dists<-st_read(filenames_aggs[id], quiet=TRUE)
  setwd(vra_plans_wd)
  
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

  
  converged[i]<-check_convergence(plans, name)
  city[i]<-name
  
  
}



