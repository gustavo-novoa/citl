library(ggplot2)
library(dplyr)
library(broom)
library(stringr)

df <- read.csv("~/Documents/GitHub/citl/Compiled Results/full_data_for_models.csv")

df_filtered_h <- df %>% filter(hisp_viables_c == 1)
df_filtered_b <- df %>% filter(black_viables_c == 1)



tercile_plots <- function(data, ntiles, prop_var_name, x_var_name, y_var_name, x_label, group_name, prop_var_label){
  # Use column names as strings
  dat <- data
  
  clean_prop<- (gsub('_'," ",prop_var_name))
  clean_prop<- (gsub('seg',"Segregation",clean_prop))
  clean_prop<- (gsub('prop',"Proportion",clean_prop))
  clean_prop<- stringr::str_to_title(gsub(' c',"",clean_prop))
  
  
  
  if(ntiles == 2){
    df_filtered <- dat %>%
      mutate(prop_tercile = ntile(.data[[prop_var_name]], ntiles)) %>%
      mutate(prop_tercile = factor(prop_tercile, 
                                   labels = c(paste0("Below Median ", clean_prop), paste0("Above Median ", clean_prop))))
  } else {
    df_filtered <- dat %>%
      mutate(prop_tercile = ntile(.data[[prop_var_name]], ntiles)) %>%
      mutate(prop_tercile = factor(prop_tercile, 
                                   labels = rep(1:ntiles)))
  }
  
  # Calculate regression coefficients for each tercile
  regression_coeffs <- df_filtered %>%
    group_by(prop_tercile) %>%
    do(tidy(lm(reformulate(x_var_name, y_var_name), data = .))) %>%
    filter(term == x_var_name) %>%
    mutate(label = paste0("Slope: ", round(estimate, 2)))

  
  # Create the plot
  prop_plot <- ggplot(data = df_filtered, aes(x = .data[[x_var_name]], y = .data[[y_var_name]])) +
    geom_point() +
    xlim(0, 1) +
    ylim(0, 1) +
    ylab(paste0("Proportion of Majority-", group_name, " Districts Implemented")) +
    xlab(x_label) +
    geom_smooth(method = "lm", formula = y ~ x, fill = 'darkred',na.rm=TRUE) +
    geom_text(data = regression_coeffs, aes(x = 0.25, y = 0.2, label = label), 
              color = "black", size = 4, inherit.aes = FALSE) +
    facet_wrap(~prop_tercile, ncol = ntiles) +
    theme_minimal()
  
  print(prop_plot)
}

# *** Appendix Figure 11 ***
tercile_plots(data = df_filtered_h, 
              ntiles = 2, 
              prop_var_name = "hisp_seg_c",
              x_var_name = "hisp_seg_c", 
              y_var_name = "actual_maj_hisp_c",
              x_label = "Hispanic Segregation", 
              group_name = "Hisp", 
              prop_var_label = "Hisp. Segregation")

ggsave(filename='Figure_A11.tiff', path='~/Documents/GitHub/citl/Figures/')

# *** Appendix Figure 12 ***

tercile_plots(data = df_filtered_b, 
              ntiles = 2, 
              prop_var_name = "black_seg_c",
              x_var_name = "black_seg_c", 
              y_var_name = "actual_maj_blk_c",
              x_label = "Black Segregation", 
              group_name = "Black", 
              prop_var_label = "Black Segregation")

ggsave(filename='Figure_A12.tiff', path='~/Documents/GitHub/citl/Figures/')


# *** Appendix Figure 13 ***

p13<-tercile_plots(data = df_filtered_b, 
              ntiles = 2, 
              prop_var_name = "prop_black_c",
              x_var_name = "black_seg_c", 
              y_var_name = "actual_maj_blk_c",
              x_label = "Black Proportion (CVAP)", 
              group_name = "Black", 
              prop_var_label = "Black Proportion (CVAP)")
p13<-p13+theme(strip.text=element_text(size=14))

ggsave(filename='Figure_A13.tiff', path='~/Documents/GitHub/citl/Figures/')


# *** Appendix Figure 14 ***

p14<-tercile_plots(data = df_filtered_b, 
              ntiles = 3, 
              prop_var_name = "prop_black_c",
              x_var_name = "black_seg_c", 
              y_var_name = "actual_maj_blk_c",
              x_label = "Black Proportion (CVAP)", 
              group_name = "Black", 
              prop_var_label = "Black Proportion (CVAP)")

   p14<-p14+ggtitle("Segregation and Black Districts Implemented by Proportion Black (CVAP) Terciles")
   
   ggsave(filename='Figure_A14.tiff', path='~/Documents/GitHub/citl/Figures/')
   
