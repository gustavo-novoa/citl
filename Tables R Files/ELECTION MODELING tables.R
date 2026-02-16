library(dplyr)
library(rstanarm)
library(sjPlot)

full<-read.csv("/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/RR/full_data_for_models.csv")

linkedDBK<-read.csv("/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/RR/Yamil data/districtswDBK.csv")

data<-left_join(linkedDBK, full%>%dplyr::select(-city_pop), by=c('city', 'state'))

data<-data%>%filter(year!=2010)

data$city_pop_ms <-as.numeric(gsub(",", "", data$city_pop))/1e6

data <- data %>%
  group_by(state,city, year, month, district) %>%
  mutate(total_votes = sum(votes, na.rm = TRUE)) %>%
  ungroup()


#  **** TABLE 5 *******
# Election Models 

b1ns<-stan_glm(data=data%>%filter(winner=='win'), race_est=='black'~ b_pct_c +black_seg_c  +demshare_pres+ city_pop_ms, family=binomial(link='logit') ,cores=parallel::detectCores()) 

h1ns<-stan_glm(data=data%>%filter(winner=='win'), race_est=='hispanic'~ h_pct_c +hisp_seg_c + demshare_pres+city_pop_ms, family=binomial(link='logit') , cores=parallel::detectCores()) 

b1ns_city<-stan_glm(data=data%>%filter(winner=='win'), race_est=='black'~ b_pct_c +black_seg_c  +demshare_pres+ city_pop_ms + prop_black_c, family=binomial(link='logit') , cores=parallel::detectCores()) 

h1ns_city<-stan_glm(data=data%>%filter(winner=='win'), race_est=='hispanic'~ h_pct_c +hisp_seg_c + demshare_pres+city_pop_ms +prop_hisp_c, family=binomial(link='logit') , cores=parallel::detectCores()) 


h1L_ns<-stan_glmer(data=data%>%filter(winner=='win'), race_est=='hispanic'~ h_pct_c +hisp_seg_c+demshare_pres+city_pop_ms+ (1|year)+ (1|city), family=binomial(link='logit'), cores=parallel::detectCores()) 

b1L_ns<-stan_glmer(data=data%>%filter(winner=='win'), race_est=='black'~ b_pct_c +black_seg_c  +
                     demshare_pres+ city_pop_ms+ (1|year)+ (1|city),
                   family=binomial(link='logit'), cores=parallel::detectCores() ) 


tab_model(b1ns,  h1ns, b1ns_city, h1ns_city, b1L_ns,h1L_ns, transform=NULL, collapse.ci = TRUE, ci.hyphen = ":", show.icc = FALSE, show.aic = FALSE,
                  show.re.var = FALSE, show.ngroups = FALSE, digits=1, 
                  pred.labels=c("Intercept", "District Percent Black (CVAP)", "Black Segregation by CVAP",
                                "Dem Vote Share '08", 'City Pop. (in millions)', 
                                "District Percent Hispanic (CVAP)", "Hispanic Segregation (CVAP)",
                                "City Proportion Black (CVAP)", "City Proportion Hispanic (CVAP)"),
                  dv.labels = rep(c("Black Candidate Elected", "Hispanic Candidate Elected"),3),file = "Table5.html")



# **** TABLE A8 ******
## Check Interactions 

h1L_nsi<-stan_glmer(data=data%>%filter(winner=='win'), race_est=='hispanic'~ h_pct_c +
                     hisp_seg_c+demshare_pres+city_pop_ms+ h_pct_c*hisp_seg_c+
                     (1|year)+ (1|city) , family=binomial(link='logit'),cores=parallel::detectCores()) 

b1L_nsi<-stan_glmer(data=data%>%filter(winner=='win'), race_est=='black'~ b_pct_c +
                     black_seg_c  +demshare_pres+ city_pop_ms+ + b_pct_c*black_seg_c+
                     (1|year)+ (1|city), family=binomial(link='logit'), cores=parallel::detectCores()) 

a1L_nsi<-stan_glmer(data=data%>%filter(winner=='win'), race_est=='asian'~ a_pct_c +
                      asian_seg_c+ demshare_pres +city_pop_ms+ a_pct_c*asian_seg_c+
                      (1|year) + (1|city), family=binomial(link='logit') ,cores=parallel::detectCores()) 

tab_model(b1L_nsi, a1L_nsi,h1L_nsi,  transform=NULL, collapse.ci = TRUE, ci.hyphen = ":", show.icc = FALSE, show.aic = FALSE,
                  show.re.var = FALSE, show.ngroups = FALSE, digits=1, 
                  pred.labels=c("Intercept", "District Percent Black (CVAP)", "Black Segregation by CVAP",
                                                                                     "Dem Vote Share '08", 'City Pop. (in millions)', "Percent Black x Segregation",
                                                                                     "District Percent Asian (CVAP)", "Asian Segregation by CVAP", "Percent Asian x Segregation",
                                                                                     "District Percent Hispanic (CVAP)", "Hispanic Segregation (CVAP)",
                                                                                     "Percent Hispanic x Segregation"),
                  dv.labels = c('Black Candidate Elected', "Asian Candidate Elected", "Hispanic Candidate Elected"),file = "TableA8.html")


