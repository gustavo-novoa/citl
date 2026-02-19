library(dplyr)

setwd("/Users/gnovoa/Documents/GitHub/citl")

df<-read.csv("./Compiled Results/full_data_for_models.csv")
df$city_pop<-gsub(",", "", df$city_pop )
df$city_pop1m<-as.numeric(df$city_pop)/1e6

# Bring in JdBK data 
jdbk<-read.csv("./External Data/ledb_candidatelevel.csv")

jdbk <- jdbk %>%
  subset(year < 2011 & year > 2005 & office_consolidated == "Mayor" & winner == 'win') %>%
  mutate(
    city = gsub('\\.', '', geo_name),
    state = state_abb
  ) %>%
  group_by(city, state) %>%
  filter(year == max(year)) %>%
  ungroup()

df2<-left_join(df, jdbk, by=c('city', 'state')) 


df2$diff_black <- df2$actual_maj_blk_c - df2$majority_black_c_median
df2$diff_hisp <- df2$actual_maj_hisp_c - df2$majority_hisp_c_median
df2$diff_black_absolute <- round(df2$actual_maj_blk_c*df2$ndists - df2$majority_black_c_median*df2$ndists)
df2$diff_hisp_absolute <- round(df2$actual_maj_hisp_c*df2$ndists - df2$majority_hisp_c_median*df2$ndists)
df2$diff_combined<-df2$actual_maj_blk_c + df2$actual_maj_hisp_c - df2$majority_black_c_median +df2$majority_hisp_c_median
df2$mayor_race<-df2$race_est
df2$hisp_mayor<-ifelse(df2$mayor_race=='hispanic',1,0)
df2$black_mayor<-ifelse(df2$mayor_race=='black',1,0)


# Add in councilor data

ccprops2010<-read.csv("./External Data/ccprops2010.csv")
df2<-left_join(df2, ccprops2010%>%select(-ndists), by='city')

# Add in councilor props 

write.csv(df2, file= './Compiled Results/full_data_for_models_wdbk.csv')
