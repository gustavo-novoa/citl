library(ggplot2)
library(dplyr)

setwd("~/Documents/GitHub/citl")
dat<-read.csv("./Compiled Results/districtswDBK.csv")



dat_black <- dat %>%
  filter(b_pct_c >= 0.45, winner == "win", race_est!='other') %>%
  count(race_est) %>%
  mutate(proportion = n*100 / sum(n))
dat_black$race_est<-c("Asian", "Black", "White", "Hisp.")

b<-ggplot(dat_black, aes(x = race_est, y = proportion, color = race_est)) +
  geom_point(size = 2.5) +
  geom_text(aes(label = n), vjust = -1, size = 3.5) +  # Add count text above the dots
  labs(
    title = "Majority-Black Districts",
    x = "Winner Race/Ethnicity",
    y = ""
  ) +
  theme_minimal() + ylim(0,100)+
  theme(legend.position = "none")  # Remove the legend


dat_white <- dat %>%
  filter(w_pct_c >= 0.45, winner == "win", race_est!='other') %>%
  count(race_est) %>%
  mutate(proportion = n*100 / sum(n))
dat_white$race_est<-c("Asian", "Black", "White", "Hisp.")



w<-ggplot(dat_white, aes(x = race_est, y = proportion, color = race_est)) +
  geom_point(size = 2.5) +
  geom_text(aes(label = n), vjust = -1, size = 3.5) +  # Add count text above the dots
  labs(
    title = "Majority-White Districts",
    x = "Winner Race/Ethnicity",
    y = "Percentage"
  ) +
  theme_minimal() + ylim(0,100)+
  theme(legend.position = "none")  # Remove the legend

dat_hisp <- dat %>%
  filter(h_pct_c >= 0.45, winner == "win", race_est!='other') %>%
  count(race_est) %>%
  mutate(proportion = n*100 / sum(n))

dat_hisp$race_est<-c("Asian", "Black", "White", "Hisp.")


h<-ggplot(dat_hisp, aes(x = race_est, y = proportion, color = race_est)) +
  geom_point(size =2.5) +
  geom_text(aes(label = n), vjust = -1, size = 3.5) +  # Add count text above the dots
  labs(
    title = "Majority-Hisp. Districts",
    x = "Winner Race/Ethnicity",
    y = ""
  ) +
  theme_minimal() + ylim(0,100)+
  theme(legend.position = "none")  # Remove the legend

dat_asian <- dat %>%
  filter(a_pct_c >= 0.45, winner == "win", race_est!='other') %>%
  count(race_est) %>%
  mutate(proportion = n*100 / sum(n))
dat_asian$race_est<-c("Asian", "White", "Hisp.")


a<-ggplot(dat_asian, aes(x = race_est, y = forcats::fct_reorder(proportion, proportion),  color = race_est)) +
  geom_point(size =2.5) +
  geom_text(aes(label = n), vjust = -1, size = 3.5) +  # Add count text above the dots
  labs(
    title = "Majority-Hisp. Districts",
    x = "Winner Race/Ethnicity",
    y = ""
  ) +
  theme_minimal() + ylim(0,100)+
  theme(legend.position = "none")  # Remove the legend

gridExtra::grid.arrange(w,b,h, nrow=1)



