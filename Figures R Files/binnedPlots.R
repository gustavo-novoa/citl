library(dplyr)

setwd("~/Documents/GitHub/citl")

full<-read.csv("./Compiled Results/full_data_for_models.csv")

linkedDBK<-read.csv("./Compiled Results/districtswDBK.csv")

data<-left_join(linkedDBK, full, by=c('city', 'state'))

binned_plot <- function(x, y, n_bins=round(.5*sqrt(length(x))),main,xlab,ylab, colorcode) {
  n <- length(x)
  cutpoints <- c(-Inf, sort(x)[round(n*seq(1:(n_bins-1))/n_bins)], Inf)
  x_mean <- rep(NA, n_bins)
  y_mean <- rep(NA, n_bins)
  for (j in 1:n_bins){
    in_bin <- x > cutpoints[j] & x <= cutpoints[j+1]
    x_mean[j] <- mean(x[in_bin], na.rm=TRUE)
    y_mean[j] <- mean(y[in_bin],na.rm=TRUE)
  }
  
  plot(xlab=xlab,ylab=ylab,main=main,x_mean, y_mean, ylim=c(0,1.02), xlim=c(0,1), mgp=c(1.5,.5,0), 
       tck=-0.05, pch=19, bty='l', cex.axis=1, cex.main=1.5,cex=1,  xaxs = "i", yaxs = "i", cex.lab=1, xaxt = "n", yaxt='n',
       col=ifelse(colorcode == '0', 'dark green',
                  ifelse(colorcode == 1, 'blue', 'purple')))
  abline(a=0, b=1)
  axis(1, at = c(0, 0.25, 0.5, 0.75, 1), labels=FALSE)
  axis(2, at = c(0, 0.25, 0.5, 0.75, 1), labels=FALSE)
  mtext(seq(0, 1, by = 0.25), side = 2, at = seq(0, 1, by = 0.25), line = .5, cex=.75)
  
  mtext(seq(0, 1, by = 0.25), side = 1, at = seq(0, 1, by = 0.25), line = .5, cex=.75)
}


plot_data <- data %>%
  group_by(state, city, district, year) %>%
  arrange(desc(vote_share)) %>%
  mutate(
    top_two_hispanic = ifelse(
      race_est[1] == "hispanic" | (race_est[2] == "hispanic" & vote_share[2] >= 0.25), 1, 0
    ),
    top_two_white = ifelse(
      race_est[1] == "caucasian" | (race_est[2] == "caucasian" & vote_share[2] >= 0.25), 1, 0
    ),
    top_two_asian = ifelse(
      race_est[1] == "asian" | (race_est[2] == "asian" & vote_share[2] >= 0.25), 1, 0
    ),
    top_two_black = ifelse(
      race_est[1] == "black" | (race_est[2] == "black" & vote_share[2] >= 0.25), 1, 0
    ),
    hispanic_candidate=ifelse(
      any(race_est == "hispanic"), 1, 0
    ),
    white_candidate=ifelse(
      any(race_est == "caucasian"), 1, 0
    ),
    black_candidate=ifelse(
      any(race_est == "black"), 1, 0
    )
  ) %>%
  ungroup()

plot_data<-plot_data%>%filter(winner=='win')

# **** FIGURE 7 *****
tiff("./Figures/Figure_7.tiff", width=6.5, height=6.5, units="in", res=300)
par(mfrow=c(2,2), mar=c(3, 3, 1, 2), mgp=c(3, 1, 0), tck=-.03)
binned_plot(plot_data$b_pct_c, plot_data$top_two_black==1, xlab="% Black District (CVAP)", ylab="Black Candidate Top Two", main="", colorcode=1)
binned_plot(plot_data$h_pct_c, plot_data$top_two_hispanic==1, xlab="% Hisp. District (CVAP)", ylab="Hisp. Candidate Top Two", main="",colorcode=0)
binned_plot(plot_data$b_pct_c, plot_data$race_est=='black', xlab="% Black District (CVAP)", ylab="Black Candidate Elected", main="", colorcode=1)
binned_plot(plot_data$h_pct_c, plot_data$race_est=='hispanic', xlab="% Hisp. District (CVAP)", ylab="Hisp. Candidate Elected", main="",colorcode=0)
dev.off()


