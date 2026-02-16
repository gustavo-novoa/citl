full<-read.csv("/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/RR/full_data_for_models.csv")

linkedDBK<-read.csv("/Users/gnovoa/Library/Mobile Documents/com~apple~CloudDocs/Documents/Coloring in the Lines/RR/Yamil data/districtswDBK.csv")

data<-left_join(linkedDBK, full, by=c('city', 'state'))

data$year<-as.factor(data$year)
data$state<-as.factor(data$state)

b1<-stan_glm(data=data%>%filter(winner=='win'), race_est=='black'~ b_pct_c   , family=binomial(link='logit') ) 

w1<-stan_glm(data=data%>%filter(winner=='win'), race_est=='caucasian'~ w_pct_c , family=binomial(link='logit') ) 

h1<-stan_glm(data=data%>%filter(winner=='win'& h_pct_c<.45), race_est=='hispanic'~ h_pct_c  , family=binomial(link='logit') ) 



library(rstanarm)
library(dplyr)
q
# Create a sequence of values for b_pct_c (for the first model)
b_pct_c_seq <- seq(min(data$b_pct_c, na.rm = TRUE), max(data$b_pct_c, na.rm = TRUE), length.out = 100)

# Create a sequence of values for h_pct_c (for the second model)
h_pct_c_seq <- seq(min(data$h_pct_c, na.rm = TRUE), max(data$h_pct_c, na.rm = TRUE), length.out = 100)

# Create a sequence of values for w_pct_c (for the third model)
w_pct_c_seq <- seq(min(data$w_pct_c, na.rm = TRUE), max(data$w_pct_c, na.rm = TRUE), length.out = 100)

# Create a new data frame for the first model (b1)
new_data_b1 <- data.frame(
  b_pct_c = b_pct_c_seq
  #black_seg = mean(data$black_seg_c, na.rm = TRUE),
  #majority_black_c_median = mean(data$majority_black_c_median, na.rm = TRUE)
)

# Create a new data frame for the second model (h1)
new_data_h1 <- data.frame(
  h_pct_c = h_pct_c_seq
  #hisp_seg_c = mean(data$hisp_seg_c, na.rm = TRUE),
  #year = median(data$year, na.rm = TRUE),
  #majority_hisp_c_median = mean(data$majority_hisp_c_median, na.rm = TRUE)
)

# Create a new data frame for the third model (w1)
new_data_w1 <- data.frame(
  w_pct_c = w_pct_c_seq
  #white_seg = mean(data$white_seg, na.rm = TRUE),
  #year = median(data$year, na.rm = TRUE),
  #majority_white_c_median = mean(data$majority_white_c_median, na.rm = TRUE)
)

# Get predicted probabilities for the first model (b1)
predicted_probs_b1 <- posterior_linpred(b1, newdata = new_data_b1, transform = TRUE)
mean_probs_b1 <- colMeans(predicted_probs_b1)
std_errors_b1 <- apply(predicted_probs_b1, 2, sd)

# Get predicted probabilities for the second model (h1)
predicted_probs_h1 <- posterior_linpred(h1, newdata = new_data_h1, transform = TRUE)
mean_probs_h1 <- colMeans(predicted_probs_h1)
std_errors_h1 <- apply(predicted_probs_h1, 2, sd)

# Get predicted probabilities for the third model (w1)
predicted_probs_w1 <- posterior_linpred(w1, newdata = new_data_w1, transform = TRUE)
mean_probs_w1 <- colMeans(predicted_probs_w1)
std_errors_w1 <- apply(predicted_probs_w1, 2, sd)

# Plot the logit curve for the first model (b1)
plot(b_pct_c_seq, mean_probs_b1, type = "l", col = "blue", lwd = 2,
     xlab = "Group Size within District by CVAP %",
     ylab = "Predicted Probability of Descriptive Representation",
     xlim = range(c(b_pct_c_seq, h_pct_c_seq, w_pct_c_seq)),  # Set x-axis limits to include all curves
     ylim = c(0, 1),
     cex.lab=1.3)  # Set y-axis limits for probabilities

# Add the logit curve for the second model (h1)
lines(h_pct_c_seq, mean_probs_h1, col = "red", lwd = 2)

# Add the logit curve for the third model (w1)
lines(w_pct_c_seq, mean_probs_w1, col = "darkgreen", lwd = 2)


# Add standard errors for models
polygon(c(b_pct_c_seq, rev(b_pct_c_seq)), 
        c(mean_probs_b1 + std_errors_b1, rev(mean_probs_b1 - std_errors_b1)),
        col = adjustcolor("blue", alpha.f = 0.2), border = NA)

polygon(c(w_pct_c_seq, rev(w_pct_c_seq)), 
        c(mean_probs_w1 + std_errors_w1, rev(mean_probs_w1 - std_errors_w1)),
        col = adjustcolor("green", alpha.f = 0.2), border = NA)

polygon(c(h_pct_c_seq, rev(h_pct_c_seq)), 
        c(mean_probs_h1 + std_errors_h1, rev(mean_probs_h1 - std_errors_h1)),
        col = adjustcolor("red", alpha.f = 0.2), border = NA)


mtext("Black", col="blue", at=.45, line=-4, cex=1.5)
mtext("Hispanic", col='red', line=-3.5 , at=.72, cex=1.5)
mtext("White", col='darkgreen', line=-7 , at=.68, cex=1.5)

abline(a=0, b=1, lty=5)
