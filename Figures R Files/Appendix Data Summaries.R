

data <- read.csv("~/Documents/GitHub/citl/Compiled Results/districtswDBK.csv")

setwd("~/Documents/GitHub/citl/")


#Distribution of District Concentrations


# *** APPENDIX FIGURE 15 ***

tiff("./Figures/Figure_A15.tiff", width=6.5, height=4.5, units="in",  res=300)

par(mfrow=c(1,2))
hist(data$b_pct_c, main='Distribution of Black CVAP %', col='lightblue',
     xlab='Black CVAP %')
hist(data$b_pct_c[data$b_pct_c>.45], main='Black CVAP Above 45%',
     col='lightblue',  xlab='B' ,xlim=c(.4,1))

dev.off()

# *** APPENDIX FIGURE 16 ***

tiff("./Figures/Figure_A16.tiff", width=6.5, height=4.5, units="in",  res=300)

par(mfrow=c(1,2))
hist(data$h_pct_c, main='Distribution of Hisp. CVAP %', col='lightblue',
     xlab='Hisp. CVAP %')
hist(data$h_pct_c[data$h_pct_c>.45], main='Hisp. CVAP Above 45%',
     col='lightblue',  xlab='B' ,xlim=c(.4,1))

dev.off()

# *** APPENDIX FIGURE 17 ***

tiff("./Figures/Figure_A17.tiff", width=6.5, height=4.5, units="in",  res=300)

par(mfrow=c(1,2))
hist(data$a_pct_c, main='Distribution of Asian CVAP %', col='lightblue',
     xlab='Asian CVAP %',xlim=c(0,1))
hist(data$a_pct_c[data$a_pct_c>.45], main='Asian CVAP Above 45%',
     col='lightblue',  xlab='B' ,xlim=c(.4,1))

dev.off()

