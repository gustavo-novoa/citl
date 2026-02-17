library(dplyr)
library(ggplot2)
library(gridExtra)

df<-read.csv("./Compiled Results/full_data_for_models.csv")

df$black_viable_c<-ifelse(df$prop_black_c>(1/df$ndists), 1,0)
df$hisp_viable_c<-ifelse(df$prop_hisp_c>(1/df$ndists), 1,0)
df$white_viable_c<-ifelse(df$prop_white_c>(1/df$ndists), 1,0)
df$black_viable_v<-ifelse(df$prop_black_v>(1/df$ndists), 1,0)
df$hisp_viable_v<-ifelse(df$prop_hisp_v>(1/df$ndists), 1,0)
df$white_viable_v<-ifelse(df$prop_white_v>(1/df$ndists), 1,0)


#Proportion Against Simulation

# **** APPENDIX FIGURE 1 ***
prop_sim_b<-ggplot(data=df, aes(x=prop_black_c, y=actual_maj_blk_c))+ geom_point() + xlim(0,1)+ylim(0,1)+
  geom_abline(slope=1, intercept=0,linetype='dotted', col='blue')+ylab("Proportion of Simulated Majority-Black Districts")+xlab("Black Proportion of City by CVAP")

# **** APPENDIX FIGURE 2 ***
prop_sim_h<-ggplot(data=df, aes(x=prop_hisp_c, y=actual_maj_hisp_c))+ geom_point() + xlim(0,1)+ylim(0,1)+
  geom_abline(slope=1, intercept=0,linetype='dotted', col='blue')+ylab("Proportion of Simulated Majority-Hisp. Districts")+xlab("Hisp. Proportion of City by CVAP")
# **** APPENDIX FIGURE 3 ***
prop_sim_w<-ggplot(data=df, aes(x=prop_white_c, y=actual_maj_wht_c))+ geom_point() + xlim(0,1)+ylim(0,1)+
  geom_abline(slope=1, intercept=0,linetype='dotted', col='blue')+ylab("Proportion of Simulated Majority-White Districts")+xlab("White Proportion of City by CVAP")

grid.arrange(prop_sim_b, prop_sim_h, prop_sim_w ,nrow= 1)


#  *** FIGURE 3 **** Simulation Against Actual

bsim<-ggplot(data=df, aes(x= majority_black_c_median, y=actual_maj_blk_c))+ geom_point() + xlim(0,1)+ylim(0,1)+
  geom_abline(slope=1, intercept=0,linetype='dotted', col='blue')+ylab("Proportion of Majority-Black Districts Implemented")+xlab("Prop. Black-Majority Simulated")

hsim<-ggplot(data=df, aes(x=majority_hisp_c_median, y=actual_maj_hisp_c))+ geom_point() +xlim(0,1)+ylim(0,1)+
  geom_abline(slope=1, intercept=0,linetype='dotted', col='blue')+ylab("Proportion of Majority-Hisp. Districts Implemented")+xlab("Prop. Hisp.-Majority Simulated")

wsim<-ggplot(data=df, aes(x=majority_white_c_median, y=actual_maj_wht_c))+ geom_point() +xlim(0,1)+ylim(0,1)+
  geom_abline(slope=1, intercept=0,linetype='dotted', col='blue')+ylab("Proportion of Majority-White Districts Implemented")+xlab("Prop. White-Majority Simulated")

grid.arrange(bsim, hsim, nrow = 1)



# *** FIGURE 1 *** Proportion Against Actual

bprop<-ggplot(data=df, aes(x=prop_black_c, y=actual_maj_blk_c))+ geom_point()+
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) + 
  geom_abline(slope=1, intercept=0,linetype='dotted', col='blue')+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0.005, 0.005))+
  ylab("Proportion of Majority-Black Districts")+xlab("Black Proportion of City by CVAP")+geom_smooth(method='lm', color='red', se=FALSE)

hprop<-ggplot(data=df, aes(x=prop_hisp_c, y=actual_maj_hisp_c))+ geom_point()+
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) + 
  geom_abline(slope=1, intercept=0,linetype='dotted', col='blue')+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0.005, 0.005))+
  ylab("Proportion of Majority-Hisp. Districts")+xlab("Hisp. Proportion of City by CVAP")+geom_smooth(method='lm', color='red', se=FALSE)


wprop<-ggplot(data=df, aes(x=prop_white_c, y=actual_maj_wht_c))+ geom_point() +xlim(0,1)+ylim(0,1)+
  geom_abline(slope=1, intercept=0,linetype='dotted', col='blue')+ylab("Proportion of Majority-White Districts")+xlab("White Proportion of City by CVAP")

aprop<-ggplot(data=df, aes(x=prop_asian_c, y=actual_maj_asn_c))+ geom_point() +xlim(0,1)+ylim(0,1)+
  geom_abline( slope=1, intercept=0,linetype='dotted', col='blue')+ylab("Proportion of Majority-Asian Districts")+xlab("Asian Proportion of City by CVAP")


grid.arrange(bprop, hprop, nrow = 1)

# **** FIGURE 2 ***** Segregation Against Actual

bseg<-ggplot(data=df%>%filter(black_viable_c==1), aes(x=black_seg_c, y=actual_maj_blk_c))+ geom_point() +xlim(0,1)+ylim(0,1)+
  ylab("Proportion of Majority-Black Districts")+xlab("Black Dissimilarity Index")+
  geom_smooth(method = "lm", formula=y~x, fill='darkred')

hseg<-ggplot(data=df%>%filter(hisp_viable_c==1), aes(x=hisp_seg, y=actual_maj_hisp_c))+ geom_point() +xlim(0,1)+ylim(0,1)+
  geom_smooth(method = "lm", formula=y~x, fill='darkred')+
  ylab("Proportion of Majority-Hisp. Districts")+xlab("Hisp. Dissimilarity Index")

wseg<-ggplot(data=df%>%filter(white_viable_c==1), aes(x=white_seg, y=actual_maj_wht_c))+ geom_point() +xlim(0,1)+ylim(0,1)+
  geom_smooth(method = "lm", formula=y~x, fill='darkred')+
  ylab("Proportion of Majority-White Districts")+xlab("White Dissimilarity Index")

aseg<-ggplot(data=df, aes(x=asian_seg, y=actual_maj_asn_c))+ geom_point() +xlim(0,1)+ylim(0,1)+
  geom_smooth(method = "lm", formula=y~x, fill='darkred')+
  ylab("Proportion of Majority-Asian Districts")+xlab("Asian Dissimilarity Index")

grid.arrange(bseg, hseg, nrow = 1)


