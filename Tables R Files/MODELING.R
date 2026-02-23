library(rstanarm)
library(sjPlot)
library(dplyr)
library(arm)

wd<-"~/Documents/GitHub/citl"
setwd(wd)
load("./Compiled Results/vra_ftp_data.RData")



#dataframe with more election data -----
df2<-read.csv("./Compiled Results/full_data_for_models_wdbk.csv")
df_2000<-read.csv("./Compiled Results/sumactualplans2000.csv")
df_2000$city<-df_2000$cities_2000
df2<-left_join(df2, df_2000, by='city')
df2$hispanic_mayor<-ifelse(df2$mayor_race=='hispanic',1,0)


# Models ----------------------------------------

# ***TABLE A2 ***** Black Proportion and Segregration v Simulated
b_m0<-stan_glm(data=df2%>%filter(black_viables_c==1), majority_black_c_median ~ prop_black_c + prop_white_c + black_seg_c,refresh=0  )

b_m1<-stan_glm(data=df2%>%filter(black_viables_c==1), majority_black_c_median ~ prop_black_c + prop_white_c + black_seg_c + black_seg_c*prop_black_c,refresh=0 )

b_m2<-stan_glm(data=df2%>%filter(black_viables_c==1), majority_black_c_95th ~ prop_black_c + prop_white_c + black_seg_c ,refresh=0 )

b_m3<-stan_glm(data=df2%>%filter(black_viables_c==1), majority_black_c_95th ~ prop_black_c + prop_white_c + black_seg_c + black_seg_c*prop_black_c,refresh=0 )

tab_model(b_m0, b_m1, b_m2, b_m3 , digits=1,collapse.ci = TRUE, ci.hyphen = ":",dv.labels=c("Median Proportion Majority-Black Simulated","Median Proportion Majority-Black Simulated",
                                                                                   "95th Percentile Majority-Black Simulated", "95th Percentile Majority-Black Simulated"),
          pred.labels=c('Intercept', "Proportion Black CVAP", "Proportion White CVAP", "Black Segregation", "Proportion Black:Black Segregation"),file = "./Table HTML Files/TableA2.html")

# ***TABLE A3 ***** Hisp. Proporiton and Segregration v Simulated

h_m0<-stan_glm(data=df2%>%filter(hisp_viables_c==1), majority_hisp_c_median ~ prop_hisp_c + prop_white_c + hisp_seg_c,refresh=0  )

h_m1<-stan_glm(data=df2%>%filter(hisp_viables_c==1), majority_hisp_c_median ~ prop_hisp_c + prop_white_c + hisp_seg_c + hisp_seg_c*prop_hisp_c,refresh=0 )

h_m2<-stan_glm(data=df2%>%filter(hisp_viables_c==1), majority_hisp_c_95th ~ prop_hisp_c + prop_white_c + hisp_seg_c,refresh=0 )

h_m3<-stan_glm(data=df2%>%filter(hisp_viables_c==1), majority_hisp_c_95th ~ prop_hisp_c + prop_white_c + hisp_seg_c + hisp_seg_c*prop_hisp_c,refresh=0 )

tab_model(h_m0, h_m1, h_m2, h_m3, digits=1,  collapse.ci = TRUE, ci.hyphen = ":",dv.labels=c("Median Proportion Majority-Hisp. Simulated","Median Proportion Majority-Hisp. Simulated",
                                                                                   "95th Percentile Majority-Hisp. Simulated", "95th Percentile Majority-Hisp. Simulated"),
          pred.labels=c('Intercept', "Proportion Hisp. CVAP", "Proportion White CVAP", "Hisp. Segregation", "Proportion Hisp.:Hisp. Segregation"),file = "./Table HTML Files/TableA3.html")




# *** TABLE A4 **** Black CVAP Prop vs Actual
b_mod0<-stan_glm(data=df2%>%filter(black_viables_c==1), actual_maj_blk_c ~ prop_black_c + prop_white_c + black_seg_c,refresh=0)

b_mod1<-stan_glm(data=df2%>%filter(black_viables_c==1), actual_maj_blk_c ~ prop_black_c + prop_white_c + black_seg_c+black_cc_prop_2010 + black_mayor +prob_democrat,refresh=0)

b_mod2<-stan_glm(data=df2%>%filter(black_viables_c==1), actual_maj_blk_c ~ prop_black_c + prop_white_c + black_seg_c + black_seg_c*prop_black_c,refresh=0)

b_mod3<-stan_glm(data=df2%>%filter(black_viables_c==1), actual_maj_blk_c ~ prop_black_c + prop_white_c + black_seg_c+ majority_black_c_median,refresh=0)

b_mod4<-stan_glm(data=df2%>%filter(black_viables_c==1), actual_maj_blk_c ~ prop_black_c + prop_white_c + black_seg_c + city_pop1m+
                  black_mayor + majority_black_c_median  + black_cc_prop_2010 + prob_democrat,refresh=0)

b_mod5<-stan_glm(data=df2%>%filter(black_viables_c==1), actual_maj_blk_c ~ prop_black_c + prop_white_c + black_seg_c + city_pop1m+
                  black_mayor + majority_black_c_median  + 
              black_cc_prop_2010+ demshare_pres,refresh=0)

tab_model(b_mod0, b_mod1, b_mod2, b_mod3, b_mod4, b_mod5, collapse.ci = TRUE, ci.hyphen = ":", 
          dv.labels = rep("Majority-Black Districts Implemented",6), 
          pred.labels=(c("Intercept", "City Proportion Black (CVAP)", "City Proporiton White (CVAP)",
                         "Black Segregation (CVAP)","Proportion Black Councilors" ,
                         "Black Mayor", "Democrat Mayor",
                         "Proportion Black:Black Segregation",
                         "Majority Black Districts in Median Simulation", "City Population in Millions",
                         "Dem Vote Share '08")),digits=1,file = "./Table HTML Files/TableA4.html")

# *** TABLE A5 ***** Hispanic CVAP Actual vs...


h_mod0<-stan_glm(data=df2%>%filter(hisp_viables_c==1), actual_maj_hisp_c ~ prop_hisp_c + prop_white_c + 
                   hisp_seg_c,refresh=0  )
h_mod1<-stan_glm(data=df2%>%filter(hisp_viables_c==1), actual_maj_hisp_c ~ prop_hisp_c + prop_white_c + 
                   hisp_seg_c+hispanic_cc_prop_2010+hispanic_mayor + prob_democrat,refresh=0  )

h_mod2<-stan_glm(data=df2%>%filter(hisp_viables_c==1), actual_maj_hisp_c ~ prop_hisp_c + prop_white_c +
                   hisp_seg_c + hisp_seg_c*prop_hisp_c,refresh=0 )

h_mod3<-stan_glm(data=df2%>%filter(hisp_viables_c==1), actual_maj_hisp_c ~ prop_hisp_c + prop_white_c +
                   hisp_seg_c+ majority_hisp_c_median,refresh=0 )

h_mod4<-stan_glm(data=df2%>%filter(hisp_viables_c==1), actual_maj_hisp_c ~ prop_hisp_c + prop_white_c + hisp_seg_c + city_pop1m+
                   hispanic_mayor +hispanic_cc_prop_2010+ majority_hisp_c_median +prob_democrat,refresh=0 )

h_mod5<-stan_glm(data=df2%>%filter(hisp_viables_c==1), actual_maj_hisp_c ~ prop_hisp_c + prop_white_c + hisp_seg_c + city_pop1m+
                   hispanic_mayor   +
                   demshare_pres,refresh=0 )

tab_model(h_mod0, h_mod1, h_mod2, h_mod3, h_mod4, h_mod5, collapse.ci = TRUE, ci.hyphen = ":", 
                  digits=1,dv.labels = rep("Majority-Hispanic Districts Implemented",6), 
                  pred.labels=(c("Intercept", "City Proportion Hispanic", "City Proporiton White",
                                 "Hispanic Segregation", "City Council Hispanic Proportion",
                                 "Hispanic Mayor", "Democrat Mayor",
                                 "Proportion Hispanic:Hispanic Segregation",
                                 "Majority Hispanic Districts in Median Simulation", "City Population in Millions",
                                 "Democratic Voteshare")),file = "./Table HTML Files/TableA5.html")




# ***** TABLE 3 ******
# GAP (actual - sim) Black VAP Prop vs Actual----filtered

b_mod0_gap<-stan_glm(data=df2%>%filter(black_viables_c==1), diff_black ~  prop_black_c + prop_white_c + black_seg_c ,refresh=0 )


b_mod1_gap<-stan_glm(data=df2%>%filter(black_viables_c==1), diff_black ~ prop_black_c + prop_white_c + black_seg_c + city_pop1m+
                       black_mayor   + black_cc_prop_2010,refresh=0 )

b_mod2_gap<-stan_glm(data=df2%>%filter(black_viables_c==1), diff_black ~ prop_black_c + prop_white_c + black_seg_c + city_pop1m+
                       black_mayor   + 
                       black_cc_prop_2010+ prob_democrat+ demshare_pres,refresh=0 )
b_mod3_gap<-stan_glm(data=df2%>%filter(black_viables_c==1), diff_black ~ prop_black_c + prop_white_c + black_seg_c +
                       majority_hisp_c_median+ black_cc_prop_2010,refresh=0 )

b_mod4_gap_2000<-stan_glm(data=df2%>%filter(black_viables_c==1), diff_black ~ prop_black_c + prop_white_c + black_seg_c +
                            black_cc_prop_2010 +actual_maj_blk_v_2000,refresh=0 )

tab_model(b_mod0_gap, b_mod1_gap, b_mod2_gap, b_mod3_gap, b_mod4_gap_2000, collapse.ci = TRUE, ci.hyphen = ":", 
          digits=2, dv.labels = rep("Gap Between Simulated and Implemented Majority-Black Districts",5),
          pred.labels=c("(Intercept)", "City Proportion Black (CVAP)", "City Proportion White (CVAP)",
                        "Black Segregation (CVAP)", "City Pop. (in millions)", "Black Mayor in 2010",
                        "Proportion Black Councilors in 2010", "Democrat Mayor",
                        "Dem Vote Share in '08", "Proportion Majority Hisp. Districts in Median Sim.",
                        "Proportion Majority-Black Districts in 2000"),file = "./Table HTML Files/Table3.html")

# ****** TABLE 4 *****
#Hispanic CVAP Gap Between Actual and Simulated vs... filtered ---- TABLE 2-------

h_mod0_gap<-stan_glm(data=df2%>%filter(hisp_viables_c==1), diff_hisp ~prop_hisp_c + prop_white_c + 
                       hisp_seg_c,refresh=0  )
h_mod1_gap<-stan_glm(data=df2%>%filter(hisp_viables_c==1), diff_hisp ~ prop_hisp_c + prop_white_c + 
                       hisp_seg_c+hispanic_cc_prop_2010+hispanic_mayor + city_pop1m,refresh=0  )

h_mod2_gap<-stan_glm(data=df2%>%filter(hisp_viables_c==1), diff_hisp ~ prop_hisp_c + prop_white_c + hisp_seg_c + city_pop1m+
                       hispanic_mayor+ hispanic_cc_prop_2010 +demshare_pres +prob_democrat   ,refresh=0 )

h_mod3_gap<-stan_glm(data=df2%>%filter(hisp_viables_c==1), diff_hisp ~prop_hisp_c + prop_white_c + hisp_seg_c +
                       hispanic_cc_prop_2010+majority_black_c_median,refresh=0 )


tab_model(h_mod0_gap, h_mod1_gap, h_mod2_gap, h_mod3_gap, collapse.ci = TRUE, ci.hyphen = ":", 
                  digits=2,dv.labels = rep("Gap Between Simulated and Implemented Majority-Hispanic Districts",4),
          pred.labels=c("(Intercept)", "City Proportion Hisp. (CVAP)", "City Proportion White (CVAP)",
                        "Hisp. Segregation (CVAP)", "Proportion Hisp. Councilors in 2010", "Hisp. Mayor in 2010",
                        "City Pop. (in millions)","Dem Vote Share in '08","Democrat Mayor", 
                        "Proportion Majority Black Districts in Median Sim." ),file = "./Table HTML Files/Table4.html") 





# **** TABLE A6 ***** 

# Black Gap Interactions----
b_mod0_gap_all<-stan_glm(data=df2, diff_black  ~ majority_black_c_median+  prop_black_c + prop_white_c + black_seg_c,refresh=0 )

b_mod2_gap_all<-stan_glm(data=df2, diff_black ~majority_black_c_median+  prop_black_c + prop_white_c + black_seg_c + black_seg_c*prop_black_c,refresh=0 )

b_mod4_gap_all<-stan_glm(data=df2, diff_black ~ majority_black_c_median+  prop_black_c + prop_white_c + black_seg_c + city_pop1m+
                           demshare_pres+prob_democrat,refresh=0 )

b_mod1_gap_all<-stan_glm(data=df2, diff_black ~ majority_black_c_median+  prop_black_c + prop_white_c + black_seg_c+
                           black_cc_prop_2010 + black_mayor,refresh=0 )

b_mod3_gap_all<-stan_glm(data=df2, diff_black ~ majority_black_c_median+ prop_black_c + prop_white_c + black_seg_c,refresh=0 )



b_mod5_gap_all<-stan_glm(data=df2, diff_black ~ prop_black_c + prop_white_c + black_seg_c + city_pop1m+
                           black_mayor + majority_black_c_median+
                           prob_democrat +black_cc_prop_2010+ demshare_pres,refresh=0 )

b_mod6_gap_all<-stan_glm(data=df2, diff_black ~ prop_black_c + prop_white_c + black_seg_c + city_pop1m+
                           black_mayor + majority_black_c_median+
                           prob_democrat +black_cc_prop_2010*black_seg_c+ demshare_pres,refresh=0 )


tab_model(b_mod0_gap_all, b_mod2_gap_all, b_mod4_gap_all, b_mod1_gap_all, b_mod5_gap_all,b_mod6_gap_all, collapse.ci = TRUE, ci.hyphen = ":", 
                          dv.labels = rep("Gap Between Implemented and Simulated",6),digits=1,file = "./Table HTML Files/TableA6.html")




# ******* Table A7***** 

#Alternate Specifications /Robustness Check


b2000_1<-stan_glm(data=df2%>%filter(black_viables_c), diff_black ~ prop_black_c   + majority_black_c_median
                +black_cc_prop_2010 + actual_maj_blk_v_2000 
                 )

b2000_2<-stan_glm(data=df2%>%filter(black_viables_c), diff_black ~ prop_black_c + prop_white_c + black_seg_c + majority_black_c_median
                            +black_cc_prop_2010 + actual_maj_blk_v_2000 +  prop_black_v_2000+ demshare_pres
                 )

b2000_3<-stan_glm(data=df2%>%filter(black_viables_c), diff_black ~ prop_black_c + prop_white_c + black_seg_c + majority_black_c_median+
                  prob_democrat +black_cc_prop_2010 + actual_maj_blk_v_2000 + demshare_pres +prop_black_v_2000
                  +black_mayor  )
tab_model(b2000_1, b2000_2, b2000_3, collapse.ci =TRUE, ci.hyphen = ':', digits=2,
          dv.labels="Gap Between Implemented and Simulated Black-Districts",
          pred.labels=c("(Intercept)", "City Proportion Black (CVAP)", 
                        "Median Proportion Black Districts Simulated", 
                        "City Council Proportion Black in 2010",
                        "Proportion Black-Majority Implemented in 2000",
                        "City Proportion White (CVAP)",
                        "Black Segregation (CVAP)",
                        "City Proportion Black (CVAP) in 2000)",
                        "Dem Pres. Vote Share in '08",
                        "Democrat Mayor in 2010",
                        "Black Mayor in 2010"),file = "./Table HTML Files/TableA7.html")

