################################################################################
# Program: 03_combine_sim_table.R
# Purpose: Simulation study for transportability paper (risk ratio scenarios): Combine Sim results and output table 
# Single trial, small sample size
# Programmer: Bonnie Shook-Sa
# Inputs: n/a
# Outputs: simulation summary table
################################################################################

library(tidyverse)

#bring in true value for each scenario
setwd("<path>")

truth_list_mod <- lapply(1:5, function(i) {
  df <- read.csv(paste0("00_simulation_scen", i, "_moderate_truth_RR.csv"))
  df$x
})

truth_list_strong <- lapply(1:5, function(i) {
  df <- read.csv(paste0("00_simulation_scen", i, "_strong_truth_RR.csv"))
  df$x
})

#bring in simulation results for each scenario
Sc1.mod<-as.data.frame(read.csv("01_simulation_results_scen1_RR_ST_smalln_mod.csv"))
Sc2.mod<-as.data.frame(read.csv("02_simulation_results_scen2_RR_ST_smalln_mod.csv"))
Sc3.mod<-as.data.frame(read.csv("03_simulation_results_scen3_RR_ST_smalln_mod.csv"))
Sc4.mod<-as.data.frame(read.csv("04_simulation_results_scen4_RR_ST_smalln_mod.csv"))
Sc5.mod<-as.data.frame(read.csv("05_simulation_results_scen5_RR_ST_smalln_mod.csv"))

Sc1.strong<-as.data.frame(read.csv("01_simulation_results_scen1_RR_ST_smalln_strong.csv"))
Sc2.strong<-as.data.frame(read.csv("02_simulation_results_scen2_RR_ST_smalln_strong.csv"))
Sc3.strong<-as.data.frame(read.csv("03_simulation_results_scen3_RR_ST_smalln_strong.csv"))
Sc4.strong<-as.data.frame(read.csv("04_simulation_results_scen4_RR_ST_smalln_strong.csv"))
Sc5.strong<-as.data.frame(read.csv("05_simulation_results_scen5_RR_ST_smalln_strong.csv"))

#Function to calculate bias, ASE, ESE, SER, and CI overlap
calc_bias <- function(ATTest,seATTest,truth,scen,est,overlap){
  ATTbias<-mean(100*ATTest, na.rm = TRUE)-100*truth
  ATTrelbias<-100*ATTbias/(100*truth)
  ll<-ATTest-1.96*seATTest
  ul<-ATTest+1.96*seATTest
  cov <-100*mean((ll <= truth & ul >= truth))
  ASE<-mean(na.omit(100*seATTest))
  ESE<-sd(na.omit(100*ATTest))
  SER<-ASE/ESE
  estimate<-ifelse(est==1,"Marginal","Conditional")
  CI_overlap<-100*mean(na.omit(overlap))
  results<-c(scen, estimate, round(ATTbias,digits=1), round(ATTrelbias,digits=1),round(ASE, digits=2),round(ESE, digits=2),round(SER, digits=2),round(cov, digits=0),round(CI_overlap,digits=1))
}

#Run for marginal and conditional estimator results
prep_data <- function(dat,Scen,truth){
  est.Marg<-calc_bias(dat$ATT.est.marg,dat$SE.ATT.marg.MC,truth,Scen,1,dat$CI.overlap.cond)
  est.Cond<-calc_bias(dat$ATT.est.cond,dat$SE.ATT.cond.MC,truth,Scen,2,dat$CI.overlap.cond)
  combine<-rbind(est.Marg,est.Cond)
}

#Run for each scenario
Sc1.mod.res<-prep_data(Sc1.mod,1,truth_list_mod[[1]])
Sc2.mod.res<-prep_data(Sc2.mod,2,truth_list_mod[[2]])
Sc3.mod.res<-prep_data(Sc3.mod,3,truth_list_mod[[3]])
Sc4.mod.res<-prep_data(Sc4.mod,4,truth_list_mod[[4]])
Sc5.mod.res<-prep_data(Sc5.mod,5,truth_list_mod[[5]])

Sc1.strong.res<-prep_data(Sc1.strong,1,truth_list_strong[[1]])
Sc2.strong.res<-prep_data(Sc2.strong,2,truth_list_strong[[2]])
Sc3.strong.res<-prep_data(Sc3.strong,3,truth_list_strong[[3]])
Sc4.strong.res<-prep_data(Sc4.strong,4,truth_list_strong[[4]])
Sc5.strong.res<-prep_data(Sc5.strong,5,truth_list_strong[[5]])


#Combine and output primary results
all_scen<-as.data.frame(rbind(Sc1.mod.res,Sc2.mod.res,Sc3.mod.res,Sc4.mod.res,Sc5.mod.res,Sc1.strong.res,Sc2.strong.res,Sc3.strong.res,Sc4.strong.res,Sc5.strong.res))
colnames(all_scen)<-c("Scenario","Estimator","Bias (%)", "Relative Bias (%)", "ASE", "ESE", "SER", "Coverage","CI Overlap")

all_scen<- all_scen %>%
  mutate(Scenario = case_when(
    Scenario == "1" ~ "1A",
    Scenario == "2" ~ "1B",
    Scenario == "3" ~ "1C",
    Scenario == "4" ~ "1D",
    Scenario == "5" ~ "1E",
  ))

write.csv(all_scen, "05a_SimSummary_table_RR_ST_smalln.csv")

all_scen<- all_scen %>% select(-c("CI Overlap"))

Latex.tab <- apply(all_scen, 1, function(x) {
  paste0(" & ", paste(x, collapse = " & "), " \\\\")
})

# save to a text file
writeLines(Latex.tab, "Latex.tab.RR.ST.small.n.txt")
















