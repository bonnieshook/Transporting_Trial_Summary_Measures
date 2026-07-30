################################################################################
# Program: 03_combine_sim_table.R
# Purpose: Simulation study for transportability paper (risk ratio scenarios) - single trial: Combine Sim results and output table 
# Programmer: Bonnie Shook-Sa
# Inputs: n/a
# Outputs: simulation summary table
################################################################################

#bring in true value for each scenario
setwd("<name of directory>")

truth_list <- lapply(1:5, function(i) {
  df <- read.csv(paste0("00_simulation_scen", i, "_truth_RR.csv"))
  df$x
})

setwd("<name of directory>")

#bring in simulation results for each scenario
Sc1<-as.data.frame(read.csv("01_simulation_results_scen1_RR_ST_smalln.csv"))
Sc2<-as.data.frame(read.csv("02_simulation_results_scen2_RR_ST_smalln.csv"))
Sc3<-as.data.frame(read.csv("03_simulation_results_scen3_RR_ST_smalln.csv"))
Sc4<-as.data.frame(read.csv("04_simulation_results_scen4_RR_ST_smalln.csv"))
Sc5<-as.data.frame(read.csv("05_simulation_results_scen5_RR_ST_smalln.csv"))

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
  results<-c(scen, estimate, round(ATTbias,digits=1), round(ATTrelbias,digits=1),round(ASE, digits=2),round(ESE, digits=2),round(SER, digits=2),round(cov, digits=0))
}

#Run for marginal and conditional esimator results
prep_data <- function(dat,Scen,truth){
  est.Marg<-calc_bias(dat$ATT.est.marg,dat$SE.ATT.marg.MC,truth,Scen,1,dat$CI.overlap.cond)
  est.Cond<-calc_bias(dat$ATT.est.cond,dat$SE.ATT.cond.MC,truth,Scen,2,dat$CI.overlap.cond)
  combine<-rbind(est.Marg,est.Cond)
}

#Run for each scenario
Sc1.res<-prep_data(Sc1,1,truth_list[[1]])
Sc2.res<-prep_data(Sc2,2,truth_list[[2]])
Sc3.res<-prep_data(Sc3,3,truth_list[[3]])
Sc4.res<-prep_data(Sc4,4,truth_list[[4]])
Sc5.res<-prep_data(Sc5,5,truth_list[[5]])

#Combine and output primary results
all_scen<-as.data.frame(rbind(Sc1.res,Sc2.res,Sc3.res,Sc4.res,Sc5.res))
colnames(all_scen)<-c("Scenario","Estimator","Bias (%)", "Relative Bias (%)", "ASE", "ESE", "SER", "Coverage")

Latex.tab <- apply(all_scen, 1, function(x) {
  paste0(paste(x, collapse = " & "), " \\\\")
})

# save to a text file
writeLines(Latex.tab, "Latex.tab.n650.txt")

write.csv(Latex.tab, "06_SimSummary_table_RR_ST_n650.csv")


