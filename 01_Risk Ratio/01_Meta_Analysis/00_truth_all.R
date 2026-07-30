################################################################################
# Program: 00_truth_all.R
# Purpose: Simulation study for transportability paper (Risk ratio scenarios)
# Programmer: Bonnie Shook-Sa
# Inputs: n/a
# Outputs: 00_simulation_scen1_truth_RR.csv -- 00_simulation_scen5_truth_RR.csv
################################################################################

library(boot)
library(tibble)
library(tidyverse)
library(survival)
library(meta)

## user-specified arguments
num.part.TP <- 50000000 #size of target population 

## user-specified arguments (scenario-specific)
S1.RR<-0.9

S2.RR.X1<-0.85 #True risk ratio at 15 years, X=1
S2.RR.X0<-0.90 #True risk ratio at 15 years, X=0

S3.RR.X1<-0.75 #True risk ratio at 15 years, X=1
S3.RR.X0<-0.90 #True risk ratio at 15 years, X=0

S4.RR.U1<-0.85 #True risk ratio at 15 years, U=1
S4.RR.U0<-0.90 #True risk ratio at 15 years, U=0

S5.RR.U1<-0.75 #True risk ratio at 15 years, U=1
S5.RR.U0<-0.90 #True risk ratio at 15 years, U=0

set.seed(29012026)

for (Scenario in 1:5) {
  
  cat("Running Scenario", Scenario, "\n")
  
  outfile <- paste0("00_simulation_scen", Scenario, "_truth_RR.csv")

  #generate superpopulation of target population and limit to treated population
dat.TP <- tibble(
  part_id = seq_len(num.part.TP)) 

dat.TP <- dat.TP %>%
  mutate(
    R = 0,                              # population indicator
    X = rbinom(n(), 1, 0.4),            # measured covariate 
    U = rbinom(n(), 1, 0.30),            # unmeasured confounder
    
    #exposure 
    pA = 1.2+0.6*X-0.24*U,    
    A = rbinom(n(), 1, plogis(pA)),
    
    #linear predictor for outcome 
    lp_Y0 = log(0.22) + log(1.8) * X + log(1.6) * U,
    lp_Y1 = case_when(
      Scenario == "1" ~ lp_Y0 + log(S1.RR),
      Scenario == "2" ~ lp_Y0 + log(S2.RR.X1)*X + log(S2.RR.X0)*(1-X),
      Scenario == "3" ~ lp_Y0 + log(S3.RR.X1)*X + log(S3.RR.X0)*(1-X),
      Scenario == "4" ~ lp_Y0 + log(S4.RR.U1)*U + log(S4.RR.U0)*(1-U),
      Scenario == "5" ~ lp_Y0 + log(S5.RR.U1)*U + log(S5.RR.U0)*(1-U),
    ),

    pY0 = exp(lp_Y0),
    pY1 = exp(lp_Y1),
    
    Y0 = rbinom(n(), 1, pY0),
    Y1 = rbinom(n(), 1, pY1),
    
    # observed outcome
    Y = ifelse(A == 1, Y1, Y0)
  )

dat.TP <- dat.TP %>% filter(A==1)

results_df<-mean(dat.TP$Y1)-mean(dat.TP$Y0)

# write results to CSV
setwd("<name of directory>")
write.csv(results_df, file = outfile, row.names = FALSE)

rm(dat.TP)
gc()
}



