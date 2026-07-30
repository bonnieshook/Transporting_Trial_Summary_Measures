################################################################################
# Program: 00_truth_all.R
# Purpose: Simulation study for transportability paper (Hazard ratio scenarios)
# Programmer: Bonnie Shook-Sa
# Inputs: n/a
# Outputs: 00_simulation_scen1_truth_HR.csv -- 00_simulation_scen5_truth_HR.csv
################################################################################

library(boot)
library(tibble)
library(tidyverse)
library(survival)
library(meta)

## user-specified arguments
num.part.TP <- 50000000 #size of target population 

## user-specified arguments (scenario-specific)
S1.beta<-log(0.70)
S1.delta<-0
S1.xi<-0

S2.beta<-log(0.71)
S2.delta<-log(0.63)-log(0.71)
S2.xi<-0

S3.beta<-log(0.80)
S3.delta<-log(0.55)-log(0.80)
S3.xi<-0

S4.beta<-log(0.71)
S4.delta<-0
S4.xi<-log(0.63)-log(0.71)

S5.beta<-log(0.80)
S5.delta<-0
S5.xi<-log(0.55)-log(0.80)


set.seed(08062026)

for (Scenario in 1:5) {
  
  cat("Running Scenario", Scenario, "\n")
  
  outfile <- paste0("00_simulation_scen", Scenario, "_truth_HR.csv")

  #generate superpopulation of target population and limit to treated population
dat.TP <- tibble(
  part_id = seq_len(num.part.TP)) 

dat.TP <- dat.TP %>%
  mutate(
    #R = 0,                              # population indicator
    X = rbinom(n(), 1, 0.4),            # measured covariate 
    U = rbinom(n(), 1, 0.30),            # unmeasured confounder
    
    #exposure 
    pA = 1.2+0.6*X-0.24*U,    
    A = rbinom(n(), 1, plogis(pA)),
    
    #hazard for outcome 
    lp_Y0 = 0.03*exp(0.4*X + 0.4*U),
    lp_Y1 = case_when(
      Scenario == "1" ~ 0.03*exp(0.4*X + 0.4*U + S1.beta + S1.delta*X + S1.xi*U),
      Scenario == "2" ~ 0.03*exp(0.4*X + 0.4*U + S2.beta + S2.delta*X + S2.xi*U),
      Scenario == "3" ~ 0.03*exp(0.4*X + 0.4*U + S3.beta + S3.delta*X + S3.xi*U),
      Scenario == "4" ~ 0.03*exp(0.4*X + 0.4*U + S4.beta + S4.delta*X + S4.xi*U),
      Scenario == "5" ~ 0.03*exp(0.4*X + 0.4*U + S5.beta + S5.delta*X + S5.xi*U),
    ),
    
    runi1 = runif(n()),
    runi2 = runif(n()),
    
    T0 = -log(runi1)/lp_Y0,
    T1 = -log(runi1)/lp_Y1,
    
    C0 = pmin(15, -log(runi2)/(-log(0.92)/15)),
    C1 = pmin(15, -log(runi2)/(-log(0.92)/15)),
    
    Tstar0 = pmin(T0,C0),
    Tstar1 = pmin(T1,C1),
    
    event.0 = as.numeric(Tstar0==T0),
    event.1 = as.numeric(Tstar1==T1)
    )


dat.TP <- dat.TP %>% filter(A==1)

#Estimate survival at 15 years in each group
fit_km0 <- survfit(Surv(Tstar0, event.0) ~ 1, data = dat.TP)
S0_15 <- summary(fit_km0, times = 15, extend = TRUE)$surv

fit_km1 <- survfit(Surv(Tstar1, event.1) ~ 1, data = dat.TP)
S1_15 <- summary(fit_km1, times = 15, extend = TRUE)$surv

results_df<-S0_15-S1_15

# write results to CSV
setwd("<name of directory>")
write.csv(results_df, file = outfile, row.names = FALSE)

rm(dat.TP)
gc()
}



