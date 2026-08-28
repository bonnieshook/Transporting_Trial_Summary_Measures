################################################################################
# Program: 00_truth_all.R
# Purpose: Simulation study for transportability paper (Hazard ratio scenarios)
#          Determine the truth for each scenario
# Programmer: Bonnie Shook-Sa
# Inputs: n/a
# Outputs: 00_simulation_scen1_truth_HR.csv -- 00_simulation_scen5_truth_HR.csv
################################################################################

library(boot)
library(tibble)
library(tidyverse)
library(survival)
library(meta)


set.seed(08062026)

for (Scenario in 1:5) {
  
  cat("Running Scenario", Scenario, "\n")
  
  outfile <- paste0("00_simulation_scen", Scenario, "_", strength, "_truth_HR.csv")

  #generate superpopulation of target population and limit to treated population
dat.TP <- tibble(
  part_id = seq_len(num.part.TP.truth)) 

dat.TP <- dat.TP %>%
  mutate(
    #R = 0,                              # population indicator
    X = rbinom(n(), 1, p_X_TP),          # measured covariate 
    U = rbinom(n(), 1, p_U_TP),          # unmeasured confounder
    
    #exposure 
    pA = p_A_TP_int+p_A_TP_X*X+p_A_TP_U*U,    
    A = rbinom(n(), 1, plogis(pA)),
    
    #hazard for outcome 
    lp_Y0 = lp_Y0_TP*exp(lp_Y0_TP_X*X + lp_Y0_TP_U*U),
    lp_Y1 = case_when(
      Scenario == "1" ~ lp_Y0_TP*exp(lp_Y0_TP_X*X + lp_Y0_TP_U*U + S1.beta + S1.delta*X + S1.xi*U),
      Scenario == "2" ~ lp_Y0_TP*exp(lp_Y0_TP_X*X + lp_Y0_TP_U*U + S2.beta + S2.delta*X + S2.xi*U),
      Scenario == "3" ~ lp_Y0_TP*exp(lp_Y0_TP_X*X + lp_Y0_TP_U*U + S3.beta + S3.delta*X + S3.xi*U),
      Scenario == "4" ~ lp_Y0_TP*exp(lp_Y0_TP_X*X + lp_Y0_TP_U*U + S4.beta + S4.delta*X + S4.xi*U),
      Scenario == "5" ~ lp_Y0_TP*exp(lp_Y0_TP_X*X + lp_Y0_TP_U*U + S5.beta + S5.delta*X + S5.xi*U),
    ),
    
    runi1 = runif(n()),
    runi2 = runif(n()),
    
    T0 = -log(runi1)/lp_Y0,
    T1 = -log(runi1)/lp_Y1,
    
    C0 = pmin(15, -log(runi2)/(-log(censor)/15)),
    C1 = pmin(15, -log(runi2)/(-log(censor)/15)),
    
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
setwd("<path>")
write.csv(results_df, file = outfile, row.names = FALSE)

rm(dat.TP)
gc()
}



