################################################################################
# Program: 01_sims.R
# Purpose: Simulation study for transportability paper; code to run sims for all scenarios (hazard ratio scenarios)
# Programmer: Bonnie Shook-Sa
# Inputs: n/a
# Outputs: output file for given scenario
################################################################################

library(boot)
library(tibble)
library(tidyverse)
library(survival)
library(meta)
library(future.apply)

#Set up parallel workers
plan(multisession, workers = max(1, parallel::detectCores() - 1))


## user-specified arguments (common to all scenarios)
num.sims <- 2000 #number of simulations
num.reps <- 500 #for monte carlo procedure/variance estimation
num.part.TP <- 15000 #number of target population participants
num.part.Trials <- 650 #number of participants per trial
num.trials <- 15 #number of trials

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

#Initalize dataset to store results
results_df <- tibble(
  sim.num = integer(),
  ATT.est.marg = numeric(),
  ATT.est.cond = numeric(),
  CI.overlap.cond = numeric(),
  SE.ATT.marg.MC = numeric(),
  SE.ATT.marg.naive = numeric(),
  SE.ATT.cond.MC = numeric(),
  SE.ATT.cond.naive = numeric()
)

set.seed(08062026)

#Helper function - generate data for a single trial
one_trial <- function(t) {
  pNpos <- runif(1, 0.08, 0.20)
  
  dat.trial <- tibble(part_id = seq_len(num.part.Trials)) %>%
    mutate(
      R = t,
      X = rbinom(n(), 1, pNpos),
      U = rbinom(n(), 1, 0.08),
      A = sample(rep(0:1, each = num.part.Trials / 2)),
      lp_Y0 = 0.0267*exp(0.4*X + 0.4*U),
      lp_Y1 = case_when(
        Scenario == "1" ~ 0.0267*exp(0.4*X + 0.4*U + S1.beta + S1.delta*X + S1.xi*U),
        Scenario == "2" ~ 0.0267*exp(0.4*X + 0.4*U + S2.beta + S2.delta*X + S2.xi*U),
        Scenario == "3" ~ 0.0267*exp(0.4*X + 0.4*U + S3.beta + S3.delta*X + S3.xi*U),
        Scenario == "4" ~ 0.0267*exp(0.4*X + 0.4*U + S4.beta + S4.delta*X + S4.xi*U),
        Scenario == "5" ~ 0.0267*exp(0.4*X + 0.4*U + S5.beta + S5.delta*X + S5.xi*U)),
      
      runi1 = runif(n()),
      runi2 = runif(n()),
      
      T0 = -log(runi1)/lp_Y0,
      T1 = -log(runi1)/lp_Y1,
      
      C0 = pmin(15, -log(runi2)/(-log(0.92)/15)),
      C1 = pmin(15, -log(runi2)/(-log(0.92)/15)),
      
      Tstar0 = pmin(T0,C0),
      Tstar1 = pmin(T1,C1),
      
      event.0 = as.numeric(Tstar0==T0),
      event.1 = as.numeric(Tstar1==T1),
      
      T_obs = ifelse(A == 1, Tstar1, Tstar0),
      event_obs = ifelse(A == 1, event.1, event.0)
    )
  
  #Fit marginal model
  fit.marg <- coxph(Surv(T_obs, event_obs) ~ A, data = dat.trial)
  logHR.marg <- coef(fit.marg)["A"]
  se_logHR.marg <- sqrt(vcov(fit.marg)["A", "A"])
  trial_summary_marg=as.data.frame(cbind(logHR.marg,se_logHR.marg))
  
  #Fit conditional on X
  X0<-dat.trial %>% filter(X==0)
  X1<-dat.trial %>% filter(X==1)
  
  fit.X0 <- coxph(Surv(T_obs, event_obs) ~ A, data = X0)
  logHR.X0 <- coef(fit.X0)["A"]
  se_logHR.X0 <- sqrt(vcov(fit.X0)["A", "A"])
  
  fit.X1 <- coxph(Surv(T_obs, event_obs) ~ A, data = X1)
  logHR.X1 <- coef(fit.X1)["A"]
  se_logHR.X1 <- sqrt(vcov(fit.X1)["A", "A"])
  
  trial_summary_cond=as.data.frame(cbind(logHR.X1,se_logHR.X1,logHR.X0,se_logHR.X0))
  
  list(
    marg = trial_summary_marg,
    cond = trial_summary_cond
  )
}

#Helper function - run a single meta analysis and return the log HR and se(log RR)
meta_analysis <- function(data,log.HR,SE.log.HR) {
META <- metagen(
  TE = log.HR,
  seTE = SE.log.HR,
  data = data,
  sm = "HR",
  random = FALSE,
  backtransf = FALSE
)
META.SUM<-summary(META)
Meta.logHR<-(META.SUM$common$TE)
Meta.selogHR<-META.SUM$common$seTE
res<-c(Meta.logHR,Meta.selogHR)
return(res)
}

#Function to run a single iteration of the simulation
run_one_sim <- function(sim_k,
                        num.part.TP, num.part.Trials, num.trials,
                        num.reps) {
  
  #generate target population data and limit to treated population
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
    event.1 = as.numeric(Tstar1==T1),
    
    T_obs = ifelse(A == 1, Tstar1, Tstar0),
    event_obs = ifelse(A == 1, event.1, event.0))

dat.TP <- dat.TP %>% filter(A==1)

# Generate data for all trials and summarize
all.trials <- purrr::map_dfr(seq_len(num.trials), ~ one_trial(.x))
all.trials.marg <- all.trials$marg
all.trials.cond <- all.trials$cond


# Conduct meta-analysis and extract log HR and standard error (marginal and conditional)
META.marg<-meta_analysis(all.trials.marg,all.trials.marg$logHR.marg,all.trials.marg$se_logHR.marg)
META.cond.X0<-meta_analysis(all.trials.cond,all.trials.cond$logHR.X0,all.trials.cond$se_logHR.X0)
META.cond.X1<-meta_analysis(all.trials.cond,all.trials.cond$logHR.X1,all.trials.cond$se_logHR.X1)

#### Estimator 1: Marginally transport to general population to estimate risk difference

#Estimate of survival at 15 years from the general population
fit_km_trt <- survfit(Surv(T_obs, event_obs) ~ 1, data = dat.TP)
S_15_trt_TP <- summary(fit_km_trt, times = 15, extend = TRUE)$surv

#Fit the estimator for the ATT
ATT.est.marg<-S_15_trt_TP^(1/exp(META.marg[1]))-S_15_trt_TP 

### Estimator 2: Conditionally transport to general population to estimate risk difference

#Estimate of survival at 15 years from the general population
fit_km_trt_X0 <- survfit(Surv(T_obs, event_obs) ~ 1, data = dat.TP[dat.TP$X==0,])
S_15_trt_TP_X0 <- summary(fit_km_trt_X0, times = 15, extend = TRUE)$surv

fit_km_trt_X1 <- survfit(Surv(T_obs, event_obs) ~ 1, data = dat.TP[dat.TP$X==1,])
S_15_trt_TP_X1 <- summary(fit_km_trt_X1, times = 15, extend = TRUE)$surv

#Fit the estimator for the ATT
ATT.est.cond<-(S_15_trt_TP_X0^(1/exp(META.cond.X0[1]))-S_15_trt_TP_X0)*mean(dat.TP$X==0)+(S_15_trt_TP_X1^(1/exp(META.cond.X1[1]))-S_15_trt_TP_X1)*mean(dat.TP$X==1)


#Estimate the variance of each estimator using a monte carlo procedure 
n.trt <- nrow(dat.TP)
dat.TP.sub <- dat.TP[, c("X", "T_obs", "event_obs")]

one_mc_rep <- function(i, dat.TP.sub, n.trt, META.marg, META.cond.X0, META.cond.X1) {
  boot.TP <- dat.TP.sub[sample.int(n.trt, n.trt, replace = TRUE), , drop = FALSE]
  
  meta.draw.marg <- rnorm(1, mean = META.marg[1], sd = META.marg[2])
  
  fit_km_marg.MC <- survfit(Surv(T_obs, event_obs) ~ 1, data = boot.TP)
  S_15_trt_TP.MC <- summary(fit_km_marg.MC, times = 15, extend = TRUE)$surv
  
  ATT.marg.MC <- S_15_trt_TP.MC^(1 / exp(meta.draw.marg)) - S_15_trt_TP.MC
  ATT.marg.naive <- S_15_trt_TP.MC^(1 / exp(META.marg[1])) - S_15_trt_TP.MC
  
  meta.draw.cond.X0 <- rnorm(1, mean = META.cond.X0[1], sd = META.cond.X0[2])
  meta.draw.cond.X1 <- rnorm(1, mean = META.cond.X1[1], sd = META.cond.X1[2])
  
  boot0 <- boot.TP[boot.TP$X == 0, , drop = FALSE]
  boot1 <- boot.TP[boot.TP$X == 1, , drop = FALSE]
  
  fit_km_trt_X0.MC <- survfit(Surv(T_obs, event_obs) ~ 1, data = boot0)
  fit_km_trt_X1.MC <- survfit(Surv(T_obs, event_obs) ~ 1, data = boot1)
  
  S_15_trt_TP_X0.MC <- summary(fit_km_trt_X0.MC, times = 15, extend = TRUE)$surv
  S_15_trt_TP_X1.MC <- summary(fit_km_trt_X1.MC, times = 15, extend = TRUE)$surv
  
  p0 <- mean(boot.TP$X == 0)
  p1 <- 1 - p0
  
  ATT.cond.MC <-
    (S_15_trt_TP_X0.MC^(1 / exp(meta.draw.cond.X0)) - S_15_trt_TP_X0.MC) * p0 +
    (S_15_trt_TP_X1.MC^(1 / exp(meta.draw.cond.X1)) - S_15_trt_TP_X1.MC) * p1
  
  ATT.cond.naive <-
    (S_15_trt_TP_X0.MC^(1 / exp(META.cond.X0[1])) - S_15_trt_TP_X0.MC) * p0 +
    (S_15_trt_TP_X1.MC^(1 / exp(META.cond.X1[1])) - S_15_trt_TP_X1.MC) * p1
  
  c(
    ATT.marg.MC = ATT.marg.MC,
    ATT.marg.naive = ATT.marg.naive,
    ATT.cond.MC = ATT.cond.MC,
    ATT.cond.naive = ATT.cond.naive
  )
}

att_reps <- future_lapply(
  seq_len(num.reps),
  one_mc_rep,
  dat.TP.sub = dat.TP.sub,
  n.trt = n.trt,
  META.marg = META.marg,
  META.cond.X0 = META.cond.X0,
  META.cond.X1 = META.cond.X1,
  future.seed = TRUE
)

#get standard errors for each estimator
att_reps <- do.call(cbind, att_reps)
att_se <- apply(att_reps, 1, sd)
att_se <- setNames(att_se, paste0("SE.", names(att_se)))

# See whether CIs overlapped for the stratified estimates of the HR
logHR0 <- META.cond.X0[1]; se0 <- META.cond.X0[2]
logHR1 <- META.cond.X1[1]; se1 <- META.cond.X1[2]

ci0_lower <- logHR0 - 1.96 * se0
ci0_upper <- logHR0 + 1.96 * se0

ci1_lower <- logHR1 - 1.96 * se1
ci1_upper <- logHR1 + 1.96 * se1

CI_overlap_cond <- as.integer((ci0_lower <= ci1_upper) & (ci1_lower <= ci0_upper))


# return only the small result tibble
tibble(
  sim.num = sim_k,
  ATT.est.marg = ATT.est.marg,
  ATT.est.cond = ATT.est.cond,
  CI.overlap.cond = CI_overlap_cond
) %>%
  bind_cols(as_tibble_row(att_se))
}

#Run num.sims simulations, compiling results in results_df
res_list <- vector("list", num.sims)
  
  for (k in seq_len(num.sims)) {
    res_list[[k]] <- run_one_sim(k, num.part.TP, num.part.Trials, num.trials, num.reps)
    if (k %% 50 == 0) message("Completed simulation ", k, " / ", num.sims)
  }
  
  results_df <- dplyr::bind_rows(res_list)
  

# write results to CSV
setwd("<name of directory>")
write.csv(results_df, file = outfile, row.names = FALSE)


