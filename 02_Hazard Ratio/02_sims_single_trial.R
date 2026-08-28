################################################################################
# Program: 02_sims_single_trial.R
# Purpose: Simulation study for transportability paper; code to run sims for all scenarios (hazard ratio scenarios) 
# Single trial setting
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

#Initialize dataset to store results
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

  dat.trial <- tibble(part_id = seq_len(num.part.Trials)) %>%
    mutate(
      R = t,
      X = rbinom(n(), 1, pX_single_trial),
      U = rbinom(n(), 1, p_U_trials),
      A = sample(rep(0:1, each = num.part.Trials / 2)),
      lp_Y0 = lp_Y0_trials*exp(lp_Y0_trials_X*X + lp_Y0_trials_U*U),
      lp_Y1 = case_when(
        Scenario == "1" ~ lp_Y0_trials*exp(lp_Y0_trials_X*X + lp_Y0_trials_U*U + S1.beta + S1.delta*X + S1.xi*U),
        Scenario == "2" ~ lp_Y0_trials*exp(lp_Y0_trials_X*X + lp_Y0_trials_U*U + S2.beta + S2.delta*X + S2.xi*U),
        Scenario == "3" ~ lp_Y0_trials*exp(lp_Y0_trials_X*X + lp_Y0_trials_U*U + S3.beta + S3.delta*X + S3.xi*U),
        Scenario == "4" ~ lp_Y0_trials*exp(lp_Y0_trials_X*X + lp_Y0_trials_U*U + S4.beta + S4.delta*X + S4.xi*U),
        Scenario == "5" ~ lp_Y0_trials*exp(lp_Y0_trials_X*X + lp_Y0_trials_U*U + S5.beta + S5.delta*X + S5.xi*U)),
      
      runi1 = runif(n()),
      runi2 = runif(n()),
      
      T0 = -log(runi1)/lp_Y0,
      T1 = -log(runi1)/lp_Y1,
      
      C0 = pmin(15, -log(runi2)/(-log(censor)/15)),
      C1 = pmin(15, -log(runi2)/(-log(censor)/15)),
      
      Tstar0 = pmin(T0,C0),
      Tstar1 = pmin(T1,C1),
      
      event.0 = as.numeric(Tstar0==T0),
      event.1 = as.numeric(Tstar1==T1),
      
      T_obs = ifelse(A == 1, Tstar1, Tstar0),
      event_obs = ifelse(A == 1, event.1, event.0)
    )
  
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
    X = rbinom(n(), 1, p_X_TP),         # measured covariate 
    U = rbinom(n(), 1, p_U_TP),         # unmeasured confounder
    
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
    event.1 = as.numeric(Tstar1==T1),
    
    T_obs = ifelse(A == 1, Tstar1, Tstar0),
    event_obs = ifelse(A == 1, event.1, event.0))

dat.TP <- dat.TP %>% filter(A==1)

# Generate data for all trials and summarize
all.trials <- purrr::map_dfr(seq_len(num.trials), ~ one_trial(.x))
all.trials.marg <- all.trials$marg
all.trials.cond <- all.trials$cond


# extract marginal and conditional estimates
META.marg<-data.frame(
  logHR.marg = as.numeric(all.trials.marg$logHR.marg),
  se_logHR.marg = as.numeric(all.trials.marg$se_logHR.marg)
)

META.cond.X0<-data.frame(
  logHR.X0 = as.numeric(all.trials.cond$logHR.X0),
  se_logHR.X0 = as.numeric(all.trials.cond$se_logHR.X0)
)
META.cond.X1<-data.frame(
  logHR.X1 = as.numeric(all.trials.cond$logHR.X1),
  se_logHR.X1 = as.numeric(all.trials.cond$se_logHR.X1)
)


#### Estimator 1: Marginally transport to general population to estimate risk difference

#Estimate of survival at 15 years from the general population
fit_km_trt <- survfit(Surv(T_obs, event_obs) ~ 1, data = dat.TP)
S_15_trt_TP <- summary(fit_km_trt, times = 15, extend = TRUE)$surv

#Fit the estimator for the ATT
ATT.est.marg<-S_15_trt_TP^(1/exp(META.marg$logHR.marg))-S_15_trt_TP 

### Estimator 2: Conditionally transport to general population to estimate risk difference

#Estimate of survival at 15 years from the general population
fit_km_trt_X0 <- survfit(Surv(T_obs, event_obs) ~ 1, data = dat.TP[dat.TP$X==0,])
S_15_trt_TP_X0 <- summary(fit_km_trt_X0, times = 15, extend = TRUE)$surv

fit_km_trt_X1 <- survfit(Surv(T_obs, event_obs) ~ 1, data = dat.TP[dat.TP$X==1,])
S_15_trt_TP_X1 <- summary(fit_km_trt_X1, times = 15, extend = TRUE)$surv

#Fit the estimator for the ATT
ATT.est.cond<-(S_15_trt_TP_X0^(1/exp(META.cond.X0$logHR.X0))-S_15_trt_TP_X0)*mean(dat.TP$X==0)+(S_15_trt_TP_X1^(1/exp(META.cond.X1$logHR.X1))-S_15_trt_TP_X1)*mean(dat.TP$X==1)


#Estimate the variance of each estimator using a monte carlo procedure 
n.trt <- nrow(dat.TP)
dat.TP.sub <- dat.TP[, c("X", "T_obs", "event_obs")]

one_mc_rep <- function(i, dat.TP.sub, n.trt, META.marg, META.cond.X0, META.cond.X1) {
  boot.TP <- dat.TP.sub[sample.int(n.trt, n.trt, replace = TRUE), , drop = FALSE]
  
  meta.draw.marg <- rnorm(1, mean = META.marg$logHR.marg, sd = META.marg$se_logHR.marg)
  
  fit_km_marg.MC <- survfit(Surv(T_obs, event_obs) ~ 1, data = boot.TP)
  S_15_trt_TP.MC <- summary(fit_km_marg.MC, times = 15, extend = TRUE)$surv
  
  ATT.marg.MC <- S_15_trt_TP.MC^(1 / exp(meta.draw.marg)) - S_15_trt_TP.MC
  ATT.marg.naive <- S_15_trt_TP.MC^(1 / exp(META.marg$logHR.marg)) - S_15_trt_TP.MC
  
  meta.draw.cond.X0 <- rnorm(1, mean = META.cond.X0$logHR.X0, sd = META.cond.X0$se_logHR.X0)
  meta.draw.cond.X1 <- rnorm(1, mean = META.cond.X1$logHR.X1, sd = META.cond.X1$se_logHR.X1)
  
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
    (S_15_trt_TP_X0.MC^(1 / exp(META.cond.X0$logHR.X0)) - S_15_trt_TP_X0.MC) * p0 +
    (S_15_trt_TP_X1.MC^(1 / exp(META.cond.X1$logHR.X1)) - S_15_trt_TP_X1.MC) * p1
  
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
logHR0 <- META.cond.X0$logHR.X0; se0 <- META.cond.X0$se_logHR.X0
logHR1 <- META.cond.X1$logHR.X1; se1 <- META.cond.X1$se_logHR.X1

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
results_dir <- "<path>"
write.csv(results_df, file = file.path(results_dir, outfile), row.names = FALSE)


