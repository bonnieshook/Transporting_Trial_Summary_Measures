################################################################################
# Program: 02_sims_single_trial.R
# Purpose: Simulation study for transportability paper; code to run sims for all scenarios (Risk Ratio scenarios) 
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

set.seed(29012026)

#Helper function - generate data for a single trial
one_trial <- function(t) {

 dat.trial <- tibble(part_id = seq_len(num.part.Trials)) %>%
    mutate(
      R = t,
      X = rbinom(n(), 1, pX_single_trial),
      U = rbinom(n(), 1, p_U_trials),
      A = sample(rep(0:1, each = num.part.Trials / 2)),
      lp_Y0 = lp_Y0_trials_int + lp_Y0_trials_X * X + lp_Y0_trials_U * U,
      #effect depends on scenario
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
      Y = ifelse(A == 1, Y1, Y0)
    )
  

  #Summarize the results of the trials
  trial_summary_marg <- dat.trial %>%
    group_by(R, A) %>%
    summarise(n = n(), events = sum(Y), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = A,
                       values_from = c(n, events),
                       names_glue = "{.value}_A{A}",
                       values_fill = list(n = 0, events = 0)) %>%
    mutate(trial = t) %>% relocate(trial)
  
  trial_summary_cond <- dat.trial %>%
    group_by(R, X, A) %>%
    summarise(
      n = n(),
      events = sum(Y),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = A,
      values_from = c(n, events),
      names_glue = "{.value}_A{A}",
      values_fill = list(n = 0, events = 0)
    ) 
  
  list(
    marg = trial_summary_marg,
    cond = trial_summary_cond
  )
}

#Helper function - calculate the RR and SE(RR) for the single trial
risk_ratio<-function(data){
  p1 <- data$events_A1 / data$n_A1
  p0 <- data$events_A0 / data$n_A0
  
  # Risk ratio
  RR <- p1 / p0
  
  # Log risk ratio
  logRR <- log(RR)
  
  # Standard error of log(RR)
  se_logRR <- sqrt((data$n_A1-data$events_A1)/(data$events_A1*data$n_A1)+(data$n_A0-data$events_A0)/(data$events_A0*data$n_A0))
  
  res<-c(logRR,se_logRR)
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
    X = rbinom(n(), 1, p_X_TP),         # measured covariate 
    U = rbinom(n(), 1, p_U_TP),         # unmeasured covariate
    
    #exposure 
    pA = p_A_TP_int+p_A_TP_X*X + p_A_TP_U*U,    
    A = rbinom(n(), 1, plogis(pA)),
    
    #linear predictor for outcome 
    lp_Y0 = lp_Y0_TP_int + lp_Y0_TP_X * X + lp_Y0_TP_U * U,
    #effect depends on scenario
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

# Generate data for all trials and summarize
trial_summaries <- purrr::map(seq_len(num.trials), one_trial)

all.trials.marg <- purrr::map_dfr(trial_summaries, "marg")
all.trials.cond <- purrr::map_dfr(trial_summaries, "cond")

# Estimate RR and standard error (marginal and conditional)
META.marg<-risk_ratio(all.trials.marg)
META.cond.X0<-risk_ratio(all.trials.cond[all.trials.cond$X==0,])
META.cond.X1<-risk_ratio(all.trials.cond[all.trials.cond$X==1,])

#Estimator 1: Marginally transport to general population to estimate risk difference
ATT.est.marg<-mean(dat.TP$Y)*(1-(exp(META.marg[1])^(-1)))

#Estimator 2: Conditionally transport to general population to estimate risk difference
ATT.est.cond<-mean(dat.TP$Y[dat.TP$X == 0])*(1-(exp(META.cond.X0[1])^(-1)))*mean(dat.TP$X==0)+mean(dat.TP$Y[dat.TP$X == 1])*(1-(exp(META.cond.X1[1])^(-1)))*mean(dat.TP$X==1)

#Estimate the variance of each estimator using a monte carlo procedure 
n.trt<-nrow(dat.TP)
dat.TP.sub <- dat.TP[, c("X", "Y")]

one_mc_rep <- function(i, dat.TP.sub, n.trt, META.marg, META.cond.X0, META.cond.X1) {
  boot.TP <- dat.TP.sub[sample.int(n.trt, n.trt, replace = TRUE), , drop = FALSE]
  meta.draw.marg <- rnorm(1, mean = META.marg[1], sd = META.marg[2]) #monte carlo draw from the meta analysis result (marginal)
  ATT.marg.MC<- mean(boot.TP$Y) * (1 - (exp(meta.draw.marg)^(-1))) #estimate of the ATT for this replicate (marginal), MC method
  ATT.marg.naive<- mean(boot.TP$Y) * (1 - (exp(META.marg[1])^(-1))) #estimate of the ATT for this replicate (marginal), naive method
  
  meta.draw.cond.X0 <- rnorm(1, mean = META.cond.X0[1], sd = META.cond.X0[2]) #monte carlo draw from the meta analysis result (cond)
  meta.draw.cond.X1 <- rnorm(1, mean = META.cond.X1[1], sd = META.cond.X1[2]) #monte carlo draw from the meta analysis result (cond)
  ATT.cond.MC<- mean(boot.TP$Y[boot.TP$X == 0])*(1-(exp(meta.draw.cond.X0)^(-1)))*mean(boot.TP$X==0)+mean(boot.TP$Y[boot.TP$X == 1])*(1-(exp(meta.draw.cond.X1)^(-1)))*mean(boot.TP$X==1) #estimate of the ATT for this replicate (cond), MC method
  ATT.cond.naive<- mean(boot.TP$Y[boot.TP$X == 0])*(1-(exp(META.cond.X0[1])^(-1)))*mean(boot.TP$X==0)+mean(boot.TP$Y[boot.TP$X == 1])*(1-(exp(META.cond.X1[1])^(-1)))*mean(boot.TP$X==1) #estimate of the ATT for this replicate (cond), naive method
  
  c(
    ATT.marg.MC = ATT.marg.MC,
    ATT.marg.naive = ATT.marg.naive,
    ATT.cond.MC = ATT.cond.MC,
    ATT.cond.naive = ATT.cond.naive
  )
}

#run the MC replicates for variance estimation
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


# See whether CIs overlapped for the stratified estimates of the RR
logRR0 <- META.cond.X0[1]; se0 <- META.cond.X0[2]
logRR1 <- META.cond.X1[1]; se1 <- META.cond.X1[2]

ci0_lower <- logRR0 - 1.96 * se0
ci0_upper <- logRR0 + 1.96 * se0

ci1_lower <- logRR1 - 1.96 * se1
ci1_upper <- logRR1 + 1.96 * se1

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
setwd("<path>")
write.csv(results_df, file = outfile, row.names = FALSE)



