################################################################################
# Program: 03_run_all_scenarios.R
# Purpose: Simulation study for transportability paper (risk ratio scenarios)
# Programmer: Bonnie Shook-Sa
# Inputs: n/a
# Outputs: output files for each scenario
################################################################################

library(boot)
library(tibble)
library(tidyverse)
library(survival)
library(meta)

###First calculate the truth for moderate and strong prognostic factor scenarios
truth <- function(strength) {
  program_dir <- "<path>"
  source(file.path(program_dir, "00_parameters.R"))
  if (strength == "moderate") {

    lp_Y0_TP_X<- log(1.8)
    lp_Y0_TP_U<- log(1.6)
    lp_Y0_trials_X<- log(1.8)
    lp_Y0_trials_U<- log(1.6)
    
  } else if (strength == "strong") {
    lp_Y0_TP_X<- log(2.5)
    lp_Y0_TP_U<- log(2.2)
    lp_Y0_trials_X<- log(2.5)
    lp_Y0_trials_U<- log(2.2)
  }
  source(file.path(program_dir, "01_truth_all.R"), local = TRUE)
}

truth("moderate")
truth("strong")



#Function to run the simulations for a given scenario - meta analysis setting
run_scenario_meta <- function(scenario, n_per_trial, strength, outfile) {
  program_dir <- "<path>"
  source(file.path(program_dir, "00_parameters.R"))
  Scenario <- scenario
  num.part.Trials <- n_per_trial
  num.trials <- 15
  if (strength == "moderate") {
    
    lp_Y0_TP_X<- log(1.8)
    lp_Y0_TP_U<- log(1.6)
    lp_Y0_trials_X<- log(1.8)
    lp_Y0_trials_U<- log(1.6)
    
  } else if (strength == "strong") {
    lp_Y0_TP_X<- log(2.5)
    lp_Y0_TP_U<- log(2.2)
    lp_Y0_trials_X<- log(2.5)
    lp_Y0_trials_U<- log(2.2)
  }
  source(file.path(program_dir, "02_sims_meta.R"), local = TRUE)
  rm(list = ls())
}

#Function to run the simulations for a given scenario - single trial setting
run_scenario_single_trial <- function(scenario, n_per_trial, strength, outfile) {
  program_dir <- "<path>"
  source(file.path(program_dir, "00_parameters.R"))
  Scenario <- scenario
  num.part.Trials <- n_per_trial
  num.trials <- 1
  if (strength == "moderate") {
    
    lp_Y0_TP_X<- log(1.8)
    lp_Y0_TP_U<- log(1.6)
    lp_Y0_trials_X<- log(1.8)
    lp_Y0_trials_U<- log(1.6)
    
  } else if (strength == "strong") {
    lp_Y0_TP_X<- log(2.5)
    lp_Y0_TP_U<- log(2.2)
    lp_Y0_trials_X<- log(2.5)
    lp_Y0_trials_U<- log(2.2)
  }
  source(file.path(program_dir, "02_sims_single_trial.R"), local = TRUE)
  rm(list = ls())
}

## Meta analysis
run_scenario_meta(1, 650, "moderate", "01_simulation_results_scen1_RR_meta_mod.csv")
run_scenario_meta(2, 650, "moderate", "02_simulation_results_scen2_RR_meta_mod.csv")
run_scenario_meta(3, 650, "moderate", "03_simulation_results_scen3_RR_meta_mod.csv")
run_scenario_meta(4, 650, "moderate", "04_simulation_results_scen4_RR_meta_mod.csv")
run_scenario_meta(5, 650, "moderate", "05_simulation_results_scen5_RR_meta_mod.csv")

run_scenario_meta(1, 650, "strong", "01_simulation_results_scen1_RR_meta_strong.csv")
run_scenario_meta(2, 650, "strong", "02_simulation_results_scen2_RR_meta_strong.csv")
run_scenario_meta(3, 650, "strong", "03_simulation_results_scen3_RR_meta_strong.csv")
run_scenario_meta(4, 650, "strong", "04_simulation_results_scen4_RR_meta_strong.csv")
run_scenario_meta(5, 650, "strong", "05_simulation_results_scen5_RR_meta_strong.csv")


## Small n
run_scenario_single_trial(1, 650, "moderate", "01_simulation_results_scen1_RR_ST_smalln_mod.csv")
run_scenario_single_trial(2, 650, "moderate", "02_simulation_results_scen2_RR_ST_smalln_mod.csv")
run_scenario_single_trial(3, 650, "moderate", "03_simulation_results_scen3_RR_ST_smalln_mod.csv")
run_scenario_single_trial(4, 650, "moderate", "04_simulation_results_scen4_RR_ST_smalln_mod.csv")
run_scenario_single_trial(5, 650, "moderate", "05_simulation_results_scen5_RR_ST_smalln_mod.csv")

run_scenario_single_trial(1, 650, "strong", "01_simulation_results_scen1_RR_ST_smalln_strong.csv")
run_scenario_single_trial(2, 650, "strong", "02_simulation_results_scen2_RR_ST_smalln_strong.csv")
run_scenario_single_trial(3, 650, "strong", "03_simulation_results_scen3_RR_ST_smalln_strong.csv")
run_scenario_single_trial(4, 650, "strong", "04_simulation_results_scen4_RR_ST_smalln_strong.csv")
run_scenario_single_trial(5, 650, "strong", "05_simulation_results_scen5_RR_ST_smalln_strong.csv")

## Medium n
run_scenario_single_trial(1, 1500, "moderate", "01_simulation_results_scen1_RR_ST_medn_mod.csv")
run_scenario_single_trial(2, 1500, "moderate","02_simulation_results_scen2_RR_ST_medn_mod.csv")
run_scenario_single_trial(3, 1500, "moderate","03_simulation_results_scen3_RR_ST_medn_mod.csv")
run_scenario_single_trial(4, 1500, "moderate","04_simulation_results_scen4_RR_ST_medn_mod.csv")
run_scenario_single_trial(5, 1500, "moderate","05_simulation_results_scen5_RR_ST_medn_mod.csv")

run_scenario_single_trial(1, 1500, "strong", "01_simulation_results_scen1_RR_ST_medn_strong.csv")
run_scenario_single_trial(2, 1500, "strong","02_simulation_results_scen2_RR_ST_medn_strong.csv")
run_scenario_single_trial(3, 1500, "strong","03_simulation_results_scen3_RR_ST_medn_strong.csv")
run_scenario_single_trial(4, 1500, "strong","04_simulation_results_scen4_RR_ST_medn_strong.csv")
run_scenario_single_trial(5, 1500, "strong","05_simulation_results_scen5_RR_ST_medn_strong.csv")

## Large n
run_scenario_single_trial(1, 5000, "moderate", "01_simulation_results_scen1_RR_ST_largen_mod.csv")
run_scenario_single_trial(2, 5000, "moderate","02_simulation_results_scen2_RR_ST_largen_mod.csv")
run_scenario_single_trial(3, 5000, "moderate","03_simulation_results_scen3_RR_ST_largen_mod.csv")
run_scenario_single_trial(4, 5000, "moderate", "04_simulation_results_scen4_RR_ST_largen_mod.csv")
run_scenario_single_trial(5, 5000, "moderate","05_simulation_results_scen5_RR_ST_largen_mod.csv")

run_scenario_single_trial(1, 5000, "strong", "01_simulation_results_scen1_RR_ST_largen_strong.csv")
run_scenario_single_trial(2, 5000, "strong","02_simulation_results_scen2_RR_ST_largen_strong.csv")
run_scenario_single_trial(3, 5000, "strong","03_simulation_results_scen3_RR_ST_largen_strong.csv")
run_scenario_single_trial(4, 5000, "strong", "04_simulation_results_scen4_RR_ST_largen_strong.csv")
run_scenario_single_trial(5, 5000, "strong","05_simulation_results_scen5_RR_ST_largen_strong.csv")



