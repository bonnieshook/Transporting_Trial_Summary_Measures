################################################################################
# Program: 02_run_all_scenarios.R
# Purpose: Simulation study for transportability paper (hazard ratio scenarios) - single trial
# Programmer: Bonnie Shook-Sa
# Inputs: n/a
# Outputs:  output files for each scenario
################################################################################

library(boot)
library(tibble)
library(tidyverse)
library(survival)
library(meta)

program_dir <- "<path1>"
results_dir <- "<path2>"

run_scenario <- function(scenario, n_per_trial, outfile) {
  Scenario <- scenario
  num.part.Trials <- n_per_trial
  source(file.path(program_dir, "01_sims.R"), local = TRUE)
}

## Small n
run_scenario(1, 650,  "01_simulation_results_scen1_HR_ST_smalln.csv")
run_scenario(2, 650,  "02_simulation_results_scen2_HR_ST_smalln.csv")
run_scenario(3, 650,  "03_simulation_results_scen3_HR_ST_smalln.csv")
run_scenario(4, 650,  "04_simulation_results_scen4_HR_ST_smalln.csv")
run_scenario(5, 650,  "05_simulation_results_scen5_HR_ST_smalln.csv")

## Medium n
run_scenario(1, 1500, "01_simulation_results_scen1_HR_ST.csv")
run_scenario(2, 1500, "02_simulation_results_scen2_HR_ST.csv")
run_scenario(3, 1500, "03_simulation_results_scen3_HR_ST.csv")
run_scenario(4, 1500, "04_simulation_results_scen4_HR_ST.csv")
run_scenario(5, 1500, "05_simulation_results_scen5_HR_ST.csv")

## Large n
run_scenario(1, 5000, "01_simulation_results_scen1_HR_ST_largen.csv")
run_scenario(2, 5000, "02_simulation_results_scen2_HR_ST_largen.csv")
run_scenario(3, 5000, "03_simulation_results_scen3_HR_ST_largen.csv")
run_scenario(4, 5000, "04_simulation_results_scen4_HR_ST_largen.csv")
run_scenario(5, 5000, "05_simulation_results_scen5_HR_ST_largen.csv")

