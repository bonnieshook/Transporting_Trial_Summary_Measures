################################################################################
# Program: 02_run_all_scenarios.R
# Purpose: Simulation study for transportability paper (hazard ratio scenarios)
# Programmer: Bonnie Shook-Sa
# Inputs: n/a
# Outputs:  output files for each scenario
################################################################################

library(boot)
library(tibble)
library(tidyverse)
library(survival)
library(meta)

## Specify scenario, output file, run, and clear contents. Repeat for each scenario

Scenario<-1
outfile <- "01_simulation_results_scen1_HR.csv"
setwd("<name of directory>")
source("01_sims.R")
rm(list = ls())

Scenario<-2
outfile <- "02_simulation_results_scen2_HR.csv"
setwd("<name of directory>")
source("01_sims.R")
rm(list = ls())

Scenario<-3
outfile <- "03_simulation_results_scen3_HR.csv"
setwd("<name of directory>")
source("01_sims.R")
rm(list = ls())

Scenario<-4
outfile <- "04_simulation_results_scen4_HR.csv"
setwd("<name of directory>")
source("01_sims.R")
rm(list = ls())

Scenario<-5
outfile <- "05_simulation_results_scen5_HR.csv"
setwd("<name of directory>")
source("01_sims.R")
rm(list = ls())


