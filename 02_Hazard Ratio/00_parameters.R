################################################################################
# Program: 00_parameters.R
# Purpose: Specify the parameters for models in the hazard ratio scenarios
# Programmer: Bonnie Shook-Sa
################################################################################

## user-specified arguments (common to all scenarios)
num.sims <- 2000 #number of simulations per scenario
num.reps <- 500 #for monte carlo procedure/variance estimation
num.part.TP <- 15000 #number of target population participants (for sims)
num.part.TP.truth <- 50000000  #number of target population participants (for determining truth empirically)

## Scenario-specific parameters for hazard ratios
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


censor<-0.92

#Target population parameters
p_X_TP<- 0.4
p_U_TP<- 0.30
p_A_TP_int<- 1.2
p_A_TP_X<- 0.6
p_A_TP_U<- -0.24
lp_Y0_TP<- 0.03

#Trial population parameters
X_min_trials<- 0.08 #Min value or p(X) in trials
X_max_trials<- 0.20 #Max value or p(X) in trials
pX_single_trial <- 0.20 #p(X) for single-trial setting
p_U_trials<- 0.08
lp_Y0_trials<- 0.0267





