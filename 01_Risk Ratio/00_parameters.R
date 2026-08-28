################################################################################
# Program: 00_parameters.R
# Purpose: Specify the parameters for models in the risk ratio scenarios
# Programmer: Bonnie Shook-Sa
################################################################################

## user-specified arguments (common to all scenarios)
num.sims <- 2000 #number of simulations per scenario
num.reps <- 500 #for monte carlo procedure/variance estimation
num.part.TP <- 15000 #number of target population participants (for sims)
num.part.TP.truth <- 50000000  #number of target population participants (for determining truth empirically)

## Scenario-specific risk ratios
S1.RR<-0.9

S2.RR.X1<-0.85 #True risk ratio at 15 years, X=1
S2.RR.X0<-0.90 #True risk ratio at 15 years, X=0

S3.RR.X1<-0.75 #True risk ratio at 15 years, X=1
S3.RR.X0<-0.90 #True risk ratio at 15 years, X=0

S4.RR.U1<-0.85 #True risk ratio at 15 years, U=1
S4.RR.U0<-0.90 #True risk ratio at 15 years, U=0

S5.RR.U1<-0.75 #True risk ratio at 15 years, U=1
S5.RR.U0<-0.90 #True risk ratio at 15 years, U=0

#Target population parameters
p_X_TP<- 0.4
p_U_TP<- 0.30
p_A_TP_int<- 1.2
p_A_TP_X<- 0.6
p_A_TP_U<- -0.24
lp_Y0_TP_int<- log(0.16)


#Trial population parameters
X_min_trials<- 0.08 #Min value or p(X) in trials
X_max_trials<- 0.20 #Max value or p(X) in trials
pX_single_trial <- 0.20 #p(X) for single-trial setting
p_U_trials<- 0.08
lp_Y0_trials_int<- log(0.15)





