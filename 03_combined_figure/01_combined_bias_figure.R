################################################################################
# Program: 01_combined_bias_figure.R
# Purpose: Simulation study for transportability paper: bias figure for all meta-analysis scenarios (RR and HR) 
# Programmer: Bonnie Shook-Sa
# Inputs: n/a
# Outputs: bias figure
################################################################################


library(tidyverse)
library(ggplot2)
theme_set(theme_bw(16))
library(RColorBrewer)
library(ggplot2)
library(ggimage)
library(gridExtra)
library(png)
library(grid)
library(lattice)
library(boot)
library(ggplot2)
library(scales)
library(cowplot)
library(ggimage)
library(gridExtra)
library(png)
library(grid)
library(lattice)
library(forestplot)
library(patchwork)


#This program generate plots separately for risk ratios and hazard ratios and then panels them

############### Risk ratios #################################

#bring in true value for each scenario
setwd("<name of directory>")

truth_list <- lapply(1:5, function(i) {
  df <- read.csv(paste0("00_simulation_scen", i, "_truth_RR.csv"))
  df$x
})


#bring in results
Sc1.RR<-as.data.frame(read.csv("01_simulation_results_scen1_RR.csv"))
Sc2.RR<-as.data.frame(read.csv("02_simulation_results_scen2_RR.csv"))
Sc3.RR<-as.data.frame(read.csv("03_simulation_results_scen3_RR.csv"))
Sc4.RR<-as.data.frame(read.csv("04_simulation_results_scen4_RR.csv"))
Sc5.RR<-as.data.frame(read.csv("05_simulation_results_scen5_RR.csv"))

#Calculate bias
calc_bias<- function(ATTest,truth,Scen,est){
  Scenario<-Scen
  Estimate<-est
  Bias<-100*(ATTest-truth)
  RelBias<-100*((ATTest-truth)/truth)
  Return<-cbind(Scenario,Estimate,Bias,RelBias,ATTest,truth)
}

#Run for both marginal and conditional estimators
prep_data <- function(dat,Scen){
  Marg<-calc_bias(dat$ATT.est.marg,truth_list[[Scen]],Scen,'Marginal')
  Cond<-calc_bias(dat$ATT.est.cond,truth_list[[Scen]],Scen,'Conditional')
  combine<-rbind(Marg,Cond)
}

#Run for all scenarios
Sc1.RR<-prep_data(Sc1.RR,1)
Sc2.RR<-prep_data(Sc2.RR,2)
Sc3.RR<-prep_data(Sc3.RR,3)
Sc4.RR<-prep_data(Sc4.RR,4)
Sc5.RR<-prep_data(Sc5.RR,5)

#combine
all.RR<-as.data.frame(rbind(Sc1.RR,Sc2.RR,Sc3.RR,Sc4.RR,Sc5.RR))

all.RR$Scenario2 <- factor(all.RR$Scenario,
                           levels = 1:5,
                           labels = c("1A", "1B", "1C", "1D", "1E"))
all.RR$Bias2<-as.numeric(all.RR$Bias)
all.RR$Estimate2<-factor(all.RR$Estimate,levels=c("Marginal","Conditional"))

#boxplot
max(all.RR$Bias2)
min(all.RR$Bias2)
sum(is.na(all.RR$Bias2))

bias.RR<-ggplot(all.RR,aes(x=Scenario2, y=Bias2, fill=Estimate2))+ scale_y_continuous(limits=c(-8,8),breaks=c(-8,-6,-4,-2,0,2,4,6,8))+geom_boxplot(outlier.shape = 1) + scale_fill_manual(values = c("#191DF7","#57B0FF"))+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.position="bottom",legend.title = element_blank())+xlab("Risk Ratio Scenarios")+ylab("Bias (x 100)")+geom_vline(xintercept=c(1.5,2.5,3.5,4.5), colour='light grey',lwd=1)+ geom_hline(yintercept=0.0, linetype="dashed", color = "black")
bias.RR


############### Hazard ratios #################################

rm(list = setdiff(ls(), c("calc_bias", "prep_data", "bias.RR")))
gc()


#bring in true value for each scenario
setwd("<name of directory>")

truth_list <- lapply(1:5, function(i) {
  df <- read.csv(paste0("00_simulation_scen", i, "_truth_HR.csv"))
  df$x
})


#bring in results
Sc1.HR<-as.data.frame(read.csv("01_simulation_results_scen1_HR.csv"))
Sc2.HR<-as.data.frame(read.csv("02_simulation_results_scen2_HR.csv"))
Sc3.HR<-as.data.frame(read.csv("03_simulation_results_scen3_HR.csv"))
Sc4.HR<-as.data.frame(read.csv("04_simulation_results_scen4_HR.csv"))
Sc5.HR<-as.data.frame(read.csv("05_simulation_results_scen5_HR.csv"))

#Calculate bias
calc_bias<- function(ATTest,truth,Scen,est){
  Scenario<-Scen
  Estimate<-est
  Bias<-100*(ATTest-truth)
  RelBias<-100*((ATTest-truth)/truth)
  Return<-cbind(Scenario,Estimate,Bias,RelBias,ATTest,truth)
}

#Run for both marginal and conditional estimators
prep_data <- function(dat,Scen){
  Marg<-calc_bias(dat$ATT.est.marg,truth_list[[Scen]],Scen,'Marginal')
  Cond<-calc_bias(dat$ATT.est.cond,truth_list[[Scen]],Scen,'Conditional')
  combine<-rbind(Marg,Cond)
}

#Run for all scenarios
Sc1.HR<-prep_data(Sc1.HR,1)
Sc2.HR<-prep_data(Sc2.HR,2)
Sc3.HR<-prep_data(Sc3.HR,3)
Sc4.HR<-prep_data(Sc4.HR,4)
Sc5.HR<-prep_data(Sc5.HR,5)

all.HR<-as.data.frame(rbind(Sc1.HR,Sc2.HR,Sc3.HR,Sc4.HR,Sc5.HR))

all.HR$Scenario2 <- factor(all.HR$Scenario,
                           levels = 1:5,
                           labels = c("2A", "2B", "2C", "2D", "2E"))
all.HR$Bias2<-as.numeric(all.HR$Bias)
all.HR$Estimate2<-factor(all.HR$Estimate,levels=c("Marginal","Conditional"))

#boxplot
max(all.HR$Bias2)
min(all.HR$Bias2)
sum(is.na(all.HR$Bias2))

bias.HR<-ggplot(all.HR,aes(x=Scenario2, y=Bias2, fill=Estimate2))+ scale_y_continuous(limits=c(-8,8),breaks=c(-8,-6,-4,-2,0,2,4,6,8))+geom_boxplot(outlier.shape = 1) + scale_fill_manual(values = c("#191DF7","#57B0FF"))+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.position="bottom",legend.title = element_blank())+xlab("Hazard Ratio Scenarios")+ylab(" ")+geom_vline(xintercept=c(1.5,2.5,3.5,4.5), colour='light grey',lwd=1)+ geom_hline(yintercept=0.0, linetype="dashed", color = "black")
bias.HR


################### Panel and output

fig1 <- bias.RR + bias.HR +
  plot_layout(ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")

fig1

ggsave(filename = "bias_plot_12.06.26.jpeg", plot = fig1,
       width = 11, height = 8.5, units = "in", dpi=800)





