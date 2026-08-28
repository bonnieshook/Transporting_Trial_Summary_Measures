################################################################################
# Program: 01_combined_bias_figure.R
# Purpose: Simulation study for transportability paper: bias figure for all scenarios (RR and HR) 
# Meta-analysis setting
# Programmer: Bonnie Shook-Sa
# Inputs: n/a
# Outputs: bias figure
################################################################################


library(tidyverse)
library(ggplot2)
library(cowplot)
library(ggimage)
library(gridExtra)
library(png)
library(grid)
library(patchwork)

theme_set(theme_bw(16))


#Generate plots separately for risk ratios and hazard ratios by strength and then panel them

############### Risk ratios #################################

#bring in true value for each scenario
setwd("<path>")

truth_list_mod <- lapply(1:5, function(i) {
  df <- read.csv(paste0("00_simulation_scen", i, "_moderate_truth_RR.csv"))
  df$x
})

truth_list_strong <- lapply(1:5, function(i) {
  df <- read.csv(paste0("00_simulation_scen", i, "_strong_truth_RR.csv"))
  df$x
})

#bring in simulation results for each scenario
Sc1.mod.RR<-as.data.frame(read.csv("01_simulation_results_scen1_RR_meta_mod.csv"))
Sc2.mod.RR<-as.data.frame(read.csv("02_simulation_results_scen2_RR_meta_mod.csv"))
Sc3.mod.RR<-as.data.frame(read.csv("03_simulation_results_scen3_RR_meta_mod.csv"))
Sc4.mod.RR<-as.data.frame(read.csv("04_simulation_results_scen4_RR_meta_mod.csv"))
Sc5.mod.RR<-as.data.frame(read.csv("05_simulation_results_scen5_RR_meta_mod.csv"))

Sc1.strong.RR<-as.data.frame(read.csv("01_simulation_results_scen1_RR_meta_strong.csv"))
Sc2.strong.RR<-as.data.frame(read.csv("02_simulation_results_scen2_RR_meta_strong.csv"))
Sc3.strong.RR<-as.data.frame(read.csv("03_simulation_results_scen3_RR_meta_strong.csv"))
Sc4.strong.RR<-as.data.frame(read.csv("04_simulation_results_scen4_RR_meta_strong.csv"))
Sc5.strong.RR<-as.data.frame(read.csv("05_simulation_results_scen5_RR_meta_strong.csv"))

#Calculate bias
calc_bias<- function(ATTest,truth,Scen,est){
  Scenario<-Scen
  Estimate<-est
  Bias<-100*(ATTest-truth)
  RelBias<-100*((ATTest-truth)/truth)
  Return<-cbind(Scenario,Estimate,Bias,RelBias,ATTest,truth)
}

#Run for both marginal and conditional estimators
prep_data <- function(dat,Scen,truth){
  Marg<-calc_bias(dat$ATT.est.marg,truth,Scen,'Marginal')
  Cond<-calc_bias(dat$ATT.est.cond,truth,Scen,'Conditional')
  combine<-rbind(Marg,Cond)
}

#Run for each scenario
Sc1.mod.res.RR<-prep_data(Sc1.mod.RR,1,truth_list_mod[[1]])
Sc2.mod.res.RR<-prep_data(Sc2.mod.RR,2,truth_list_mod[[2]])
Sc3.mod.res.RR<-prep_data(Sc3.mod.RR,3,truth_list_mod[[3]])
Sc4.mod.res.RR<-prep_data(Sc4.mod.RR,4,truth_list_mod[[4]])
Sc5.mod.res.RR<-prep_data(Sc5.mod.RR,5,truth_list_mod[[5]])

Sc1.strong.res.RR<-prep_data(Sc1.strong.RR,1,truth_list_strong[[1]])
Sc2.strong.res.RR<-prep_data(Sc2.strong.RR,2,truth_list_strong[[2]])
Sc3.strong.res.RR<-prep_data(Sc3.strong.RR,3,truth_list_strong[[3]])
Sc4.strong.res.RR<-prep_data(Sc4.strong.RR,4,truth_list_strong[[4]])
Sc5.strong.res.RR<-prep_data(Sc5.strong.RR,5,truth_list_strong[[5]])

#combine
all.RR.mod<-as.data.frame(rbind(Sc1.mod.res.RR,Sc2.mod.res.RR,Sc3.mod.res.RR,Sc4.mod.res.RR,Sc5.mod.res.RR))
all.RR.strong<-as.data.frame(rbind(Sc1.strong.res.RR,Sc2.strong.res.RR,Sc3.strong.res.RR,Sc4.strong.res.RR,Sc5.strong.res.RR))

all.RR.mod$Scenario2 <- factor(all.RR.mod$Scenario,
                           levels = 1:5,
                           labels = c("1A", "1B", "1C", "1D", "1E"))
all.RR.mod$Bias2<-as.numeric(all.RR.mod$Bias)
all.RR.mod$Estimate2<-factor(all.RR.mod$Estimate,levels=c("Marginal","Conditional"))

all.RR.strong$Scenario2 <- factor(all.RR.strong$Scenario,
                               levels = 1:5,
                               labels = c("1A", "1B", "1C", "1D", "1E"))
all.RR.strong$Bias2<-as.numeric(all.RR.strong$Bias)
all.RR.strong$Estimate2<-factor(all.RR.strong$Estimate,levels=c("Marginal","Conditional"))

#boxplots
max(all.RR.mod$Bias2)
min(all.RR.mod$Bias2)
sum(is.na(all.RR.mod$Bias2))

max(all.RR.strong$Bias2)
min(all.RR.strong$Bias2)
sum(is.na(all.RR.strong$Bias2))

bias.RR.mod<-ggplot(all.RR.mod,aes(x=Scenario2, y=Bias2, fill=Estimate2))+ scale_y_continuous(limits=c(-8,8),breaks=c(-8,-6,-4,-2,0,2,4,6,8))+geom_boxplot(outlier.shape = 1) + scale_fill_manual(values = c("#191DF7","#57B0FF"))+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.position="bottom",legend.title = element_blank())+xlab("")+ylab("Bias (x 100)")+geom_vline(xintercept=c(1.5,2.5,3.5,4.5), colour='light grey',lwd=1)+ geom_hline(yintercept=0.0, linetype="dashed", color = "black")
bias.RR.mod

bias.RR.strong<-ggplot(all.RR.strong,aes(x=Scenario2, y=Bias2, fill=Estimate2))+ scale_y_continuous(limits=c(-8,8),breaks=c(-8,-6,-4,-2,0,2,4,6,8))+geom_boxplot(outlier.shape = 1) + scale_fill_manual(values = c("#191DF7","#57B0FF"))+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.position="bottom",legend.title = element_blank())+xlab("")+ylab("")+geom_vline(xintercept=c(1.5,2.5,3.5,4.5), colour='light grey',lwd=1)+ geom_hline(yintercept=0.0, linetype="dashed", color = "black")
bias.RR.strong


############### Hazard ratios #################################

rm(list = setdiff(ls(), c("calc_bias", "prep_data", "bias.RR.mod", "bias.RR.strong")))
gc()

#bring in true value for each scenario
setwd("<path>")

truth_list_mod <- lapply(1:5, function(i) {
  df <- read.csv(paste0("00_simulation_scen", i, "_moderate_truth_HR.csv"))
  df$x
})

truth_list_strong <- lapply(1:5, function(i) {
  df <- read.csv(paste0("00_simulation_scen", i, "_strong_truth_HR.csv"))
  df$x
})

#bring in simulation results for each scenario
Sc1.mod.HR<-as.data.frame(read.csv("01_simulation_results_scen1_HR_meta_mod.csv"))
Sc2.mod.HR<-as.data.frame(read.csv("02_simulation_results_scen2_HR_meta_mod.csv"))
Sc3.mod.HR<-as.data.frame(read.csv("03_simulation_results_scen3_HR_meta_mod.csv"))
Sc4.mod.HR<-as.data.frame(read.csv("04_simulation_results_scen4_HR_meta_mod.csv"))
Sc5.mod.HR<-as.data.frame(read.csv("05_simulation_results_scen5_HR_meta_mod.csv"))

Sc1.strong.HR<-as.data.frame(read.csv("01_simulation_results_scen1_HR_meta_strong.csv"))
Sc2.strong.HR<-as.data.frame(read.csv("02_simulation_results_scen2_HR_meta_strong.csv"))
Sc3.strong.HR<-as.data.frame(read.csv("03_simulation_results_scen3_HR_meta_strong.csv"))
Sc4.strong.HR<-as.data.frame(read.csv("04_simulation_results_scen4_HR_meta_strong.csv"))
Sc5.strong.HR<-as.data.frame(read.csv("05_simulation_results_scen5_HR_meta_strong.csv"))

#Run for each scenario
Sc1.mod.res.HR<-prep_data(Sc1.mod.HR,1,truth_list_mod[[1]])
Sc2.mod.res.HR<-prep_data(Sc2.mod.HR,2,truth_list_mod[[2]])
Sc3.mod.res.HR<-prep_data(Sc3.mod.HR,3,truth_list_mod[[3]])
Sc4.mod.res.HR<-prep_data(Sc4.mod.HR,4,truth_list_mod[[4]])
Sc5.mod.res.HR<-prep_data(Sc5.mod.HR,5,truth_list_mod[[5]])

Sc1.strong.res.HR<-prep_data(Sc1.strong.HR,1,truth_list_strong[[1]])
Sc2.strong.res.HR<-prep_data(Sc2.strong.HR,2,truth_list_strong[[2]])
Sc3.strong.res.HR<-prep_data(Sc3.strong.HR,3,truth_list_strong[[3]])
Sc4.strong.res.HR<-prep_data(Sc4.strong.HR,4,truth_list_strong[[4]])
Sc5.strong.res.HR<-prep_data(Sc5.strong.HR,5,truth_list_strong[[5]])

#combine
all.HR.mod<-as.data.frame(rbind(Sc1.mod.res.HR,Sc2.mod.res.HR,Sc3.mod.res.HR,Sc4.mod.res.HR,Sc5.mod.res.HR))
all.HR.strong<-as.data.frame(rbind(Sc1.strong.res.HR,Sc2.strong.res.HR,Sc3.strong.res.HR,Sc4.strong.res.HR,Sc5.strong.res.HR))

all.HR.mod$Scenario2 <- factor(all.HR.mod$Scenario,
                               levels = 1:5,
                               labels = c("2A", "2B", "2C", "2D", "2E"))
all.HR.mod$Bias2<-as.numeric(all.HR.mod$Bias)
all.HR.mod$Estimate2<-factor(all.HR.mod$Estimate,levels=c("Marginal","Conditional"))

all.HR.strong$Scenario2 <- factor(all.HR.strong$Scenario,
                                  levels = 1:5,
                                  labels = c("2A", "2B", "2C", "2D", "2E"))
all.HR.strong$Bias2<-as.numeric(all.HR.strong$Bias)
all.HR.strong$Estimate2<-factor(all.HR.strong$Estimate,levels=c("Marginal","Conditional"))

#boxplots
max(all.HR.mod$Bias2)
min(all.HR.mod$Bias2)
sum(is.na(all.HR.mod$Bias2))

max(all.HR.strong$Bias2)
min(all.HR.strong$Bias2)
sum(is.na(all.HR.strong$Bias2))

bias.HR.mod<-ggplot(all.HR.mod,aes(x=Scenario2, y=Bias2, fill=Estimate2))+ scale_y_continuous(limits=c(-8,8),breaks=c(-8,-6,-4,-2,0,2,4,6,8))+geom_boxplot(outlier.shape = 1) + scale_fill_manual(values = c("#191DF7","#57B0FF"))+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.position="bottom",legend.title = element_blank())+xlab("")+ylab("Bias (x 100)")+geom_vline(xintercept=c(1.5,2.5,3.5,4.5), colour='light grey',lwd=1)+ geom_hline(yintercept=0.0, linetype="dashed", color = "black")
bias.HR.mod

bias.HR.strong<-ggplot(all.HR.strong,aes(x=Scenario2, y=Bias2, fill=Estimate2))+ scale_y_continuous(limits=c(-8,8),breaks=c(-8,-6,-4,-2,0,2,4,6,8))+geom_boxplot(outlier.shape = 1) + scale_fill_manual(values = c("#191DF7","#57B0FF"))+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.position="bottom",legend.title = element_blank())+xlab("")+ylab("")+geom_vline(xintercept=c(1.5,2.5,3.5,4.5), colour='light grey',lwd=1)+ geom_hline(yintercept=0.0, linetype="dashed", color = "black")
bias.HR.strong


################### Panel and output

p1 <- bias.RR.mod +
  labs(title = NULL) +
  theme(legend.position = "none")

p2 <- bias.RR.strong +
  labs(title = NULL, y = NULL) +
  theme(legend.position = "none")

p3 <- bias.HR.mod +
  labs(title = NULL) +
  theme(legend.position = "none")

p4 <- bias.HR.strong +
  labs(title = NULL, y = NULL) +
  theme(legend.position = "none")


# Extract one common legend

legend <- get_legend(
  bias.RR.mod +
    theme(
      legend.position = "bottom",
      legend.box.margin = margin(0, 0, 0, 0)
    )
)


# Create column headings
blank_heading <- ggdraw()

mod_heading <- ggdraw() +
  draw_label(
    "Moderate Prognostic Factors",
    fontface = "bold",
    size = 14,
    x = 0.5,
    y = 0.5
  )

strong_heading <- ggdraw() +
  draw_label(
    "Strong Prognostic Factors",
    fontface = "bold",
    size = 14,
    x = 0.5,
    y = 0.5
  )


# Create row headings
rr_heading <- ggdraw() +
  draw_label(
    "Risk Ratio",
    angle = 90,
    fontface = "bold",
    size = 14,
    x = 0.5,
    y = 0.5
  )

hr_heading <- ggdraw() +
  draw_label(
    "Hazard Ratio",
    angle = 90,
    fontface = "bold",
    size = 14,
    x = 0.5,
    y = 0.5
  )


# Create the top (heading) row
top_row <- plot_grid(
  blank_heading,
  mod_heading,
  strong_heading,
  nrow = 1,
  rel_widths = c(0.06, 1, 1)
)


# Risk Ratio row
RR_row <- plot_grid(
  rr_heading,
  p1,
  p2,
  nrow = 1,
  rel_widths = c(0.06, 1, 1),
  align = "h",
  axis = "tb"
)


# Hazard Ratio row
HR_row <- plot_grid(
  hr_heading,
  p3,
  p4,
  nrow = 1,
  rel_widths = c(0.06, 1, 1),
  align = "h",
  axis = "tb"
)


# Put everything together
fig <- plot_grid(
  top_row,
  RR_row,
  HR_row,
  legend,
  ncol = 1,
  rel_heights = c(0.07, 1, 1, 0.10)
)

fig

ggsave(
  "Bias.Figure_17.08.26.pdf",
  fig,
  width = 12,
  height = 8,
  units = "in"
)