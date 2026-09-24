# Transporting summary measures of relative effects from randomised trials to the treated patient population: an application to breast cancer endocrine therapy

### Bonnie E. Shook-Sa, Paul N. Zivich, Carolyn Taylor, David Dodwell, Jake Probert, Sarah C Darby, and Paul McGale

**Citation**: Shook-Sa BE, Zivich PN, Taylor C, Dodwell D, Probert J, Darby SC, McGale P. "Transporting summary measures of relative effects from randomised trials to the treated patient population: an application to breast cancer endocrine therapy." https://arxiv.org/abs/2609.24305
--------------------------------

## Abstract

Randomised trials often report relative treatment effects, such as risk ratios and hazard ratios, for trial populations. Clinical decision-making, however, often benefits from estimates of absolute treatment effects in the population eligible for treatment. Trial participants may not represent this target population well, and restrictions on access to individual participant trial data can further complicate absolute effect estimation. Routine care data are often representative of the target population but may be subject to uncontrolled confounding. We consider estimation of the average treatment effect on the treated (ATT), an absolute measure, by combining a representative sample of treated routine care patients with summary measures (i.e., estimated risk or hazard ratios) from either a randomised trial or a meta-analysis of trials. Under marginal or conditional transportability assumptions, the ATT is shown to be identifiable. The implications of collapsibility of the effect measure on transportability are discussed, and plug-in estimators of the ATT are presented. Simulation studies are used to assess finite sample performance of the estimators in a range of settings. The proposed methods are applied to estimate the ATT of endocrine therapy on 15-year breast cancer mortality using results from a meta-analysis of randomised trials and England's National Disease Registration Service. 
--------------------------------

## File Manifesto

These programs implement the simulation study described in Shook-Sa et al., "Transporting summary measures of relative effects from randomised trials to the treated patient population: an application to breast cancer endocrine therapy" in R.

Developed by: Bonnie Shook-Sa

There are separate folders for the simulations conducted for transporting the risk ratio (01_Risk Ratio) and the hazard ratio (02_Hazard Ratio). Programs are numbered sequentially within each sub-folder. 

Descriptions of each program are below:

* 00_parameters.R: This program contains the parameter values, sample sizes, number of simulations, and number of Monte Carlo replicates for the data-generating mechanisms described in the paper.

* 01_truth_all.R: This program calculates the true ATT in the patient population empirically for each scenario based on 50 million realisations of the data-generating mechanism. Note that the truth is the same for the single-trial settings as the meta-analysis settings.
                  
* 02_sims_meta.R: This program contains code to run the simulation study for the meta-analysis setting for a given simulation scenario. 

* 02_sims_single_trial.R: This program contains code to run the simulation study for the single-trial setting for a given scenario under user-specified parameters of interest. 
                  
* 03_run_all_scenarios.R: This program calls the 01_truth_all.R, 02_sims_meta.R, and 02_sims_single_trial.R programs for each scenario to calculate the truth and run the simulations. Moderate and strong prognostic parameters are specified within this program. Output file names are also specified.

* 04_combine_sim_table.R and 05_combine_sim_table_ST_nX.R: These programs summarise the results of the simulation study across scenarios and generate an output table with summary measures. The 04_ program is for the meta-analysis setting, and the 05_ programs are for the (sample-size specific) single-trial settings. 
                  
There is a third folder in the main directory called 03_combined_figure. The program in this folder combines bias results from the risk ratio and hazard ratio meta-analysis scenarios and generates Figure 1 from the main text.

Note that in the hazard ratio programs, two of the parameter names in the DGM differ from the labels in the manuscript as follows:
* beta in the programs is eta from the manuscript
* delta in the programs is phi from the manuscript
