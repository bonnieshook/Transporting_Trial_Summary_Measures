# Transporting summary measures of relative effects from randomised trials to the treated patient population: an application to breast cancer endocrine therapy

### Bonnie E. Shook-Sa, Paul N. Zivich, Carolyn Taylor, David Dodwell, Jake Probert, Sarah C Darby, and Paul McGale

**Citation**: Shook-Sa BE, Zivich PN, Taylor C, Dodwell D, Probert J, Darby SC, McGale P. "Transporting summary measures of relative effects from randomised trials to the treated patient population: an application to breast cancer endocrine therapy." 
--------------------------------

## Abstract

Randomised trials often report relative treatment effects, such as risk ratios and hazard ratios, in trial populations. Clinical decision-making, however, often benefits from estimates of absolute treatment effects in the population eligible for treatment. Trial participants may not represent this population well, and restrictions on access to individual-level trial data can further complicate absolute effect estimation. Routine care data are often representative of the target population but may be subject to uncontrolled confounding. We consider estimation of the average treatment effect on the treated (ATT), an absolute measure, by combining a representative sample of treated routine care patients with summary measures (i.e., estimated risk or hazard ratios) from either a randomised trial or a meta-analysis of trials. Under marginal or conditional transportability assumptions, the ATT is shown to be identifiable. Plug-in estimators of the ATT are presented along with a variance estimator. Simulation studies are used to assess finite-sample performance of the estimators. The proposed methods are applied to estimate the ATT of endocrine therapy on 15-year breast cancer mortality using results from a meta-analysis of randomised trials and England’s breast cancer register. The results suggest protective effects across subgroups, with larger absolute benefits for patients with higher-risk tumours.
--------------------------------

## File Manifesto

These programs implement the simulation study described in Shook-Sa et al, "Transporting summary measures of relative effects from randomised trials to the treated patient population: an application to breast cancer endocrine therapy" in R

Developed by: Bonnie Shook-Sa

There are separate folders for the simulations conducted for transporting the risk ratio (01_Risk Ratio) and the hazard ratio (02_Hazard Ratio). Within each folder, there are sub-folders for the meta-analysis setting (01_Meta_Analysis) and the single-trial setting (02_Single_Trial). Programs are numbered sequentially within each sub-folder. 

Descriptions of each program are below:

* 00_truth_all.R: This program is contained in both of the 01_Meta_Analysis sub-folders. It calculates the true ATT in the patient population empirically for each scenario based on 50 million realisations of the data generating mechanism. Note the truth is the same for the single-trial settings as the meta-analysis settings.
                  
* 01_sims.R: This program contains code to run the simulation study for a given scenario under user-specified parameters of interest. It is tailored to the risk ratio or hazard ratio setting, as well as the meta-analysis or single-trial setting, so separate versions of this program are available in each of the four sub-folders. 
                  
* 02_run_all_scenarios.R: This program calls the 01_sims.R program for each scenario and/or sample size of interest and specifies output file names where the results are saved.

* 03_combine_sim_table.R: This program summarises the results of the simulation study across scenarios and generates an output table with summary measures. In the single-trial sub-folders, there are separate versions of this program for each sample size of interest. Each program creates the output table corresponding to that sample size.
                  
There is a third folder in the main directory called 03_combined_figure. This program combines bias results from the risk ratio and hazard ratio meta-analysis scenarios and generates Figure 1 from the main text.


Note in the hazard ratio programs, two of the parameter names in the DGM differ from the labels in the manuscript as follows:
* beta in the programs is eta from the manuscript
* delta in the programs is phi from the manuscript
