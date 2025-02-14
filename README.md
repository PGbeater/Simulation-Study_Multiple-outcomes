# Simulation-Study_Multiple-outcomes
Propensity score method in multiple outcome settings: A simulation study
1. In the setups folder: input parameters, generate the association pattern (between covariates and outcomes), and generate the data using the pre-made functions in the function.R (Junjie Wu Feb 2025 branch).
2. In the Function folder:
   Function.R includes the Association_Function, the Simulation_Function, and the Form_Functions.

   Check.R is to check the distribution of  the association patterns.

3. In the Plots folder:
   geom_flat_violin.R is the script to make half/flat violin plot.
   
   Other boxplot_and_violin.R is to make plots of the combination of boxplot and violin plot comparing the 10% model, 25% model, and generic model under different scenarios (different Prevalence of treatment & Incidence of outcomes).
   
