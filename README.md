# LDA_NTX-IGA
Survival analysis of iga and ntx patients.

## Structure
The `R` folder contains all relevant scripts for execution.
The necessary data is expected to be stored in the corresponding `data` folder.
The workload is spitted into several scripts which are combined/'sourced' in the `main.R` script.
Images and other data created during excecution in saved into `assets` folder.

## Content
Iga and ntx data is pre-processed in the `read_data.R` script. Patients younger than 18 years are dropped, levels of some columns are adapted to actual content (most of the time transformed into factors when appropriate).<br>
After pre-processing several descriptive statistics are computed, most commonly this is done by stating [5-number summaries](https://en.wikipedia.org/wiki/Five-number_summary) either as console output when running the script or in form of a boxplots.<br>
After pure descriptive statistics several survival analysis are conducted, each for both the event of graft loss ('Regime1') and patient death within follow up period ('Regime2') (see corresponding function in `functions.R` for event and censoring definition):<br>
First the Kaplan-Meier estimator is used to calculate survivor functions for iga and ntx patients. Afterwards a Log Rank test is used to assess differences in survival probabilities between iga patients with and without a proven recurrent biopsy and total ntx patients. <br>
Finally a Cox-Regression for iga patients is conducted where certain numerical covariables are additionally converted into groups. The Cox-regressions include one large regression and several univariate regressions. 
