Association between sleep–wake cycle profiles and incident cardiovascular disease in older adults: results from the UK Biobank and Whitehall II accelerometer sub-studies
-
Overview
-
This repository contains the code used to examine the association between a holistic measure of the sleep–wake cycle (SWC) and the risk of incident cardiovascular disease (CVD) among adults aged 60 years and older, using data from two independent cohort studies: UK Biobank (UKB) and Whitehall II (WII) accelerometer sub-studies.

The scripts are organised to allow analyses to be run sequentially, so that outputs generated in earlier steps are used in subsequent analyses.

Project Structure
-
More details on each analytical step are provided in the following sections.

Step 1: Data Preparation
- 

Loading pre-processed datasets for the UKB and WII accelerometer sub-studies. (01. Data importation)
Preparation and harmonisation of sociodemographic, behavioural, general health-related and cardiometabolic factors. (02. Covariates)
Construction of the final analytical samples for each cohort. (02. Covariates)

Step 2: Exploratory analysis 
- 
Description of participant characteristics and sleep–wake cycle profile (Table 1, figure 1 and figure S1 of the manuscript). (0.3 Descriptive analysis)

Step 3: Statistical analysis 
- 
Main analysis: 
Analysis on the association of SWC profiles with incident CVD using Cox proportional hazard models with age as time-scale (Table 2 of the manuscript). (04. Cox proportional hazard regression (CVD))
Anaysis testing interactions of the SWC profiles with age (<70, ≥70 years), sex (female, male), and BMI (<25, 25-30, ≥30 kg/m²) on the risk of CVD event. (05. Testing interactions)
Sensitivity analysis:  
Analysis changing the reference group from SWC profile 3 to profiles 1 and 6 (Table 3 of the manuscript). (06. Cox proportional hazard regression (CVD) profiles 1 & 6 as the reference group)
Analysis excluding fatal CVD events, to test whether associations were driven by fatal events (Table S4 of the manuscript). (07. Cox proportional hazard regression (non-fatal CVD))
Secondary analysis:
Analysis on the association of SWC profiles with coronary heart disease (CHD), heart failure (HF), and stroke analysed separately to examine disease-specific associations (Table S5 of the manuscript). (08. Cox proportional hazard regression (CVD subtypes)).

Manuscript information
-
Link to paper:
Manuscript submitted to Circulation (under review at the time of repository release).





