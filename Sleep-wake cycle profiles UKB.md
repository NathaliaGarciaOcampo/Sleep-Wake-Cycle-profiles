#These codes can be used in order to reproduce all tables and figures from the article 

### Libraries 
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

###-------------------------------------###
#Data management --> Outcome preparation: 
###-------------------------------------###

##Notes:
#This script assumes a preprocessed dataset containing:
#Exposure: 36 accelerometer derived metrics and 9 sleep-wake cycle (SWC) profiles(SWCprofiles_UKB).For readers interested in the methodological details of clusters (profiles) identification, the codes are available at: "".
#Dates: Date of mortality(mortality_date), Date of coronary heart disease (CHD) event(chd_out_sro_date), Date of heart faiulre (HF) event(hfailure_out_sro_date), Date of stroke event(stroke_out_sro_date), Date of enrollment (doacc).
#CVD-subtypes event indicators, this include all cases before and after date of enrollment: CHD cases (chd_out_sro), HF cases (hfailure_out_sro) and stroke cases (stroke_out_sro).
#Covariates: Sex(sex), Education level(edu), marital status(livingalone), occupational status(work_cat), smoking status(smoking), alcohol consumption(alcohol_3cat), fruit and vegetable consumption(fruitveg_cat),Intake of central nervious system medication(CNS), Number of chronic conditions(multimorbidity_index), BMI (bmi_cat),Hypertension status (Hypertension_UK),Diabetes status(Diabetes_UK), Hyperlipidaemia status(HYPLIP). 

#Loading the data set: 
Data <- read.csv("path/name.csv")

##Covariates prepation: 
###Socio-demographic and behavioural factors:
#Age:
#Numerical variable:
Data_UKBB$age_inclusion <- as.numeric(difftime(Data_UKBB$doacc, Data_UKBB$dob, units = "days")) / 365.25
#Categorical variable
Data_UKBB$age_inclusion_cat = Data_UKBB$age_inclusion
Data_UKBB$age_inclusion_cat[Data_UKBB$age_inclusion<=70] =">=60 - <70 years"
Data_UKBB$age_inclusion_cat[Data_UKBB$age_inclusion>70] = ">=70 years"
Data_UKBB$age_inclusion_cat <- factor(Data_UKBB$age_inclusion_cat, levels = c("<70 years", ">=70 years"))

#Sex
Data_UKBB$sex<-factor(Data_UKBB$sex, levels = c("1", "2"),labels = c("Male", "Female"))

#Education
Data_UKBB$edu <-factor(Data_UKBB$edu, levels = c("0", "1", "2"), labels = c("Lower secondary school or less", "Secondary school", "Higher than secondary school"))

#Marital status
Data_UKBB$livingalone <- factor(Data_UKBB$livingalone, levels =  c("0", "1"),labels = c("Married or Cohabiting", "Not Married or Cohabiting"))

#Occupational status
Data_UKBB$work_cat <- Data_UKBB$work
Data_UKBB$work_cat[Data_UKBB$work_cat == -7 | Data_UKBB$work_cat == -3] <- NA
Data_UKBB$work_cat[Data_UKBB$work_cat == 1] <- 1
Data_UKBB$work_cat[!is.na(Data_UKBB$work_cat) & Data_UKBB$work_cat != 1] <- 0 #Participants with unpaid/voluntary work were classified as unemployed 
Data_UKBB$work_cat <- factor(Data_UKBB$work_cat, levels = c(0, 1), labels = c("Unemployed", "Employed"))

#Smoking status
Data_UKBB$smoking <-factor(Data_UKBB$smoking, levels =  c("0", "1", "2"), labels = c("never-smoker", "ex-smoker", "current smoker"))

#Alcohol consumption
Data_UKBB$alcohol_3cat <-factor(Data_UKBB$alcohol_3cat, levels =  c("0", "1", "2"),  labels = c("No consumption", "1-14 units per week", ">14 units per week"))

#Daily intake of fruits and vegetables
Data_UKBB$fruitveg_cat = Data_UKBB$fruitveg
Data_UKBB$fruitveg_cat[Data_UKBB$fruitveg == 2] <- 1
Data_UKBB$fruitveg_cat[Data_UKBB$fruitveg == 0 | Data_UKBB$fruitveg == 1 ] <- 0
Data_UKBB$fruitveg_cat <-factor(Data_UKBB$fruitveg_cat, levels = c("0", "1"), labels = c("< 2 times per day", ">= 2 times per day"))

##General health related factors##

#Intake of Central Nervous System (CNS) medications
Data_UKBB$CNS <- factor(Data_UKBB$CNS, levels = c("0","1"), labels = c("No intake", "Intake"))


#Number of chronic diseases
#Parkinson
Data_UKBB$parkinson_out_sro[is.na(Data_UKBB$parkinson_out_sro)] <- 0
Data_UKBB$parkinson_UK = Data_UKBB$parkinson_out_sro
Data_UKBB$parkinson_UK <- ifelse(
  !is.na(Data_UKBB$parkinson_out_sro) & Data_UKBB$parkinson_out_sro == 1 &
  !is.na(Data_UKBB$parkinson_out_sro_date) & Data_UKBB$parkinson_out_sro_date <= Data_UKBB$doacc,
  1, 0)

#COPD
Data_UKBB$copd_out_sro[is.na(Data_UKBB$copd_out_sro)] <- 0
Data_UKBB$COPD_UK = Data_UKBB$copd_out_sro
Data_UKBB$COPD_UK <- ifelse(
  !is.na(Data_UKBB$copd_out_sro) & Data_UKBB$copd_out_sro == 1 &
  !is.na(Data_UKBB$copd_out_sro_date) & Data_UKBB$copd_out_sro_date <= Data_UKBB$doacc,
  1, 0)

#Depression
Data_UKBB$depression_out_sro[is.na(Data_UKBB$depression_out_sro)] <- 0
Data_UKBB$depression_UK = Data_UKBB$depression_out_sro
Data_UKBB$depression_UK <- ifelse(
  !is.na(Data_UKBB$depression_out_sro) & Data_UKBB$depression_out_sro == 1 &
  !is.na(Data_UKBB$depression_out_sro_date) & Data_UKBB$depression_out_sro_date <= Data_UKBB$doacc,
  1, 0)

#Other mental disorders
Data_UKBB$mentaldisorders_out_sro[is.na(Data_UKBB$mentaldisorders_out_sro)] <- 0
Data_UKBB$OtherMD_UK = Data_UKBB$mentaldisorders_out_sro
Data_UKBB$OtherMD_UK <- ifelse(
  !is.na(Data_UKBB$mentaldisorders_out_sro) & Data_UKBB$mentaldisorders_out_sro == 1 &
  !is.na(Data_UKBB$mentaldisorders_out_sro_date) & Data_UKBB$mentaldisorders_out_sro_date <= Data_UKBB$doacc,
  1, 0)

#Cancer
Data_UKBB$cancer_out_sro[is.na(Data_UKBB$cancer_out_sro)] <- 0
Data_UKBB$cancer_UK = Data_UKBB$cancer_out_sro
Data_UKBB$cancer_UK <- ifelse(
  !is.na(Data_UKBB$cancer_out_sro) & Data_UKBB$cancer_out_sro == 1 &
  !is.na(Data_UKBB$cancer_out_sro_date) & Data_UKBB$cancer_out_sro_date <= Data_UKBB$doacc,
  1, 0)

#Liver disease
Data_UKBB$liver_out_sro[is.na(Data_UKBB$liver_out_sro)] <- 0
Data_UKBB$Liverd_UK = Data_UKBB$liver_out_sro
Data_UKBB$Liverd_UK <- ifelse(
  !is.na(Data_UKBB$liver_out_sro) & Data_UKBB$liver_out_sro == 1 &
  !is.na(Data_UKBB$liver_out_sro_date) & Data_UKBB$liver_out_sro_date <= Data_UKBB$doacc,
  1, 0)

#Arthritis
Data_UKBB$osteoarthritis_out_sro[is.na(Data_UKBB$osteoarthritis_out_sro)] <- 0
Data_UKBB$Arthritis_UK = Data_UKBB$osteoarthritis_out_sro
Data_UKBB$Arthritis_UK <- ifelse(
  !is.na(Data_UKBB$osteoarthritis_out_sro) & Data_UKBB$osteoarthritis_out_sro == 1 &
  !is.na(Data_UKBB$osteoarthritis_out_sro_date) & Data_UKBB$osteoarthritis_out_sro_date <= Data_UKBB$doacc,
  1, 0)

#Multimorbidity index:
Data_UKBB$multimorbidity <- apply(Data_UKBB[, c("parkinson_UK", "COPD_UK", "OtherMD_UK", "depression_UK", "cancer_UK", "Liverd_UK", "Arthritis_UK")], 
                                      1, function(x) ifelse(any(is.na(x)), NA, sum(x)))

Data_UKBB$multimorbidity_index <- factor(
  ifelse(Data_UKBB$multimorbidity == 0, "0 diseases", 
         ifelse(Data_UKBB$multimorbidity == 1, "1 disease", ">=2 diseases")),
  levels = c("0 diseases", "1 disease", ">=2 diseases"))


##Cardiometabolic risk factors
#BMI
#Numerical variable
summary(Data_UKBB$bmi)
#categorical variable
Data_UKBB$bmi_cat = Data_UKBB$bmi
Data_UKBB$bmi_cat[Data_UKBB$bmi  < 25] <- "<25 kg/m2"
Data_UKBB$bmi_cat[Data_UKBB$bmi >= 25 & Data_UKBB$bmi < 30] <- ">=25 - <30 kg/m2"
Data_UKBB$bmi_cat[Data_UKBB$bmi >= 30] <- ">=30 kg/m2"
Data_UKBB$bmi_cat<-factor(Data_UKBB$bmi_cat)

#Diabetes status
Data_UKBB$diabetes_out_sro[is.na(Data_UKBB$diabetes_out_sro)] <- 0
Data_UKBB$Diabetes_UK = Data_UKBB$diabetes_out_sro
Data_UKBB$Diabetes_UK <- ifelse(
  !is.na(Data_UKBB$diabetes_out_sro) & Data_UKBB$diabetes_out_sro == 1 &
  !is.na(Data_UKBB$diabetes_out_sro_date) & Data_UKBB$diabetes_out_sro_date <= Data_UKBB$doacc,
  1, 0)

#Hypertension
Data_UKBB$hypertension_out_sro[is.na(Data_UKBB$hypertension_out_sro)] <- 0
Data_UKBB$Hypertension_UK = Data_UKBB$hypertension_out_sro
Data_UKBB$Hypertension_UK <- ifelse(
  !is.na(Data_UKBB$hypertension_out_sro) & Data_UKBB$hypertension_out_sro == 1 &
  !is.na(Data_UKBB$hypertension_out_sro_date) & Data_UKBB$hypertension_out_sro_date <= Data_UKBB$doacc,
  1, 0)

#Hyperlipidaemia
Data_UKBB$HYPLIP<-factor(Data_UKBB$HYPLIP, levels = c("0", "1"), labels = c("No Hyperlipidaemia", "Hyperlipidaemia"))


###Exposure: SWC profiles
sample_cvd_uk$SWCprofiles_UKB <- factor(sample_cvd_uk$SWCprofiles_UKB, levels = c("SWCprofiles_UKBProfile 1","SWCprofiles_UKBProfile 2", "SWCprofiles_UKBProfile 3", "SWCprofiles_UKBProfile 4", "SWCprofiles_UKBProfile 5", "SWCprofiles_UKBProfile 6", "SWCprofiles_UKBProfile 7", "SWCprofiles_UKBProfile 8", "SWCprofiles_UKBProfile 9" ))
#Setting the reference:
sample_cvd_uk$SWCprofiles_UKB <- relevel(sample_cvd_uk$SWCprofiles_UKB, ref = "SWCprofiles_UKBProfile 3")


#####Creating prevalent disease indicators:

#Prevalent CHD cases at enrollment date:
Data_UKBB$CHD_Prevalence_UK <- ifelse(
  !is.na(Data_UKBB$chd_out_sro) & Data_UKBB$chd_out_sro == 1 &
    !is.na(Data_UKBB$chd_out_sro_date) & Data_UKBB$chd_out_sro_date <= Data_UKBB$doacc,
  1, 0)

#Prevalent HF cases at enrollment date:
Data_UKBB$HF_Prevalence_UK <- ifelse(
  !is.na(Data_UKBB$hfailure_out_sro) & Data_UKBB$hfailure_out_sro == 1 &
    !is.na(Data_UKBB$hfailure_out_sro_date) & Data_UKBB$hfailure_out_sro_date <= Data_UKBB$doacc,
  1, 0)

#Prevalent stroke cases at enrollment date:
Data_UKBB$Stroke_Prevalence_UK <- ifelse(
  !is.na(Data_UKBB$stroke_out_sro) & Data_UKBB$stroke_out_sro == 1 &
    !is.na(Data_UKBB$stroke_out_sro_date) & Data_UKBB$stroke_out_sro_date <= Data_UKBB$doacc,
  1, 0)

#Prevalent CVD cases at enrollment date:
Data_UKBB$CVD_prevalence_UK<- ifelse(
  (!is.na(Data_UKBB$Stroke_Prevalence_UK) & Data_UKBB$Stroke_Prevalence_UK == 1) | 
    (!is.na(Data_UKBB$CHD_Prevalence_UK) & Data_UKBB$CHD_Prevalence_UK == 1) | 
    (!is.na(Data_UKBB$HF_Prevalence_UK) & Data_UKBB$HF_Prevalence_UK == 1), 1, 0)

#####Creating incident disease indicators:

##First CVD event:
#The outcome of interest (CVD) was defined as the incidence of the first fatal or non-fatal CHD, HF and stroke. 
Data$first_cvd_date_UK <- apply(Data[, c("hfailure_out_sro_date", "chd_out_sro_date", "stroke_out_sro_date")], 1, function(x) {
  if (all(is.na(x))) NA else min(x, na.rm = TRUE)})
Data$first_cvd_date_UK <- as.Date(Data$first_cvd_date_UK)

##Incidence of CVD, CHD, HF & stroke, note that these variables refers to the incidence of the first event.

#CVD incidence:
Data$CVD_incidence_UK <- ifelse(
  (is.na(Data$CVD_prevalence_UK) | Data$CVD_prevalence_UK == 0) &  # Not prevalent CVD at baseline
    (!is.na(Data$first_cvd_date_UK) & Data$first_cvd_date_UK > Data$doacc), 1, 0) # First CVD event AFTER inclusion date

#CHD incidence:
Data$CHD_incidence_UK <- ifelse(
  !is.na(Data$CHD_Prevalence_UK) & Data$CHD_Prevalence_UK == 0 &
    !is.na(Data$first_cvd_date_UK) &!is.na(Data$chd_out_sro_date) &
    Data$first_cvd_date_UK > Data$doacc &
    Data$first_cvd_date_UK == Data$chd_out_sro_date, 1,0)

#HF incidence:
Data$HF_incidence_UK <- ifelse(
  !is.na(Data$HF_Prevalence_UK) & Data$HF_Prevalence_UK == 0 &
    !is.na(Data$first_cvd_date_UK) & !is.na(Data$hfailure_out_sro_date) &
    Data$first_cvd_date_UK > Data$doacc &
    Data$first_cvd_date_UK == Data$hfailure_out_sro_date, 1, 0)

#Stroke incidence:
Data$Stroke_incidence_UK <- ifelse(
  !is.na(Data$Stroke_Prevalence_UK) & Data$Stroke_Prevalence_UK == 0 &
    !is.na(Data$first_cvd_date_UK) & !is.na(Data$stroke_out_sro_date) &
    Data$first_cvd_date_UK > Data$doacc &
    Data$first_cvd_date_UK == Data$stroke_out_sro_date, 1, 0)


###Final population of study#####
missing_participants <- Data_UKBB[is.na(Data_UKBB$age_inclusion)|is.na(Data_UKBB$sex)|is.na(Data_UKBB$edu)| is.na(Data_UKBB$edu)| is.na(Data_UKBB$livingalone)| is.na(Data_UKBB$work_cat) | is.na(Data_UKBB$smoking) | is.na(Data_UKBB$alcohol_3cat) | is.na(Data_UKBB$fruitveg) | is.na(Data_UKBB$CNS) | is.na(Data_UKBB$multimorbidity_index) | is.na(Data_UKBB$bmi_cat) | is.na(Data_UKBB$Diabetes_UK) | is.na(Data_UKBB$Hypertension_UK) | is.na(Data_UKBB$HYPLIP), ] 
n_missing <- nrow(missing_participants)
print(n_missing)
Data_CVD_uk <- Data_UKBB[Data_UKBB$CVD_prevalence_UK != 1, ] #Removing prevalent CVD cases
sample_cvd_uk <- Data_CVD_uk[complete.cases(Data_CVD_uk[, c("livingalone", "smoking", "alcohol_3cat", "fruitveg", "bmi_cat", "work_cat")]), ]



###Univariate & bivariate analysis 



###-------------------------------------###
#Data management --> Statistical analysis preparation: 
###-------------------------------------###


#Creation of follow_up time and event indicator variables

#Follow_up time: Participants were censored at the first date of their CVD event. 
#For example, if CHD was the outcome of interest and a participant experienced a HF event before CHD or the end of follow-up, 
#the censoring time was the date of HF.

end_follow_up_date_CVD_uk <- as.Date("2022-11-30") #End of follow up date for this study.

Data$end_date_cvd <- pmin(
  Data$first_cvd_date_UK, # Date of first CVD fatal or non fatal event
  Data$mortality_date, # Date of dead, this includes CVD death
  end_follow_up_date_CVD_uk, # End of follow up date
  na.rm = TRUE)

Data$follow_up_time_cvd_uk <- as.numeric( 
  difftime( Data$end_date_cvd, Data$doacc,
            units = "days")) / 365.25
#CVD event indicator 
sample_cvd_uk$cvd_event_uk <- ifelse( 
  !is.na(sample_cvd_uk$first_cvd_date_UK) & # the date of first cvd event is different from NA
  sample_cvd_uk$first_cvd_date_UK == sample_cvd_uk$end_date_cvd, 1, 0) # The first cvd event date is the same as the end date that was previously identified

#CHD event indicator 
Data$CHD_event_UK <- ifelse(
  Data$CHD_incidence_UK == 1 &
    !is.na(Data$first_cvd_date_UK) & !is.na(Data$chd_out_sro_date) &
    Data$first_cvd_date_UK == Data$chd_out_sro_date & #First CVD date and CHD date are the same
    Data$first_cvd_date_UK <= end_follow_up_date_CVD_uk, 1, 0) #First CVD date within observed follow-up

#HF event indicator
Data$heart_f_event_uk <- ifelse(
  Data$HF_incidence_UK == 1 & 
    !is.na(Data$first_cvd_date_UK) & !is.na(Data$hfailure_out_sro_date) &
    Data$first_cvd_date_UK == Data$hfailure_out_sro_date &
    Data$first_cvd_date_UK <= end_follow_up_date_CVD_uk, 1, 0)

#Stroke event indicator
Data$stroke_event_uk <- ifelse(
  Data$Stroke_incidence_UK == 1 & 
    !is.na(Data$first_cvd_date_UK) & !is.na(Data$stroke_out_sro_date) &
    Data$first_cvd_date_UK == Data$stroke_out_sro_date &
    Data$first_cvd_date_UK <= end_follow_up_date_CVD_uk, 1, 0)

###Main analysis 
##Cox models CVD 


#Cox proportional hazard models, using age as time-scale:
Model 1:
cox_cvd_1_uk <- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, cvd_event_uk) ~ c9  + sex + edu + livingalone + work_cat + smoking +alcohol_3cat + fruitveg_cat,  data = sample_cvd_uk)
summary(cox_cvd_1_uk)
#Testing for the Proportional hazards assumption (In this study the proportional hazards assumption was verified using Schoenfeld residuals)
ph_test1_CVD <- cox.zph(cox_cvd_1_uk)

Model 2:
cox_cvd_2_uk <- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, cvd_event_uk) ~ c9  + sex + edu + livingalone + work_cat + smoking +alcohol_3cat + fruitveg_cat + CNS + multimorbidity_index ,  data = sample_cvd_uk)
summary(cox_cvd_2_uk)
#Testing for the Proportional hazards assumption 
ph_test2_CVD <- cox.zph(cox_cvd_2_uk)

Model 3:
cox_cvd_3_uk <- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, cvd_event_uk) ~ c9  + sex + edu + livingalone + work_cat + smoking +alcohol_3cat + fruitveg_cat + CNS + multimorbidity_index + bmi_cat + Hypertension_UK + Diabetes_UK + HYPLIP ,  data = sample_cvd_uk)
summary(cox_cvd_3_uk)
#Testing for the Proportional hazards assumption 
ph_test2_CVD <- cox.zph(cox_cvd_2_uk)

##########################################

##Cox models for testing interaction by age, sex and BMI:

##Age --> ###
####Model 1:  with age as an effect modifier
cox_cvd_1_uk_age_interaction <- coxph(Surv( follow_up_time_cvd_uk, cvd_event_uk) ~ c9*age_inclusion_cat + sex + edu + livingalone + work_cat + smoking + alcohol_3cat + fruitveg_cat,  data = sample_cvd_uk)
summary(cox_cvd_1_uk_age_interaction)
####Model 1:  without age as an effect modifier
cox_cvd_1_uk_age <- coxph(Surv( follow_up_time_cvd_uk, cvd_event_uk) ~ c9 + age_inclusion_cat + sex + edu + livingalone + work_cat + smoking + alcohol_3cat + fruitveg_cat,  data = sample_cvd_uk)
summary(cox_cvd_1_uk_age)
#Applying the Likelihood Ratio Test (LRT) --> to formally test whether including interaction (sex) improves overall model fit.
anova(cox_cvd_1_uk_age_interaction, cox_cvd_1_uk_age, test = "LRT") 
#Testing for the proportional hazard assumption
ph_test1_age_interaction <- cox.zph(cox_cvd_1_uk_age_interaction)
ph_test1_age <- cox.zph(cox_cvd_1_uk_age)

####Model 2:  with age as an effect modifier
cox_cvd_2_uk_age_interaction <- coxph(Surv( follow_up_time_cvd_uk, cvd_event_uk) ~ c9*age_inclusion_cat + sex + edu + livingalone + work_cat + smoking + alcohol_3cat + fruitveg_cat + CNS + multimorbidity_index ,  data = sample_cvd_uk)
summary(cox_cvd_2_uk_age_interaction)
####Model 2:  without age as an effect modifier
cox_cvd_2_uk_age <- coxph(Surv( follow_up_time_cvd_uk, cvd_event_uk) ~ c9 + age_inclusion_cat + sex + edu + livingalone + work_cat + smoking + alcohol_3cat + fruitveg_cat + CNS + multimorbidity_index ,  data = sample_cvd_uk)
summary(cox_cvd_2_uk_age)
#Applying the Likelihood Ratio Test (LRT) 
anova(cox_cvd_2_uk_age_interaction, cox_cvd_2_uk_age, test = "LRT") 
#Testing for the proportional hazard assumption
ph_test2_age_interaction <- cox.zph(cox_cvd_2_uk_age_interaction)
ph_test2_age <- cox.zph(cox_cvd_2_uk_age)

####Model 3:  with age as an effect modifier
cox_cvd_3_uk_age_interaction <- coxph(Surv( follow_up_time_cvd_uk, cvd_event_uk) ~ c9*age_inclusion_cat + sex + edu + livingalone + work_cat + smoking + alcohol_3cat + fruitveg_cat + CNS + multimorbidity_index + Hypertension_UK + Diabetes_UK + HYPLIP ,  data = sample_cvd_uk)
summary(cox_cvd_3_uk_age_interaction)
####Model 3:  without age as an effect modifier
cox_cvd_3_uk_age <- coxph(Surv( follow_up_time_cvd_uk, cvd_event_uk) ~ c9 + age_inclusion_cat + sex + edu + livingalone + work_cat + smoking + alcohol_3cat + fruitveg_cat + CNS + multimorbidity_index + Hypertension_UK + Diabetes_UK + HYPLIP ,  data = sample_cvd_uk)
summary(cox_cvd_3_uk_age)
#Applying the Likelihood Ratio Test (LRT) 
anova(cox_cvd_3_uk_age_interaction, cox_cvd_3_uk_age, test = "LRT") 
#Testing for the proportional hazard assumption
ph_test2_age_interaction <- cox.zph(cox_cvd_3_uk_age_interaction)
ph_test2_age <- cox.zph(cox_cvd_3_uk_age)


###Sex--> Female/Male####

####Model 1:  with sex as an effect modifier
cox_cvd_1_uk_sex_interaction <- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, cvd_event_uk) ~ c9*sex + edu + livingalone + work_cat + smoking +alcohol_3cat + fruitveg_cat,  data = sample_cvd_uk)
summary(cox_cvd_1_uk_sex_interaction)
####Model 1:  without sex as an effect modifier
cox_cvd_1_uk_sex <- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, cvd_event_uk) ~ c9  + sex + edu + livingalone + work_cat + smoking +alcohol_3cat + fruitveg_cat,  data = sample_cvd_uk)
summary(cox_cvd_1_uk_sex)
#Applying the Likelihood Ratio Test (LRT) 
anova(cox_cvd_1_uk_sex_interaction, cox_cvd_1_uk_sex, test = "LRT") 
#Testing for the proportional hazard assumption
ph_test1_sex <- cox.zph(cox_cvd_1_uk_sex_interaction)

####Model 2:  with sex as an effect modifier
cox_cvd_2_uk_sex_interaction <- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, cvd_event_uk) ~ c9*sex + edu + livingalone + work_cat + smoking +alcohol_3cat + fruitveg_cat + CNS + multimorbidity_index ,  data = sample_cvd_uk)
summary(cox_cvd_2_uk_sex_interaction)
####Model 2:  without sex as an effect modifier
cox_cvd_2_uk_sex <- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, cvd_event_uk) ~ c9 + sex + edu + livingalone + work_cat + smoking +alcohol_3cat + fruitveg_cat + CNS + multimorbidity_index ,  data = sample_cvd_uk)
summary(cox_cvd_2_uk_sex)
#Applying the Likelihood Ratio Test (LRT)
anova(cox_cvd_2_uk_sex_interaction, cox_cvd_2_uk_sex, test = "LRT") 
#Testing for the proportional hazard assumption
ph_test2_sex <- cox.zph(cox_cvd_2_uk_sex_interaction)

####Model 3:  with sex as an effect modifier
cox_cvd_3_uk_sex_interaction <- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, cvd_event_uk) ~ c9*sex + edu + livingalone + work_cat + smoking +alcohol_3cat + fruitveg_cat + CNS + multimorbidity_index + bmi_cat + Hypertension_UK + Diabetes_UK + HYPLIP ,  data = sample_cvd_uk)
summary(cox_cvd_3_uk_sex_interaction)
####Model 3:  without sex as an effect modifier
cox_cvd_3_uk_sex <- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, cvd_event_uk) ~ c9  + sex + edu + livingalone + work_cat + smoking +alcohol_3cat + fruitveg_cat + CNS + multimorbidity_index + bmi_cat + Hypertension_UK + Diabetes_UK + HYPLIP ,  data = sample_cvd_uk)
summary(cox_cvd_3_uk_sex)
#Applying the Likelihood Ratio Test (LRT)
anova(cox_cvd_3_uk_sex_interaction, cox_cvd_3_uk_sex, test = "LRT") 
#Testing for the proportional hazard assumption
ph_test3_sex <- cox.zph(cox_cvd_3_uk_sex_interaction)

##BMI --> ###

####Model 1:  with BMI as an effect modifier
cox_cvd_1_uk_bmi_interaction <- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, cvd_event_uk) ~ c9*bmi_cat + sex + edu + livingalone + work_cat + smoking + alcohol_3cat + fruitveg_cat,  data = sample_cvd_uk)
summary(cox_cvd_1_uk_bmi_interaction)
####Model 1:  without BMI as an effect modifier
cox_cvd_1_uk_bmi <- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, cvd_event_uk) ~ c9 +bmi_cat + sex + edu + livingalone + work_cat + smoking + alcohol_3cat + fruitveg_cat,  data = sample_cvd_uk)
summary(cox_cvd_1_uk_bmi)
#Applying the Likelihood Ratio Test (LRT) 
anova(cox_cvd_1_uk_bmi_interaction, cox_cvd_1_uk_bmi, test = "LRT")
#Testing for the proportional hazard assumption
ph_test1_bmi <- cox.zph(cox_cvd_1_uk_bmi_interaction)

####Model 2:  with BMI as an effect modifier
cox_cvd_2_uk_bmi_interaction <- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, cvd_event_uk) ~ c9*bmi_cat + sex + edu + livingalone + work_cat + smoking + alcohol_3cat + fruitveg_cat + CNS + multimorbidity_index ,  data = sample_cvd_uk)
summary(cox_cvd_2_uk_bmi_interaction)
####Model 2:  without BMI as an effect modifier
cox_cvd_2_uk_bmi <- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, cvd_event_uk) ~ c9 + bmi_cat + sex + edu + livingalone + work_cat + smoking + alcohol_3cat + fruitveg_cat + CNS + multimorbidity_index ,  data = sample_cvd_uk)
summary(cox_cvd_2_uk_bmi)
#Applying the Likelihood Ratio Test (LRT) 
anova(cox_cvd_2_uk_bmi_interaction, cox_cvd_2_uk_bmi, test = "LRT")
#Testing for the proportional hazard assumption
ph_test2_bmi <- cox.zph(cox_cvd_2_uk_bmi_interaction)

####Model 3:  with BMI as an effect modifier
cox_cvd_3_uk_bmi_interaction <- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, cvd_event_uk) ~ c9*bmi_cat + sex + edu + livingalone + work_cat + smoking + alcohol_3cat + fruitveg_cat + CNS + multimorbidity_index + Hypertension_UK + Diabetes_UK + HYPLIP ,  data = sample_cvd_uk)
summary(cox_cvd_3_uk_bmi_interaction)
####Model 3:  without BMI as an effect modifier
cox_cvd_3_uk_bmi<- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, cvd_event_uk) ~ c9 +bmi_cat + sex + edu + livingalone + work_cat + smoking + alcohol_3cat + fruitveg_cat + CNS + multimorbidity_index + Hypertension_UK + Diabetes_UK + HYPLIP ,  data = sample_cvd_uk)
summary(cox_cvd_3_uk_bmi)
#Applying the Likelihood Ratio Test (LRT) 
anova(cox_cvd_3_uk_bmi_interaction, cox_cvd_3_uk_bmi, test = "LRT")
#Testing for the proportional hazard assumption
ph_test3_bmi <- cox.zph(cox_cvd_3_uk_bmi_interaction)

##Sensitivity analysis 
##Cox models changing the references

#Setting SWC profile 1 as the reference:
sample_cvd_uk$SWCprofiles_UKB <- factor(sample_cvd_uk$SWCprofiles_UKB, levels = c("SWCprofiles_UKBProfile 1","SWCprofiles_UKBProfile 2", "SWCprofiles_UKBProfile 3", "SWCprofiles_UKBProfile 4", "SWCprofiles_UKBProfile 5", "SWCprofiles_UKBProfile 6", "SWCprofiles_UKBProfile 7", "SWCprofiles_UKBProfile 8", "SWCprofiles_UKBProfile 9" ))
sample_cvd_uk$SWCprofiles_UKB <- relevel(sample_cvd_uk$SWCprofiles_UKB, ref = "SWCprofiles_UKBProfile 1")

#Rerunning the models with SWC profile 1 as the reference:
#Model 1
cox_cvd_1_uk_profile1 <- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, cvd_event_uk) ~ c9  + sex + edu + livingalone + work_cat + smoking +alcohol_3cat + fruitveg_cat ,  data = sample_cvd_uk)
summary(cox_cvd_1_uk_profile1)
#Model 2
cox_cvd_2_uk_profile1 <- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, cvd_event_uk) ~ c9  + sex + edu + livingalone + work_cat + smoking +alcohol_3cat + fruitveg_cat + CNS + multimorbidity_index ,  data = sample_cvd_uk)
summary(cox_cvd_2_uk_profile1)
#Rerunning in model 3
cox_cvd_3_uk_profile1 <- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, cvd_event_uk) ~ c9  + sex + edu + livingalone + work_cat + smoking +alcohol_3cat + fruitveg_cat + CNS + multimorbidity_index + bmi_cat + Hypertension_UK + Diabetes_UK + HYPLIP ,  data = sample_cvd_uk)
summary(cox_cvd_3_uk_profile1)

#Setting SWC profile 1 as the reference:
sample_cvd_uk$SWCprofiles_UKB <- relevel(sample_cvd_uk$SWCprofiles_UKB, ref = "SWCprofiles_UKBProfile 6")

#Rerunning the models with SWC profile 6 as the reference:
#Model 1
cox_cvd_1_uk_profile6 <- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, cvd_event_uk) ~ c9  + sex + edu + livingalone + work_cat + smoking +alcohol_3cat + fruitveg_cat ,  data = sample_cvd_uk)
summary(cox_cvd_1_uk_profile6)
#Model 2
cox_cvd_2_uk_profile6 <- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, cvd_event_uk) ~ c9  + sex + edu + livingalone + work_cat + smoking +alcohol_3cat + fruitveg_cat + CNS + multimorbidity_index ,  data = sample_cvd_uk)
summary(cox_cvd_2_uk_profile6)
#Rerunning in model 3
cox_cvd_3_uk_profile6 <- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, cvd_event_uk) ~ c9  + sex + edu + livingalone + work_cat + smoking +alcohol_3cat + fruitveg_cat + CNS + multimorbidity_index + bmi_cat + Hypertension_UK + Diabetes_UK + HYPLIP ,  data = sample_cvd_uk)
summary(cox_cvd_3_uk_profile6)


#Setting back SWC profile 3 as the reference:
sample_cvd_uk$SWCprofiles_UKB <- relevel(sample_cvd_uk$SWCprofiles_UKB, ref = "SWCprofiles_UKBProfile 3")
sample_cvd_uk$SWCprofiles_UKB <- factor(sample_cvd_uk$SWCprofiles_UKB, levels = c("SWCprofiles_UKBProfile 1","SWCprofiles_UKBProfile 2", "SWCprofiles_UKBProfile 3", "SWCprofiles_UKBProfile 4", "SWCprofiles_UKBProfile 5", "SWCprofiles_UKBProfile 6", "SWCprofiles_UKBProfile 7", "SWCprofiles_UKBProfile 8", "SWCprofiles_UKBProfile 9" ))



#####
##Secondary analysis 
##Cox models CVD subtypes    
###CHD 
#Cox proportional hazard models, using age as time-scale:
#Model 1: 
cox_cvd_1_uk_CHD<- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, CHD_event_UK) ~ SWCprofiles_UKB  + sex + edu + livingalone + work_cat + smoking +alcohol_3cat +   fruitveg_cat,  data = Data)
summary(cox_cvd_1_uk_CHD)
#Testing for the Proportional hazards assumption
ph_test1_CHD <- cox.zph(cox_cvd_1_uk_CHD)

#Model 2:
cox_cvd_2_uk_CHD<- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, CHD_event_UK) ~ SWCprofiles_UKB  + sex + edu + livingalone + work_cat + smoking +alcohol_3cat + fruitveg_cat + CNS + multimorbidity_index ,  data = Data)
summary(cox_cvd_2_uk_CHD)
#Testing for the Proportional hazards assumption
ph_test2_CHD <- cox.zph(cox_cvd_2_uk_CHD)

#Model 3:
cox_cvd_3_uk_CHD<- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, CHD_event_UK) ~ SWCprofiles_UKB  + sex + edu + livingalone + work_cat + smoking +alcohol_3cat + fruitveg_cat + CNS + multimorbidity_index + bmi_cat + Hypertension_UK + Diabetes_UK + HYPLIP ,  data = Data)
summary(cox_cvd_3_uk_CHD)
#Testing for the Proportional hazards assumption 
ph_test3_CHD <- cox.zph(cox_cvd_2_uk_CHD)

###HF

#Cox proportional hazard models, using age as time-scale for HF:
#Model 1:
cox_cvd_1_uk_Heart_f <- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, heart_f_event_uk) ~ SWCprofiles_UKB  + sex + edu + livingalone + work_cat + smoking +alcohol_3cat + fruitveg_cat,  data = Data)
summary(cox_cvd_1_uk_Heart_f)
#Testing for the Proportional hazards assumption
ph_test1_HF <- cox.zph(cox_cvd_1_uk_Heart_f)

#Model 2:
cox_cvd_2_uk_Heart_f <- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, heart_f_event_uk) ~ SWCprofiles_UKB  + sex + edu + livingalone + work_cat + smoking +alcohol_3cat + fruitveg_cat + CNS + multimorbidity_index ,  data = Data)
summary(cox_cvd_2_uk_Heart_f)
#Testing for the Proportional hazards assumption
ph_test2_HF <- cox.zph(cox_cvd_2_uk_Heart_f)

#Model 3:
cox_cvd_3_uk_Heart_f <- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, heart_f_event_uk) ~ SWCprofiles_UKB  + sex + edu + livingalone + work_cat + smoking +alcohol_3cat + fruitveg_cat + CNS + multimorbidity_index + bmi_cat + Hypertension_UK + Diabetes_UK + HYPLIP ,  data = Data)
summary(cox_cvd_3_uk_Heart_f)
#Testing for the Proportional hazards assumption
ph_test3_HF <- cox.zph(cox_cvd_3_uk_Heart_f)

#Stroke
#Cox proportional hazard models, using age as time-scale for stroke:
#Model 1:
cox_cvd_1_uk_stroke <- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, stroke_event_uk) ~ SWCprofiles_UKB  + sex + edu + livingalone + work_cat + smoking +alcohol_3cat + fruitveg_cat,  data = Data)
summary(cox_cvd_1_uk_stroke)
#Testing for the Proportional hazards assumption
ph_test1_Str <- cox.zph(cox_cvd_1_uk_stroke)

#Model 2:
cox_cvd_2_uk_stroke <- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, stroke_event_uk) ~ SWCprofiles_UKB  + sex + edu + livingalone + work_cat + smoking +alcohol_3cat + fruitveg_cat + CNS + multimorbidity_index ,  data = Data)
summary(cox_cvd_2_uk_stroke)
#Testing for the Proportional hazards assumption
ph_test2_Str <- cox.zph(cox_cvd_2_uk_stroke)

#Model 3
cox_cvd_3_uk_stroke <- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, stroke_event_uk) ~ SWCprofiles_UKB  + sex + edu + livingalone + work_cat + smoking +alcohol_3cat + fruitveg_cat + CNS + multimorbidity_index + bmi_cat + Hypertension_UK + Diabetes_UK + HYPLIP ,  data = Data)
summary(cox_cvd_3_uk_stroke)
#Testing for the Proportional hazards assumption
ph_test3_Str <- cox.zph(cox_cvd_3_uk_stroke)



### Figures:
##------------------------------------------------------------------------------------------------------------------------------------------##
## Figure 1: Standardized mean scores on 36 metrics as a function of the nine sleep-wake cycle profiles in the UKB accelerometer sub-study
##------------------------------------------------------------------------------------------------------------------------------------------##


##1. Define profile ordering
Data$SWCprofiles_order <- factor(Data$SWCprofiles_UKB, 
                                          levels = c("RAR ++ PA ++", "RAR + PA + Sleep -","RAR + LIPA + Sleep +", "MVPA +",
                                                     "RAR - Chronotype --", "RAR - PA - Sleep +", "RAR - PA - Sleep --", "RAR - PA + Restless sleep",
                                                     "RAR -- PA -- Chronotype -"))

##2. Scale the 36 accelerometer metrics 
vars_to_scale <- names(Data)[2:37] # # Assumes metrics are columns 2–37, this is specific to our dataset
scaled_data <- Data %>%
  select(all_of(vars_to_scale), SWCprofiles_UKB, SWCprofiles_order) %>%
  mutate(across(all_of(vars_to_scale), scale)) 

##3. Compute mean z-scores by SWC profile
SWCprofile_means <- scaled_data %>%
  group_by(SWCprofiles_UKB, SWCprofiles_order) %>%
  summarise(across(all_of(vars_to_scale), mean, na.rm = TRUE), .groups = "drop")

##4. Reshape data to long format
SWCprofile_long <- SWCprofile_means %>%
  pivot_longer(cols = all_of(vars_to_scale), 
               names_to = "Metric", values_to = "Z_score")

##5. Assign SWC dimensions
SWCprofile_long <- SWCprofile_long %>%
  mutate(Dimensions = case_when(
    Metric %in% c("relativeamplitude", "cosinormesor", "cosinoramplitude", "cosinorr2", "is", "iv" ) ~ "RAR",
    Metric %in% c("sbduration", "numberofboutsinsb", "meandurationsbbouts", "tpard", "lipaduration", "mvpaduration", "numberofboutsinlipa", "numberofboutsinmvpa", "meandurationlipabouts", "meandurationmvpabouts", "tprad", "m10value", "meanaccelerationduringwaking", "igintercept", "igslope" ) ~ "Daytime\nActivity",
    Metric %in% c("durationsleepwindow", "sleepefficiency", "durationsleepbouts", "tpwsn", "accelerationduringsleep", "numbersleepbouts", "l5value", "tpswn", "durationwake", "durationwakebouts" ) ~ "Sleep",
    Metric %in% c("sleeponset", "sleepoffset", "m10timing","l5timing", "cosinoracrotime") ~ "Chronotype"))

##6. Prepare labels for plotting
SWCprofile_long <- SWCprofile_long %>%
  mutate(Dimensions = factor(Dimensions, levels = rev(c("Chronotype", "Sleep", "Daytime\nActivity", "RAR")))) %>%
  mutate(Varname2 = "NULL") %>%
  mutate(Varname2 = ifelse(Metric == "meanaccelerationduringwaking", "Acceleration during waking", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "lipaduration", "LIPA duration", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "mvpaduration", "MVPA duration", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "numberofboutsinlipa", "Number of LIPA bouts", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "numberofboutsinmvpa", "Number of MVPA bouts", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "meandurationlipabouts", "Mean duration of LIPA bouts", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "meandurationmvpabouts", "Mean duration of MVPA bouts", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "m10value", "M10 mean acceleration", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "relativeamplitude", "Relative amplitude", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "tprad", "TPra,d", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "tpard", "TPar,d", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "sbduration", "SB duration", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "numberofboutsinsb", "Number of SB bouts", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "meandurationsbbouts", "Mean duration of SB bouts", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "igintercept", "IG intercept", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "igslope", "IG slope", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "iv", "Intradaily variability", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "is", "Interdaily stability", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "sleeponset", "Sleep onset", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "sleepoffset", "Waking time", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "m10timing", "M10 start", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "l5timing", "L5 start", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "cosinoracrotime", "Cosinor acrotime", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "cosinormesor", "Cosinor mesor", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "cosinoramplitude", "Cosinor amplitude", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "cosinorr2", "Cosinor R2", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "durationsleepwindow", "Sleep duration", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "sleepefficiency", "Sleep efficiency", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "tpwsn", "TPws,n", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "durationsleepbouts", "Mean duration of sleep bouts", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "numbersleepbouts", "Number of sleep bouts", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "accelerationduringsleep", "Mean acceleration during sleep", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "l5value", "L5 mean acceleration", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "durationwake", "WASO", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "durationwakebouts", "Mean duration of wake bouts", Varname2)) %>%
  mutate(Varname2 = ifelse(Metric == "tpswn", "TPsw,n", Varname2)) %>%
  mutate(Varname2 = factor(Varname2, levels = rev(c("SB duration", "Number of SB bouts", "Mean duration of SB bouts","TPar,d","LIPA duration","MVPA duration","Number of LIPA bouts","Number of MVPA bouts","Mean duration of LIPA bouts","Mean duration of MVPA bouts", "TPra,d","M10 mean acceleration","Acceleration during waking", "IG intercept", "IG slope","Relative amplitude","Cosinor mesor","Cosinor amplitude", "Cosinor R2", "Interdaily stability", "Intradaily variability", "Sleep onset", "Waking time", "M10 start", "L5 start", "Cosinor acrotime", "Sleep duration", "Sleep efficiency", "Mean duration of sleep bouts", "TPws,n", "Mean acceleration during sleep", "Number of sleep bouts", "L5 mean acceleration", "TPsw,n", "WASO", "Mean duration of wake bouts")))) %>%
  mutate(SWCprofiles_UKB2 = "NULL") %>%
  mutate(SWCprofiles_UKB2 = ifelse(SWCprofiles_UKB == "RAR ++ PA ++",          "Profile 1\n\nRAR ++\nPA ++\n\n\nN = 5,426\n(11.1%)", SWCprofiles_UKB2)) %>%
  mutate(SWCprofiles_UKB2 = ifelse(SWCprofiles_UKB == "RAR + PA + Sleep -",  "Profile 2\n\nRAR +\nPA +\nSleep -\n\nN = 6,259\n(12.8%)", SWCprofiles_UKB2)) %>%
  mutate(SWCprofiles_UKB2 = ifelse(SWCprofiles_UKB == "RAR + LIPA + Sleep +",  "Profile 3\n\nRAR +\nLIPA +\nSleep +\n\nN = 8,577\n(17.5%)", SWCprofiles_UKB2)) %>%
  mutate(SWCprofiles_UKB2 = ifelse(SWCprofiles_UKB == "MVPA +", "Profile 4\n\n\nMVPA +\n\n\nN = 6,498\n(13.3%)", SWCprofiles_UKB2)) %>%
  mutate(SWCprofiles_UKB2 = ifelse(SWCprofiles_UKB == "RAR - Chronotype --", "Profile 5\n\nRAR -\nChronotype --\n\n\nN = 5,279\n(10.8%)", SWCprofiles_UKB2)) %>%
  mutate(SWCprofiles_UKB2 = ifelse(SWCprofiles_UKB == "RAR - PA - Sleep +",    "Profile 6\n\nRAR -\nPA -\nSleep +\n\nN = 7,748\n(15.8%)", SWCprofiles_UKB2)) %>%
  mutate(SWCprofiles_UKB2 = ifelse(SWCprofiles_UKB == "RAR - PA - Sleep --",   "Profile 7\n\nRAR -\nPA -\nSleep --\n\nN = 4,500\n(9.2%)", SWCprofiles_UKB2)) %>%
  mutate(SWCprofiles_UKB2 = ifelse(SWCprofiles_UKB == "RAR - PA + Restless sleep",  "Profile 8\n\nRAR -\nPA +\nRestless sleep\n\nN = 1,672\n(3.4%)", SWCprofiles_UKB2)) %>%
  mutate(SWCprofiles_UKB2 = ifelse(SWCprofiles_UKB == "RAR -- PA -- Chronotype -",  "Profile 9\n\nRAR --\nPA --\nChronotype -\n\nN = 2,987\n(6.1%)", SWCprofiles_UKB2))

##7. Plot figure 1 
SWC_profiles_UKBB <-ggplot(data = SWCprofile_long, aes(y = Varname2, x = Z_score, fill = Dimensions)) +
  facet_grid(Dimensions ~ SWCprofiles_UKB2, scales = "free", space = "free") + 
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    strip.background = element_rect(fill = "#f8f9fa", color = NA),
    strip.text.x = element_text(color = "#49525E", size = 13, face = "bold"),
    strip.text.y = element_text(angle = 0, color = "#49525E", size = 14, face = "bold")
  ) +
  labs(x = "z-score", y = "")  +
  geom_vline(xintercept = 0, color = "#49525E") +
  geom_vline(xintercept = c(-1, 1), color = "#49525E", lty = 2) +
  geom_vline(xintercept = c(-2, 2), color = "#49525E", lty = 3) +
  geom_col(position = position_dodge2()) +
  scale_fill_manual(values = c( "#515A66","#727D84","#9AA2A3", "#BFC2C1"))

##8. Save figure 1
ggsave("path/figure_1.svg", SWC_profiles_UKBB, width = 500, height = 300, units = "mm")

###-------------------------------------------------------------------------------------------------------------------------------------------------------------###
## Figure 2: Hazard ratios from Cox proportional hazard models for the association between sleep-wake cycle profiles and types of cardiovascular disease events (coronary heart disease, heart failure and stroke) in the UKB accelerometer sub-study.  
###-------------------------------------------------------------------------------------------------------------------------------------------------------------###

#Preparing the results for the plot: 
cox_results <- function(model, disease, model_label) {
  s <- summary(model)
  df <- data.frame(
    Variable = rownames(s$coefficients),
    HR = exp(s$coefficients[, "coef"]),
    Lower_CI = s$conf.int[, "lower .95"],
    Upper_CI = s$conf.int[, "upper .95"],
    p_value = s$coefficients[, "Pr(>|z|)"],
    stringsAsFactors = FALSE ) %>%
    filter(grepl("^c9", Variable)) %>%
    mutate(Model = model_label,
           Disease = disease)
           
#Add the SWC reference profile 3 
  ref_row <- data.frame(
    Variable = "c9Profile 3",
    HR = 1,
    Lower_CI = 1,
    Upper_CI = 1,
    p_value = NA,
    Disease = disease,
    Model = model_label )
  
  df <- bind_rows(ref_row, df)
  
  label_map <- c(
    "c9Profile 1" = "1.RAR++/PA++",
    "c9Profile 2" = "2.RAR+/PA+/Sleep-",
    "c9Profile 3" = "3.RAR+/LIPA+/Sleep+ (Ref.)",
    "c9Profile 4" = "4.MVPA+",
    "c9Profile 5" = "5.RAR-/Chronotype--",
    "c9Profile 6" = "6.RAR-/PA-/Sleep+",
    "c9Profile 7" = "7.RAR-/PA-/Sleep--",
    "c9Profile 8" = "8.RAR-/PA+/Restless sleep",
    "c9Profile 9" = "9.RAR--/PA--/Chronotype-")
  
  df <- df %>% mutate(Label = label_map[Variable])
  df}

#CHD results
results_all_CHD <- bind_rows(
  cox_results(Model_1, "CORONARY HEART DISEASE", "Model 1"),
  cox_results(Model_2, "CORONARY HEART DISEASE", "Model 2"),
  cox_results(Model_3, "CORONARY HEART DISEASE", "Model 3"))

plot_order_CHD <- rev(c(
  "1.RAR++/PA++", "2.RAR+/PA+/Sleep-", "3.RAR+/LIPA+/Sleep+ (Ref.)", "4.MVPA+", "5.RAR-/Chronotype--", "6.RAR-/PA-/Sleep+",
  "7.RAR-/PA-/Sleep--", "8.RAR-/PA+/Restless sleep", "9.RAR--/PA--/Chronotype-"))
  
results_all_CHD$Label <- factor(results_all_CHD$Label, levels = plot_order_CHD)

results_all_CHD <- results_all_CHD %>%
  mutate(HR_Pvalue = case_when(
      p_value < 0.05 & HR < 1  ~ "HR <1, Pvalue <0.05",    
      p_value < 0.05 & HR > 1  ~ "HR >1, Pvalue <0.05", 
      p_value >= 0.05 ~  "HR >or<1, Pvalue ≥0.05",
      TRUE  ~ "Ref."))
      
#CHD plot
CHD <-ggplot(results_all_CHD, aes(x = Label, y = HR, color = HR_Pvalue)) +
  geom_hline(yintercept = c(0.5,1.5,2.0,2.5, 3.0), linetype = "solid", size = 0.5, color = "#E5E5E5") +
  geom_hline(yintercept = 1, linetype = "dashed", size = 0.5) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, size = 0.5) +
  geom_point(size = 3, shape = 19) +
  facet_wrap(~ Model, nrow = 1) +  
  coord_flip() +
  scale_y_continuous(trans = "log",breaks = c(0.5, 1, 1.5, 2, 2.5),limits = c(0.5, 3.0)) +
  scale_color_manual( name = "Hazard Ratio (95% CI)", values = c( "#0000EE","#CD2626", "#9AA2A3", "black" )) +
 labs( y = "",x = "", title = "CORONARY HEART DISEASE") +
  theme_bw(base_size = 15) +
  theme(plot.title = element_text(hjust = -0.22, vjust = -11.0, face = "bold", size = 12),
  strip.background = element_rect(fill = "#BFC2C1", color = "#9AA2A3"),
    strip.text.y = element_text(face = "bold", size = 11.5), 
    strip.text.x = element_text(face = "bold", size = 12.5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = 0.3),
    panel.border = element_rect(size = 0.3, color = "#515A66"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(size = 13, face = "bold"),
    axis.title.y = element_text(size = 13, face = "bold"))
    
#Heart failure results
results_all_HF <- bind_rows(
  cox_results(Model_1_HF, "HEART FAILURE", "Model 1"), 
  cox_results(Model_2_HF, "HEART FAILURE", "Model 2"),
  cox_results(Model_3_HF, "HEART FAILURE", "Model 3"))

plot_order_HF <- rev(c(
  "1.RAR++/PA++", "2.RAR+/PA+/Sleep-", "3.RAR+/LIPA+/Sleep+ (Ref.)", "4.MVPA+", "5.RAR-/Chronotype--", "6.RAR-/PA-/Sleep+",
  "7.RAR-/PA-/Sleep--", "8.RAR-/PA+/Restless sleep", "9.RAR--/PA--/Chronotype-"))

results_all_HF$Label <- factor(results_all_HF$Label, levels = plot_order_HF)

results_all_HF <- results_all_HF %>%
  mutate(HR_Pvalue = case_when(
      p_value < 0.05 & HR < 1  ~ "HR <1, Pvalue <0.05",    
      p_value < 0.05 & HR > 1  ~ "HR >1, Pvalue <0.05", 
      p_value >= 0.05 ~  "HR >or<1, Pvalue ≥0.05",
      TRUE  ~ "Ref."))
#HF plot
HF <- ggplot(results_all_HF, aes(x = Label, y = HR, color = HR_Pvalue)) +
  geom_hline(yintercept = c(0.5,1.5,2.0,2.5,3.0), linetype = "solid", size = 0.5, color = "#E5E5E5") +
  geom_hline(yintercept = 1, linetype = "dashed", size = 0.5) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, size = 0.5) +
  geom_point(size = 3, shape = 19) +
  facet_wrap(~ Model, nrow = 1) +  
  coord_flip() +
  scale_y_continuous(trans = "log",breaks = c(0.5, 1, 1.5, 2, 2.5),limits = c(0.5, 3.0)) +
   scale_color_manual( name = "Hazard Ratio (95% CI)", values = c( "#0000EE","#CD2626", "#9AA2A3", "black" )) +
  labs( y = "",x = "Sleep-wake cycle profiles",title = "HEART FAILURE" ) +
  theme_bw(base_size = 15) +
  theme(plot.title = element_text(hjust = -0.12, vjust = -2.0, face = "bold", size = 12),
    strip.background = element_rect(fill = "#BFC2C1", color = "#9AA2A3"),
    strip.text.y = element_text(face = "bold", size = 11.5), 
    strip.text.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = 0.3),
    panel.border = element_rect(size = 0.3, color = "#515A66"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(size = 13, face = "bold"),
    axis.title.y = element_text(size = 13, face = "bold"))
    
#Stroke results
results_all_Stroke <- bind_rows(
  cox_results(Model_1_Stroke, "STROKE", "Model 1"),
  cox_results(Model_2_Stroke, "STROKE", "Model 2"),
  cox_results(Model_3_Stroke, "STROKE", "Model 3"))

plot_order_Stroke <- rev(c(
  "1.RAR++/PA++", "2.RAR+/PA+/Sleep-", "3.RAR+/LIPA+/Sleep+ (Ref.)", "4.MVPA+", "5.RAR-/Chronotype--", "6.RAR-/PA-/Sleep+",
  "7.RAR-/PA-/Sleep--", "8.RAR-/PA+/Restless sleep","9.RAR--/PA--/Chronotype-"))

results_all_Stroke$Label <- factor(results_all_Stroke$Label, levels = plot_order_Stroke)

results_all_Stroke <- results_all_Stroke %>%
  mutate( HR_Pvalue = factor(
    case_when(
      p_value < 0.05 & HR < 1  ~ "HR <1, Pvalue <0.05",    
      p_value < 0.05 & HR > 1  ~ "HR >1, Pvalue <0.05", 
      p_value >= 0.05 ~  "HR >or<1, Pvalue ≥0.05",
      TRUE  ~ "Ref."),
      levels = c(
        "HR <1, Pvalue <0.05",
        "HR >1, Pvalue <0.05",
        "HR >or<1, Pvalue ≥0.05",
        "Ref.")))

Stroke <- ggplot(results_all_Stroke, aes(x = Label, y = HR, color = HR_Pvalue)) +
  geom_hline(yintercept = c(0.5,1.5,2.0,2.5, 3.0), linetype = "solid", size = 0.5, color = "#E5E5E5") +
  geom_hline(yintercept = 1, linetype = "dashed", size = 0.5) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, size = 0.5) +
  geom_point(size = 3, shape = 19) +
  facet_wrap(~ Model, nrow = 1) +  
  coord_flip() +
  scale_y_continuous(trans = "log",breaks = c(0.5, 1, 1.5, 2, 2.5),limits = c(0.5, 3.0)) +
  scale_color_manual(
    name = "Hazard Ratio (95% CI)",
    values = c( "HR <1, Pvalue <0.05" = "#0000EE", "HR >1, Pvalue <0.05" = "#CD2626","HR >or<1, Pvalue ≥0.05" = "#9AA2A3","Ref." = "black"),
    drop = FALSE) +
  labs( y = "Hazard Ratio (95% CI)",x = "",title = "STROKE") +
  theme_bw(base_size = 15) +
  theme(plot.title = element_text(hjust = -0.065, vjust = -2.0, face = "bold", size = 12),
        strip.background = element_rect(fill = "#BFC2C1", color = "#9AA2A3"),
        strip.text.y = element_text(face = "bold", size = 11.5), 
        strip.text.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.3),
        panel.border = element_rect(size = 0.3, color = "#515A66"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, hjust = 0),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 13, face = "bold"))

    
#Preparing the final plot
CHD <- CHD + theme(plot.margin = unit(c(0,0,0.2,0), "cm"))
HF <- HF + theme(plot.margin = unit(c(-0.5,0.5,0.2,-0.25), "cm"))
Stroke <- Stroke + theme(plot.margin = unit(c(-0.5,0,0.2,0.5), "cm"))

Figure_2 <- ggarrange(CHD, HF, Stroke, ncol = 1, nrow = 3, align = "v", heights = c(1.0,0.89,0.95), common.legend = TRUE, legend = "right")

#To save the plot
ggsave("//path/figure_2.svg", Figure_2, width = 500, height = 350, units = "mm")



### Tables
###------------------------------------------------------------------------------------------------------------------------------------###
## Table 1 Characteristics of participants at baseline by incident cardiovascular disease in the UKB and WII accelerometer sub-studies.
###------------------------------------------------------------------------------------------------------------------------------------###

#Select the variables of interest
descriptive_cvd <- subset(Data, select = c(
  "sex", "age_inclusion", "edu", "livingalone", "work_cat", "smoking", "alcohol_3cat", 
  "fruitveg_cat", "CNS", "bmi_cat", "HYPLIP","Chronic_conditions", "Diabetes_UK", 
  "Hypertension_UK", "SWCprofiles_UKB"))

#Select categorical and numerical variables 
categorical_vars <- c( "sex", "edu", "livingalone", "work_cat", "smoking", "alcohol_3cat", 
                       "fruitveg_cat", "CNS", "bmi_cat", "HYPLIP","Chronic_conditions", "Diabetes_UK", "Hypertension_UK", "SWCprofiles_UKB")
numerical_vars <- c("age_inclusion") 

#Compute the frequency and percentage table for each categorical variable
for (var in categorical_vars) {
  cat("\nVariable:", var, "\n")  
  print(table(descriptive_cvd[[var]], useNA = "always"))  
  print(prop.table(table(descriptive_cvd[[var]])) * 100)  
  cat("---------------------------------------------------\n")}

#Compute the mean, SD, median and IQR
summary_stats <- data.frame(
  Variable = numerical_vars,
  Mean = sapply(descriptive_cvd[numerical_vars], function(x) mean(x, na.rm = TRUE)),
  SD = sapply(descriptive_cvd[numerical_vars], function(x) sd(x, na.rm = TRUE)),
  Median = sapply(descriptive_cvd[numerical_vars], function(x) median(x, na.rm = TRUE)),
  IQR = sapply(descriptive_cvd[numerical_vars], function(x) IQR(x, na.rm = TRUE)))

#Check the assumptions for categorical variables 
for (var in categorical_vars) {
  chitable <- table(Data$CVD_incidence_UK, Data[[var]])
  testtable <- chisq.test(chitable)
  cat("\nChi-Square Test for:", var, "\n")
  print(testtable)
  if(any(testtable$expected < 5)) {
    warning("Chi-squared approximation may be incorrect for", var)
  }
  cat("---------------------------------------------------\n")}

#Check the assumptions for numerical variables
ggplot(Data, aes(x=age_inclusion)) +
  geom_histogram(binwidth = 5, fill="#FFF6F4", color='#AC6A9F') 

#summary table by Incident CVD
tab1 <- Data %>%
  select(all_of(c(numerical_vars, categorical_vars, "CVD_incidence_UK"))) %>%
  tbl_summary(
    by = CVD_incidence_UK,  
    statistic = list(
      all_continuous() ~ "{mean} ({sd})", 
      all_categorical() ~ "{n} ({p}%)"),
    digits = list(
      all_categorical() ~ c(0, 1),   
      all_continuous() ~ 1),
    label = list(
      age_inclusion ~ "Age (years)",
      sex ~ "Sex",
      edu ~ "Education level",
      livingalone ~ "Marital status",
      work_cat ~ "Professional activity status",
      smoking ~ "Smoking Status",
      fruitveg_cat ~ "Fruit and vegetable consumption",
      alcohol_3cat ~ "Alcohol consumption",
      Hypertension_UK ~ "Hypertension",
      Diabetes_UK ~ "Diabetes",
      HYPLIP ~ "Hyperlipidaemia",
      Chronic_conditions ~ "Number of chronic conditions ",
      CNS ~ "Intake of Central Nervous System medications",
      bmi_cat ~ "BMI categories",
      SWCprofiles_UKB ~ "SWC Profiles")) %>%
  add_p(
    test = list(
      all_continuous() ~ "wilcox.test", 
      all_categorical() ~ "chisq.test" )) %>%
  italicize_labels() %>%
  bold_labels() %>%
  bold_p()
print(tab1)

#To print the table in a word document
tab1_flex <- as_flex_table(tab1)
doc <- read_docx() %>%
body_add_flextable(tab1_flex) %>%
body_add_par("Table 1: Characteristics of study population by Incident CVD UKB", style = "heading 2")
print(doc, target = "Characteristics of study population by Incident CVD UKB.docx")
