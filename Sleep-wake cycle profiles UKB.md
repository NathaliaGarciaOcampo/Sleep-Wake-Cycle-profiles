#These codes can be used in order to reproduce all tables and figures from the article 

### Libraries 
library(dplyr)
library(tidyr)
library(ggplot2)


### Figures:
##------------------------------------------------------------------------------------------------------------------------------------------##
## Figure 1: Standardized mean scores on 36 metrics as a function of the nine sleep-wake cycle profiles in the UKB accelerometer sub-study
##------------------------------------------------------------------------------------------------------------------------------------------##

##Notes##
#This script assumes a preprocessed dataset containing 36 accelerometer derived metrics and 9 sleep-wake cycle profiles (SWC).For readers interested in the methodological details of clusters (profiles) identification, the codes are available at: ""

##1. Load data 
Data <- read.csv("path/name.csv")

##1. Define profile ordering
Data$SWCprofiles_order <- factor(Data$SWCprofiles_UKB, 
                                          levels = c("RAR ++ PA ++", "RAR + PA + Sleep -","RAR + LIPA + Sleep +", "MVPA +",
                                                     "RAR - Chronotype --", "RAR - PA - Sleep +", "RAR - PA - Sleep --", "RAR - PA + Restless sleep",
                                                     "RAR -- PA -- Chronotype -"))

##3. Scale the 36 accelerometer metrics 
vars_to_scale <- names(Data)[2:37] # # Assumes metrics are columns 2–37, this is specific to our dataset
scaled_data <- Data %>%
  select(all_of(vars_to_scale), SWCprofiles_UKB, SWCprofiles_order) %>%
  mutate(across(all_of(vars_to_scale), scale)) 

##4. Compute mean z-scores by SWC profile
SWCprofile_means <- scaled_data %>%
  group_by(SWCprofiles_UKB, SWCprofiles_order) %>%
  summarise(across(all_of(vars_to_scale), mean, na.rm = TRUE), .groups = "drop")

##5. Reshape data to long format
SWCprofile_long <- SWCprofile_means %>%
  pivot_longer(cols = all_of(vars_to_scale), 
               names_to = "Metric", values_to = "Z_score")

##6. Assign SWC dimensions
SWCprofile_long <- SWCprofile_long %>%
  mutate(Dimensions = case_when(
    Metric %in% c("relativeamplitude", "cosinormesor", "cosinoramplitude", "cosinorr2", "is", "iv" ) ~ "RAR",
    Metric %in% c("sbduration", "numberofboutsinsb", "meandurationsbbouts", "tpard", "lipaduration", "mvpaduration", "numberofboutsinlipa", "numberofboutsinmvpa", "meandurationlipabouts", "meandurationmvpabouts", "tprad", "m10value", "meanaccelerationduringwaking", "igintercept", "igslope" ) ~ "Daytime\nActivity",
    Metric %in% c("durationsleepwindow", "sleepefficiency", "durationsleepbouts", "tpwsn", "accelerationduringsleep", "numbersleepbouts", "l5value", "tpswn", "durationwake", "durationwakebouts" ) ~ "Sleep",
    Metric %in% c("sleeponset", "sleepoffset", "m10timing","l5timing", "cosinoracrotime") ~ "Chronotype"))

##7. Prepare labels for plotting
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

##8. Plot figure 1 
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

#Save figure 1
ggsave("path/figure_1.svg", SWC_profiles_UKBB, width = 500, height = 300, units = "mm")

###-------------------------------------------------------------------------------------------------------------------------------------------------------------###
## Figure 2: Hazard ratios from Cox proportional hazard models for the association between sleep-wake cycle profiles and types of cardiovascular disease events (coronary heart disease, heart failure and stroke) in the UKB accelerometer sub-study.  
###-------------------------------------------------------------------------------------------------------------------------------------------------------------###


#Creation of incident CVD events variables:

  #To identify the first CVD event: as the outcome (CVD event) was defined as the incidence of the first fatal or non-fatal CHD, HF and stroke. 
    Data_UKBB$first_cvd_date_UK <- apply(Data_UKBB[, c("hfailure_out_sro_date", "chd_out_sro_date", "stroke_out_sro_date")], 1, function(x) {
      if (all(is.na(x))) NA else min(x, na.rm = TRUE)})
    Data_UKBB$first_cvd_date_UK <- as.Date(Data_UKBB$first_cvd_date_UK)

  #Incidence of CVD (Example for general CVD):
    Data_UKBB$CVD_incidence_UK <- ifelse(
  (is.na(Data_UKBB$CVD_prevalence_UK) | Data_UKBB$CVD_prevalence_UK == 0) &  # Not prevalent at baseline
  (!is.na(Data_UKBB$first_cvd_date_UK) & Data_UKBB$first_cvd_date_UK > Data_UKBB$doacc), 1, 0) # First event AFTER inclusion date

#Creation of follow_up time and event indicator variables

#Follow_up time: Participants were censored at the first date of their CVD event. For example, if CHD was the outcome of interest and a participant experienced a HF event before CHD or the end of follow-up, the censoring time was the date of HF. 
    end_follow_up_date_CVD_uk <- as.Date("2022-11-30")
   sample_cvd_uk$end_date_cvd <- pmin(
    sample_cvd_uk$first_cvd_date_UK, # Date of first CVD fatal or non fatal event
    sample_cvd_uk$mortality_date, # Date of dead, this includes CVD death
    end_follow_up_date_CVD_uk, # End of follow up date
    na.rm = TRUE)

  sample_cvd_uk$follow_up_time_cvd_uk <- as.numeric( 
    difftime( sample_cvd_uk$end_date_cvd, sample_cvd_uk$doacc,
    units = "days")) / 365.25

#Event indicator (example for CHD)
sample_cvd_uk$CHD_event_UK <- ifelse(
 sample_cvd_uk$CHD_incidence_UK == 1 &
 !is.na(sample_cvd_uk$first_cvd_date_UK) & !is.na(sample_cvd_uk$chd_out_sro_date) &
 sample_cvd_uk$first_cvd_date_UK == sample_cvd_uk$chd_out_sro_date & #First CVD date and CHD date are the same
 sample_cvd_uk$first_cvd_date_UK <= end_follow_up_date_CVD_uk, 1, 0) #First CVD date within observed follow-up

#Cox proportional hazard models, using age as time-scale. (Example for CHD)
#Model 1: 
  Model_1<- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, CHD_event_UK) ~ c9  + sex + edu + livingalone + work_cat + smoking +alcohol_3cat +   fruitveg_cat,  data = sample_cvd_uk)
  #Testing for the Proportional Hazards Assumption with Schoenfeld Residuals Test
  ph_test1_m <- cox.zph(Model_1)
  print(ph_test1_m)

#Model 2:
  Model_2<- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, CHD_event_UK) ~ c9  + sex + edu + livingalone + work_cat + smoking +alcohol_3cat + fruitveg_cat + CNS + multimorbidity_index ,  data = sample_cvd_uk)
  #Testing for the Proportional Hazards Assumption 
  ph_test2_m <- cox.zph(Model_2)
  print(ph_test2_m)

#Model 3:
Model_3<- coxph(Surv(age_inclusion, age_inclusion + follow_up_time_cvd_uk, CHD_event_UK) ~ c9  + sex + edu + livingalone + work_cat + smoking +alcohol_3cat + fruitveg_cat + CNS + multimorbidity_index + bmi_cat + Hypertension_UK + Diabetes_UK + HYPLIP ,  data = sample_cvd_uk)
#Testing for the Proportional Hazards Assumption 
ph_test3_m <- cox.zph(Model_3)
print(ph_test3_m)

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
