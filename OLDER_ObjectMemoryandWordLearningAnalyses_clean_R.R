#### Full Analysis Script for Object Memory and Word Learning 
#### The Relation between Visual Paired Associates (VPA) performance and vocabulary (OCDI) 
#### 26/10/25 ######################################################### 
#### Analyses and plots follow the order presented in the manuscript. After running the
#### first section that imports the data and sets up the initial structures it is possible 
#### to jump to later analyses. 

#### Script created in RStudio 2024.12.1+563 (2024.12.1+563)
#### Running R version 4.4.3 (2025-02-28) -- "Trophy Case"


# Load Necessary Libraries ------------------------------------------------
library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2); theme_set(theme_classic(base_size = 14))
library(eyetrackingR)
library(lme4)
library(glmmTMB)
library(MASS)
library(forcats)
library(lubridate)
library(performance)
library(patchwork)
library(ggpubr)
library(car)
library(here)

# Import data and set up initial data structures --------------------------

#### This will set up the initial data set that is the basis for all the data sets used
#### in the analyses. It combines the coded looking data and an excel file of participant
#### information.


#### Import the looking VPA data
#### ################################# See README.md for more information
data1_Full <- readRDS(here("data", "data1_Full.rds"))


#### Label trial types
data2 <- data1_Full %>%
  mutate(Type_of_Trial = case_when(
    familiar_novel == 'N' & random == 'Random' ~ 'Novel_Random',
    familiar_novel == 'F' & random == 'Random' ~ 'Familiar_Random',
    familiar_novel == 'N' & random == 'Regular' ~ 'Novel_Associated',
    familiar_novel == 'F' & random == 'Regular' ~ 'Familiar_Associated',
    TRUE ~ NA_character_
  ),
  Fam_Novel = case_when(
    Type_of_Trial %in% c("Novel_Random", "Novel_Associated") ~ "Novel",
    Type_of_Trial %in% c("Familiar_Random", "Familiar_Associated") ~ "Familiar",
    TRUE ~ "0"
  ),
  Random_Associated = case_when(
    Type_of_Trial %in% c("Novel_Random", "Familiar_Random") ~ "Random",
    Type_of_Trial %in% c("Novel_Associated", "Familiar_Associated") ~ "Associated",
    TRUE ~ "0"
  ))


#### Read in excel with participant demographics info and vocabulary data
VPAR_Part_sheet_percentiles <-read_csv(here("data", "VPAR_Part_sheet_percentiles.csv"))


#### Get values for participants section
# Number of participants
length(unique(VPAR_Part_sheet_percentiles$Final_ID)) #52

# Sex
table(VPAR_Part_sheet_percentiles$Gender) #F26, M 26

# Age range
min(VPAR_Part_sheet_percentiles$Age_months) #14
max(VPAR_Part_sheet_percentiles$Age_months) #24


#### Filter files for NAs
data3 <- data2 %>% filter(!is.na(TRIAL_TIME))
data4 <- data3 %>% filter(Final_ID != 'NA')


#### Merge VPA and demographic/vocab data
data5 <- left_join(data4, VPAR_Part_sheet_percentiles, by = c("Final_ID", "OCDI_S","Gender","Age_days","Age_months"))


#### Create eye tracking data
Etlooking_data  <- make_eyetrackingr_data(
  data5,
  participant_column = "Final_ID",
  trial_column = "trial_unique",
  time_column = "TRIAL_TIME2",
  trackloss_column = "trackloss",
  aoi_columns = c("area_t","area_d"),
  treat_non_aoi_looks_as_missing = TRUE,
  item_columns = c("Age_days","Age_months", "block", "Gender", "version", "Random_Associated", "Fam_Novel")
)

# Sanity check - should still have 52 participants at this point
length(unique(Etlooking_data$Final_ID))


#### Create time window for test period
response_window <- subset_by_window(Etlooking_data, window_start_time = 0, window_end_time = 3000)


# Get descriptive data for the full sample --------------------------------

# Create data frame with one row per participant 
Descriptives_sample <-response_window %>% 
  dplyr::group_by(Final_ID,OCDI_S,Age_months,Gender,Quartiles2_S_June2023) %>% 
  dplyr::summarise(Final_ID=first(Final_ID))

# Change variables to correct type
Descriptives_sample$Age_months2<- as.character(Descriptives_sample$Age_months)
Descriptives_sample$Age_months2<- as.numeric(Descriptives_sample$Age_months2)
Descriptives_sample$OCDI_S2<- as.character(Descriptives_sample$OCDI_S)
Descriptives_sample$OCDI_S2<- as.numeric(Descriptives_sample$OCDI_S2)

# Descriptive statistics - productive vocabulary
mean(Descriptives_sample$OCDI_S2, na.rm = TRUE) #150.9318
min(Descriptives_sample$OCDI_S2, na.rm = TRUE) #2
max(Descriptives_sample$OCDI_S2, na.rm = TRUE) #387
sd(Descriptives_sample$OCDI_S2, na.rm = TRUE) #125.0015
median(Descriptives_sample$OCDI_S2, na.rm = TRUE) #120

# Descriptive statistics - Age
median(Descriptives_sample$Age_months2) #20.5
min(Descriptives_sample$Age_months2) #14
max(Descriptives_sample$Age_months2) #24
sd(Descriptives_sample$Age_months2) #2.63
mean(Descriptives_sample$Age_months2) #20.17308


# Descriptive statistics - Sex
table(Descriptives_sample$Gender) #F19 M25


# Get information about trials completed ----------------------------------

#### Check trial count before track loss
# Get total number of trials for each trial type for each participant
one <-describe_data(response_window , describe_column = "area_t", group_columns = c("Final_ID","familiar_novel", "Random_Associated","Age_months","OCDI_S"))

# Sum total number of trials before track loss = 1965
sum(one$NumTrials)

### How many trials (of 48) did participants complete?
part_trial_count <- one %>%
  group_by(Final_ID) %>%
  summarize(Count = sum(NumTrials), .groups = 'drop')

count_part_trial_count<- as.data.frame(table(part_trial_count$Count))
colnames(count_part_trial_count) <- c("Number_of_Trials", "Number_of_Participants")

# data distribution from count_part_trial_count:
# 7 13 19 20 22 24 26 29 31 32 34 35 36 38 39 41 42 46 48 
# 1  2  2  1  1  3  1  1  3  1  2  1  3  1  2  1  2  2 22 

length(unique(part_trial_count$Final_ID)) #52

# create dataframe to get correlations
one_unique <- one %>%
  group_by(Final_ID) %>%
  summarize(TrialCount = sum(NumTrials),
            Age_months = first(Age_months),  # or mean(), max(), etc.
            Age_days = first(Age_days),
            OCDI_S = first(OCDI_S),
            .groups = 'drop')

## Correlation between age and number of trials completed
## Changing age to be numeric rather than a factor to run the correlation
one_unique$Age_days<- as.character(one_unique$Age_days) 
one_unique$Age_days<- as.numeric(one_unique$Age_days)

ggscatter(one_unique, x = "Age_days", y = "TrialCount", 
          add = "reg.line", conf.int = F, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Age in Days", ylab = "Number of Trials completed")

cor.test(one_unique$TrialCount, one_unique$Age_days, 
         method = "pearson")
# data:  one_unique$TrialCount and one_unique$Age_days
# t = 0.96101, df = 50, p-value = 0.3412
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.1435046  0.3931222
# sample estimates:
#   cor 
# 0.1346697 

## Correlation between OCDI says and number of trials completed
## Changing age to be numeric rather than a factor to run the correlation
one_unique$OCDI_S<- as.character(one_unique$OCDI_S)
one_unique$OCDI_S<- as.numeric(one_unique$OCDI_S)

ggscatter(one_unique, x = "OCDI_S", y = "TrialCount", 
          add = "reg.line", conf.int = F, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Productive vocabulary", ylab = "Number of Trials completed")

cor.test(one_unique$TrialCount, one_unique$OCDI_S, 
         method = "pearson")
# Pearson's product-moment correlation
# 
# data:  one_unique$TrialCount and one_unique$OCDI_S
# t = 0.40511, df = 49, p-value = 0.6872
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.2213317  0.3281351
# sample estimates:
#        cor 
# 0.05777601 

## Correlation between OCDI_S and Age 
ggscatter(one_unique, x = "Age_days", y = "OCDI_S", 
          add = "reg.line", conf.int = F, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Dayss of Age", ylab = "OCDI Says")

cor.test(one_unique$OCDI_S, one_unique$Age_days, 
         method = "pearson")
# Pearson's product-moment correlation
# 
# data:  one_unique$OCDI_S and one_unique$Age_days
# t = 5.5354, df = 49, p-value = 1.206e-06
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.4157585 0.7650771
# sample estimates:
#       cor 
# 0.6202736 


# Clean the Data --------------------------------------------------------------

#### Remove trials in which track loss exceeds 75%
response_window_clean <- clean_by_trackloss(response_window,trial_prop_thresh = .75)

#### Check trial count after track loss
# Get total number of trials for each trial type for each participant
one1 <-describe_data(response_window_clean, describe_column = "area_t", group_columns = c("Final_ID","familiar_novel", "Random_Associated"))

# Sum total number of trials after track loss = 1072; 45% of trials lost
sum(one1$NumTrials) 

#### Remove participants that do not have at least one trial per trial type
#### these are numbers after track loss
response_window_clean_filtered1<-response_window_clean %>% 
  filter(!Final_ID == "16VPARXX060G",# only 1 remaining trial
         !Final_ID == "16VPARXX049B",# 6 remaining trials (2AF, 1AN, 3RF), missing RN
         !Final_ID == "18VPARXX023G",# 6 remaining trials (2AF, 1AN, 3RF), missing RN
         !Final_ID == "19VPARXX037G",# 7 remaining trials (3AF, 2RF, 2RN), missing AN 
         !Final_ID == "21VPARXX048G",# 6 remaining trials (1AF, 4AN, 1RF), missing RN
         !Final_ID == "22VPARXX044G")# 1 remaining trial (AF)

# Check number of participants 
length(unique(response_window_clean_filtered1$Final_ID)) 
#N = 45 - 1 participant lost for track loss + 6 participants lost for trial types


#### Import information about the objects that were presented on each trial to check animacy issue
TRIALS <- read_csv(here("data", "TRIALS_VPAR_CH3.csv"))

#### Join the animacy information to the main data frame
# Fixing a typo in version number
response_window_clean_filtered1 <- response_window_clean_filtered1 %>%
  mutate(version = if_else(trial_unique == "3_28" & str_detect(version, "12"), "2", version))

# Make sure variables types match to allow join
response_window_clean_filtered1$version<-as.character(response_window_clean_filtered1$version)
TRIALS$version<-as.character(TRIALS$version)

# Join data sets
response_window_clean_filtered2<- left_join(response_window_clean_filtered1, TRIALS, by=c("trial_unique","version"))

# Create new column combining version and trial_unique info
response_window_clean_filtered2$v_trial_new <- paste(response_window_clean_filtered2$version,response_window_clean_filtered2$trial_unique,sep="_")


#### Remove rows where participant was not looking in any AOI
response_window_clean_filtered4 <- response_window_clean_filtered2 %>% 
  filter(area!= "NA")


#### Remove participant with no OCDI data (16VPARXX061G)
response_window_clean_filtered4 <-response_window_clean_filtered4  %>% 
  filter(OCDI_S!= "NA")

# Sanity check - participant count
length(unique(response_window_clean_filtered4$Final_ID))# N = 44

## Check number of trials remaining 
# Get total number of trials for each trial type for each participant
one2 <-describe_data(response_window_clean_filtered4, describe_column = "area_t", group_columns = c("Final_ID","familiar_novel", "Random_Associated","StimChar"))

# Sum total number of trials after remaining = 1019 
sum(one2$NumTrials) 



#### Prior research shows that children attend to pictures of animate objects more than inanimate objects.
#### Our trials sometimes mixed animate and inanimate objects. 
#### This next section counts the number of trials that have just animate, just inanimate, and mixed animacy, 
#### and whether the animate object was the target or distractor. We will include this factor in our analysis,
#### but because we only had 8 familiar trials where both the target and distractor were animate,
#### we decided to remove these from the analysis as we would not have enough power to interpret results in this cell.  

### Get number of trials with pictures of animate objects
trial_counts <- one2 %>%
  group_by(StimChar) %>%
  summarize(Count = sum(NumTrials), .groups = 'drop')

### Remove trials where both target and distractor were animate
response_window_clean_filtered4_f<-response_window_clean_filtered4 %>% 
  filter(StimChar!="aBoth_f") # removes 8 trials

# Sanity check - to make sure removing these trials does not remove participants
length(unique(response_window_clean_filtered4_f$Final_ID))# N = 44

#### Creating time course data
time_course_FULL_f <- make_time_sequence_data(response_window_clean_filtered4_f,
                                              time_bin_size = 100,
                                              aois = c("area_t"),
                                              predictor_columns = c('OCDI_S', 'Gender', "Random_Associated", "Fam_Novel","Age_months","Age_days","Quartiles2_S_June2023","percentiles_S_June2023","OCDI_S_2_10","ANIMATE_","StimChar", "Type_of_Trial"),
                                              summarize_by = 'Final_ID')



# Get descriptives data for the analysed data -----------------------------

# Create data frame with one row per participant 
Descriptives <-time_course_FULL_f %>% 
  dplyr::group_by(Final_ID,OCDI_S,Age_months,Gender,Quartiles2_S_June2023) %>% 
  dplyr::summarise(Final_ID=first(Final_ID))

# Change variables to correct type
Descriptives$Age_months2<- as.character(Descriptives$Age_months)
Descriptives$Age_months2<- as.numeric(Descriptives$Age_months2)

# Descriptive statistics - productive vocabulary
mean(Descriptives$OCDI_S) #150.9318
min(Descriptives$OCDI_S) #2
max(Descriptives$OCDI_S) #365
sd(Descriptives$OCDI_S) #122.3733
median(Descriptives$OCDI_S) #123

# Descriptive statistics - Age
median(Descriptives$Age_months2) #21
min(Descriptives$Age_months2) #14
max(Descriptives$Age_months2) #24

# Descriptive statistics - Sex
table(Descriptives$Gender) #F19 M25


#### Create Productive Vocabulary groups - median split
Descriptives<- Descriptives %>%
  mutate(OCDI_VGrp =ifelse(OCDI_S > 123,"High","Low"))

# Check groups against percentile groupings (over or under 25th percentile)
table(Descriptives$OCDI_VGrp,Descriptives$Quartiles2_S_June2023)
#      <25 >25
#High   0  22
#Low    8  14


# Get area_t descriptive stats and total number of trials for each trial type for each participant submitted to analysis
Descriptives2 <-describe_data(response_window_clean_filtered4_f, describe_column = "area_t", group_columns = c("Final_ID","OCDI_S","Age_months","Type_of_Trial","Random_Associated","Fam_Novel"))

# Sum total number of trials completed and submitted to analysis
sum(Descriptives2$NumTrials) #1011

# Summary of number of trials completed across all trial types
summary(Descriptives2$NumTrials)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   4.000   6.000   5.744   7.000  12.000 

# Mean and range for different trial types
# 'Table 1 : Mean number and range across participants of the four trial types included in analyses.'
Descriptives3  <- Descriptives2  %>% 
  group_by(Type_of_Trial) %>%
  summarise(Mean_trials = mean(NumTrials),
            Range_trials =range(NumTrials)) %>%
  ungroup()

#### Examining Task engagement 
#### Check if number of trials completed is significantly different across trial types 
# Standardize variable types
Descriptives2$Age_months2<- as.character(Descriptives2$Age_months)
Descriptives2$Age_months2<- as.numeric(Descriptives2$Age_months2)


# Run model
Descriptives2$Type_of_Trial <-as.factor(Descriptives2$Type_of_Trial)
ModelTrials2 <-lm(NumTrials ~ Type_of_Trial, data = Descriptives2)
summary(ModelTrials2)

# Correlation Analysis for data submitted to analysis -----------------------------------------

#### Create data frame for analysis 
#### Get total number of trials for each participant in the data submitted for analysis
Descriptives1 <-describe_data(response_window_clean_filtered4_f, describe_column = "area_t", group_columns = c("Final_ID","OCDI_S","Age_months", "percentiles_S_June2023"))


#### Correlation between trials completed and productive vocabulary
ggscatter(Descriptives1, x =  "OCDI_S", y="NumTrials",
          add = "reg.line", conf.int = F, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "OCDI (Number of words)", ylab = "Number of Trials")


#### Correlation between trials completed and age in months
Descriptives1$Age_months <- as.character(Descriptives1$Age_months)
Descriptives1$Age_months <- as.numeric(Descriptives1$Age_months)
ggscatter(Descriptives1, x =  "Age_months", y="NumTrials",
          add = "reg.line", conf.int = F, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Age in months", ylab = "Number of Trials")


#### Correlation between trials completed and productive vocabulary percentiles
ggscatter(Descriptives1, x =  "percentiles_S_June2023", y="NumTrials",
          add = "reg.line", conf.int = F, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Vocabulary Percentile", ylab = "Number of Trials")


#### Correlation between productive vocabulary and age 
ggscatter(Descriptives, x = "Age_months2", y = "OCDI_S", 
          add = "reg.line", conf.int = F, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Age in months", ylab = "OCDI (Number of words)")+ 
          labs(x = 'Months of Age', y = 'Number of Nouns Produced') 

cor.test(Descriptives$OCDI_S, Descriptives$Age_months2, 
         method = "pearson")
# t = 5.1351, df = 42, p-value = 6.858e-06

# Analysis of Looking Time Course -----------------------------------------

#### Prepare variables for time course analysis - Scale and center
time_course_FULL_f$OCDI_S_sc<- scale(time_course_FULL_f$OCDI_S, center=TRUE, scale=TRUE)
time_course_FULL_f$percentiles_S_June2023_sc  <- scale(time_course_FULL_f$percentiles_S_June2023, center=TRUE, scale=TRUE)
time_course_FULL_f$OCDI_S_2_10_sc  <- scale(time_course_FULL_f$OCDI_S_2_10, center=TRUE, scale=TRUE)

time_course_FULL_f$Random_Associated<- factor((time_course_FULL_f$Random_Associated), levels=c("Random","Associated"))
time_course_FULL_f$StimChar <- as.factor(time_course_FULL_f$StimChar)

time_course_FULL_f$Fam_Novel_s <-
  ifelse(time_course_FULL_f$Fam_Novel == 'Familiar' , 0.5,
         ifelse(time_course_FULL_f$Fam_Novel == 'Novel', -0.5, NA))

time_course_FULL_f$Age_months <- as.character(time_course_FULL_f$Age_months)
time_course_FULL_f$Age_months<- as.numeric(time_course_FULL_f$Age_months)
time_course_FULL_f$Age_months_sc<- scale(time_course_FULL_f$Age_months, center=TRUE, scale=TRUE)


#### Model comparison - different Main effects/variables - OCDI, Age, Percentiles

### OCDI MODEL: Full model with intercepts for random/associated and trial animacy nested in participants and OCDI in FE
Model1_OCDI<- glmmTMB(cbind(SamplesInAOI, SamplesTotal - SamplesInAOI) ~  
                        (ot1 + ot2 + ot3+ot4) * Random_Associated * Fam_Novel_s * OCDI_S_sc  +
                        (ot1 + ot2 + ot3+ot4+Random_Associated|Final_ID:StimChar),
                      family = betabinomial,
                      data = time_course_FULL_f)

DHARMa::simulateResiduals(Model1_OCDI, plot = T)
performance::check_model(Model1_OCDI)


### AGE MODEL: Full model with intercepts for random/associated and trial animacy nested in participants and Age in FE
Model1_Age <- glmmTMB(cbind(SamplesInAOI, SamplesTotal - SamplesInAOI) ~ 
                        (ot1 + ot2 + ot3+ot4) * Random_Associated * Fam_Novel_s * Age_months_sc +
                        (ot1 + ot2 + ot3+ot4+Random_Associated|Final_ID: StimChar),
                      family = betabinomial,
                      data = time_course_FULL_f)

DHARMa::simulateResiduals(Model1_Age , plot = T)
performance::check_model(Model1_Age )


### Compare OCDI and Age models
anova(Model1_OCDI, Model1_Age)
#            Df   AIC   BIC logLik deviance Chisq Chi Df Pr(>Chisq)
#Model1_OCDI 62 62619 63056 -31248    62495                        
#Model1_Age  62 62623 63060 -31250    62499     0      0          1
performance::test_performance(Model1_OCDI, Model1_Age)


### PERCENTILES MODEL 1: Full model with intercepts for random/associated and trial animacy nested in participants and Percentiles in FE
Model1_P<- glmmTMB(cbind(SamplesInAOI, SamplesTotal - SamplesInAOI) ~  
                           (ot1 + ot2 + ot3+ot4) * Random_Associated * Fam_Novel_s * percentiles_S_June2023_sc +
                           (ot1 + ot2 + ot3+ot4+Random_Associated|Final_ID:StimChar),
                            family = betabinomial,
                            data = time_course_FULL_f)

DHARMa::simulateResiduals(Model1_P, plot = T)
performance::check_model(Model1_P)


### Compare OCDI and Percentile models
anova(Model1_OCDI, Model1_P)
#            Df   AIC   BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
#Model1_OCDI 62 62619 63056 -31248    62495                             
#Model1_P    62 62584 63021 -31230    62460 35.078      0  < 2.2e-16 ***
performance::test_performance(Model1_OCDI, Model1_P)



#### Model comparison - Percentile with different fixed effects structures

### PERCENTILES MODEL 2: with 2way interactions
Model2_P<- glmmTMB(cbind(SamplesInAOI, SamplesTotal - SamplesInAOI) ~  
                     (ot1 + ot2 + ot3+ot4) * Random_Associated * Fam_Novel_s +
                     (ot1 + ot2 + ot3+ot4) * Random_Associated * percentiles_S_June2023_sc +
                     (ot1 + ot2 + ot3+ot4) * Fam_Novel_s * percentiles_S_June2023_sc +
                     (ot1 + ot2 + ot3+ot4+Random_Associated|Final_ID:StimChar),
                   family = betabinomial,
                   data = time_course_FULL_f)

DHARMa::simulateResiduals(Model2_P, plot = T)
performance::check_model(Model2_P)

### Compare percentile Models 1 and 2
anova(Model1_P, Model2_P)
#         Df   AIC   BIC logLik deviance  Chisq Chi Df Pr(>Chisq)  
#Model2_P 57 62588 62989 -31237    62474                           
#Model1_P 62 62584 63021 -31230    62460 13.611      5    0.01828 *
performance::test_performance(Model1_P, Model2_P)


### PERCENTILES MODEL 3: with ME of percentiles
Model3_P<- glmmTMB(cbind(SamplesInAOI, SamplesTotal - SamplesInAOI) ~  
                     (ot1 + ot2 + ot3+ot4) * Random_Associated * Fam_Novel_s +
                     (ot1 + ot2 + ot3+ot4) *percentiles_S_June2023_sc +
                     (ot1 + ot2 + ot3+ot4+Random_Associated|Final_ID:StimChar),
                   family = betabinomial,
                   data = time_course_FULL_f)

DHARMa::simulateResiduals(Model3_P, plot = T)
performance::check_model(Model3_P)


### Compare Models 1 and 3
anova(Model1_P, Model3_P)
#         Df   AIC   BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
#Model3_P 47 62614 62945 -31260    62520                             
#Model1_P 62 62584 63021 -31230    62460 59.653     15  2.893e-07 ***
performance::test_performance(Model1_P,Model3_P)


### PERCENTILES MODEL 4: Check if percentiles is necessary
Model4_P<- glmmTMB(cbind(SamplesInAOI, SamplesTotal - SamplesInAOI) ~  
                     (ot1 + ot2 + ot3+ot4) * Random_Associated * Fam_Novel_s +
                     (ot1 + ot2 + ot3+ot4+Random_Associated|Final_ID:StimChar),
                   family = betabinomial,
                   data = time_course_FULL_f)

DHARMa::simulateResiduals(Model4_P, plot = T)
performance::check_model(Model4_P)


### Compare models 1 and 4
anova(Model1_P, Model4_P)
#         Df   AIC   BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
#Model4_P 42 62621 62917 -31268    62537                             
#Model1_P 62 62584 63021 -31230    62460 76.666     20  1.434e-08 ***
performance::test_performance(Model1_P, Model4_P)

# Above suggests Model1_P with just percentiles is best, and that percentiles
# is thus the best predictor of performance. But to double-check that 
# age doesn't add (given that we already showed percentiles model was slightly better than age)
# add age as a co-variate:

Model1_P_AgeCoV<- glmmTMB(cbind(SamplesInAOI, SamplesTotal - SamplesInAOI) ~  
                     (ot1 + ot2 + ot3+ot4) * Random_Associated * Fam_Novel_s * percentiles_S_June2023_sc + Age_months_sc +
                     (ot1 + ot2 + ot3+ot4+Random_Associated|Final_ID:StimChar),
                   family = betabinomial,
                   data = time_course_FULL_f)

DHARMa::simulateResiduals(Model1_P_AgeCoV , plot = T) # no improvement
performance::check_model(Model1_P_AgeCoV ) # no improvement 

summary (Model1_P_AgeCoV)

performance::test_performance(Model1_P, Model1_P_AgeCoV)
# # Name            |   Model |    BF | df | df_diff | Chi2 |     p
# # model comparison with Age control---------------------------------------------------------------
# #   Model1_P        | glmmTMB |       | 62 |         |      |      
# #   Model1_P_AgeCoV | glmmTMB | 0.026 | 63 |    1.00 | 1.71 | 0.191
# # Models were detected as nested (in terms of fixed parameters) and are compared in sequential order.
# 
# # No significant improvement with adding age.
# # The likelihood ratio test shows no significant improvement when adding age
# # and the Bayes Factor strongly favors the simpler model (Model1_P without age) as the Bayes Factor under 1 
# # suggests the simpler model is better and the 0.026 says the data are about 38 times more likely 
# # under the model without age.

#### Summary of Final Percentile Model - PERCENTILES MODEL 1
#### Table in supplementary materials 
summary(Model1_P)
#     AIC      BIC   logLik deviance df.resid 
#  62584.1  63021.1 -31230.1  62460.1     8435 


# Create Data Figure ------------------------------------------------------

#### R plot with 3 percentile groups
time_course_FULL_f<- time_course_FULL_f %>%
  mutate(PercentGrp =ifelse(percentiles_S_June2023 < 25,"< 25th Percentile",
                          if_else(percentiles_S_June2023 >= 25 & percentiles_S_June2023 < 75, "25th - 75th Percentile", "> 75th Percentile")),
  predictM1 = predict(Model1_P, type = "response"))

#### Reorder the levels for the plot
time_course_FULL_f$PercentGrp<- factor((time_course_FULL_f$PercentGrp), levels=c("< 25th Percentile","25th - 75th Percentile", "> 75th Percentile"))


#### 'Figure 2:  Proportion looking to the target over the response period for trials with familiar
#### (top row) and novel (bottom row) objects, following associated (green) and random (grey)
#### probes. Data are divided into groups of late talking, typical and high vocabulary children,
#### for visualization purposes. Lines are model predictions.'
Plotw3grps <- time_course_FULL_f %>% 
  ggplot(aes(y = Prop, x = Time,colour = Random_Associated, fill = Random_Associated)) + #, color = type2
  stat_summary(geom = "pointrange", fun.data = "mean_se", size = .2) +
  coord_cartesian(ylim = c(0, 1))+xlab('Time in test trial')+
  stat_summary(aes(y=predictM1), geom= "line", fun = "mean", linewidth =1) + 
  ylab('Proportion looking to target')+
  geom_hline(yintercept = 0.5, linetype = 2, colour = "black")+
  scale_color_manual(values=c("grey29","green4"))+
  scale_fill_manual(values=c("grey29","green4"))+
  facet_wrap(~Fam_Novel+PercentGrp,  nrow = 2)+
  theme(legend.position = "bottom", legend.direction = "horizontal",
    legend.text = element_text(size = 8), legend.title = element_blank(),
    legend.key.size = unit(0.8, "cm"), legend.key.width = unit(1.2, "cm"), legend.key.height = unit(.1, "cm"),
    plot.margin = margin(5, 20, 10, 5, "pt"), legend.margin = margin(0, 0, 0, 0, "pt"),
    legend.box.margin = margin(-5, 0, 0, 0, "pt"))
Plotw3grps


# Descriptive statistics for the percentile groups
# 'Table 3: Descriptive statistics for vocabulary percentile groupings used for visualization in Figure 2.' 
DescriptivesP <-time_course_FULL_f %>% 
  dplyr::group_by(Final_ID,OCDI_S, Age_months,Gender,percentiles_S_June2023,PercentGrp) %>% 
  dplyr::summarise(Final_ID=first(Final_ID))

DescriptivesP2  <- DescriptivesP  %>% 
  group_by(PercentGrp) %>%
  summarise(Mean_age = mean(Age_months),
            Age_range  =range(Age_months),
            Mean_PV = mean(OCDI_S),
            PV_range = range(OCDI_S),
            N_females = sum(Gender == "Female"), 
            N_total = n())%>%
  ungroup()
DescriptivesP2

# check finding with percent groups (rather than continuous percentile variable)
Model1_P_groups<- glmmTMB(cbind(SamplesInAOI, SamplesTotal - SamplesInAOI) ~  
                     (ot1 + ot2 + ot3+ot4) * Random_Associated * Fam_Novel_s * PercentGrp +
                     (ot1 + ot2 + ot3+ot4+Random_Associated|Final_ID:StimChar),
                   family = betabinomial,
                   data = time_course_FULL_f)

DHARMa::simulateResiduals(Model1_P_groups, plot = T)
performance::check_model(Model1_P_groups)

summary(Model1_P_groups) 
# timecourse components and typical and high vocabulary groups show several interactions,
# confirming findings of model with continuous variable. 






