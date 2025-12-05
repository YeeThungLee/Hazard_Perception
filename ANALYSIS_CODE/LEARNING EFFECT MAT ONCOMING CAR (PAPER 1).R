# LEARNING EFFECT FOR MAT ONCOMING CAR
# LOAD LIBRARIES
library(ggplot2)
library(tidyr)
library(dplyr)
library(lme4)
library(glmmTMB)
library (emmeans)

# LOAD DATA IN (CSV FILE) - MAT ONCOMING CAR
# LOAD DATA FILES
# LOAD DATA IN (CSV FILE) 
# FILE DIRECTORY
setwd ("C:/Users/tsytl/Desktop/Submission to PlosOne/Data/")
# ONCOMING CAR MAT
oncomingCarMats <- read.table("Time to First Fixation (EVENT MAT ONCOMING CAR).csv", header = TRUE, sep=",")

#DEFINING THESE COLUMNS AS FACTORS (SUBJECT ID & INDEPENDENT VARIABLES) - ONCOMING CAR MAT
oncomingCarMats$subNumber <- factor(oncomingCarMats$subNumber)
oncomingCarMats$lightCondition <- factor(oncomingCarMats$lightCondition)
oncomingCarMats$automationState <- factor(oncomingCarMats$automationState)


#DEFINING MY DEPENDENT VARIABLES - ONCOMING CAR MAT
str(oncomingCarMats$timeToFirstFixation_seconds) 
oncomingCarMats$timeToFirstFixation_seconds <- as.numeric(oncomingCarMats$timeToFirstFixation_seconds)
str(oncomingCarMats$eventNumber_Recoded)
oncomingCarMats$eventNumber_Recoded <- as.numeric(oncomingCarMats$eventNumber_Recoded)


# FIT THE GLMM WITH EVENTNUMBER (WITH TWEEDIE FAMILY DUE TO THE POSITIVELY-SKEWED DATA WITH MEANINGFUL ZERO VALUE)
oncomingCarMats_GLMM_EventNumber_Recoded_Model <- glmmTMB (timeToFirstFixation_seconds ~ eventNumber_Recoded + (1|subNumber), family = tweedie(link = "log"), data = oncomingCarMats)

# DISPLAY THE MODEL
summary (oncomingCarMats_GLMM_EventNumber_Recoded_Model)

# EXTRACT FIXED EFFECTS COEFFICIENTS, STANDARD ERRORS, AND Z-VALUES
fixed_effects <- summary(oncomingCarMats_GLMM_EventNumber_Recoded_Model)$coefficients$cond
coef <- fixed_effects[, "Estimate"]
se <- fixed_effects[, "Std. Error"]
z_value <- fixed_effects[, "z value"]

# Round p-values to display in standard decimal format
p_value <- round(fixed_effects[, "Pr(>|z|)"], digits = 4)

# CALCULATE EXPONENTIATED COEFFICIENTS (DUE TO LOG LINK FUNCTION)
exp_coef <- exp(coef)

# CALCULATE 95% CONFIDENCE INTERVALS FOR EXP(COEFFICIENT)
ci_lower <- exp(coef - 1.96 * se)
ci_upper <- exp(coef + 1.96 * se)

# COMBINE RESULTS INTO A DATA FRAME
results <- data.frame(
  Coefficient = coef,
  `Standard Error` = se,
  `z-value` = z_value,
  `p-value` = p_value,
  `Exp(Coefficient)` = exp_coef,
  `95% CI Lower` = ci_lower,
  `95% CI Higher` = ci_upper
)

# DISPLAY THE RESULTS
print(results, digits = 4)

# LOAD DATA IN (CSV FILE) - MAT ONCOMING CAR
# MATERIALISED ONCOMING CAR
# LOAD DATA FILES
# LOAD DATA IN (CSV FILE) - ONCOMING CAR MAT
# FILE DIRECTORY
setwd ("C:/Users/tsytl/Desktop/Submission to PlosOne/Data/")
# ONCOMING CAR MAT
oncomingCarMats <- read.table("MAT ONCOMING CAR VEHICLE DATA.csv", header = TRUE, sep=",")

#DEFINING THESE COLUMNS AS FACTORS (SUBJECT ID & INDEPENDENT VARIABLES) - ONCOMING CAR MAT
oncomingCarMats$subNumber <- factor(oncomingCarMats$subNumber)
oncomingCarMats$lightCondition <- factor(oncomingCarMats$lightCondition)
oncomingCarMats$automationState <- factor(oncomingCarMats$automationState)


#DEFINING MY DEPENDENT VARIABLES - ONCOMING CAR MAT
str(oncomingCarMats$timeToFirstRelevantManeuvres) 
oncomingCarMats$timeToFirstRelevantManeuvres <- as.numeric(oncomingCarMats$timeToFirstRelevantManeuvres)
str(oncomingCarMats$eventNumber_Recoded)
oncomingCarMats$eventNumber_Recoded <- as.numeric(oncomingCarMats$eventNumber_Recoded)
str(oncomingCarMats$transformedTimeToFirstRelevantManeuvres) 
oncomingCarMats$transformedTimeToFirstRelevantManeuvres <- as.numeric(oncomingCarMats$transformedTimeToFirstRelevantManeuvres)

# REMOVE ROWS WITH NA/EMPTY VALUES IN TIME TO FIRST RELEVANT MANOEUVRES
oncomingCarMats<- oncomingCarMats %>%
  filter(!is.na(timeToFirstRelevantManeuvres))

# FIT THE GLMM WITH EVENT NUMBER RECODED
oncomingCarMats_GLMM_Model_EventNum <- glmer (transformedTimeToFirstRelevantManeuvres ~ eventNumber_Recoded + (1|subNumber), 
                                            family = Gamma (link = "log"), data = oncomingCarMats, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
# DISPLAY THE MODEL
summary (oncomingCarMats_GLMM_Model_EventNum)

# EXTRACT FIXED EFFECTS COEFFICIENTS, STANDARD ERRORS, AND T-VALUES
fixed_effects <- summary(oncomingCarMats_GLMM_Model_EventNum)$coefficients
coef <- fixed_effects[, "Estimate"]
se <- fixed_effects[, "Std. Error"]
z_value <- fixed_effects[, "t value"]

# ROUND P-VALUES TO DISPLAY IN STANDARD DECIMAL FORMAT
p_value <- round(fixed_effects[, "Pr(>|z|)"], digits = 4)

# CALCULATE EXPONENTIATED COEFFICIENTS 
exp_coef <- exp(coef)

# CALCULATE 95% CONFIDENCE INTERVALS FOR EXP
ci_lower <- exp(coef - 1.96 * se)
ci_upper <- exp(coef + 1.96 * se)

# COMBINE RESULTS INTO A DATA FRAME
results <- data.frame(
  Coefficient = coef,
  `Standard Error` = se,
  `z-value` = z_value,
  `p-value` = p_value,
  `Exp(Coefficient)` = exp_coef,
  `95% CI Lower` = ci_lower,
  `95% CI Higher` = ci_upper
)

# DISPLAY THE RESULTS
print(results, digits = 4)

#DEFINING MY DEPENDENT VARIABLES - ONCOMING CAR MAT
str(oncomingCarMats$ttc) 
oncomingCarMats$ttc <- as.numeric(oncomingCarMats$ttc)

# FIT THE GLMM WITH EVENT NUMBER RECODED
oncomingCarMats_GLMM_Model_EventNum_ttc <- lme4::glmer (ttc ~ eventNumber_Recoded + (1|subNumber), 
                                                      family = Gamma (link = "log"), data = oncomingCarMats, control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
# DISPLAY THE MODEL
summary (oncomingCarMats_GLMM_Model_EventNum_ttc)

# EXTRACT FIXED EFFECTS COEFFICIENTS, STANDARD ERRORS, AND T-VALUES
fixed_effects <- summary(oncomingCarMats_GLMM_Model_EventNum_ttc)$coefficients
coef <- fixed_effects[, "Estimate"]
se <- fixed_effects[, "Std. Error"]
z_value <- fixed_effects[, "t value"]

# ROUND P-VALUES TO DISPLAY IN STANDARD DECIMAL FORMAT
p_value <- round(fixed_effects[, "Pr(>|z|)"], digits = 4)

# CALCULATE EXPONENTIATED COEFFICIENTS 
exp_coef <- exp(coef)

# CALCULATE 95% CONFIDENCE INTERVALS FOR EXP
ci_lower <- exp(coef - 1.96 * se)
ci_upper <- exp(coef + 1.96 * se)

# COMBINE RESULTS INTO A DATA FRAME
results <- data.frame(
  Coefficient = coef,
  `Standard Error` = se,
  `z-value` = z_value,
  `p-value` = p_value,
  `Exp(Coefficient)` = exp_coef,
  `95% CI Lower` = ci_lower,
  `95% CI Higher` = ci_upper
)

# DISPLAY THE RESULTS
print(results, digits = 4)
