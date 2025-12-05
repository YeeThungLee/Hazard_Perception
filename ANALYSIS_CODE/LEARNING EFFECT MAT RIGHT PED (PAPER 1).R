# LEARNING EFFECT FOR MAT RIGHT PED EVENT

# LOAD LIBRARIES
library(ggplot2)
library(tidyr)
library(dplyr)
library(lme4)
library(glmmTMB)
library (emmeans)

# MATERIALISED RIGHT PEDESTRIAN
# LOAD DATA FILES
# LOAD DATA IN (CSV FILE) 
# FILE DIRECTORY
setwd ("C:/Users/tsytl/Desktop/Submission to PlosOne/Data/")
# RIGHT MAT PED
rightPedsMats <- read.table("Time to First Fixation (EVENT MAT RIGHT PED).csv", header = TRUE, sep=",")

#DEFINING THESE COLUMNS AS FACTORS (SUBJECT ID & INDEPENDENT VARIABLES) - RIGHT PEDS MAT
rightPedsMats$subNumber <- factor(rightPedsMats$subNumber)
rightPedsMats$lightCondition <- factor(rightPedsMats$lightCondition)
rightPedsMats$automationState <- factor(rightPedsMats$automationState)


#DEFINING MY DEPENDENT VARIABLES - RIGHT PEDS MAT
str(rightPedsMats$timeToFirstFixation_seconds) 
rightPedsMats$timeToFirstFixation_seconds <- as.numeric(rightPedsMats$timeToFirstFixation_seconds)
str(rightPedsMats$eventNumber_Recoded)
rightPedsMats$eventNumber_Recoded <- as.numeric(rightPedsMats$eventNumber_Recoded)

# FIT THE GLMM WITH EVENTNUMBER (WITH TWEEDIE FAMILY DUE TO THE POSITIVELY-SKEWED DATA WITH MEANINGFUL ZERO VALUE)
rightPedsMats_GLMM_EventNumber_Recoded_Model <- glmmTMB (timeToFirstFixation_seconds ~ eventNumber_Recoded + (1|subNumber), family = tweedie(link = "log"), data = rightPedsMats)

# DISPLAY THE MODEL
summary (rightPedsMats_GLMM_EventNumber_Recoded_Model)

# EXTRACT FIXED EFFECTS COEFFICIENTS, STANDARD ERRORS, AND Z-VALUES
fixed_effects <- summary(rightPedsMats_GLMM_EventNumber_Recoded_Model)$coefficients$cond
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


# MATERIALISED RIGHT PEDESTRIAN
# LOAD DATA FILES
# LOAD DATA IN (CSV FILE) - RIGHT PEDS MAT - 5 SEPTEMBER 2024 (REMVOE THE PARTICIPANT WHO HAVE TTC = 0)
# FILE DIRECTORY
setwd ("C:/Users/tsytl/Desktop/Submission to PlosOne/Data/")
# RIGHT MAT PED
rightPedsMats <- read.table("MAT RIGHT PED VEHICLE DATA.csv", header = TRUE, sep=",")

#DEFINING THESE COLUMNS AS FACTORS (SUBJECT ID & INDEPENDENT VARIABLES) - RIGHT PEDS MAT
rightPedsMats$subNumber <- factor(rightPedsMats$subNumber)
rightPedsMats$lightCondition <- factor(rightPedsMats$lightCondition)
rightPedsMats$automationState <- factor(rightPedsMats$automationState)


#DEFINING MY DEPENDENT VARIABLES - RIGHT PEDS MAT
str(rightPedsMats$timeToFirstRelevantManeuvres) 
rightPedsMats$timeToFirstRelevantManeuvres <- as.numeric(rightPedsMats$timeToFirstRelevantManeuvres)
str(rightPedsMats$eventNumber_Recoded)
rightPedsMats$eventNumber_Recoded <- as.numeric(rightPedsMats$eventNumber_Recoded)
str(rightPedsMats$transformedTimeToFirstManoevres) 
rightPedsMats$transformedTimeToFirstManoevres <- as.numeric(rightPedsMats$transformedTimeToFirstManoevres)

# REMOVE ROWS WITH NA/EMPTY VALUES IN TIME TO FIRST RELEVANT MANOEUVRES
rightPedsMats<- rightPedsMats %>%
  filter(!is.na(timeToFirstRelevantManeuvres))


# FIT THE GLMM WITH EVENT NUMBER RECODED
rightPedsMats_GLMM_Model_EventNum <- glmer (transformedTimeToFirstManoevres ~ eventNumber_Recoded + (1|subNumber), 
                                           family = Gamma (link = "log"), data = rightPedsMats)
# DISPLAY THE MODEL
summary (rightPedsMats_GLMM_Model_EventNum)

# EXTRACT FIXED EFFECTS COEFFICIENTS, STANDARD ERRORS, AND T-VALUES
fixed_effects <- summary(rightPedsMats_GLMM_Model_EventNum)$coefficients
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

#DEFINING MY DEPENDENT VARIABLES - RIGHT PEDS MAT
str(rightPedsMats$ttc) 
rightPedsMats$ttc <- as.numeric(rightPedsMats$ttc)

# FIT THE GLMM WITH EVENT NUMBER RECODED
rightPedsMats_GLMM_Model_EventNum_ttc <- lme4::glmer (ttc ~ eventNumber_Recoded + (1|subNumber), 
                                                     family = Gamma (link = "log"), data = rightPedsMats)
# DISPLAY THE MODEL
summary (rightPedsMats_GLMM_Model_EventNum_ttc)

# EXTRACT FIXED EFFECTS COEFFICIENTS, STANDARD ERRORS, AND T-VALUES
fixed_effects <- summary(rightPedsMats_GLMM_Model_EventNum_ttc)$coefficients
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

