# lEARNING EFFECT MAT LEFT PED

# LOAD LIBRARIES
library(ggplot2)
library(tidyr)
library(dplyr)
library(lme4)
library(glmmTMB)
library (emmeans)

# LOAD DATA IN (CSV FILE) - LEFT PEDS MAT
# LOAD DATA FILES
# LOAD DATA IN (CSV FILE) 
# FILE DIRECTORY
setwd ("C:/Users/tsytl/Desktop/Submission to PlosOne/Data/")
# LEFT MAT PED
leftPedsMats <- read.table("Time To First Fixation (EVENT MAT LEFT PED).csv", header = TRUE, sep=",")

#DEFINING THESE COLUMNS AS FACTORS (SUBJECT ID & INDEPENDENT VARIABLES) - LEFT PEDS MAT
leftPedsMats$subNumber <- factor(leftPedsMats$subNumber)
leftPedsMats$lightCondition <- factor(leftPedsMats$lightCondition)
leftPedsMats$automationState <- factor(leftPedsMats$automationState)


#DEFINING MY DEPENDENT VARIABLES - LEFT PEDS MAT
str(leftPedsMats$timeToFirstFixation_seconds) 
leftPedsMats$timeToFirstFixation_seconds <- as.numeric(leftPedsMats$timeToFirstFixation_seconds)
str(leftPedsMats$eventNumber_Recoded)
leftPedsMats$eventNumber_Recoded <- as.numeric(leftPedsMats$eventNumber_Recoded)


# FIT THE GLMM WITH EVENTNUMBER (WITH TWEEDIE FAMILY DUE TO THE POSITIVELY-SKEWED DATA WITH MEANINGFUL ZERO VALUE)
leftPedsMats_GLMM_EventNumber_Recoded_Model <- glmmTMB (timeToFirstFixation_seconds ~ eventNumber_Recoded + (1|subNumber), family = tweedie(link = "log"), data = leftPedsMats)

# DISPLAY THE MODEL
summary (leftPedsMats_GLMM_EventNumber_Recoded_Model)

# EXTRACT FIXED EFFECTS COEFFICIENTS, STANDARD ERRORS, AND Z-VALUES
fixed_effects <- summary(leftPedsMats_GLMM_EventNumber_Recoded_Model)$coefficients$cond
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

# LOAD DATA IN (CSV FILE) - LEFT PEDS MAT (RERUN STATS 5 SEPTEMBER 2024 - TO REMOVE THE PERSON WHO REACTED AFTER CRASH)
# LOAD DATA FILES
# LOAD DATA IN (CSV FILE) 
# FILE DIRECTORY
setwd ("C:/Users/tsytl/Desktop/Submission to PlosOne/Data/")
# LEFT MAT PED
leftPedsMats <- read.table("MAT LEFT PED VEHICLE DATA.csv", header = TRUE, sep=",")

#DEFINING THESE COLUMNS AS FACTORS (SUBJECT ID & INDEPENDENT VARIABLES) - LEFT PEDS MAT
leftPedsMats$subNumber <- factor(leftPedsMats$subNumber)
leftPedsMats$lightCondition <- factor(leftPedsMats$lightCondition)
leftPedsMats$automationState <- factor(leftPedsMats$automationState)


#DEFINING MY DEPENDENT VARIABLES - LEFT PEDS MAT
str(leftPedsMats$timeToFirstRelevantManeuvres) 
leftPedsMats$timeToFirstRelevantManeuvres <- as.numeric(leftPedsMats$timeToFirstRelevantManeuvres)
str(leftPedsMats$eventNumber_Recoded)
leftPedsMats$eventNumber_Recoded <- as.numeric(leftPedsMats$eventNumber_Recoded)
str(leftPedsMats$transformedTimeToFirstManoevres) 
leftPedsMats$transformedTimeToFirstManoevres <- as.numeric(leftPedsMats$transformedTimeToFirstManoevres)

# REMOVE ROWS WITH NA/EMPTY VALUES IN TIME TO FIRST RELEVANT MANOEUVRES
leftPedsMats<- leftPedsMats %>%
  filter(!is.na(timeToFirstRelevantManeuvres))

# FIT THE GLMM WITH EVENT NUMBER RECODED
leftPedsMats_GLMM_Model_EventNum <- glmer (transformedTimeToFirstManoevres ~ eventNumber_Recoded + (1|subNumber), 
                                  family = Gamma (link = "log"), data = leftPedsMats)
# DISPLAY THE MODEL
summary (leftPedsMats_GLMM_Model_EventNum)

# EXTRACT FIXED EFFECTS COEFFICIENTS, STANDARD ERRORS, AND T-VALUES
fixed_effects <- summary(leftPedsMats_GLMM_Model_EventNum)$coefficients
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

#DEFINING MY DEPENDENT VARIABLES - LEFT PEDS MAT
str(leftPedsMats$ttc) 
leftPedsMats$ttc <- as.numeric(leftPedsMats$ttc)

# FIT THE GLMM WITH EVENT NUMBER RECODED
leftPedsMats_GLMM_Model_EventNum_ttc <- lme4::glmer (ttc ~ eventNumber_Recoded + (1|subNumber), 
                                           family = Gamma (link = "log"), data = leftPedsMats)
# DISPLAY THE MODEL
summary (leftPedsMats_GLMM_Model_EventNum_ttc)

# EXTRACT FIXED EFFECTS COEFFICIENTS, STANDARD ERRORS, AND T-VALUES
fixed_effects <- summary(leftPedsMats_GLMM_Model_EventNum_ttc)$coefficients
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


