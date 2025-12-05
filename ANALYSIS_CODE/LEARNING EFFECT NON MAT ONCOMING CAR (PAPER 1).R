# LEARNING EFFECT FOR NON-MAT ONCOMING CAR

# LOAD LIBRARIES
library(ggplot2)
library(tidyr)
library(dplyr)
library(lme4)
library(glmmTMB)
library (emmeans)

# NON-MATERIALISED ONCOMING CAR
# LOAD DATA FILES
# LOAD DATA IN (CSV FILE) 
# FILE DIRECTORY
setwd ("C:/Users/tsytl/Desktop/Submission to PlosOne/Data/")
# NON-MAT ONCOMING CAR
oncomingCarNonMat <- read.table("Time to First Fixation (EVENT NON MAT ONCOMING CAR).csv", header = TRUE, sep=",")

#DEFINING THESE COLUMNS AS FACTORS (SUBJECT ID & INDEPENDENT VARIABLES) - NON MAT ONCOMING CAR
oncomingCarNonMat$subNumber <- factor(oncomingCarNonMat$subNumber)
oncomingCarNonMat$lightCondition <- factor(oncomingCarNonMat$lightCondition)
oncomingCarNonMat$automationState <- factor(oncomingCarNonMat$automationState)


#DEFINING MY DEPENDENT VARIABLES - NON MAT ONCOMING CAR
str(oncomingCarNonMat$timeToFirstFixation_seconds) 
oncomingCarNonMat$timeToFirstFixation_seconds <- as.numeric(oncomingCarNonMat$timeToFirstFixation_seconds)
str(oncomingCarNonMat$eventNumber_Recoded)
oncomingCarNonMat$eventNumber_Recoded <- as.numeric(oncomingCarNonMat$eventNumber_Recoded)

# FIT THE GLMM WITH EVENTNUMBER (WITH TWEEDIE FAMILY DUE TO THE POSITIVELY-SKEWED DATA WITH MEANINGFUL ZERO VALUE)
oncomingCarNonMat_GLMM_EventNumber_Recoded_Model <- glmmTMB (timeToFirstFixation_seconds ~ eventNumber_Recoded + (1|subNumber), family = tweedie(link = "log"), data = oncomingCarNonMat)

# DISPLAY THE MODEL
summary (oncomingCarNonMat_GLMM_EventNumber_Recoded_Model)

# EXTRACT FIXED EFFECTS COEFFICIENTS, STANDARD ERRORS, AND Z-VALUES
fixed_effects <- summary(oncomingCarNonMat_GLMM_EventNumber_Recoded_Model)$coefficients$cond
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


# NON-MATERIALISED ONCOMING CAR
# LOAD DATA IN (CSV FILE) 
# FILE DIRECTORY
setwd ("C:/Users/tsytl/Desktop/Submission to PlosOne/Data/")
# NON-MAT ONCOMING CAR
oncomingCarNonMat <- read.table("NON MAT ONCOMING CAR VEHICLE DATA.csv", header = TRUE, sep=",")

#DEFINING THESE COLUMNS AS FACTORS (SUBJECT ID & INDEPENDENT VARIABLES) - ONCOMING CAR NON-MAT
oncomingCarNonMat$subNumber <- factor(oncomingCarNonMat$subNumber)
oncomingCarNonMat$lightCondition <- factor(oncomingCarNonMat$lightCondition)
oncomingCarNonMat$automationState <- factor(oncomingCarNonMat$automationState)

str(oncomingCarNonMat$eventNumber_Recoded)
oncomingCarNonMat$eventNumber_Recoded <- as.numeric(oncomingCarNonMat$eventNumber_Recoded)

#FIT THE GLMM WITH EVENT NUMBER
oncomingCarNonMat_GLMM_EventNumber_Recoded_Model_ReactionStatus <- glmer (reactionStatus ~ eventNumber_Recoded + (1|subNumber), 
                                                                       family = binomial(), data = oncomingCarNonMat)

# DISPLAY THE MODEL
summary (oncomingCarNonMat_GLMM_EventNumber_Recoded_Model_ReactionStatus)

# EXTRACT FIXED EFFECTS COEFFICIENTS, STANDARD ERRORS, AND T-VALUES
fixed_effects <- summary(oncomingCarNonMat_GLMM_EventNumber_Recoded_Model_ReactionStatus)$coefficients
coef <- fixed_effects[, "Estimate"]
se <- fixed_effects[, "Std. Error"]
z_value <- fixed_effects[, "z value"]

# ROUND P-VALUES TO DISPLAY IN STANDARD DECIMAL FORMAT
p_value <- round(fixed_effects[, "Pr(>|z|)"], digits = 4)

# CALCULATE ODDS RATIO
odds_Ratio <- exp(coef)

# CALCULATE 95 % CONFIDENCE INTERVALS
ci_lower <- exp(coef - 1.96 * se)
ci_upper <- exp(coef + 1.96 * se)

# COMBINE RESULTS INTO A DATA FRAME
results <- data.frame(
  Coefficient = coef,
  `Standard Error` = se,
  `z-value` = z_value,
  `p-value` = p_value,
  `Odds Ratio` = odds_Ratio,
  `95% CI Lower` = ci_lower,
  `95% CI Higher` = ci_upper
)

# DISPLAY THE RESULTS
print(results, digits = 4)