# LEARNING EFFECT NON-MAT LEFT PED

# LOAD LIBRARIES
library(ggplot2)
library(tidyr)
library(dplyr)
library(lme4)
library(glmmTMB)
library (emmeans)

# NON-MATERIALISED LEFT PEDESTRIAN 
# LOAD DATA FILES
# LOAD DATA IN (CSV FILE) 
# FILE DIRECTORY
setwd ("C:/Users/tsytl/Desktop/Submission to PlosOne/Data/")
# NON-MAT LEFT PED
leftPedNonMat <- read.table("Time to First Fixation (EVENT NON MAT LEFT PED).csv", header = TRUE, sep=",")


#DEFINING THESE COLUMNS AS FACTORS (SUBJECT ID & INDEPENDENT VARIABLES) - NON MAT LEFT PED
leftPedNonMat$subNumber <- factor(leftPedNonMat$subNumber)
leftPedNonMat$lightCondition <- factor(leftPedNonMat$lightCondition)
leftPedNonMat$automationState <- factor(leftPedNonMat$automationState)


#DEFINING MY DEPENDENT VARIABLES - NON MAT LEFT PED
str(leftPedNonMat$timeToFirstFixation_seconds) 
leftPedNonMat$timeToFirstFixation_seconds <- as.numeric(leftPedNonMat$timeToFirstFixation_seconds)
str(leftPedNonMat$eventNumber_Recoded)
leftPedNonMat$eventNumber_Recoded <- as.numeric(leftPedNonMat$eventNumber_Recoded)

# FIT THE GLMM WITH EVENTNUMBER (WITH TWEEDIE FAMILY DUE TO THE POSITIVELY-SKEWED DATA WITH MEANINGFUL ZERO VALUE)
leftPedNonMat_GLMM_EventNumber_Recoded_Model <- glmmTMB (timeToFirstFixation_seconds ~ eventNumber_Recoded + (1|subNumber), family = tweedie(link = "log"), data = leftPedNonMat)

# DISPLAY THE MODEL
summary (leftPedNonMat_GLMM_EventNumber_Recoded_Model)

# EXTRACT FIXED EFFECTS COEFFICIENTS, STANDARD ERRORS, AND Z-VALUES
fixed_effects <- summary(leftPedNonMat_GLMM_EventNumber_Recoded_Model)$coefficients$cond
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


# NON-MATERIALISED LEFT PEDESTRIAN
# LOAD DATA IN (CSV FILE) 
# FILE DIRECTORY
setwd ("C:/Users/tsytl/Desktop/Submission to PlosOne/Data/")
# LEFT NON-MAT PED
leftPedsNonMats <- read.table("NON MAT LEFT PED VEHICLE DATA.csv", header = TRUE, sep=",")

#DEFINING THESE COLUMNS AS FACTORS (SUBJECT ID & INDEPENDENT VARIABLES) - LEFT PEDS NON-MAT
leftPedsNonMats$subNumber <- factor(leftPedsNonMats$subNumber)
leftPedsNonMats$lightCondition <- factor(leftPedsNonMats$lightCondition)
leftPedsNonMats$automationState <- factor(leftPedsNonMats$automationState)

str(leftPedsNonMats$eventNumber_Recoded)
leftPedsNonMats$eventNumber_Recoded <- as.numeric(leftPedsNonMats$eventNumber_Recoded)

#FIT THE GLMM WITH EVENT NUMBER
leftPedsNonMats_GLMM_EventNumber_Recoded_Model_ReactionStatus <- glmer (reactionStatus ~ eventNumber_Recoded + (1|subNumber), 
                                                                family = binomial(), data = leftPedsNonMats)

# DISPLAY THE MODEL
summary (leftPedsNonMats_GLMM_EventNumber_Recoded_Model_ReactionStatus)

# EXTRACT FIXED EFFECTS COEFFICIENTS, STANDARD ERRORS, AND T-VALUES
fixed_effects <- summary(leftPedsNonMats_GLMM_EventNumber_Recoded_Model_ReactionStatus)$coefficients
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