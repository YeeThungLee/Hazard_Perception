# REACTION TO THE NON-MATERIALISED EVENTS (PAPER 1)

# LOAD LIBRARIES
library(ggplot2)
library(tidyr)
library(dplyr)
library(lme4)
library (emmeans)

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

#GLMM
# FIT THE GLMM WITH LIGHTING AND AUTOMATION 
leftPedsNonMats_GLMM_Model_ReactionStatus <- glmer (reactionStatus ~ lightCondition + automationState + lightCondition*automationState + (1|subNumber), 
                                                    family = binomial(), data = leftPedsNonMats)
# DISPLAY THE MODEL
summary (leftPedsNonMats_GLMM_Model_ReactionStatus)

# EXTRACT FIXED EFFECTS COEFFICIENTS, STANDARD ERRORS, AND T-VALUES
fixed_effects <- summary(leftPedsNonMats_GLMM_Model_ReactionStatus)$coefficients
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

# MAIN EFFECT OF LIGHTING
library(emmeans)
emm <- emmeans(leftPedsNonMats_GLMM_Model_ReactionStatus, ~ lightCondition, type = "response")
summary (emm)
pairs(emm, type = "response", adjust = "bonferroni") 

# MAIN EFFECT OF LEVEL OF AUTOMATION
library(emmeans)
emm <- emmeans(leftPedsNonMats_GLMM_Model_ReactionStatus, ~ automationState, type = "response")
summary (emm)
pairs(emm, type = "response", adjust = "bonferroni") 

# NON-MATERIALISED RIGHT PEDESTRIAN
# LOAD DATA IN (CSV FILE) 
# FILE DIRECTORY
setwd ("C:/Users/tsytl/Desktop/Submission to PlosOne/Data/")
# RIGHT NON-MAT PED
rightPedsNonMats <- read.table("NON MAT RIGHT PED VEHICLE DATA.csv", header = TRUE, sep=",")

#DEFINING THESE COLUMNS AS FACTORS (SUBJECT ID & INDEPENDENT VARIABLES) - RIGHT PEDS NON-MAT
rightPedsNonMats$subNumber <- factor(rightPedsNonMats$subNumber)
rightPedsNonMats$lightCondition <- factor(rightPedsNonMats$lightCondition)
rightPedsNonMats$automationState <- factor(rightPedsNonMats$automationState)

# GLMM
# FIT THE GLMM WITH LIGHTING AND AUTOMATION 
rightPedsNonMats_GLMM_Model_ReactionStatus <- glmer (reactionStatus ~ lightCondition + automationState + lightCondition*automationState + (1|subNumber), 
                                                     family = binomial(), data = rightPedsNonMats)

# DISPLAY THE MODEL
summary (rightPedsNonMats_GLMM_Model_ReactionStatus)

# EXTRACT FIXED EFFECTS COEFFICIENTS, STANDARD ERRORS, AND Z-VALUES
fixed_effects <- summary(rightPedsNonMats_GLMM_Model_ReactionStatus)$coefficients
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

# MAIN EFFECT OF LIGHTING
library(emmeans)
emm <- emmeans(rightPedsNonMats_GLMM_Model_ReactionStatus, ~ lightCondition, type = "response")
summary (emm)
pairs(emm, type = "response", adjust = "bonferroni") 

# MAIN EFFECT OF LEVEL OF AUTOMATION
library(emmeans)
emm <- emmeans(rightPedsNonMats_GLMM_Model_ReactionStatus, ~ automationState, type = "response")
summary (emm)
pairs(emm, type = "response", adjust = "bonferroni") 


# NON-MATERIALISED ONCOMING CAR
# LOAD DATA IN (CSV FILE) 
# FILE DIRECTORY
setwd ("C:/Users/tsytl/Desktop/Submission to PlosOne/Data/")
# NON-MAT ONCOMING CAR
oncomingCarNonMats <- read.table("NON MAT ONCOMING CAR VEHICLE DATA.csv", header = TRUE, sep=",")

#DEFINING THESE COLUMNS AS FACTORS (SUBJECT ID & INDEPENDENT VARIABLES) - ONCOMING CAR NON-MAT
oncomingCarNonMats$subNumber <- factor(oncomingCarNonMats$subNumber)
oncomingCarNonMats$lightCondition <- factor(oncomingCarNonMats$lightCondition)
oncomingCarNonMats$automationState <- factor(oncomingCarNonMats$automationState)

# GLMM
# FIT THE GLMM WITH LIGHTING AND AUTOMATION 
oncomingCarNonMats_GLMM_Model_ReactionStatus <- glmer (reactionStatus ~ lightCondition + automationState + lightCondition*automationState + (1|subNumber), 
                                                       family = binomial(), data = oncomingCarNonMats)

# DISPLAY THE MODEL
summary (oncomingCarNonMats_GLMM_Model_ReactionStatus)

# EXTRACT FIXED EFFECTS COEFFICIENTS, STANDARD ERRORS, AND Z-VALUES
fixed_effects <- summary(oncomingCarNonMats_GLMM_Model_ReactionStatus)$coefficients
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

# MAIN EFFECT OF LIGHTING
library(emmeans)
emm <- emmeans(oncomingCarNonMats_GLMM_Model_ReactionStatus, ~ lightCondition, type = "response")
summary (emm)
pairs(emm, type = "response", adjust = "bonferroni") 

# MAIN EFFECT OF LEVEL OF AUTOMATION
library(emmeans)
emm <- emmeans(oncomingCarNonMats_GLMM_Model_ReactionStatus, ~ automationState, type = "response")
summary (emm)
pairs(emm, type = "response", adjust = "bonferroni") 