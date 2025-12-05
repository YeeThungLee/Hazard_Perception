# TIME TO FIRST FIXATION (PAPER 1)

# LOAD LIBRARIES
library(ggplot2)
library(tidyr)
library(dplyr)
library(lme4)
library(glmmTMB)
library (emmeans)

# MATERIALISED LEFT PEDESTRIAN EVENT
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

# GLMM
# FIT THE GLMM WITH LIGHTING AND AUTOMATION (WITH TWEEDIE FAMILY DUE TO THE POSITIVELY-SKEWED DATA WITH MEANINGFUL ZERO VALUE)
leftPedsMats_GLMM_Model <- glmmTMB (timeToFirstFixation_seconds ~ lightCondition + automationState + lightCondition*automationState + (1|subNumber), family = tweedie(link = "log"), data = leftPedsMats)
# DISPLAY THE MODEL
summary (leftPedsMats_GLMM_Model)

# EXTRACT FIXED EFFECTS COEFFICIENTS, STANDARD ERRORS, AND Z-VALUES
fixed_effects <- summary(leftPedsMats_GLMM_Model)$coefficients$cond
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

# MAIN EFFECT OF LIGHTING
library(emmeans)
emm <- emmeans(leftPedsMats_GLMM_Model, ~ lightCondition, type = "response")
summary (emm)
pairs(emm, type = "response", adjust = "bonferroni")

# MAIN EFFECT OF LEVEL OF AUTOMATION
library(emmeans)
emm <- emmeans(leftPedsMats_GLMM_Model, ~ automationState, type = "response")
summary (emm)
pairs(emm, type = "response", adjust = "bonferroni")

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

#GLMM
# FIT THE GLMM WITH LIGHTING AND AUTOMATION (WITH TWEEDIE FAMILY DUE TO THE POSITIVELY-SKEWED DATA WITH MEANINGFUL ZERO VALUE)
rightPedsMats_GLMM_Model <- glmmTMB (timeToFirstFixation_seconds ~ lightCondition + automationState + lightCondition*automationState + (1|subNumber), family = tweedie(link = "log"), data = rightPedsMats)
# DISPLAY THE MODEL
summary (rightPedsMats_GLMM_Model)

# EXTRACT FIXED EFFECTS COEFFICIENTS, STANDARD ERRORS, AND Z-VALUES
fixed_effects <- summary(rightPedsMats_GLMM_Model)$coefficients$cond
coef <- fixed_effects[, "Estimate"]
se <- fixed_effects[, "Std. Error"]
z_value <- fixed_effects[, "z value"]

# Round p-values to display in standard decimal format
p_value <- round(fixed_effects[, "Pr(>|z|)"], digits = 4)

# CALCULATE EXPONENTIATED COEFFICIENTS (ODDS RATIOS)
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

# MAIN EFFECT OF LIGHTING
library(emmeans)
emm <- emmeans(rightPedsMats_GLMM_Model, ~ lightCondition, type = "response")
summary (emm)
pairs(emm, type = "response", adjust = "bonferroni")

# MAIN EFFECT OF LEVEL OF AUTOMATION
library(emmeans)
emm <- emmeans(rightPedsMats_GLMM_Model, ~ automationState, type = "response")
summary (emm)
pairs(emm, type = "response", adjust = "bonferroni")

# MATERIALISED ONCOMING CAR
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

# GLMM
# FIT THE GLMM WITH LIGHTING AND AUTOMATION (WITH TWEEDIE FAMILY DUE TO THE POSITIVELY-SKEWED DATA WITH MEANINGFUL ZERO VALUE)
oncomingCarMats_GLMM_Model <- glmmTMB (timeToFirstFixation_seconds ~ lightCondition + automationState + lightCondition*automationState + (1|subNumber), family = tweedie(link = "log"), data = oncomingCarMats)
# DISPLAY THE MODEL
summary (oncomingCarMats_GLMM_Model)

# EXTRACT FIXED EFFECTS COEFFICIENTS, STANDARD ERRORS, AND Z-VALUES
fixed_effects <- summary(oncomingCarMats_GLMM_Model)$coefficients$cond
coef <- fixed_effects[, "Estimate"]
se <- fixed_effects[, "Std. Error"]
z_value <- fixed_effects[, "z value"]

# Round p-values to display in standard decimal format
p_value <- round(fixed_effects[, "Pr(>|z|)"], digits = 4)

# CALCULATE EXPONENTIATED COEFFICIENTS (ODDS RATIOS)
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

# MAIN EFFECT OF LIGHTING
library(emmeans)
emm <- emmeans(oncomingCarMats_GLMM_Model, ~ lightCondition, type = "response")
summary (emm)
pairs(emm, type = "response", adjust = "bonferroni")

# MAIN EFFECT OF LEVEL OF AUTOMATION
library(emmeans)
emm <- emmeans(oncomingCarMats_GLMM_Model, ~ automationState, type = "response")
summary (emm)
pairs(emm, type = "response", adjust = "bonferroni")


# NON-MATERIALISED LEFT PEDESTRIAN 
# LOAD DATA FILES
# LOAD DATA IN (CSV FILE) 
# FILE DIRECTORY
setwd ("C:/Users/tsytl/Desktop/Submission to PlosOne/Data/")
# NON-MAT LEFT PED
leftPedsNonMats <- read.table("Time to First Fixation (EVENT NON MAT LEFT PED).csv", header = TRUE, sep=",")

#DEFINING THESE COLUMNS AS FACTORS (SUBJECT ID & INDEPENDENT VARIABLES) - NON-MAT LEFT PED
leftPedsNonMats$subNumber <- factor(leftPedsNonMats$subNumber)
leftPedsNonMats$lightCondition <- factor(leftPedsNonMats$lightCondition)
leftPedsNonMats$automationState <- factor(leftPedsNonMats$automationState)

#DEFINING MY DEPENDENT VARIABLES - NON-MAT LEFT PED
str(leftPedsNonMats$timeToFirstFixation_seconds)
leftPedsNonMats$timeToFirstFixation_seconds <- as.numeric(leftPedsNonMats$timeToFirstFixation_seconds)

# GLMM
# FIT THE GLMM WITH LIGHTING AND AUTOMATION (WITH TWEEDIE FAMILY DUE TO THE POSITIVELY-SKEWED DATA WITH MEANINGFUL ZERO VALUE)
leftPedsNonMats_GLMM_Model <- glmmTMB (timeToFirstFixation_seconds ~ lightCondition + automationState + lightCondition*automationState + (1|subNumber), family = tweedie(link = "log"), data = leftPedsNonMats)

# DISPLAY THE MODEL
summary (leftPedsNonMats_GLMM_Model)

# EXTRACT FIXED EFFECTS COEFFICIENTS, STANDARD ERRORS, AND Z-VALUES
fixed_effects <- summary(leftPedsNonMats_GLMM_Model)$coefficients$cond
coef <- fixed_effects[, "Estimate"]
se <- fixed_effects[, "Std. Error"]
z_value <- fixed_effects[, "z value"]

# Round p-values to display in standard decimal format
p_value <- round(fixed_effects[, "Pr(>|z|)"], digits = 4)

# CALCULATE EXPONENTIATED COEFFICIENTS (ODDS RATIOS)
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

# MAIN EFFECT OF LIGHTING
library(emmeans)
emm <- emmeans(leftPedsNonMats_GLMM_Model, ~ lightCondition, type = "response")
summary (emm)
pairs(emm, type = "response", adjust = "bonferroni")

# MAIN EFFECT OF LEVEL OF AUTOMATION
library(emmeans)
emm <- emmeans(leftPedsNonMats_GLMM_Model, ~ automationState, type = "response")
summary (emm)
pairs(emm, type = "response", adjust = "bonferroni")


# NON-MATERIALISED RIGHT PEDESTRIAN
# LOAD DATA FILES
# LOAD DATA IN (CSV FILE) 
# FILE DIRECTORY
setwd ("C:/Users/tsytl/Desktop/Submission to PlosOne/Data/")

# NON-MAT RIGHT PED
rightPedsNonMats <- read.table("Time to First Fixation (EVENT NON MAT RIGHT PED).csv", header = TRUE, sep=",")

#DEFINING THESE COLUMNS AS FACTORS (SUBJECT ID & INDEPENDENT VARIABLES) - NON-MAT RIGHT PED
rightPedsNonMats$subNumber <- factor(rightPedsNonMats$subNumber)
rightPedsNonMats$lightCondition <- factor(rightPedsNonMats$lightCondition)
rightPedsNonMats$automationState <- factor(rightPedsNonMats$automationState)

#DEFINING MY DEPENDENT VARIABLES - NON-MAT RIGHT PED
str(rightPedsNonMats$timeToFirstFixation_seconds) 
rightPedsNonMats$timeToFirstFixation_seconds <- as.numeric(rightPedsNonMats$timeToFirstFixation_seconds)

# GLMM
# FIT THE GLMM WITH LIGHTING AND AUTOMATION (WITH TWEEDIE FAMILY DUE TO THE POSITIVELY-SKEWED DATA WITH MEANINGFUL ZERO VALUE)
rightPedsNonMats_GLMM_Model <- glmmTMB (timeToFirstFixation_seconds ~ lightCondition + automationState + lightCondition*automationState + (1|subNumber), family = tweedie(link = "log"), data = rightPedsNonMats)

# DISPLAY THE MODEL
summary (rightPedsNonMats_GLMM_Model)

# EXTRACT FIXED EFFECTS COEFFICIENTS, STANDARD ERRORS, AND Z-VALUES
fixed_effects <- summary(rightPedsNonMats_GLMM_Model)$coefficients$cond
coef <- fixed_effects[, "Estimate"]
se <- fixed_effects[, "Std. Error"]
z_value <- fixed_effects[, "z value"]

# Round p-values to display in standard decimal format
p_value <- round(fixed_effects[, "Pr(>|z|)"], digits = 4)

# CALCULATE EXPONENTIATED COEFFICIENTS (ODDS RATIOS)
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

# MAIN EFFECT OF LIGHTING
library(emmeans)
emm <- emmeans(rightPedsNonMats_GLMM_Model, ~ lightCondition, type = "response")
summary (emm)
pairs(emm, type = "response", adjust = "bonferroni")

# MAIN EFFECT OF LEVEL OF AUTOMATION
library(emmeans)
emm <- emmeans(rightPedsNonMats_GLMM_Model, ~ automationState, type = "response")
summary (emm)
pairs(emm, type = "response", adjust = "bonferroni")

# NON-MATERIALISED ONCOMING CAR
# LOAD DATA FILES
# LOAD DATA IN (CSV FILE) 
# FILE DIRECTORY
setwd ("C:/Users/tsytl/Desktop/Submission to PlosOne/Data/")
# NON-MAT ONCOMING CAR
oncomingCarNonMats <- read.table("Time to First Fixation (EVENT NON MAT ONCOMING CAR).csv", header = TRUE, sep=",")

#DEFINING THESE COLUMNS AS FACTORS (SUBJECT ID & INDEPENDENT VARIABLES) - NON-MAT ONCOMING CAR
oncomingCarNonMats$subNumber <- factor(oncomingCarNonMats$subNumber)
oncomingCarNonMats$lightCondition <- factor(oncomingCarNonMats$lightCondition)
oncomingCarNonMats$automationState <- factor(oncomingCarNonMats$automationState)

#DEFINING MY DEPENDENT VARIABLES - NON-MAT ONCOMING CAR
str(oncomingCarNonMats$timeToFirstFixation_seconds) 
oncomingCarNonMats$timeToFirstFixation_seconds <- as.numeric(oncomingCarNonMats$timeToFirstFixation_seconds)

# GLMM
# FIT THE GLMM WITH LIGHTING AND AUTOMATION (WITH TWEEDIE FAMILY DUE TO THE POSITIVELY-SKEWED DATA WITH MEANINGFUL ZERO VALUE)
oncomingCarNonMats_GLMM_Model <- glmmTMB(timeToFirstFixation_seconds ~ lightCondition + automationState + 
                                           lightCondition*automationState + (1|subNumber), 
                                         family = tweedie(link = "log"), 
                                         data = oncomingCarNonMats)

# DISPLAY THE MODEL SUMMARY
summary(oncomingCarNonMats_GLMM_Model)

# EXTRACT FIXED EFFECTS COEFFICIENTS, STANDARD ERRORS, AND Z-VALUES
fixed_effects <- summary(oncomingCarNonMats_GLMM_Model)$coefficients$cond
coef <- fixed_effects[, "Estimate"]
se <- fixed_effects[, "Std. Error"]
z_value <- fixed_effects[, "z value"]

# Round p-values to display in standard decimal format
p_value <- round(fixed_effects[, "Pr(>|z|)"], digits = 4)

# CALCULATE EXPONENTIATED COEFFICIENTS (ODDS RATIOS)
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

# MAIN EFFECT OF LIGHTING
library(emmeans)
emm <- emmeans(oncomingCarNonMats_GLMM_Model, ~ lightCondition, type = "response")
summary (emm)
pairs(emm, type = "response", adjust = "bonferroni")

# MAIN EFFECT OF LEVEL OF AUTOMATION
library(emmeans)
emm <- emmeans(oncomingCarNonMats_GLMM_Model, ~ automationState, type = "response")
summary (emm)
pairs(emm, type = "response", adjust = "bonferroni")
