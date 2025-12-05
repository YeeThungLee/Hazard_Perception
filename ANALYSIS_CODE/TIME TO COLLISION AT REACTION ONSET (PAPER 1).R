# TIME TO COLLISION AT REACTION ONSET (PAPER 1)

# LOAD LIBRARIES
library(ggplot2)
library(tidyr)
library(dplyr)
library(lme4)
library (emmeans)

# MATERIALISED LEFT PEDESTRIAN EVENT
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
str(leftPedsMats$ttc)
leftPedsMats$ttc <- as.numeric(leftPedsMats$ttc)

# REMOVE ROWS WITH NA/EMPTY VALUES IN TIME TO FIRST COLLISION
leftPedsMats<- leftPedsMats %>%
  filter(!is.na(ttc))

# GLMM
# FIT THE GLMM WITH LIGHTING AND AUTOMATION 
leftPedsMats_GLMM_Model_TTC <- glmer (ttc ~ lightCondition + automationState + lightCondition*automationState + (1|subNumber), 
                                      family = Gamma (link = "log"), data = leftPedsMats)
# DISPLAY THE MODEL
summary (leftPedsMats_GLMM_Model_TTC)

# EXTRACT FIXED EFFECTS COEFFICIENTS, STANDARD ERRORS, AND T-VALUES
fixed_effects <- summary(leftPedsMats_GLMM_Model_TTC)$coefficients
coef <- fixed_effects[, "Estimate"]
se <- fixed_effects[, "Std. Error"]
z_value <- fixed_effects[, "t value"]

# ROUND P-VALUES TO DISPLAY IN STANDARD DECIMAL FORMAT
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
emm <- emmeans(leftPedsMats_GLMM_Model_TTC, ~ lightCondition, type = "response")
summary (emm)
pairs(emm, type = "response", adjust = "bonferroni")  

# MAIN EFFECT OF AUTOMATION
library(emmeans)
emm <- emmeans(leftPedsMats_GLMM_Model_TTC, ~ automationState, type = "response")
summary (emm)
pairs(emm, type = "response", adjust = "bonferroni")  

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
str(rightPedsMats$ttc)
rightPedsMats$ttc <- as.numeric(rightPedsMats$ttc)


# REMOVE ROWS WITH NA/EMPTY VALUES IN TIME TO FIRST COLLISION
rightPedsMats<- rightPedsMats %>%
  filter(!is.na(ttc))

# GLMM
# FIT THE GLMM WITH LIGHTING AND AUTOMATION 
rightPedsMats_GLMM_Model_TTC <- glmer (ttc ~ lightCondition + automationState + lightCondition*automationState + (1|subNumber), 
                                       family = Gamma (link = "log"), data = rightPedsMats)
# DISPLAY THE MODEL
summary (rightPedsMats_GLMM_Model_TTC)

# EXTRACT FIXED EFFECTS COEFFICIENTS, STANDARD ERRORS, AND T-VALUES
fixed_effects <- summary(rightPedsMats_GLMM_Model_TTC)$coefficients
coef <- fixed_effects[, "Estimate"]
se <- fixed_effects[, "Std. Error"]
z_value <- fixed_effects[, "t value"]

# ROUND P-VALUES TO DISPLAY IN STANDARD DECIMAL FORMAT
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
emm <- emmeans(rightPedsMats_GLMM_Model_TTC, ~ lightCondition, type = "response")
summary (emm)
pairs(emm, type = "response", adjust = "bonferroni")  

# MAIN EFFECT OF LEVEL OF AUTOMATION
library(emmeans)
emm <- emmeans(rightPedsMats_GLMM_Model_TTC, ~ automationState, type = "response")
summary (emm)
pairs(emm, type = "response", adjust = "bonferroni")  

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
str(oncomingCarMats$ttc) 
oncomingCarMats$ttc <- as.numeric(oncomingCarMats$ttc)

# REMOVE ROWS WITH NA/EMPTY VALUES IN TIME TO FIRST COLLISION
oncomingCarMats<- oncomingCarMats %>%
  filter(!is.na(ttc))

# GLMM
# FIT THE GLMM WITH LIGHTING AND AUTOMATION 
oncomingCarMats_GLMM_Model_TTC <- glmer (ttc ~ lightCondition + automationState + lightCondition*automationState + (1|subNumber), 
                                         family = Gamma (link = "log"), data = oncomingCarMats)
# DISPLAY THE MODEL
summary (oncomingCarMats_GLMM_Model_TTC)

# EXTRACT FIXED EFFECTS COEFFICIENTS, STANDARD ERRORS, AND T-VALUES
fixed_effects <- summary(oncomingCarMats_GLMM_Model_TTC)$coefficients
coef <- fixed_effects[, "Estimate"]
se <- fixed_effects[, "Std. Error"]
z_value <- fixed_effects[, "t value"]

# ROUND P-VALUES TO DISPLAY IN STANDARD DECIMAL FORMAT
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
emm <- emmeans(oncomingCarMats_GLMM_Model_TTC, ~ lightCondition, type = "response")
summary (emm)
pairs(emm, type = "response", adjust = "bonferroni")

# MAIN EFFECT OF AUTOMATION
library(emmeans)
emm <- emmeans(oncomingCarMats_GLMM_Model_TTC, ~ automationState, type = "response")
summary (emm)
pairs(emm, type = "response", adjust = "bonferroni") 

