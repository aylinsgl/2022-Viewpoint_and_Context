#################################
#     model_selection           #
#################################
# This script does:
# finds random effects structure
# for chosen version of preprocessed data
# Author: Aylin Kallmayer
# Date: 18.03.2022

# load functions
source("src/01_mixed-models_functions.R")

# run to preprocess new data
source("00_preprocessing.R")

if (!require("pacman")) install.packages("pacman")
p_load("dplyr","lmerTest")

#----ACCURACY----
#----badsrem outrem data----
data <- prepare_for_model(data_type="badsrem_outrem")

# Find RE structure
summary(acc.model <- glmer(correct ~ poly(angle,2)*match*movable + # fixed effects
                             (1 + match + movable | sub) +
                             (1 + match | imgID), # random effects
                           family = binomial(),
                           control=glmerControl(optimizer="bobyqa"),
                           data = data))
summary(rePCA(acc.model)) #deprecate
summary(acc.model.2 <- glmer(correct ~ poly(angle,2)*match*movable + # fixed effects
                             (1 + match| sub) +
                             (1 + match | imgID), # random effects
                           family = binomial(),
                           control=glmerControl(optimizer="bobyqa"),
                           data = data))
anova(acc.model,acc.model.2) 
summary(rePCA(acc.model.2)) # not deprecate

# final model:
summary(acc.model.2)
saveRDS(acc.model.2, "models/acc-model_processed_badsrem_outrem.rds")

#----RT----
#----badsrem Data----
data <- prepare_for_model(data_type="badsrem", RT = TRUE)

#----Find RE structure----
summary(rt.model <- lmer(inverse_RT ~ poly(angle,2)*match*movable + # fixed effects
                           (1 + match + movable | sub) +
                           (1 + match | imgID), # random effects
                         data = data))
summary(rePCA(rt.model)) # is deprecate
summary(rt.model.2 <- lmer(inverse_RT ~ poly(angle,2)*match*movable + # fixed effects
                           (1 + match + movable | sub) +
                           (1 | imgID), # random effects
                         data = data))
anova(rt.model, rt.model.2) # sig worse so keep model 1
summary(rt.model)
saveRDS(rt.model, "models/rt-model_processed_badsrem_inverse.rds")

#----log transformed data----
summary(rt.model <- lmer(log_RT ~ poly(angle,2)*match*movable + # fixed effects
                           (1 + match + movable | sub) +
                           (1 + match | imgID), # random effects
                         data = data))
summary(rePCA(rt.model)) # is not deprecate
summary(rt.model)
saveRDS(rt.model, "models/rt-model_processed_badsrem_log.rds")
