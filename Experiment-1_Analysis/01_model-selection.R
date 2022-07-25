#################################
#     model_selection           #
#################################
# This script does:
# finds random effects structure
# for chosen version of preprocessed data
# Author: Aylin Kallmayer
# Year: 2022

# load functions
source("src/01_mixed-models_functions.R")

# run to preprocess new data
source("00_preprocessing.R")

if (!require("pacman")) install.packages("pacman")
p_load("dplyr","lmerTest")

#---------------------------------------------------------------------------------------------------#
#----EXPERIMENT 1A: COLOUR----
#---------------------------------------------------------------------------------------------------#

#----ACCURACY----
#----badsrem outrem data----
data <- prepare_for_model(experiment="colour",data_type="badsrem_outrem")

# Find RE structure
summary(acc.model <- glmer(correct ~ poly(angle,2)*match + # fixed effects
                             (1 + match | sub) +
                             (1 + match | imgID), # random effects
                           family = binomial(),
                           control=glmerControl(optimizer="bobyqa"),
                           data = data))
summary(rePCA(acc.model)) #deprecate, remove match sub
summary(acc.model.2 <- glmer(correct ~ poly(angle,2)*match + # fixed effects
                             (1 | sub) +
                             (1 + match | imgID), # random effects
                           family = binomial(),
                           control=glmerControl(optimizer="bobyqa"),
                           data = data))
anova(acc.model,acc.model.2) # not sig worse
summary(rePCA(acc.model.2)) #deprecate, ermove match imgid
summary(acc.model.3 <- glmer(correct ~ poly(angle,2)*match + # fixed effects
                               (1 | sub) +
                               (1 | imgID), # random effects
                             family = binomial(),
                             control=glmerControl(optimizer="bobyqa"),
                             data = data))
anova(acc.model.2,acc.model.3) # sig worse, keep model 2

# final model:
summary(acc.model.2)
saveRDS(acc.model.2, "models/colour/acc-model_processed_badsrem_outrem.rds")

#----RT----
#----badsrem Data----
data <- prepare_for_model(experiment="colour",data_type="badsrem", RT = TRUE)

#----Find RE structure----
summary(rt.model <- lmer(log_RT ~ poly(angle,2)*match + # fixed effects
                           (1 + match | sub) +
                           (1 + match | imgID), # random effects
                         data = data))
summary(rePCA(rt.model)) # not deprecate

# final model:
summary(rt.model)
saveRDS(rt.model, "models/colour/rt-model_processed_badsrem.rds")

#---------------------------------------------------------------------------------------------------#
#----EXPERIMENT 1B: GREY----
#---------------------------------------------------------------------------------------------------#

#----ACCURACY----
#----badsrem outrem data----
data <- prepare_for_model(experiment="grey",data_type="badsrem_outrem")

# Find RE structure
summary(acc.model <- glmer(correct ~ poly(angle,2)*match + # fixed effects
                             (1 + match | sub) +
                             (1 + match | imgID), # random effects
                           family = binomial(),
                           control=glmerControl(optimizer="bobyqa"),
                           data = data))
summary(rePCA(acc.model)) # not deprecate

# final model:
summary(acc.model)
saveRDS(acc.model, "models/grey/acc-model_processed_badsrem_outrem.rds")

#----RT----
#----badsrem Data----
data <- prepare_for_model(experiment="grey", data_type="badsrem", RT = TRUE)

#----Find RE structure----
summary(rt.model <- lmer(log_RT ~ poly(angle,2)*match + # fixed effects
                           (1 + match | sub) +
                           (1 + match | imgID), # random effects
                         data = data))
summary(rePCA(rt.model)) # is not deprecate

# final model:
summary(rt.model)
saveRDS(rt.model, "models/grey/rt-model_processed_badsrem_log.rds")
