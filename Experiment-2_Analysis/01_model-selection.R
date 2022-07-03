#################################
#     model_selection           #
#################################
# This script does:
# finds random effects structure
# for chosen version of preprocessed data
# Author: Aylin Kallmayer
# Date: 01.04.2022

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
summary(acc.model <- glmer(correct ~ rotation*consistency*movable + # fixed effects
                             (1 + rotation + consistency + movable | sub) +
                             (1 + rotation + consistency| imgID), # random effects
                           family = binomial(),
                           control=glmerControl(optimizer="bobyqa"),
                           data = data))
summary(rePCA(acc.model)) #deprecate
summary(acc.model.2 <- glmer(correct ~ rotation*consistency*movable + # fixed effects
                             (1 + consistency + movable | sub) +
                             (1 + rotation + consistency | imgID), # random effects
                           family = binomial(),
                           control=glmerControl(optimizer="bobyqa"),
                           data = data))
anova(acc.model, acc.model.2)
summary(rePCA(acc.model.2)) #deprecate
summary(acc.model.3 <- glmer(correct ~ rotation*consistency*movable + # fixed effects
                               (1 + consistency | sub) +
                               (1 + rotation + consistency| imgID), # random effects
                             family = binomial(),
                             control=glmerControl(optimizer="bobyqa"),
                             data = data))
anova(acc.model.2,acc.model.3)
summary(rePCA(acc.model.3)) #deprecate
summary(acc.model.4 <- glmer(correct ~ rotation*consistency*movable + # fixed effects
                               (1 + consistency | sub) +
                               (1 + rotation | imgID), # random effects
                             family = binomial(),
                             control=glmerControl(optimizer="bobyqa"),
                             data = data))
anova(acc.model.3,acc.model.4)
summary(rePCA(acc.model.4)) #deprecate
summary(acc.model.5 <- glmer(correct ~ rotation*consistency*movable + # fixed effects
                               (1 | sub) +
                               (1 + rotation | imgID), # random effects
                             family = binomial(),
                             control=glmerControl(optimizer="bobyqa"),
                             data = data))
anova(acc.model.4,acc.model.5)
summary(rePCA(acc.model.5)) # not deprecate

# final model
summary(acc.model.5)
saveRDS(acc.model.5, "models/acc-model_processed_badsrem_outrem.rds")

#----RT----
#----badsrem Data----
data <- prepare_for_model(data_type="badsrem", RT = TRUE)

#----Find RE structure----
summary(rt.model <- lmer(log_RT ~ rotation*consistency*movable + # fixed effects
                           (1 + rotation + consistency + movable | sub) +
                           (1 + rotation + consistency | imgID), # random effects
                         data = data))
summary(rePCA(rt.model)) # deprecate
summary(rt.model.2 <- lmer(log_RT ~ rotation*consistency*movable + # fixed effects
                           (1 + rotation + movable | sub) +
                           (1 + rotation + consistency | imgID), # random effects
                         data = data))
anova(rt.model, rt.model.2)
summary(rePCA(rt.model.2)) # deprecate
summary(rt.model.3 <- lmer(log_RT ~ rotation*consistency*movable + # fixed effects
                             (1 + rotation | sub) +
                             (1 + rotation + consistency | imgID), # random effects
                           data = data))
anova(rt.model.2,rt.model.3)
summary(rePCA(rt.model.3)) # not deprecate

# final model
summary(rt.model.3)
saveRDS(rt.model.3, "models/rt-model_processed_badsrem.rds")
