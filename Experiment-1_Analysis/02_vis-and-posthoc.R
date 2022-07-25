#################################
#        plots & post-hoc       #
#################################
# This script does:
# 3) post-hoc tests
# 1) creates accuracy and rt plots
# 2) creates model adjusted accuracy and rt plots

# Author: Aylin Kallmayer
# Year: 2022

if (!require("pacman")) install.packages("pacman")
p_load("dplyr","ggplot2","emmeans","remef","Rmisc","ggeffects")

# functions
source("src/01_mixed-models_functions.R")

#----Preprocess----
# run to preprocess new data
source("00_preprocessing.R")

# update ggplot axis title theme
global_theme <- theme_classic()+
  theme(axis.title=element_text(size=12),
                      axis.text=element_text(size=10),
                      legend.position = "None")
#---------------------------------------------------------------------------------------------------#
#----EXPERIMENT 1A: COLOUR----
#---------------------------------------------------------------------------------------------------#

#----ACCURACY----
# with out badsrem data
data <- prepare_for_model(experiment="colour", data_type="badsrem_outrem")
acc.model <- readRDS("models/colour/acc-model_processed_badsrem_outrem.rds")
summary(acc.model)

#----Post-hoc tests----
# poly2 : match interaction
emtrends(acc.model, pairwise ~ match, var = "angle", max.degree = 2, adjust = "tukey")
# -> sig difference between match mismatch for quadratic degree
# -> quadratic trend only in match condition

#----Plot marginal effects with ggeffects----
(a <- ggpredict(acc.model, terms=c("angle [all]","match")) %>% ggplot(aes(x, predicted, color=group))+
  geom_point()+
  geom_path()+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=group), alpha=.3)+
  scale_color_manual(values = c("#999999", "#E69F00"))+
  scale_fill_manual(values = c("#999999", "#E69F00"))+
  scale_x_continuous(labels=c("1" = "0", "2" = "60", "3" = "120", "4" = "180", "5" = "240", "6" = "300"),
                     breaks = 1:6)+
  xlab("Viewpoint")+
  ylab("Adjusted Accuracy Predictions")+
  ggtitle("")+
  global_theme)
tiff("plots/colour/acc_adjusted-badsrem_outrem.png", units="cm", width=10, height=8, res=300)
a# insert ggplot code
dev.off()

#----RT----
# with bads removed
data <- prepare_for_model(experiment="colour",data_type="badsrem", RT=TRUE)
rt.model <- readRDS("models/colour/rt-model_processed_badsrem.rds")
summary(rt.model)

#----Plot marginal effects with ggeffects----
(b <- ggpredict(rt.model, terms=c("angle [all]")) %>% ggplot(aes(x, predicted)) +
  geom_point()+
  geom_path()+
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), fill="grey", alpha=.3)+
  scale_x_continuous(labels=c("1" = "0", "2" = "60", "3" = "120", "4" = "180", "5" = "240", "6" = "300"),
                     breaks = 1:6)+
  xlab("Viewpoint")+
  ylab("Adjusted log RT Predictions")+
  ggtitle("")+
  global_theme)
tiff("plots/colour/rt_adjusted-badsrem_outrem.png", units="cm", width=10, height=8, res=300)
b# insert ggplot code
dev.off()

#---------------------------------------------------------------------------------------------------#
#----EXPERIMENT 1B: GREY----
#---------------------------------------------------------------------------------------------------#  

#----ACCURACY----
# with out badsrem data
data <- prepare_for_model(experiment="grey",data_type="badsrem_outrem")
acc.model <- readRDS("models/grey/acc-model_processed_badsrem_outrem.rds")
summary(acc.model)

#----Post-hoc tests----
# poly2 : match interaction
emtrends(acc.model, pairwise ~ match, var = "angle", max.degree = 2)
# -> sig difference between match mismatch for quadratic degree
# -> quadratic trend only in match condition

#----Plot marginal effects with ggeffects----
(c <- ggpredict(acc.model, terms=c("angle [all]","match")) %>% ggplot(aes(x, predicted, color=group))+
   geom_point()+
   geom_path()+
   geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=group), alpha=.3)+
   scale_color_manual(values = c("#999999", "#E69F00"))+
   scale_fill_manual(values = c("#999999", "#E69F00"))+
   scale_x_continuous(labels=c("1" = "0", "2" = "60", "3" = "120", "4" = "180", "5" = "240", "6" = "300"),
                      breaks = 1:6)+
   xlab("Viewpoint")+
   ylab("Adjusted Accuracy Predictions")+
   ggtitle("")+
   global_theme)
tiff("plots/grey/acc_adjusted-badsrem_outrem.png", units="cm", width=10, height=8, res=300)
c# insert ggplot code
dev.off()

#----RT (log)----
# with bads removed
data <- prepare_for_model(experiment="grey",data_type="badsrem", RT=TRUE)
rt.model <- readRDS("models/grey/rt-model_processed_badsrem_log.rds")
summary(rt.model)

#----Plot marginal effects with ggeffects----
(d <- ggpredict(rt.model, terms=c("angle [all]")) %>% ggplot(aes(x, predicted)) +
   geom_point()+
   geom_path()+
   geom_ribbon(aes(ymin=conf.low, ymax=conf.high), fill="grey", alpha=.3)+
   scale_x_continuous(labels=c("1" = "0", "2" = "60", "3" = "120", "4" = "180", "5" = "240", "6" = "300"),
                      breaks = 1:6)+
   xlab("Viewpoint")+
   ylab("Adjusted log RT Predictions")+
   ggtitle("")+
   global_theme)
tiff("plots/grey/rt_adjusted-badsrem_outrem.png", units="cm", width=10, height=8, res=300)
d# insert ggplot code
dev.off()
