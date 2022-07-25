#################################
#        Mixed modelling        #
#################################
# This script does:
# 3) post-hoc tests
# 1) creates accuracy and rt plots
# 2) creates model adjusted accuracy and rt plots

# Author: Aylin Kallmayer
# Date: 01.04.2022

if (!require("pacman")) install.packages("pacman")
p_load("dplyr","ggplot2","emmeans","remef","ggeffects","Rmisc")

# functions
source("src/01_mixed-models_functions.R")

#----Preprocess----
# run to preprocess new data
source("00_preprocessing.R")

# update ggplot theme
global_theme <- theme_classic()+
  theme(axis.title=element_text(size=12),
                      axis.text=element_text(size=10),
                      legend.position = "None")

#----ACCURACY----
# with out badsrem data
data <- prepare_for_model(data_type="badsrem_outrem")
acc.model <- readRDS("models/acc-model_processed_badsrem_outrem.rds")
summary(acc.model)

#----Post-hoc tests----
(emm_1 <- emmeans(acc.model, pairwise ~ rotation | consistency))
(emm_2 <- emmeans(acc.model, pairwise ~ consistency | rotation))

(IC_st <- contrast(emm_1[[1]], interaction = "pairwise", by = NULL))

# -> the viewpoint effect is significantly stronger in the inconsistent than the consistent condition

#----PLot adjusted accuracy---- 
(a <- ggpredict(acc.model, terms=c("consistency","rotation")) %>% ggplot(aes(x, predicted, color=group))+
   geom_point(position=position_dodge(.5))+
   geom_errorbar(aes(ymax=conf.high, ymin=conf.low), width=.5, position = position_dodge(.5))+
   scale_color_manual(values = c("#56b4e9", "#cc79a7"))+
   xlab("Consistency")+
   ylab("Adjusted Accuracy Predictions")+
   ggtitle("")+
   global_theme)
tiff("plots/acc_adjusted-badsrem_outrem.png", units="cm", width=10, height=8, res=300)
a# insert ggplot code
dev.off()

#----PLot difference score---- 
data_diff <- data %>% group_by(sub, consistency, rotation) %>%
  summarise(mean_acc = mean(correct),
            .groups="keep") %>%
  ungroup() %>%
  group_by(sub, consistency) %>%
  mutate(diff_rot = mean_acc[rotation=="canon"]-mean_acc[rotation=="noncanon"]) %>%
  summarise(diff = mean(diff_rot),
            .groups="keep")

(x <- ggplot(data_diff, aes(x=consistency, y=diff))+
  geom_violin(aes(fill=consistency), alpha=.4)+
    geom_boxplot(width=.2)+
    geom_point(alpha=.1)+
    geom_line(aes(group=sub), alpha=.2)+
    stat_summary(fun = "median", colour = "black", size = 2, geom = "point")+
    xlab("Scene Consistency")+
    ylab("Difference Score \n (canonical vs. non-canonical)")+
    scale_fill_manual(values = c("#56b4e9", "#cc79a7"))+
    global_theme)
tiff("plots/acc_diffplot.png", units="cm", width=10, height=8, res=300)
x# insert ggplot code
dev.off()

#----RT----
# with bads removed
data <- prepare_for_model(data_type="badsrem", RT=TRUE)
rt.model <- readRDS("models/rt-model_processed_badsrem.rds")
summary(rt.model)

#----PLot RT---- 
(b <- ggpredict(rt.model, terms=c("rotation")) %>% ggplot(aes(x, predicted, color=x)) +
  geom_point()+
  geom_errorbar(aes(ymax=conf.high, ymin=conf.low), width=.5)+
  scale_color_manual(values = c("#56b4e9", "#cc79a7"))+
  xlab("Viewpoint")+
  ylab("Adjusted log RT Predictions")+
  ggtitle("")+
  global_theme)
tiff("plots/rt_adjusted-badsrem_outrem.png", units="cm", width=10, height=8, res=300)
b# insert ggplot code
dev.off()
