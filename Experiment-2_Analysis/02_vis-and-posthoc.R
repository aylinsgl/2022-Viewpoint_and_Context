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
p_load("dplyr","ggplot2","emmeans","remef")

# functions
source("src/01_mixed-models_functions.R")

#----Preprocess----
# run to preprocess new data
source("00_preprocessing.R")

# update ggplot theme
theme_set(theme_classic())+
  theme_update(axis.title=element_text(size=34,face="bold"),
               axis.text=element_text(size=24))

#----ACCURACY----
# with out badsrem data
data <- prepare_for_model(data_type="badsrem_outrem")
acc.model.5 <- readRDS("models/acc-model_processed_badsrem_outrem.rds")
summary(acc.model.5)

#----Post-hoc tests----
(emm_1 <- emmeans(acc.model.5, pairwise ~ rotation | consistency))
(emm_2 <- emmeans(acc.model.5, pairwise ~ consistency | rotation))
contrast(emm_1[[1]])
contrast(emm_2[[1]])
(IC_st <- contrast(emm_1[[1]], interaction = "pairwise", by = NULL))
(IC_st <- contrast(emm_2[[1]], interaction = "pairwise", by = NULL))

# -> the viewpoint effect is significantly stronger in the inconsistent than the consistent condition

#----PLot accuracy---- 
agg_data <- aggregate(data=data, correct~consistency+rotation+movable,mean)
agg_data <- aggregate(data=data, correct~sub+rotation+consistency+movable,mean)
(a <- ggplot(agg_data, aes(x=consistency, y=correct, color=rotation))+
    geom_violin(aes(fill=rotation))+
    geom_boxplot(width=.3, position=position_dodge(.9), alpha=.8)+
    geom_point(position=position_dodge(.9), alpha=.3)+
    scale_color_manual(labels = c("canonical","non-canonical"),
                       values = c("#000000", "#000000"), 
                       name = "Rotation")+
    scale_fill_manual(labels = c("canonical","non-canonical"),
                       values = c("#999999", "#E69F00"), 
                       name = "Rotation")+
    xlab("Consistency")+
    ylab("Accuracy"))
tiff("plots/acc-badsrem_outrem.png", units="in", width=10, height=8, res=300)
a# insert ggplot code
dev.off()

#----PLot adjusted accuracy---- 
data$correct_acc_model5 <- keepef(acc.model.5, fix =  c("rotation1:consistency1"),grouping = TRUE, keep.intercept = FALSE) 

agg_data_adj <- aggregate(data=data, correct_acc_model5~sub+rotation+consistency+movable,mean)

(b <- ggplot(agg_data_adj, aes(x=consistency, y=correct_acc_model5, color=rotation))+
    geom_violin(aes(fill=rotation))+
    geom_boxplot(width=.3, position=position_dodge(.9), alpha=.8)+
    geom_point(position=position_dodge(.9), alpha=.3)+
    scale_color_manual(labels = c("canonical","non-canonical"),
                       values = c("#000000", "#000000"), 
                       name = "Rotation")+
    scale_fill_manual(labels = c("canonical","non-canonical"),
                      values = c("#999999", "#E69F00"), 
                      name = "Rotation")+
    xlab("Consistency")+
    ylab("Adjusted Accuracy"))

tiff("plots/acc_adjusted-badsrem_outrem.png", units="in", width=10, height=8, res=300)
b# insert ggplot code
dev.off()

#----RT----
# with bads removed
data <- prepare_for_model(data_type="badsrem", RT=TRUE)
rt.model <- readRDS("models/rt-model_processed_badsrem.rds")
summary(rt.model)

#----PLot RT---- 
agg_data <- aggregate(data=data, log_RT~sub+rotation+consistency+movable,mean)
(c <- ggplot(agg_data, aes(x=consistency, y=log_RT, color=rotation))+
    geom_violin(aes(fill=rotation))+
    geom_boxplot(width=.3, position=position_dodge(.9), alpha=.8)+
    geom_point(position=position_dodge(.9), alpha=.3)+
    scale_color_manual(labels = c("canonical","non-canonical"),
                       values = c("#000000", "#000000"), 
                       name = "Rotation")+
    scale_fill_manual(labels = c("canonical","non-canonical"),
                      values = c("#999999", "#E69F00"), 
                      name = "Rotation")+
    xlab("Consistency")+
    ylab("RT"))
tiff("plots/rt-badsrem.png", units="in", width=10, height=8, res=300)
c# insert ggplot code
dev.off()

#----plot adjusted rt----
data$rt_model <- keepef(rt.model, fix = c("movable1","rotation1"), grouping = TRUE, keep.intercept = FALSE) 
agg_data_adj_rt <- aggregate(data=data, rt_model~sub+rotation+consistency+movable,mean)

(d <- ggplot(agg_data_adj_rt, aes(x=rotation, y=rt_model, color=movable))+
    geom_violin(aes(fill=movable))+
    geom_boxplot(width=.3, position=position_dodge(.9), alpha=.8)+
    geom_point(position=position_dodge(.9), alpha=.3)+
    scale_color_manual(labels = c("non-movable","movable"),
                       values = c("#000000", "#000000"), 
                       name = "Movability")+
    scale_fill_manual(labels = c("non-movable","movable"),
                      values = c("#999999", "#E69F00"), 
                      name = "Movability")+
    xlab("Rotation")+
    ylab("RT"))
tiff("plots/processed_badsrem-rt_adjusted_movable.png", units="in", width=10, height=8, res=300)
d# insert ggplot code
dev.off()


