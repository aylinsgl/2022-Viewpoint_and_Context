#################################
#        Mixed modelling        #
#################################
# This script does:
# 3) post-hoc tests
# 1) creates accuracy and rt plots
# 2) creates model adjusted accuracy and rt plots

# Author: Aylin Kallmayer
# Date: 18.03.2022

if (!require("pacman")) install.packages("pacman")
p_load("dplyr","ggplot2","emmeans","remef","Rmisc")

# functions
source("src/01_mixed-models_functions.R")

#----Preprocess----
# run to preprocess new data
source("00_preprocessing.R")

# update ggplot axis title theme
theme_set(theme_classic())+
  theme_update(axis.title=element_text(size=34,face="bold"),
               axis.text=element_text(size=24))

#----ACCURACY----
# with out badsrem data
data <- prepare_for_model(data_type="badsrem_outrem")
acc.model.3 <- readRDS("models/acc-model_processed_badsrem_outrem.rds")
summary(acc.model.3)

# tables
aggregate(data=data, correct~rotation+match, mean) %>%
  arrange(correct)

#----Post-hoc tests----
# poly2 : match interaction
emtrends(acc.model.3, pairwise ~ match, var = "angle", max.degree = 2, adjust = "tukey")
# -> sig difference between match mismatch for quadratic degree
# -> quadratic trend only in match condition

#----PLot accuracy---- 
agg_data <- aggregate(data=data, correct~sub+angle+match+movable,mean)
(a <- ggplot(agg_data, aes(x=angle, y=correct, color=match))+
    geom_point(aes(color=match),position = position_jitter(), alpha=.3)+
    geom_smooth(formula = y ~ x+I(x^2), method="lm")+
    scale_color_manual(labels = c("match","mismatch"),
                       values = c("#999999", "#E69F00"), 
                       name = "Matching")+
    xlab("Rotation")+
    ylab("Accuracy"))
tiff("plots/acc-badsrem_outrem.png", units="in", width=10, height=8, res=300)
a# insert ggplot code
dev.off()

#----PLot adjusted accuracy---- 
data$correct_acc_model3 <- keepef(acc.model.3, fix =  c("poly(angle, 2)2:match1"),grouping = TRUE, keep.intercept = FALSE) 

agg_data_adj <- aggregate(data=data, correct_acc_model3~sub+angle+match+movable,mean)

(b <- ggplot(agg_data_adj, aes(x=angle, y=correct_acc_model3, color=match))+
  geom_point(aes(color=match),position = position_jitter(), alpha=.3)+
  geom_smooth(formula = y ~ x+I(x^2), method="lm")+
  scale_color_manual(labels = c("match","mismatch"),
                     values = c("#999999", "#E69F00"), 
                     name = "Matching")+
  scale_x_continuous(labels=c("1" = "0", "2" = "60", "3" = "120", "4" = "180", "5" = "240", "6" = "300"),
                       breaks = 1:6)+
  xlab("Rotation")+
  ylab("Adjusted Accuracy"))
  
tiff("plots/acc_adjusted-badsrem_outrem.png", units="in", width=10, height=8, res=300)
b# insert ggplot code
dev.off()

#----RT----
# with bads removed
data <- prepare_for_model(data_type="badsrem", RT=TRUE)
rt.model <- readRDS("models/rt-model_processed_badsrem.rds")
summary(rt.model)

#----Post-hoc tests----
emm <- emmeans(rt.model,  ~movable|match)
pairs(emm)
# -> sig diff between mov and nonmov in match condition but not in mismatch condition
emmip(rt.model, movable ~ angle | match, mult.name = "variety", cov.reduce = FALSE)
emtrends(rt.model, pairwise ~ movable | match, var = "angle", mult.name = "variety", max.degree = 2)

#----plot rt----
agg_data <- aggregate(data=data, log_RT~sub+angle+match+movable,mean)
(c <- ggplot(agg_data, aes(x=angle, y=log_RT, color=match))+
    geom_point(aes(color=match),position = position_jitter(), alpha=.3)+
    geom_smooth(formula = y ~ x+I(x^2), method="lm")+
    scale_color_manual(labels = c("match","mismatch"),
                       values = c("#999999", "#E69F00"), 
                       name = "Matching")+
    xlab("Rotation")+
    ylab("Log(RT)")+
    facet_grid(~movable))
tiff("plots/rt-badsrem.png", units="in", width=10, height=8, res=300)
c# insert ggplot code
dev.off()

#----plot adjusted rt----
data$rt_model <- keepef(rt.model, fix =  c("poly(angle, 2)2:match1:movable1"),grouping = TRUE, keep.intercept = FALSE) 
agg_data_adj_rt <- aggregate(data=data, rt_model~sub+angle+match+movable,mean)

(d <- ggplot(agg_data_adj_rt, aes(x=angle, y=rt_model, color=match))+
    geom_point(aes(color=match),position = position_jitter(), alpha=.3)+
    geom_smooth(formula = y ~ x+I(x^2), method="lm")+
    scale_color_manual(labels = c("match","mismatch"),
                       values = c("#999999", "#E69F00"), 
                       name = "Matching")+
    scale_x_continuous(labels=c("1" = "0", "2" = "60", "3" = "120", "4" = "180", "5" = "240", "6" = "300"),
                       breaks = 1:6)+
    xlab("Rotation")+
    ylab("Adjusted Log(RT)")+
    facet_grid(~movable))
tiff("plots/processed_badsrem-rt_adjusted.png", units="in", width=10, height=8, res=300)
d# insert ggplot code
dev.off()

