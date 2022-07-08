#################################
#        Preprocessing          #
#################################
# This script does:
# 1) checks if any objects produced accuracy below .6
# 2) creates files with and without bad objects for accuracy and RT
# 3) checks for accuracy participant outliers
# 4) box cox transformation for RT data

# Author: Aylin Kallmayer
# Date: 01.04.2022

source("src/00_preprocessing-functions.R")

#----Load packages----
if (!require("pacman")) install.packages("pacman")
p_load("dplyr","lattice")

#----Read raw data----
dat <- read.csv("data/raw/Rohdaten.csv", header=TRUE, sep=",")
dat <- dat[dat$matchx == "match",]

# select only relevant variables
dat <- dplyr::select(dat, sub,consistency,rotation,imgID,imagename,correct,response_time)

#----ACCURACY----
# Look at distribution of accuracy for the stimuli
agg_stim <- group_by(dat, imagename, rotation, consistency) %>%
  dplyr::summarise(mean_acc = mean(correct),
            .groups="keep")
hist(agg_stim$mean_acc)

# objects that had accuracy below 2.5 mean per condition
(bads <- find_acc_outliers(dat, by = "image"))

# create dataframe with bads removed
if (nrow(bads) != 0){
  dat_badsrem <- anti_join(dat, bads)
} else {
  dat_badsrem <- dat
}


# participants who performed worse than 2.5 SD below mean per condition (with bads)
(bad_subjs <- find_acc_outliers(dat))
# -> no outlieres 

# participants who performed worse than 2.5 SD below mean per condition (without bads)
(bad_subjs <- find_acc_outliers(dat_badsrem))
# -> no outliers    

# if outliers detected, remove them from original dataframe
dat_outrem <- filter(dat, !sub %in% bad_subjs$sub)
dat_badsrem_outrem <- filter(dat_badsrem, !sub %in% bad_subjs$sub)

#----RT----
# use either dat, dat_badsrem, dat_outrem, dat_badsrem_outrem
datRT <- dat_outrem
datRT_badsrem <- dat_badsrem_outrem

# select only correct trials 
datRT <- dat_outrem[dat_outrem$correct == 1,]
datRT_badsrem <- datRT_badsrem[datRT_badsrem$correct == 1,]

# look how many % trials were excluded 
before <- NROW(dat_outrem)
after <- NROW(datRT)
(excl_trials<-100-after/before * 100)

before <- NROW(dat_badsrem_outrem)
after <- NROW(datRT_badsrem)
(excl_trials<-100-after/before * 100)

# aggregate
agg_RT <- aggregate(data=datRT, response_time ~ sub+rotation+consistency, mean)
agg_RT_badsrem <- aggregate(data=datRT_badsrem, response_time ~ sub+rotation+consistency, mean)

# determine lambda for RTs
lambdaList <- MASS::boxcox(response_time~rotation+consistency, data=agg_RT)
(lambda <- lambdaList$x[which.max(lambdaList$y)]) #  inverse sqrt transformation or log transformation - we keep log because we hate inverse transformations

lambdaList <- MASS::boxcox(response_time~rotation+consistency, data=agg_RT_badsrem)
(lambda <- lambdaList$x[which.max(lambdaList$y)]) #  inverse sqrt transformation or log transformation - we keep log because we hate inverse transformations

# look at distribution of residuals
qqmath(agg_RT$response_time)
qqmath(log(agg_RT$response_time))

qqmath(agg_RT_badsrem$response_time)
qqmath(log(agg_RT_badsrem$response_time))

# log transform RTs 
datRT$log_RT <- log(datRT$response_time)
datRT_badsrem$log_RT <- log(datRT_badsrem$response_time)

#----Write files----
write.csv(dat, "data/processed/processed-acc.csv")
write.csv(dat_badsrem, "data/processed/processed-acc_badsrem.csv")
write.csv(dat_outrem, "data/processed/processed-acc_outrem.csv")
write.csv(dat_badsrem_outrem, "data/processed/processed-acc_badsrem_outrem.csv")
write.csv(datRT, "data/processed/processed-RT.csv")
write.csv(datRT_badsrem, "data/processed/processed-RT_badsrem.csv")

