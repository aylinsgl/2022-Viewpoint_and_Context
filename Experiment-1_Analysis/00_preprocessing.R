#################################
#        Preprocessing          #
#################################
# This script does:
# 1) outlier detection
# 2) box cox transformation for RT data
# 3) saves preprocessed files to data/processed/ 
# for respective experiment folders
# color = Experiment 1a
# grey = Experiment 1b
# 
# Flags: 
# badsrem = bad objects removed
# outrem = outliers removed
# badsrem_outrem = bads and outliers removed
# 
# Author: Aylin Kallmayer
# Year: 2022

#----Load packages----
if (!require("pacman")) install.packages("pacman")
p_load("dplyr","MASS","lattice")

source("src/00_preprocessing-functions.R")

#----Define experiments----
experiments <- c("grey","color") 

#----Do preprocessing for both experiments
for (experiment in experiments){
  #----Read raw data----
  dat <- read.csv(paste("data/raw/",experiment,"/Rohdaten.csv", sep = ""), header=TRUE, sep=",")
  
  # select only relevant variables
  dat <- dplyr::select(dat, sub, rotation, imagename, match, imgID, block, correct, response_time)
  
  #----ACCURACY----
  # Look at distribution of accuracy for the stimuli
  agg_stim <- group_by(dat, imagename, rotation) %>%
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
  agg_RT <- aggregate(data=datRT, response_time ~ sub+rotation+match, mean)
  agg_RT_badsrem <- aggregate(data=datRT_badsrem, response_time ~ sub+rotation+match, mean)
  
  # look at distribution
  densityplot(agg_RT_badsrem$response_time[agg_RT_badsrem$match=="match" & agg_RT_badsrem$rotation==120])
  
  # determine lambda for RTs
  if (experiment == "color"){
    lambdaList <- MASS::boxcox(response_time~rotation+match, data=agg_RT)
    (lambda <- lambdaList$x[which.max(lambdaList$y)]) #  log
    
    lambdaList <- MASS::boxcox(response_time~rotation+match, data=agg_RT_badsrem)
    (lambda <- lambdaList$x[which.max(lambdaList$y)]) #  log
    
    # look at distribution of residuals
    qqmath(agg_RT$response_time)
    qqmath(log(agg_RT$response_time))
    
    qqmath(agg_RT_badsrem$response_time)
    qqmath(log(agg_RT_badsrem$response_time))
    
    # log transform RTs 
    datRT$log_RT <- log(datRT$response_time)
    datRT_badsrem$log_RT <- log(datRT_badsrem$response_time)
  } else if (experiment == "grey"){
    lambdaList <- MASS::boxcox(response_time~rotation+match, data=agg_RT)
    (lambda <- lambdaList$x[which.max(lambdaList$y)]) # 
    
    lambdaList <- MASS::boxcox(response_time~rotation+match, data=agg_RT_badsrem)
    (lambda <- lambdaList$x[which.max(lambdaList$y)]) #  
    
    # look at distribution of residuals
    qqmath(agg_RT$response_time)
    qqmath(log(agg_RT$response_time))
    qqmath(1/(agg_RT$response_time))
    
    densityplot(agg_RT$response_time[agg_RT$rotation==0])
    densityplot(agg_RT$response_time)
    
    qqmath(agg_RT_badsrem$response_time)
    qqmath(log(agg_RT_badsrem$response_time))
    qqmath(1/(agg_RT_badsrem$response_time))
    
    # inverse transform RTs 
    datRT$inverse_RT <- 1/(datRT$response_time)
    datRT_badsrem$inverse_RT <- 1/(datRT_badsrem$response_time)
    
    # log transform RTs 
    datRT$log_RT <- log(datRT$response_time)
    datRT_badsrem$log_RT <- log(datRT_badsrem$response_time)
  }
  
  #----Write files----
  write.csv(dat, paste("data/processed/",experiment,"/processed-acc.csv", sep = ""))
  write.csv(dat_badsrem, paste("data/processed/",experiment,"/processed-acc_badsrem.csv",sep = ""))
  write.csv(dat_outrem, paste("data/processed/",experiment,"/processed-acc_outrem.csv",sep = ""))
  write.csv(dat_badsrem_outrem, paste("data/processed/",experiment,"/processed-acc_badsrem_outrem.csv",sep = ""))
  write.csv(datRT, paste("data/processed/",experiment,"/processed-RT.csv",sep = ""))
  write.csv(datRT_badsrem, paste("data/processed/",experiment,"/processed-RT_badsrem.csv",sep = ""))
}
