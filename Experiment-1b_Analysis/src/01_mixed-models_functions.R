#################################
#     functions for (g)lmms     #
#################################
# This script contains functions for
# 01_model-selection.R 
# Author: Aylin Kallmayer
# Date: 18.03.2022

#----PROCESSED DATA----#
prepare_for_model <- function(data_type, RT=FALSE){
#  processed = nothing removed
#  badsrem = removed bad stimuli
#  badsrem_outrem = removed bads and outliers

  if (data_type == "processed"){
    if (RT==TRUE){
      data <- read.csv("data/processed/processed-RT.csv")
    }else{
      data <- read.csv("data/processed/processed-acc.csv")
    }
  }else if (data_type=="badsrem"){
    if (RT==TRUE){
      data <- read.csv("data/processed/processed-RT_badsrem.csv")
    }else{
      data <- read.csv("data/processed/processed-acc_badsrem.csv")
    }
  }else if (data_type=="badsrem_outrem"){
    data <- read.csv("data/processed/processed-acc_badsrem_outrem.csv")
  }
  # change angles to scale from 1-6
  data <- data %>%
    mutate(angle = case_when(rotation==0~1,
                             rotation==60~2,
                             rotation==120~3,
                             rotation==180~4,
                             rotation==240~5,
                             rotation==300~6))
  data$movable <- as.factor(data$movable)
  data$match <- as.factor(data$match)
  contrasts(data$match) <- contr.sum(2)
  contrasts(data$movable) <- contr.sum(2)
  return(data)
}

