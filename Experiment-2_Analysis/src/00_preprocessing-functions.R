#################################
#   Preprocessing functions     #  
#################################
# This script does:
# 1) contains functions for 00_preprocessing.R
#
# Author: Aylin Kallmayer
# Date: 01.04.2022

find_acc_outliers <- function(dat, by="subject", condition=2.5){
  acc_outlier <- group_by(dat, rotation, consistency) %>%
    dplyr::summarise(mean_correct = mean(correct),
              SD_correct = sd(correct),
              lower = mean_correct-condition*SD_correct,
              .groups="keep") %>%
    dplyr::select(rotation, consistency, lower) %>%
    full_join(dat)
  if (by == "subject"){
    sub_summary <- group_by(acc_outlier, sub, rotation, consistency) %>%
      dplyr::summarise(mean_correct_sub = mean(correct),
                lower = mean(lower)) %>%
      mutate(out = case_when(round(mean_correct_sub,2) < lower ~ 1,
                             TRUE ~ 0))
    # outliers: 
    outlier_subjs <- filter(sub_summary, out == 1)
    return(outlier_subjs)
  }
  else if (by=="image"){
    image_summary <- group_by(acc_outlier, imagename, rotation, consistency) %>%
      dplyr::summarise(mean_correct_img = mean(correct),
                lower = mean(lower)) %>%
      mutate(out = case_when(round(mean_correct_img,2) < lower ~ 1,
                             TRUE ~ 0))
    # outliers: 
    outlier_images <- filter(image_summary, out == 1)
    return(outlier_images)
  }
}
