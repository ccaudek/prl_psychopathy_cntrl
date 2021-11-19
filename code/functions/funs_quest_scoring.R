

save_quest_scales_tripm_cntrl <- function() {
  
  all_quest <- readRDS(
    here("data", "processed", "quest", "quest_tripm_cntrl.rds")
  )
  
  
  # Scoring TriPM -----------------------------------------------------------
  
  
  # Step 1: Coding Responses
  # 
  # For items followed by [F]-i.e., items 2, 4, 10, 11, 16, 21, 25, 30, 33, 35, 
  # 39, 41, 44, 47, 50, 52, 57-code responses as follows: True = 0; Somewhat 
  # true = 1; Somewhat false = 2; False = 3.
  # 
  # Code responses for all other items as follows: True = 3; Somewhat true = 2; 
  # Somewhat false = 1; False = 0.
  # 
  # Step 2: Computing Scale Scores and Total Scores
  # 
  # Boldness subscale (19 items)-Sum coded responses for the following items:
  #   1, 4, 7, 10, 13, 16, 19, 22, 25, 28, 32, 35, 38, 41, 44, 47, 50, 54, 57
  # 
  # Meanness subscale (19 items)-Sum coded responses for the following items:
  #   2, 6, 8, 11, 14, 17, 20, 23, 26, 29, 33, 36, 39, 40, 42, 45, 48, 52, 55
  # 
  # Disinhibition subscale (20 items)-Sum coded responses for the following items:
  #   3, 5, 9, 12, 15, 18, 21, 24, 27, 30, 31, 34, 37, 43, 46, 49, 51, 53, 56, 58
  # 
  # Total Psychopathy score-Sum scores across the three subscales.
  
  nrow(all_quest)
  length(unique(all_quest$subj_code))
  
  # Only tripm items.
  temp <- all_quest %>% 
    dplyr::select(matches('TRIPM'))
  # They are coded with range: 1-4. Therefore, subtract 1 to get range: 0-3.
  temp1 <- temp - 1
  # Now coding is in the range 0-3.
  # summary(temp1)
  
  # There is a problem in how the data have been coded.
  # Examine item 58: "I have stolen something out of a vehicle."
  table(temp1$TRIPM_58)
  # The vast majority has chosen the response 3. But it should be 0.
  # This means that the numbers are inverted. So, I have to invert everything.
  reverse_coding_fun <- function(x) {
    if (max(x) < 4) {
      res = abs(x - 3)
    } else(
      res = "ERROR"
    )
    res
  }
  
  tripm_correct <- temp1 %>%
    mutate_all(reverse_coding_fun)
  
  table(tripm_correct$TRIPM_58)

  tripm_correct$subj_code <- all_quest$subj_code
  
  # Boldness
  dat_b <- tripm_correct %>% 
    dplyr::select(
      TRIPM_1, TRIPM_7, TRIPM_13, TRIPM_19, TRIPM_22, TRIPM_28, 
      TRIPM_32, TRIPM_38, TRIPM_54, 
      # reverse scoring
      TRIPM_4, TRIPM_10, TRIPM_16,
      TRIPM_25, TRIPM_35, TRIPM_41, TRIPM_44, TRIPM_47, TRIPM_50,
      TRIPM_57
    )
  
  # presence of boldness
  keys_b <- c(rep(1, 9), rep(-1, 10))
  clean_b <- psych::reverse.code(keys_b, dat_b, mini=0,maxi=3)
  all_quest$boldness <- rowSums(clean_b)
  # hist(all_quest$boldness)
  # psych::alpha(clean_b)
  # mean(all_quest$boldness)
  
  
  # Meanness
  dat_m <- tripm_correct %>% 
    dplyr::select(
      # reversed coded
      TRIPM_2, TRIPM_11, TRIPM_33, TRIPM_39, TRIPM_52, 
      # normally coded
      TRIPM_6, TRIPM_8, TRIPM_14, TRIPM_17, TRIPM_20,  TRIPM_23, 
      TRIPM_26, TRIPM_29, TRIPM_36, TRIPM_40, TRIPM_42, 
      TRIPM_45, TRIPM_48, TRIPM_55
    )
  
  # presence of meanness
  keys_m <- c(rep(-1, 5), rep(1, 14))
  clean_m <- psych::reverse.code(keys_m, dat_m, mini=0,maxi=3)
  all_quest$meanness <- rowSums(clean_m)
  hist(all_quest$meanness)
  mean(all_quest$meanness)
  
  # Disinhibition
  dat_d <- tripm_correct %>% 
    dplyr::select(
      TRIPM_3, TRIPM_5, TRIPM_9, TRIPM_12, TRIPM_15, TRIPM_18, TRIPM_24, 
      TRIPM_27, TRIPM_31, TRIPM_34, TRIPM_37, TRIPM_43, TRIPM_46, TRIPM_49, 
      TRIPM_51, TRIPM_53, TRIPM_56, TRIPM_58, 
      # reversed coded
      TRIPM_21, TRIPM_30
    )
  
  # presence of dishinibition
  keys_d <- c(rep(1, 18), rep(-1, 2))
  clean_d <- psych::reverse.code(keys_d, dat_d, mini=0,maxi=3)
  all_quest$disinhibition <- rowSums(clean_d)
  # hist(all_quest$disinhibition)
  # mean(all_quest$disinhibition)
  

  # TRIPM subscales and subject code
  tripm_scales <- data.frame(
    subj_code = all_quest$subj_code,
    boldness = all_quest$boldness,
    meanness = all_quest$meanness,
    disinhibition = all_quest$disinhibition
  )
  # summary(tripm_scales)
  
  
  # DASS-21 -----------------------------------------------------------------
  
  dass_scales <- data.frame(
    subj_code = all_quest$subj_code
  )
  
  # DASS-21: Stress subscale
  temp_s <- all_quest %>% 
    dplyr::select(
      c("DASS_1", "DASS_6", "DASS_8", "DASS_11", "DASS_12", "DASS_14", 
        "DASS_18")
    )
  dass_scales$stress <- rowSums(temp_s)
  
  # DASS-21: Anxiety subscale
  temp_a <- all_quest %>% 
    dplyr::select(
      c("DASS_2", "DASS_4", "DASS_7", "DASS_9", "DASS_15", "DASS_19", 
        "DASS_20")
    )
  dass_scales$anxiety <- rowSums(temp_a)
  
  # DASS-21: Depression subscale
  temp_d <- all_quest %>% 
    dplyr::select(
      c("DASS_3", "DASS_5", "DASS_10", "DASS_13", "DASS_16", "DASS_17", 
        "DASS_21")
    )
  dass_scales$depression <- rowSums(temp_d)
  
  
  
  
  # Rosenberg ---------------------------------------------------------------
  
  # The scale ranges from 0-30. Scores between 15 and 25 are within normal 
  # range; scores below 15 suggest low self-esteem.
  temp <- all_quest %>% 
    dplyr::select(matches('ROS'))
  
  # To have a range between 0 and 3.
  ros_scale <- temp - 1
  
  keys_ros <- c(1, 1, -1, 1, -1, 1, 1, -1, -1, -1)
  clean_ros <- psych::reverse.code(keys_ros, ros_scale, mini=0,maxi=3)

  rosenberg_scale <- data.frame(
    subj_code = all_quest$subj_code
  )
  
  rosenberg_scale$ros <- rowSums(clean_ros)
  
  
  
  # Combine three questionnaires --------------------------------------------
  
  temp <- left_join(tripm_scales, dass_scales, by = "subj_code")
  quest_dat <- left_join(temp, rosenberg_scale, by = "subj_code")
  
  
  # Save subscales TRIPM,  DASS-21, Rosenberg -------------------------------
  
  saveRDS(
    quest_dat, 
    here("data", "processed", "quest", "group_control_quest_scales.rds")
  )
  
}





