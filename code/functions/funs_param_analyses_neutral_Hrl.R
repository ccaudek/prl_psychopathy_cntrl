# Script name: funs_param_analyses_neutral_Hrl.R
# Project: TriPM and PRL. Neutral condition.
# Script purpose: Wrangling the parameters file created by Hrl.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Wed Nov 17 16:07:32 2021
# Last Modified Date: Wed Nov 17 16:07:32 2021
# 
# Notes: 

#' @description Save RDS file with PRL params and subj_code.
#' @return 
#' NULL.
#' @save
#' RDS file.
add_subj_code_to_hddm_params_neutral_hrl <- function() {
  
  # look-up table for the PRL task: integer IDs and subj_coe for all participants
  lookup_tbl <- rio::import(
    here::here(
      "data", "params", "neutral_prl_lookup_table.csv"
    )
  ) %>% 
    distinct()
  
  
  # Edit the file containing the output of the Python hDDM analysis.
  # Correct the first line:
  # ii mean std 2.5q 25q 50q 75q 97.5q mc_err
  # Delete the last three lines and the lines with the estimates of the fixed
  # effects -- preserve only the estimates of the random effects
  params_prl_ddm <- rio::import(
    here::here("data", "params", "neutral_params_Hrl.txt")
  )
  
  # get the first two characters from params_prl_ddm:
  params_prl_ddm$param <- substr(params_prl_ddm$ii, start = 1, stop = 2)
  # unique(params_prl_ddm$param)
  # [1] "a_" "v_" "t_" "z_" "al" "po"
  
  # Get subj_idx.
  params_prl_ddm$id_param_string <- gsub(".*\\).", "", params_prl_ddm$ii) 
  # params_prl_ddm$id_param_string <- stringr::str_remove(params_prl_ddm$ii, "[).]")
  params_prl_ddm$subj_idx <- as.integer(readr::parse_number(params_prl_ddm$id_param_string))
  # params_prl_ddm$subj_idx[1:10]
  # [1] 0 1 2 3 4 5 6 7 8 9
  # unique(params_prl_ddm$subj_idx)
  
  
  # Get stimulus.
  dd <- params_prl_ddm$ii
  params_prl_ddm$stim <- sub('.*\\((.*)\\).*', '\\1', dd)
  # unique(params_prl_ddm$stim)
  
  params_prl_ddm$params <- params_prl_ddm$param %>% 
    dplyr::recode(
      "v_" = "beta",
      "al" = "alpha_neg",
      "po" = "alpha_pos"
    )
  # summary(factor(params_prl_ddm$params))
  
  params_prl_ddm_clean <- params_prl_ddm %>% 
    dplyr::select(
      subj_idx, stim, params, mean
    ) %>% 
    dplyr::rename(
      value = mean
    )
  
  
  d <- left_join(params_prl_ddm_clean, lookup_tbl, by = "subj_idx") 
  d$subj_code <- factor(d$subj_code)
  
  d_wide <- d %>%
    pivot_wider(
      names_from = params,
      values_from = value
    )
  
  saveRDS(
    d_wide,
    here::here("data", "params", "Hrl_params_stim_neutral.rds")
  )
  
}

