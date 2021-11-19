# Script name: 001_data_wrangling.R
# Project: PRL and TriPM, control group.
# Script purpose: Save raw questionnaires data.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Mon Nov 15 11:09:41 2021
# Last Modified Date: Mon Nov 15 11:09:41 2021
# 
# Notes: 

# img_pair
# 1 = torta (ipercalorico) 1
# 2 = ordine 3
# 3 = insalata (ipocalorico) 1
# 4 = bilancia 3
# 5 = magrezza  2
# 6 = sovrappeso 2
# 7 = rifiuto sociale 2
# 8 = circonferenza 3


library("tidyverse")
library("here")
library("forcats")
library("tidybayes")
library("brms")
library("cmdstanr")
library("robcor")

source(here("code", "functions", "funs_quest_tripm_cntrl.R"))
source(here("code", "functions", "funs_quest_scoring.R"))



# Save raw questionnaires data --------------------------------------------


save_raw_quest_tripm_cntrl()
save_quest_scales_tripm_cntrl()



#  e  n  d  ----

