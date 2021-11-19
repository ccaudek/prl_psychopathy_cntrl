# Script name: 005_params_analyses.R
# Project: PRL and TriPM, control group.
# Script purpose: Save raw questionnaires data.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Mon Nov 15 11:09:41 2021
# Last Modified Date: Wed Nov 17 07:11:34 2021
#
# Notes:


# Meanness is defined by poor empathy, callousness, excitement-seeking, 
# and predatory aggression (Patrick et al., 2009). 
# Disinhibition is defined by externalizing behaviors such as hostile-angry, 
# impulsivity, and poor emotional regulation. 
# Boldness is defined by low anxiety, social poise, familiarity with danger, 
# and dominance; it is conceptually and empirically isomorphic to the PPI 
# higher-order factor known as Fearless Dominance (Lilienfeld et al., 2012; 
# Sellbom & Phillips, 2013)


# Neutral images.
# High fearlessness: alpha_neg increases with meanness.
# Low fearlessness: alpha_neg decreases with meanness, especially when D is 
# low (emotional regulation is high)

# Disturbing images.
# High fearlessness: alpha_neg decreases with meanness, especially when D is 
# low (emotional regulation is high)


# img_pair
# 1 = torta (ipercalorico) 1
# 2 = ordine 3
# 3 = insalata (ipocalorico) 1
# 4 = bilancia 3
# 5 = magrezza  2
# 6 = sovrappeso 2
# 7 = rifiuto sociale 2
# 8 = circonferenza 3

suppressPackageStartupMessages({
  library("tidyverse")
  library("here")
  library("forcats")
  library("tidybayes")
  library("brms")
  library("cmdstanr")
  library("performance")
  library("robcor")
})

# Increase max print
options(max.print = .Machine$integer.max)

source(here::here("code", "functions", "funs_param_analyses_neutral_Hrl.R"))


# Read questionnaires data ------------------------------------------------

quest <- readRDS(
  here("data", "processed", "quest", "group_exp_quest_scales.rds")
)


# Generate RDS file with params and subj_code -----------------------------

# Administration 3: only three blocks (food, object, social)
add_subj_code_to_hddm_params_neutral_hrl()


# Read PRL parameters -----------------------------------------------------

# Subjects with 8 blocks with food-related stimuli.
temp <- readRDS(
  here("data", "processed", "prl", "param_rp_block.rds")
) %>%
  dplyr::rename(
    subj_code = subj_name
  ) %>%
  dplyr::select(-c("subject_index", "is_food_reward_first"))

# length(unique(temp$subj_code))

temp$img_pair <- ifelse(
  is.na(temp$img_pair), 3, temp$img_pair
)

# Collapse img_pair categories
temp1 <- temp %>%
  mutate(
    stim = case_when(
      img_pair == 1 | img_pair == 3 ~ "food",
      img_pair == 5 | img_pair == 6 | img_pair == 7 ~ "body",
      img_pair == 2 | img_pair == 4 | img_pair == 8 ~ "object",
      TRUE ~ "ERROR"
    )
  ) %>%
  dplyr::select(-c("block", "img_pair")) %>% 
  dplyr::filter(total_reward > 80)
temp1$total_reward <- NULL

temp1$ap <- car::logit(temp1$Arew)
temp1$an <- car::logit(temp1$Apun)


params_food <- temp1 %>% 
  group_by(subj_code, stim) %>% 
  summarise(
    alpha_pos = mean(ap),
    alpha_neg = mean(an),
    beta = mean(beta)
  )
params_food$stim <- factor(params_food$stim)
params_food$subj_code <- factor(params_food$subj_code)
params_food$flag <- 1


# Subjects with 3 blocks with food-related and neutral stimuli.
params_neutral <- readRDS(
  here("data", "params", "Hrl_params_stim_neutral.rds")
) %>%
  dplyr::select(-c("subj_idx")) 

# params_neutral$stim <- forcats::fct_recode(
#   params_neutral$stim,
#   "body" = "social"
# )

params_neutral$stim <- forcats::fct_recode(
    params_neutral$stim,
    "body1" = "social",
    "object1" = "object",
    "food1" = "food"
  )
params_neutral$stim <- factor(params_neutral$stim)
# table(params_neutral$stim)
params_neutral$flag <- 0

# Combine summarized params for subjects with 8 blocks and subjects with
# 3 blocks. In the following data, each subject has only 3 estimates for 
# each parameter, one in each of the food, object, and body stimuli.
params_tot <- bind_rows(params_food, params_neutral)
# length(unique(params_tot$subj_code))
# length(unique(params_food$subj_code)) + length(unique(params_neutral$subj_code))


# Combine questionnaires and PRL parameters -------------------------------

rp_dat_clean <- left_join(params_tot, quest, by = "subj_code") %>%
  na.omit()
# length(unique(rp_dat_clean$subj_code))


# Save data for classification.
if (0) {
  d1 <- d %>%
    group_by(subj_code, stim) %>%
    summarise(
      alpha_pos = mean(Arew, na.rm = TRUE),
      alpha_neg = mean(Apun, na.rm = TRUE),
      alpha_pos = mean(Apun, na.rm = TRUE),
      beta = mean(beta, na.rm = TRUE),
      boldness = mean(boldness, na.rm = TRUE),
      meanness = mean(meanness, na.rm = TRUE),
      disinhibition = mean(disinhibition, na.rm = TRUE),
      stress = mean(stress, na.rm = TRUE),
      depression = mean(depression, na.rm = TRUE),
      anxiety = mean(anxiety, na.rm = TRUE),
      ros = mean(ros, na.rm = TRUE)
    )

  d1$exp <- "control"

  saveRDS(
    d1,
    "group_control.rds"
  )
}


# Data transformation -----------------------------------------------------

summary(rp_dat_clean)

# hist(rp_dat_clean$alpha_pos)
# sort(rp_dat_clean$Apun)[1:100]
# rp_dat_clean$alpha_pos <- car::logit(rp_dat_clean$Arew)
# hist(rp_dat_clean$alpha_pos)
# rp_dat_clean$alpha_neg <- car::logit(rp_dat_clean$Apun)
# hist(rp_dat_clean$alpha_neg)

rp_dat_clean$b <- scale(rp_dat_clean$boldness) %>% as.numeric()
rp_dat_clean$d <- scale(rp_dat_clean$disinhibition) %>% as.numeric()
rp_dat_clean$m <- scale(rp_dat_clean$meanness) %>% as.numeric()

robcor::robcor(
  tibble(rp_dat_clean$b, rp_dat_clean$d, rp_dat_clean$m)
)

rp_dat_clean$bmd <- 
  scale(rp_dat_clean$b * rp_dat_clean$m * rp_dat_clean$d) %>% 
  as.numeric()

rp_dat_clean$bd <- scale(rp_dat_clean$b * rp_dat_clean$d) %>% as.numeric()
rp_dat_clean$bm <- scale(rp_dat_clean$b * rp_dat_clean$m) %>% as.numeric()
rp_dat_clean$md <- scale(rp_dat_clean$m * rp_dat_clean$d) %>% as.numeric()


# Delete outliers ---------------------------------------------------------

foo <- rp_dat_clean %>% 
  dplyr::select(
    alpha_pos, alpha_neg, beta, boldness, meanness, disinhibition
  )
foo$subj_code <- NULL

foo <- as.data.frame(foo)

# Find outliers
outliers_list <- check_outliers(
  foo,
  method = c(
    "cook", 
    "pareto",
    "iqr",
    "zscore_robust",
    "bci",
    "mcd", 
    "ics", 
    "optics", 
    "lof"
  )
) 

outliers_list # Show the row index of the outliers
# as.numeric(outliers_list) # The object is a binary vector...
outliers_info <- as.data.frame(outliers_list)
filtered_data <- rp_dat_clean[outliers_info$Outlier < 0.55, ]
nrow(filtered_data)


# beta --------------------------------------------------------------------

filtered_data_1 <- filtered_data %>% 
  dplyr::filter(beta > 0)

filtered_data_1 %>%
  ggplot(aes(x = beta)) +
  geom_density()

priors <- c(
  set_prior("normal(0, 5)", class = "b")
)

brmf1 <- bf(
  beta ~ b * m * d + flag +
    (1 | subj_code) #+ (1 | stim)
)

m1 <- brm(
  brmf1,
  data = filtered_data_1,
  prior = priors,
  family = weibull(),
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  iter = 2000,
  cores = 12,
  backend = "cmdstan",
  seed = 7584
)

pp_check(m1, ndraws = 50) + xlim(-1, 10)
loo1 <- loo(m1)
plot(loo1)

summary(m1)
#           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept     0.62      0.05     0.52     0.73 1.01      648     1079
# b             0.06      0.04    -0.02     0.14 1.01      562     1073
# m            -0.08      0.07    -0.21     0.05 1.01      669     1219
# d            -0.04      0.06    -0.16     0.09 1.01      628     1417
# flag         -0.44      0.07    -0.57    -0.31 1.01      602     1102
# b:m           0.12      0.07    -0.01     0.26 1.01      577     1341
# b:d           0.02      0.06    -0.11     0.14 1.00      760     1117
# m:d           0.09      0.04     0.02     0.16 1.01      739      965
# b:m:d        -0.06      0.03    -0.13     0.00 1.00      835     1419

bayes_R2(m1)


plot(
  conditional_effects(
    m1,
    effects = "m:d",
    prob = 0
  ),
  points = FALSE,
  rug = FALSE
)
hypothesis(m1, "m:d > 0")
#   Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1  (m:d) > 0     0.09      0.04     0.03     0.15     165.67      0.99    *   

hypothesis(m1, "b:m:d < 0")
#    Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (b:m:d) < 0    -0.06      0.03    -0.12    -0.01      28.85      0.97    *

conditions <- make_conditions(m1, "b")
conditional_effects(m1, "m:d", conditions = conditions, prob = 0)





# alpha positive ----------------------------------------------------------


filtered_data %>%
  ggplot(aes(x = alpha_pos)) +
  geom_density()

priors <- c(
  set_prior("normal(0, 5)", class = "b")
  # set_prior("normal(0, 2)", class = "b", coef = "Intercept" )
)

brmf2 <- bf(
  alpha_pos ~ bmd + flag +
    (1 | subj_code) + (1 | stim),
  sigma ~ bmd
)

m2 <- brm(
  brmf2,
  data = filtered_data,
  prior = priors,
  family = student(),
  control = list(adapt_delta = 0.95, max_treedepth = 15),
  iter = 10000,
  cores = 12,
  backend = "cmdstan",
  seed = 3534534
)

pp_check(m2, ndraws = 50) + xlim(-0.5, 2.5)
loo2 <- loo(m2)
plot(loo2)

summary(m2)
#                Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept           0.31      0.14     0.02     0.61 1.00     8126     7611
# sigma_Intercept    -2.26      0.09    -2.44    -2.08 1.00    27557    14632
# bmd                -0.00      0.02    -0.03     0.03 1.00    24333    15864
# flag                0.18      0.21    -0.24     0.60 1.00     7698     7416
# sigma_bmd           0.08      0.08    -0.07     0.23 1.00    24131    15731
bayes_R2(m2)
plot(
  conditional_effects(
    m2,
    effects = "bmd"
  ),
  points = FALSE,
  rug = FALSE
)
hypothesis(m2, "bmd < 0")
#   Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1  (bmd) < 0        0      0.02    -0.02     0.03       1.17      0.54  


brmf2_int <- bf(
  alpha_pos ~ b * m * d + flag +
    (1 | subj_code) + (1 | stim)
)

m2_int <- brm(
  brmf2_int,
  data = filtered_data,
  prior = priors,
  family = student(), # asym_laplace does not work here!
  control = list(adapt_delta = 0.95, max_treedepth = 15),
  iter = 2000,
  cores = 12,
  backend = "cmdstan",
  seed = 68967
)

pp_check(m2_int, ndraws = 50) + xlim(-0.5, 2.5)
loo2 <- loo(m2_int)
plot(loo2)

summary(m2_int)
#           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept     0.31      0.12     0.04     0.56 1.00     1546     1734
# b             0.00      0.01    -0.01     0.01 1.00     5412     3411
# m             0.01      0.01    -0.01     0.02 1.00     4767     3396
# d             0.00      0.01    -0.01     0.02 1.00     4339     2870
# flag          0.19      0.18    -0.17     0.58 1.00     1431     1372
# b:m           0.00      0.01    -0.01     0.02 1.00     6466     3066
# b:d          -0.00      0.01    -0.02     0.01 1.00     5400     2955
# m:d           0.00      0.01    -0.01     0.02 1.00     5855     3423
# b:m:d        -0.01      0.01    -0.02     0.01 1.00     5169     2941



# alpha negative ----------------------------------------------------------

filtered_data %>%
  ggplot(aes(x = alpha_neg)) +
  geom_density()

priors <- c(
  set_prior("normal(0, 5)", class = "b")
  # set_prior("normal(0, 2)", class = "b", coef = "Intercept" )
)

brmf3 <- bf(
  alpha_neg ~ bmd + flag +
    (1 | subj_code) + (1 | stim),
  quantile = 0.5
  # sigma ~ bmd 
)
# get_prior(brmf3, data = rp_dat_clean)


m3 <- brm(
  brmf3,
  data = filtered_data,
  prior = priors,
  family = asym_laplace(),
  control = list(adapt_delta = 0.9, max_treedepth = 15),
  iter = 3000,
  cores = 12,
  backend = "cmdstan",
  seed = 3840821
)

pp_check(m3, ndraws = 50) + xlim(-1.5, 1.5)
loo3 <- loo(m3)
plot(loo3)

summary(m3)
#           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept     2.25      0.18     1.90     2.59 1.00     3662     6635
# bmd          -0.30      0.13    -0.57    -0.05 1.00     3238     5287
# flag         -1.81      0.23    -2.26    -1.36 1.00     2444     5658

bayes_R2(m3)
plot(
  conditional_effects(
    m3,
    effects = "bmd"
  ),
  points = FALSE,
  rug = FALSE
)
hypothesis(m3, "bmd < 0")
#   Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1  (bmd) < 0     -0.3      0.13    -0.53    -0.09      107.7      0.99    *

brmf3_pairs <- bf(
  alpha_neg ~ bd + bm + md + flag +
    (1 | subj_code) + (1 | stim),
  quantile = 0.5
)
m3_pairs <- brm(
  brmf3_pairs,
  data = filtered_data,
  prior = priors,
  family = asym_laplace(),
  control = list(adapt_delta = 0.9, max_treedepth = 15),
  iter = 10000,
  cores = 12,
  backend = "cmdstan",
  seed = 3840821
)

pp_check(m3_pairs, ndraws = 50) + xlim(-1.5, 1.5)
loo3_pairs <- loo(m3_pairs, cores = 12)
plot(loo3_pairs)

summary(m3_pairs)
#          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept     2.28      0.18     1.93     2.61 1.00     2317     5159
# bd            0.15      0.18    -0.21     0.51 1.00     1416     3600
# bm            0.03      0.18    -0.33     0.38 1.00     1308     3524
# md           -0.11      0.13    -0.36     0.14 1.00     1630     3496
# flag         -1.80      0.23    -2.23    -1.34 1.00     1747     4018

bayes_R2(m3_pairs)
plot(
  conditional_effects(
    m3_pairs,
    effects = "md"
  ),
  points = FALSE,
  rug = FALSE
)
hypothesis(m3, "bmd < 0")




# TriPM sum ---------------------------------------------------------------


filtered_data$tripm_sum <- 
  scale(filtered_data$b + filtered_data$m + filtered_data$d) %>% 
  as.numeric()

hist(filtered_data$tripm_sum)
robcor::robcor(filtered_data$tripm_sum, filtered_data$bmd)
plot(foo$tripm_sum, foo$bmd)

brmf3_sum <- bf(
  alpha_neg ~ tripm_sum + flag +
    (1 | ID | subj_code) + (1 | ID | stim),
  quantile = 0.5
)
mod_3 <- brm(
  brmf3_sum,
  data = filtered_data,
  prior = priors,
  family = asym_laplace(),
  control = list(adapt_delta = 0.9, max_treedepth = 15),
  iter = 4000,
  cores = 12,
  backend = "cmdstan",
  seed = 3840821
)

pp_check(mod_3, ndraws = 50) + xlim(-1.5, 1.5)
loo3_sum <- loo(mod_3, cores = 12)
plot(loo3_sum)

summary(mod_3)

bayes_R2(mod_3)
plot(
  conditional_effects(
    mod_3,
    effects = "tripm_sum"
  ),
  points = TRUE,
  rug = FALSE
)
hypothesis(mod_3, "tripm_sum < 0")
#        Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (tripm_sum) < 0    -0.31      0.09    -0.46    -0.15       1999         1    *




# TriPM scales interaction ------------------------------------------------

brmf3_int <- bf(
  alpha_neg ~ b * m * d + flag +
    (1 | subj_code) + (1 | stim),
  quantile = 0.5
)
mod_3 <- brm(
  brmf3_int,
  data = filtered_data,
  prior = priors,
  family = asym_laplace(),
  control = list(adapt_delta = 0.9, max_treedepth = 15),
  iter = 2000,
  cores = 12,
  backend = "cmdstan",
  seed = 3840821
)
pp_check(mod_3, ndraws = 50) + xlim(-1.5, 1.5)
loo3_int <- loo(mod_3, cores = 12)
plot(loo3_int)

summary(mod_3)


conditions <- make_conditions(mod_3, "b")
conditional_effects(mod_3, "m:d", conditions = conditions, prob = 0)


hypothesis(mod_3, "b:m:d < 0")
hypothesis(mod_3, "m:d > 0")
hypothesis(mod_3, "d < 0")

me <- conditional_effects(mod_3, "d:m", prob = 0)
plot(me)

plot(conditional_effects(mod_3, "d"))

plot(me, plot = FALSE)[[1]] +
  scale_color_grey() +
  scale_fill_grey()



# Mediation ---------------------------------------------------------------

thedat <- filtered_data %>% 
  ungroup() %>% 
  dplyr::select(
    B = (boldness), 
    M = meanness, 
    D = disinhibition, 
    AP = alpha_pos, 
    AN = alpha_neg, 
    BE = beta
  )

cor_data <- robcor::robcor(thedat)

model <- ' 
  # Meanness
  
  # direct effect
  AP ~ ap_m*M
  # mediator
  B ~ b_m*M
  AP ~ ap_b*B
  # indirect effect  
  ind_pos_m := b_m*ap_b
  # total effect
  total_pos_m := ap_m + (b_m*ap_b)
  
  # direct effect
  AN ~ an_m*M
  # mediator
  AN ~ an_b*B
  # indirect effect
  ind_neg_m := b_m*an_b
  # total effect
  total_neg_m := an_m + (b_m*an_b)
  
  # direct effect
  BE ~ be_m*M
  # indirect effect 
  ind_be_m := b_m*be_m
  ind_m := ind_pos_m + ind_neg_m + ind_be_m
  # total effect
  total_be_m := be_m + (b_m*be_m)
  total := total_pos_m + total_neg_m + total_be_m
  
  # Disihibition
  
  # direct effect
  AP ~ ap_d*D
  # mediator
  B ~ b_d*D
  # indirect effect 
  ind_pos_d := b_d*ap_b
  # total effect
  total_pos_d := ap_d + (b_d*ap_b)
  
  # direct effect
  AN ~ an_d*D
  # indirect effect 
  ind_neg_d := b_d*an_b
  # total effect
  total_neg_d := an_d + (b_d*an_b)
  
  # direct effect
  BE ~ be_d*D
  # indirect effect 
  ind_be_d := b_d*be_d
  ind_d := ind_pos_d + ind_neg_d + ind_be_d
  # total effect
  total_be_d := be_d + (b_d*be_d)
  
  ind_m := ind_pos_m + ind_neg_m + ind_be_m 
  ind_d := ind_pos_d + ind_neg_d + ind_be_d
  
  total_m := total_pos_m + total_neg_m + total_be_m 
  total_d := total_pos_d + total_neg_d + total_be_d 
  
'

fit <- sem(model, data = thedat)
summary(fit)


# alpha positive

model <- ' 
  # Meanness
  
  # direct effect
  AP ~ ap_m*M
  # mediator
  B ~ b_m*M
  AP ~ ap_b*B
  # indirect effect  
  ind_pos_m := b_m*ap_b
  # total effect
  total_pos_m := ap_m + (b_m*ap_b)
  
  # Disihibition
  
  # direct effect
  AP ~ ap_d*D
  # mediator
  B ~ b_d*D
  # indirect effect 
  ind_pos_d := b_d*ap_b
  # total effect
  total_pos_d := ap_d + (b_d*ap_b)
  
  ind_m := ind_pos_m 
  ind_d := ind_pos_d 
  
  total_m := total_pos_m 
  total_d := total_pos_d 
  
'

fit <- sem(model, data = thedat)
summary(fit, standardized=TRUE, rsquare=TRUE)


# alpha negative


model <- ' 
  # Meanness
  
  B ~ b_m*M
  
  # direct effect
  AN ~ an_m*M
  # mediator
  AN ~ an_b*B
  # indirect effect
  ind_neg_m := b_m*an_b
  # total effect
  total_neg_m := an_m + (b_m*an_b)
  
  # Disihibition
  
  B ~ b_d*D

  # direct effect
  AN ~ an_d*D
  # indirect effect 
  ind_neg_d := b_d*an_b
  # total effect
  total_neg_d := an_d + (b_d*an_b)
  
  ind_m := ind_neg_m 
  ind_d := ind_neg_d 
  
  total_m := total_neg_m 
  total_d := total_neg_d
  
'

fit <- sem(model, data = thedat)
summary(fit, standardized=TRUE, rsquare=TRUE)





