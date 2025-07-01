################################################################
# Codes to performing imputation analysis in CareQOL project
# Jitao Wang
# 08/13/2024
################################################################

library(tidyverse)
library(ggplot2)
library(geepack)
library(sjPlot)
library(gridExtra)
library(ggpubr)
options(digits = 3)
options(pillar.sigfig = 3)
source("functions.R")

prefix = "/home/jitwang/University of Michigan Dropbox/Jitao Wang/CareQOL Project/MRT_analysis_TBI"

# load data
dat <-
  read_csv(file.path(prefix, "data/imputed_data_weekly_all_with_hr_1.csv"),
           guess_max = 3000) %>%
  filter(group == "JITAI") %>%
  arrange(subjectid, week) %>%
  group_by(subjectid) %>%
  mutate(
    heart_rate_mean = (
      heart_rate_mean_Mon + heart_rate_mean_Tue + heart_rate_mean_Wed +
        heart_rate_mean_Thu + heart_rate_mean_Fri + heart_rate_mean_Sat +
        heart_rate_mean_Sun
    ) / 7
  ) %>%
  mutate(across(
    c(
      ends_with("_t_score"),
      ends_with("_count"),
      "heart_rate_mean",
      "resting_heart_rate"
    ),
    ~ lag(.x, 1),
    .names = "{.col}_pre"
  )) %>%
  ungroup()


tmp = dat %>%
  mutate(
    msg_level = case_when(
      msg_cusum == 0 ~ "None",
      msg_cusum <= 2 ~ "Low",
      msg_cusum <= 4 ~ "Medium",
      msg_cusum <= 6 ~ "High"
    ),
    msg_binary = case_when(msg_cusum <= 1 ~ "Low", msg_cusum <= 6 ~ "High"),
  ) %>%
  mutate(
    msg_level = factor(msg_level, levels = c("None", "Low", "Medium", "High")),
    msg_binary = factor(msg_binary, levels = c("Low", "High"))
  ) %>%
  mutate(dem_race = case_when(dem_race == "white" ~ "white", TRUE ~ "others")) %>%
  mutate(dem_race = factor(dem_race, levels = c("white", "others"))) %>%
  mutate(
    step_count = step_count ^ (1 / 3),
    step_count_pre = step_count_pre ^ (1 / 3),
    sleep_count = sleep_count / 60,
    sleep_count_pre = sleep_count_pre / 60
  ) %>%
  mutate(
    dem_timespent = case_when(
      dem_timespent == 1 ~ "less than 2 hours",
      dem_timespent == 2 ~ "3 to 4 hours",
      dem_timespent == 3 ~ "5 to 8 hours",
      dem_timespent == 4 ~ "9 to 12 hours",
      dem_timespent == 5 ~ "greater than 12 hours"
    )
  ) %>%
  mutate(dem_timespent = factor(
    dem_timespent,
    levels = c(
      "less than 2 hours",
      "3 to 4 hours",
      "5 to 8 hours",
      "9 to 12 hours",
      "greater than 12 hours"
    )
  ))

# subgroup
engagers = read_csv(file.path(prefix, "data/engager.csv")) %>% filter(engager == 1) %>% pull(subjectid)
non_engagers = read_csv(file.path(prefix, "data/engager.csv")) %>% filter(engager == 0) %>% pull(subjectid)

subgroups = list(engager = engagers, non_engager = non_engagers)
for (subgroup in names(subgroups)) {
  # create a folder for the subgroup
  figure_folder = file.path(prefix, "figures", subgroup)
  dir.create(figure_folder, showWarnings = FALSE)
  
  subgroup_name = subgroup
  subgroup_list = subgroups[[subgroup]]
  fit_results = list()
  if (subgroup_name == "engager") {
    fit_results_4lvl_e_tinv = run_analysis(tmp, subgroup_name, subgroup_list)
  } else {
    fit_results_4lvl_de_tinv = run_analysis(tmp, subgroup_name, subgroup_list)
  }
  
}

run_analysis = function(tmp, subgroup_name, subgroup_list) {
  # subgroup settings
  fit_results = list()
  fit_results_raw = list()
  tmp1 = tmp %>% filter(subjectid %in% subgroup_list)
  
  baseline_vars = c(
    "dem_sex",
    "dem_age",
    "dem_ethnicity",
    "mrc_tbisev_new",
    "dem_race",
    "dem_lengthcared",
    "dem_timespent",
    "Caregiver_Stress_t_score_BL",
    "Sadness_t_score_BL",
    "Worry_t_score_BL"
  )
  control_vars = c(
    "week",
    "Caregiver_Stress_t_score_pre",
    "Sadness_t_score_pre",
    "Worry_t_score_pre",
    "step_count_pre",
    "sleep_count_pre",
    "resting_heart_rate_pre"
  )
  
  ##############################################Caregiver Stress############################################################
  for (var_bl in baseline_vars) {
    print(var_bl)
    
    # caregiver stress t score
    f <- paste(
      "Caregiver_Stress_t_score ~ Caregiver_Stress_t_score_pre + msg_level * ",
      var_bl,
      " + ",
      paste(baseline_vars, collapse = " + "),
      " + ",
      paste(control_vars, collapse = " + ")
    )
    # f <- paste("Caregiver_Stress_t_score ~ Caregiver_Stress_t_score_pre + msg_level * ", var_bl, " + ", paste(baseline_vars,collapse = " + ")) # remove pre_t_score, results changed.
    # f <- paste("Caregiver_Stress_t_score ~  msg_level * ", var_bl, " + ", paste(baseline_vars,collapse = " + "))
    
    fit = geeglm(
      as.formula(f),
      data = tmp1 %>% filter(!is.na(Caregiver_Stress_t_score_pre)),
      id = factor(subjectid),
      corstr = "i"
    )
    fit_results[[paste("cs_", var_bl, sep = "")]] <- summary(fit)
    fit_results_raw[[paste("cs_", var_bl, sep = "")]] <- fit
    
    # worry t score
    f <- paste(
      "Worry_t_score ~ Worry_t_score_pre + msg_level * ",
      var_bl,
      " + ",
      paste(baseline_vars, collapse = " + "),
      " + ",
      paste(control_vars, collapse = " + ")
    )
    
    fit = geeglm(
      as.formula(f),
      data = tmp1 %>% filter(!is.na(Worry_t_score_pre)),
      id = factor(subjectid),
      corstr = "i"
    )
    fit_results[[paste("worry_", var_bl, sep = "")]] <- summary(fit)
    fit_results_raw[[paste("worry_", var_bl, sep = "")]] <- fit
    
    # sadness t score
    f <- paste(
      "Sadness_t_score ~ Sadness_t_score_pre + msg_level * ",
      var_bl,
      " + ",
      paste(baseline_vars, collapse = " + "),
      " + ",
      paste(control_vars, collapse = " + ")
    )
    
    fit = geeglm(
      as.formula(f),
      data = tmp1 %>% filter(!is.na(Sadness_t_score_pre)),
      id = factor(subjectid),
      corstr = "i"
    )
    fit_results[[paste("sadness_", var_bl, sep = "")]] <- summary(fit)
    fit_results_raw[[paste("sadness_", var_bl, sep = "")]] <- fit
  }
  return(fit_results_raw)
}

######################################################
# output table
vars0 = c(
  "dem_age",
  "dem_sex",
  "dem_ethnicity",
  "dem_race",
  "dem_lengthcared",
  "dem_timespent",
  "Caregiver_Stress_t_score_BL",
  "Sadness_t_score_BL",
  "Worry_t_score_BL"
)
vars = c(
  "dem_age",
  "dem_sexmale",
  "dem_ethnicitynon-hs",
  "dem_raceothers",
  "dem_lengthcared",
  "dem_timespent",
  "Caregiver_Stress_t_score_BL",
  "Sadness_t_score_BL",
  "Worry_t_score_BL"
)
d = tibble()
p = tibble()
# outcome_var = "cs"
# outcome_var = "worry"
outcome_var = "sadness"
for (i in 1:length(vars)) {
  var0 = vars0[i]
  var = vars[i]
  low = fit_results[[paste(outcome_var, "_", var0, sep = "")]]$coefficients[paste("msg_levelLow:", var, sep = ""), "Estimate"]
  low_std = fit_results[[paste(outcome_var, "_", var0, sep = "")]]$coefficients[paste("msg_levelLow:", var, sep = ""), "Std.err"]
  medium = fit_results[[paste(outcome_var, "_", var0, sep = "")]]$coefficients[paste("msg_levelMedium:", var, sep = ""), "Estimate"]
  medium_std = fit_results[[paste(outcome_var, "_", var0, sep = "")]]$coefficients[paste("msg_levelMedium:", var, sep = ""), "Std.err"]
  high = fit_results[[paste(outcome_var, "_", var0, sep = "")]]$coefficients[paste("msg_levelHigh:", var, sep = ""), "Estimate"]
  high_std = fit_results[[paste(outcome_var, "_", var0, sep = "")]]$coefficients[paste("msg_levelHigh:", var, sep = ""), "Std.err"]
  low = sprintf("%.2f", low)
  low_std = sprintf("%.2f", low_std)
  medium = sprintf("%.2f", medium)
  medium_std = sprintf("%.2f", medium_std)
  high = sprintf("%.2f", high)
  high_std = sprintf("%.2f", high_std)
  d = d %>% bind_rows(tibble(
    var = c(paste(outcome_var, "_", var, sep = "")),
    low = c(paste(low, " (", low_std, ")", sep = "")),
    medium = c(paste(medium, " (", medium_std, ")", sep = "")),
    high = c(paste(high, " (", high_std, ")", sep = ""))
  ))
  
  low_p = fit_results[[paste(outcome_var, "_", var0, sep = "")]]$coefficients[paste("msg_levelLow:", var, sep = ""), "Pr(>|W|)"]
  medium_p = fit_results[[paste(outcome_var, "_", var0, sep = "")]]$coefficients[paste("msg_levelMedium:", var, sep = ""), "Pr(>|W|)"]
  high_p = fit_results[[paste(outcome_var, "_", var0, sep = "")]]$coefficients[paste("msg_levelHigh:", var, sep = ""), "Pr(>|W|)"]
  p = p %>% bind_rows(tibble(
    var = c(paste(outcome_var, "_", var, sep = "")),
    low = c(sprintf("%.3f", low_p)),
    medium = c(sprintf("%.3f", medium_p)),
    high = c(sprintf("%.3f", high_p))
  ))
}
d
p
view(d)
######################################################
## three way interaction for sleep





######################################################
## supplementary plots
pdf(file.path(prefix, "figures/supplementary_plots.pdf"))
qqnorm(log(dat$Caregiver_Stress_t_score), main = "Caregiver Stress")
qqline(log(dat$Caregiver_Stress_t_score))
qqnorm(log(dat$Worry_t_score), main = "Worry")
qqline(log(dat$Worry_t_score))
qqnorm(log(dat$Sadness_t_score), main = "Sadness")
qqline(log(dat$Sadness_t_score))

tmp %>% select(msg_level,
               Caregiver_Stress_t_score,
               Worry_t_score,
               Sadness_t_score) %>%
  mutate(across(where(is.numeric), ~ log(.x))) %>%
  pivot_longer(-msg_level) %>%
  ggplot(aes(x = msg_level, y = value)) +
  geom_boxplot() +
  facet_wrap(~ name, scales = "free_y")

tmp %>% select(msg_level,
               Caregiver_Stress_t_score,
               Worry_t_score,
               Sadness_t_score) %>%
  pivot_longer(-msg_level) %>%
  ggplot(aes(x = msg_level, y = value)) +
  geom_boxplot() +
  facet_wrap(~ name, scales = "free_y")

tmp %>% select(msg_level) %>% ggplot(aes(x = msg_level)) +
  geom_bar() +
  xlab("Message frequency") +
  ylab("Count")

tmp %>% select(msg_binary) %>% ggplot(aes(x = msg_binary)) +
  geom_bar() +
  xlab("Message frequency") +
  ylab("Count")

dev.off()


## revision 1 for JMIR
tmp %>% select(starts_with("t_score"), step_count, sleep_count) %>% ungroup() %>%
  summarize(across(where(is.numeric), .fns = ~ mean(is.na(.x))))
# t_score_CaregiverStress t_score_Sadness t_score_Worry step_count sleep_count
# <dbl>           <dbl>         <dbl>      <dbl>       <dbl>
#
# 1                  0.0968          0.0970        0.0971     0.0281       0.142
tmp %>% select(starts_with("t_score"), step_count, sleep_count) %>% ungroup() %>%
  summarize(across(where(is.numeric), .fns = ~ sum(is.na(.x))))
# t_score_CaregiverStress t_score_Sadness t_score_Worry step_count sleep_count
# <int>           <int>         <int>      <int>       <int>
#   1                     561             562           563        163         822
tmp %>% select(starts_with("t_score"), step_count, sleep_count) %>% ungroup() %>% nrow() # 5796
tmp
