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
library(splines)
options(digits = 3)
options(pillar.sigfig = 3)
source("functions.R")

prefix = "/home/jitwang/University of Michigan Dropbox/Jitao Wang/CareQOL Project/MRT_analysis_TBI"

# load data
dat <- read_csv(file.path(prefix, "data/imputed_data_weekly_all_with_hr_1.csv"),
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
    step_count_pre = step_count_pre ^ (1 / 3),
    step_count = step_count ^ (1 / 3),
    sleep_count_pre = sleep_count_pre / 60,
    sleep_count = sleep_count / 60
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
  
  out = run_analysis(subgroup_name, subgroup_list)
  if (subgroup_name == "engager") {
    fit_results_4lvl_e_raw = out[["fit_raw"]]
  }
  else{
    fit_results_4lvl_de_raw = out[["fit_raw"]]
  }
  
}

run_analysis = function(subgroup_name, subgroup_list) {
  fit_results_4lvl = list()
  fit_results_4lvl_raw = list()
  # subgroup settings
  tmp1 = tmp %>% filter(subjectid %in% subgroup_list)
  
  # control vars
  control_vars = c(
    "week",
    "Caregiver_Stress_t_score_pre",
    "Sadness_t_score_pre",
    "Worry_t_score_pre",
    "step_count_pre",
    "sleep_count_pre",
    "resting_heart_rate_pre",
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
  
  ##############################################Caregiver Stress############################################################
  
  ## previous t_score_CaregiverStress
  f = as.formula(
    paste(
      "Caregiver_Stress_t_score ~ msg_level * Caregiver_Stress_t_score_pre",
      " + ",
      paste(control_vars, collapse = " + ")
    )
  )
  fit.stress_4lvl_stress <- geeglm(
    f,
    data = tmp1 %>% filter(!is.na(Caregiver_Stress_t_score_pre)),
    id = factor(subjectid),
    corstr = "i"
  )
  fit_results_4lvl[["Caregiver Stress_t_score_pre"]] <- summary(fit.stress_4lvl_stress)
  fit_results_4lvl_raw[["Caregiver Stress_t_score_pre"]] <- fit.stress_4lvl_stress
  
  ## week-in-study
  f = as.formula(paste(
    "Caregiver_Stress_t_score ~ msg_level * week",
    " + ",
    paste(control_vars, collapse = " + ")
  ))
  fit.stress_4lvl_week <- geeglm(
    f,
    data = tmp1 %>% mutate(week = week - 1) %>% filter(!is.na(Caregiver_Stress_t_score_pre)),
    id = factor(subjectid),
    corstr = "i"
  )
  fit_results_4lvl[["Caregiver Stress_week"]] <- summary(fit.stress_4lvl_week)
  fit_results_4lvl_raw[["Caregiver Stress_week"]] <- fit.stress_4lvl_week
  
  ## previous week step count
  f = as.formula(paste(
    "Caregiver_Stress_t_score ~ msg_level * step_count_pre",
    " + ",
    paste(control_vars, collapse = " + ")
  ))
  fit.stress_4lvl_step <- geeglm(
    f,
    data = tmp1 %>% filter(!is.na(step_count_pre)) %>% filter(!is.na(Caregiver_Stress_t_score_pre)),
    id = factor(subjectid),
    corstr = "i"
  )
  fit_results_4lvl[["Caregiver Stress_step_count"]] <- summary(fit.stress_4lvl_step)
  fit_results_4lvl_raw[["Caregiver Stress_step_count"]] <- fit.stress_4lvl_step
  
  ## previous week sleep
  f = as.formula(paste(
    "Caregiver_Stress_t_score ~ msg_level * sleep_count_pre",
    " + ",
    paste(control_vars, collapse = " + ")
  ))
  fit.stress_4lvl_sleep <- geeglm(
    f,
    data = tmp1 %>%  filter(!is.na(sleep_count_pre)) %>% filter(!is.na(Caregiver_Stress_t_score_pre)),
    id = factor(subjectid),
    corstr = "i"
  )
  fit_results_4lvl[["Caregiver Stress_sleep_count"]] <- summary(fit.stress_4lvl_sleep)
  fit_results_4lvl_raw[["Caregiver Stress_sleep_count"]] <- fit.stress_4lvl_sleep
  
  # resting heart rate
  f = as.formula(
    paste(
      "Caregiver_Stress_t_score ~ msg_level * resting_heart_rate_pre",
      " + ",
      paste(control_vars, collapse = " + ")
    )
  )
  fit.stress_4lvl_rhr <- geeglm(
    f,
    data = tmp1 %>% filter(!is.na(resting_heart_rate_pre)),
    id = factor(subjectid),
    corstr = "i"
  )
  fit_results_4lvl[["Caregiver Stress_resting_heart_rate"]] <- summary(fit.stress_4lvl_rhr)
  fit_results_4lvl_raw[["Caregiver Stress_resting_heart_rate"]] <- fit.stress_4lvl_rhr
  
  # worry score
  f = as.formula(paste(
    "Caregiver_Stress_t_score ~ msg_level * Worry_t_score_pre",
    " + ",
    paste(control_vars, collapse = " + ")
  ))
  fit.stress_4lvl_worry <- geeglm(
    f,
    data = tmp1 %>% filter(!is.na(Worry_t_score_pre)),
    id = factor(subjectid),
    corstr = "i"
  )
  fit_results_4lvl[["Caregiver Stress_worry_pre"]] <- summary(fit.stress_4lvl_worry)
  fit_results_4lvl_raw[["Caregiver Stress_worry_pre"]] <- fit.stress_4lvl_worry
  
  # sadness score
  f = as.formula(paste(
    "Caregiver_Stress_t_score ~ msg_level * Sadness_t_score_pre",
    " + ",
    paste(control_vars, collapse = " + ")
  ))
  fit.stress_4lvl_sadness <- geeglm(
    f,
    data = tmp1 %>% filter(!is.na(Sadness_t_score_pre)),
    id = factor(subjectid),
    corstr = "i"
  )
  fit_results_4lvl[["Caregiver Stress_sadness_pre"]] <- summary(fit.stress_4lvl_sadness)
  fit_results_4lvl_raw[["Caregiver Stress_sadness_pre"]] <- fit.stress_4lvl_sadness
  
  
  ##############################################   Worry ############################################################
  
  ## previous t_score_Worry
  f = as.formula(paste(
    "Worry_t_score ~ msg_level * Worry_t_score_pre",
    " + ",
    paste(control_vars, collapse = " + ")
  ))
  fit.worry_4lvl_worry <- geeglm(
    f,
    data = tmp1 %>% filter(!is.na(Worry_t_score_pre)),
    id = factor(subjectid),
    corstr = "i"
  )
  fit_results_4lvl[["Worry_t_score_pre"]] <- summary(fit.worry_4lvl_worry)
  fit_results_4lvl_raw[["Worry_t_score_pre"]] <- fit.worry_4lvl_worry
  
  ## week-in-study
  f = as.formula(paste(
    "Worry_t_score ~ msg_level * week",
    " + ",
    paste(control_vars, collapse = " + ")
  ))
  fit.worry_4lvl_week <- geeglm(
    f,
    data = tmp1 %>% mutate(week = week - 1) %>% filter(!is.na(Worry_t_score_pre)),
    id = factor(subjectid),
    corstr = "i"
  )
  fit_results_4lvl[["Worry_week"]] <- summary(fit.worry_4lvl_week)
  fit_results_4lvl_raw[["Worry_week"]] <- fit.worry_4lvl_week
  
  ## previous week step count
  f = as.formula(paste(
    "Worry_t_score ~ msg_level * step_count_pre",
    " + ",
    paste(control_vars, collapse = " + ")
  ))
  fit.worry_4lvl_step <- geeglm(
    f,
    data = tmp1 %>% filter(!is.na(step_count_pre)),
    id = factor(subjectid),
    corstr = "i"
  )
  fit_results_4lvl[["Worry_step_count"]] <- summary(fit.worry_4lvl_step)
  fit_results_4lvl_raw[["Worry_step_count"]] <- fit.worry_4lvl_step
  
  ## previous week sleep
  f = as.formula(paste(
    "Worry_t_score ~ msg_level * sleep_count_pre",
    " + ",
    paste(control_vars, collapse = " + ")
  ))
  fit.worry_4lvl_sleep <- geeglm(
    f,
    data = tmp1 %>%  filter(!is.na(sleep_count_pre)),
    id = factor(subjectid),
    corstr = "i"
  )
  fit_results_4lvl[["Worry_sleep_count"]] <- summary(fit.worry_4lvl_sleep)
  fit_results_4lvl_raw[["Worry_sleep_count"]] <- fit.worry_4lvl_sleep
  
  ## resting heart rate
  f = as.formula(paste(
    "Worry_t_score ~ msg_level * resting_heart_rate_pre",
    " + ",
    paste(control_vars, collapse = " + ")
  ))
  fit.worry_4lvl_rhr <- geeglm(
    f,
    data = tmp1 %>% filter(!is.na(resting_heart_rate_pre)),
    id = factor(subjectid),
    corstr = "i"
  )
  fit_results_4lvl[["Worry_resting_heart_rate"]] <- summary(fit.worry_4lvl_rhr)
  fit_results_4lvl_raw[["Worry_resting_heart_rate"]] <- fit.worry_4lvl_rhr
  
  # caregiver stress score
  f = as.formula(paste(
    "Worry_t_score ~ msg_level * Caregiver_Stress_t_score_pre",
    " + ",
    paste(control_vars, collapse = " + ")
  ))
  fit.worry_4lvl_stress <- geeglm(
    f,
    data = tmp1 %>% filter(!is.na(Caregiver_Stress_t_score_pre)),
    id = factor(subjectid),
    corstr = "i"
  )
  fit_results_4lvl[["Worry_stress_pre"]] <- summary(fit.worry_4lvl_stress)
  fit_results_4lvl_raw[["Worry_stress_pre"]] <- fit.worry_4lvl_stress
  
  # sadness score
  f = as.formula(paste(
    "Worry_t_score ~ msg_level * Sadness_t_score_pre",
    " + ",
    paste(control_vars, collapse = " + ")
  ))
  fit.worry_4lvl_sadness <- geeglm(
    f,
    data = tmp1 %>% filter(!is.na(Sadness_t_score_pre)),
    id = factor(subjectid),
    corstr = "i"
  )
  fit_results_4lvl[["Worry_sadness_pre"]] <- summary(fit.worry_4lvl_sadness)
  fit_results_4lvl_raw[["Worry_sadness_pre"]] <- fit.worry_4lvl_sadness
  
  ##############################################Sadness############################################################
  
  ## previous t_score_Sadness
  f = as.formula(paste(
    "Sadness_t_score ~ msg_level * Sadness_t_score_pre",
    " + ",
    paste(control_vars, collapse = " + ")
  ))
  fit.sadness_4lvl_sadness <- geeglm(
    f,
    data = tmp1 %>% filter(!is.na(Sadness_t_score_pre)),
    id = factor(subjectid),
    corstr = "i"
  )
  fit_results_4lvl[["Sadness_t_score_pre"]] <- summary(fit.sadness_4lvl_sadness)
  fit_results_4lvl_raw[["Sadness_t_score_pre"]] <- fit.sadness_4lvl_sadness
  
  ## week-in-study
  f = as.formula(paste(
    "Sadness_t_score ~ msg_level * week",
    " + ",
    paste(control_vars, collapse = " + ")
  ))
  fit.sadness_4lvl_week <- geeglm(
    f,
    data = tmp1 %>% mutate(week = week - 1) %>% filter(!is.na(Sadness_t_score_pre)),
    id = factor(subjectid),
    corstr = "i"
  )
  fit_results_4lvl[["Sadness_week"]] <- summary(fit.sadness_4lvl_week)
  fit_results_4lvl_raw[["Sadness_week"]] <- fit.sadness_4lvl_week
  
  ## previous week step count
  f = as.formula(paste(
    "Sadness_t_score ~ msg_level * step_count_pre",
    " + ",
    paste(control_vars, collapse = " + ")
  ))
  fit.sadness_4lvl_step <- geeglm(
    f,
    data = tmp1 %>% filter(!is.na(step_count_pre)),
    id = factor(subjectid),
    corstr = "i"
  )
  fit_results_4lvl[["Sadness_step_count"]] <- summary(fit.sadness_4lvl_step)
  fit_results_4lvl_raw[["Sadness_step_count"]] <- fit.sadness_4lvl_step
  
  ## previous week sleep
  f = as.formula(paste(
    "Sadness_t_score ~ msg_level * sleep_count_pre",
    " + ",
    paste(control_vars, collapse = " + ")
  ))
  fit.sadness_4lvl_sleep <- geeglm(
    f,
    data = tmp1 %>%  filter(!is.na(sleep_count_pre)),
    id = factor(subjectid),
    corstr = "i"
  )
  fit_results_4lvl[["Sadness_sleep_count"]] <- summary(fit.sadness_4lvl_sleep)
  fit_results_4lvl_raw[["Sadness_sleep_count"]] <- fit.sadness_4lvl_sleep
  
  
  ## resting heart rate
  f = as.formula(paste(
    "Sadness_t_score ~ msg_level * resting_heart_rate_pre",
    " + ",
    paste(control_vars, collapse = " + ")
  ))
  fit.sadness_4lvl_rhr <- geeglm(
    f,
    data = tmp1 %>% filter(!is.na(resting_heart_rate_pre)),
    id = factor(subjectid),
    corstr = "i"
  )
  fit_results_4lvl[["Sadness_resting_heart_rate"]] <- summary(fit.sadness_4lvl_rhr)
  fit_results_4lvl_raw[["Sadness_resting_heart_rate"]] <- fit.sadness_4lvl_rhr
  
  # caregiver stress score
  f = as.formula(paste(
    "Sadness_t_score ~ msg_level * Caregiver_Stress_t_score_pre",
    " + ",
    paste(control_vars, collapse = " + ")
  ))
  fit.sadness_4lvl_stress <- geeglm(
    f,
    data = tmp1 %>% filter(!is.na(Caregiver_Stress_t_score_pre)),
    id = factor(subjectid),
    corstr = "i"
  )
  fit_results_4lvl[["Sadness_stress_pre"]] <- summary(fit.sadness_4lvl_stress)
  fit_results_4lvl_raw[["Sadness_stress_pre"]] <- fit.sadness_4lvl_stress
  
  # worry score
  f = as.formula(paste(
    "Sadness_t_score ~ msg_level * Worry_t_score_pre",
    " + ",
    paste(control_vars, collapse = " + ")
  ))
  fit.sadness_4lvl_worry <- geeglm(
    f,
    data = tmp1 %>% filter(!is.na(Worry_t_score_pre)),
    id = factor(subjectid),
    corstr = "i"
  )
  fit_results_4lvl[["Sadness_worry_pre"]] <- summary(fit.sadness_4lvl_worry)
  fit_results_4lvl_raw[["Sadness_worry_pre"]] <- fit.sadness_4lvl_worry
  return(list(fit = fit_results_4lvl, fit_raw = fit_results_4lvl_raw))
  
}

######################################################

m = fit_results_4lvl_e_raw[["Caregiver Stress_step_count"]]$coefficients[c("msg_levelLow:step_count_pre", "msg_levelMedium:step_count_pre", "msg_levelHigh:step_count_pre")]
se = sqrt(diag(vcov(fit_results_4lvl_e_raw[["Caregiver Stress_step_count"]])))[c("msg_levelLow:step_count_pre", "msg_levelMedium:step_count_pre", "msg_levelHigh:step_count_pre")]
a1 = (7149+1000)**(1/3)
a2 = (7149)**(1/3)

(a1 - a2) * m
(a1 - a2) * se
((a1 - a2) * m) / ((a1 - a2) * se)

######################################################

d = tibble()
for (name in names(fit_results_4lvl)) {
  result = fit_results_4lvl[[name]]
  nrows = nrow(result$coefficients)
  out = result$coefficients[(nrows - 2):nrows, 1:2]
  out = out %>% mutate(
    lb = Estimate - 1.96 * `Std.err`,
    ub = Estimate + 1.96 * `Std.err`,
    p = pnorm(abs(Estimate / `Std.err`), lower.tail = FALSE) * 2
  )
  print(name)
  # display only three decimal places
  print(out %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
}

######################################################



## previous week sleep
spline_model_formula = as.formula(paste(
  "Sadness_t_score ~ ns(sleep_count_pre, df = 3) * msg_level",
  " + ",
  paste(control_vars, collapse = " + ")
))
fit.sadness_4lvl_sleep = glm(
  spline_model_formula,
  data = tmp1 %>% filter(!is.na(Sadness_t_score_pre)),
  family = gaussian(link = "identity")
)
(
  my_reflection_plot(
    fit = fit.sadness_4lvl_sleep,
    trt_name = "msg_level",
    trt_levels = c("Low", "Medium", "High"),
    mod_name = "sleep_count_pre",
    log = FALSE
  ) +
    xlab("Previous week's daily average sleep hours") +
    ylab("Effect of the JITAI on Sadness T score")
) %>% print()
fit.sadness_4lvl_sleep <- geeglm(
  Sadness_t_score ~ msg_level * sleep_count_pre + week + dem_sex + dem_age + dem_ethnicity + mrc_tbisev_new + Sadness_t_score_pre,
  data = tmp1 %>%  filter(!is.na(sleep_count_pre)),
  id = factor(subjectid),
  corstr = "i"
)
(
  my_reflection_plot(
    fit = fit.sadness_4lvl_sleep,
    trt_name = "msg_level",
    trt_levels = c("Low", "Medium", "High"),
    mod_name = "sleep_count_pre",
    log = FALSE
  ) +
    xlab("Previous week's daily average sleep minutes") +
    ylab("Effect of the JITAI on Sadness T score")
) %>% print()



######################################################
## three way interaction for sleep
engagers = read_csv(file.path(prefix, "data/engager.csv")) %>% filter(engager == 1) %>% pull(subjectid)
tmp = tmp %>% mutate(engage = case_when(subjectid %in% engagers ~ 1, TRUE ~ 0))
f = as.formula(paste(
  "Sadness_t_score ~ msg_level * step_count_pre * engage",
  " + ",
  paste(control_vars, collapse = " + ")
))
fit = geeglm(
  f,
  data = tmp %>% filter(!is.na(step_count_pre)),
  id = factor(subjectid),
  corstr = "i"
)

summary(fit)



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
