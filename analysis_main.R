library(tidyverse)
library(ggplot2)
library(geepack)
library(sjPlot)
library(gridExtra)
library(ggpubr)
library(knitr)
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
  mutate(across(
    c(
      ends_with("_t_score"),
      ends_with("_count"),
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
  mutate(dem_race_original = dem_race) %>% 
  mutate(dem_race = case_when(dem_race == "white" ~ "white",
                              dem_race == "black" ~ "black",
                              TRUE ~ "other")) %>%
  mutate(step_count = step_count ^ (1/3),
         step_count_pre = step_count_pre ^ (1/3),
         sleep_count = sleep_count / 60,
         sleep_count_pre = sleep_count_pre / 60) %>%
  mutate(dem_timespent = factor(dem_timespent))

engagers = read_csv(file.path(prefix, "data/engager.csv"))
tmp <- tmp %>% left_join(engagers, by = "subjectid")

# baseline variables
bl_vars = c(
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
tv_vars = c(
  "week",
  "Caregiver_Stress_t_score_pre",
  "Sadness_t_score_pre",
  "Worry_t_score_pre",
  "step_count_pre",
  "sleep_count_pre",
  "resting_heart_rate_pre"
)

# caregiver stress
fml = paste("Caregiver_Stress_t_score ~ msg_level * engager", paste(tv_vars, collapse = " + ") ,paste(bl_vars, collapse = " + "), sep = " + ")
fit.stress_4lvl <- geeglm(
  as.formula(fml),
  data = tmp %>% filter(!is.na(Caregiver_Stress_t_score_pre)),
  id = factor(subjectid),
  corstr = "i"
)
my_forest_plot(fit.stress_4lvl, var_int = "msg_levelLow") %>% print()

v = vcov(fit.stress_4lvl)
m = fit.stress_4lvl$coefficients["msg_levelLow"] + fit.stress_4lvl$coefficients["msg_levelLow:engager"]
stddev = sqrt(v["msg_levelLow", "msg_levelLow"] + v["msg_levelLow:engager", "msg_levelLow:engager"] + 2 * v["msg_levelLow", "msg_levelLow:engager"])
pvalue = 2 * pt(-abs(m / stddev), df = fit.stress_4lvl$df.residual)
c(m,stddev,m-1.96*stddev,m+1.96*stddev,pvalue)
m = fit.stress_4lvl$coefficients["msg_levelMedium"] + fit.stress_4lvl$coefficients["msg_levelMedium:engager"]
stddev = sqrt(v["msg_levelMedium", "msg_levelMedium"] + v["msg_levelMedium:engager", "msg_levelMedium:engager"] + 2 * v["msg_levelMedium", "msg_levelMedium:engager"])
pvalue = 2 * pt(-abs(m / stddev), df = fit.stress_4lvl$df.residual)
c(m,stddev,m-1.96*stddev,m+1.96*stddev,pvalue)
m = fit.stress_4lvl$coefficients["msg_levelHigh"] + fit.stress_4lvl$coefficients["msg_levelHigh:engager"]
stddev = sqrt(v["msg_levelHigh", "msg_levelHigh"] + v["msg_levelHigh:engager", "msg_levelHigh:engager"] + 2 * v["msg_levelHigh", "msg_levelHigh:engager"])
pvalue = 2 * pt(-abs(m / stddev), df = fit.stress_4lvl$df.residual)
c(m,stddev,m-1.96*stddev,m+1.96*stddev,pvalue)

fml = paste("Caregiver_Stress_t_score ~ msg_binary * engager", paste(tv_vars, collapse = " + ") ,paste(bl_vars, collapse = " + "), sep = " + ")
#Caregiver_Stress_t_score ~ msg_binary * engager + week + dem_sex + dem_age + dem_ethnicity + mrc_tbisev_new + Caregiver_Stress_t_score_pre,
fit.stress_binary <- geeglm(
  as.formula(fml),
  data = tmp %>% filter(!is.na(Caregiver_Stress_t_score_pre)),
  id = factor(subjectid),
  corstr = "i"
)
my_forest_plot(fit.stress_binary, var_int = "msg_binaryHigh") %>% print()

# worry
fml = paste("Worry_t_score ~ msg_level * engager", paste(tv_vars, collapse = " + ") ,paste(bl_vars, collapse = " + "), sep = " + ")
fit.worry_4lvl <- geeglm(
  # Worry_t_score ~ msg_level * engager + week + dem_sex + dem_age + dem_ethnicity + mrc_tbisev_new + Worry_t_score_pre,
  as.formula(fml),
  data = tmp %>% filter(!is.na(Worry_t_score_pre)),
  id = factor(subjectid),
  corstr = "i"
)
my_forest_plot(fit.worry_4lvl, var_int = "msg_levelLow") %>% print()

v = vcov(fit.worry_4lvl)
m = fit.worry_4lvl$coefficients["msg_levelLow"] + fit.worry_4lvl$coefficients["msg_levelLow:engager"]
stddev = sqrt(v["msg_levelLow", "msg_levelLow"] + v["msg_levelLow:engager", "msg_levelLow:engager"] + 2 * v["msg_levelLow", "msg_levelLow:engager"])
pvalue = 2 * pt(-abs(m / stddev), df = fit.worry_4lvl$df.residual)
c(m,stddev,m-1.96*stddev,m+1.96*stddev,pvalue)
m = fit.worry_4lvl$coefficients["msg_levelMedium"] + fit.worry_4lvl$coefficients["msg_levelMedium:engager"]
stddev = sqrt(v["msg_levelMedium", "msg_levelMedium"] + v["msg_levelMedium:engager", "msg_levelMedium:engager"] + 2 * v["msg_levelMedium", "msg_levelMedium:engager"])
pvalue = 2 * pt(-abs(m / stddev), df = fit.worry_4lvl$df.residual)
c(m,stddev,m-1.96*stddev,m+1.96*stddev,pvalue)
m = fit.worry_4lvl$coefficients["msg_levelHigh"] + fit.worry_4lvl$coefficients["msg_levelHigh:engager"]
stddev = sqrt(v["msg_levelHigh", "msg_levelHigh"] + v["msg_levelHigh:engager", "msg_levelHigh:engager"] + 2 * v["msg_levelHigh", "msg_levelHigh:engager"])
pvalue = 2 * pt(-abs(m / stddev), df = fit.worry_4lvl$df.residual)
c(m,stddev,m-1.96*stddev,m+1.96*stddev,pvalue)

fml = paste("Worry_t_score ~ msg_binary * engager", paste(tv_vars, collapse = " + ") ,paste(bl_vars, collapse = " + "), sep = " + ")
fit.worry_binary <- geeglm(
  as.formula(fml),
  # Worry_t_score ~ msg_binary * engager + week + dem_sex + dem_age + dem_ethnicity + mrc_tbisev_new + Worry_t_score_pre,
  data = tmp %>% filter(!is.na(Worry_t_score_pre)),
  id = factor(subjectid),
  corstr = "i"
)
my_forest_plot(fit.worry_binary, var_int = "msg_binaryHigh") %>% print()

# sadness
fml = paste("Sadness_t_score ~ msg_level * engager", paste(tv_vars, collapse = " + ") ,paste(bl_vars, collapse = " + "), sep = " + ")
fit.sadness_4lvl <- geeglm(
  # Sadness_t_score ~ msg_level * engager + week + dem_sex + dem_age + dem_ethnicity + mrc_tbisev_new + Sadness_t_score_pre,
  as.formula(fml),
  data = tmp %>% filter(!is.na(Sadness_t_score_pre)),
  id = factor(subjectid),
  corstr = "i"
)
my_forest_plot(fit.sadness_4lvl, var_int = "msg_levelLow") %>% print()

v = vcov(fit.sadness_4lvl)
m = fit.sadness_4lvl$coefficients["msg_levelLow"] + fit.sadness_4lvl$coefficients["msg_levelLow:engager"]
stddev = sqrt(v["msg_levelLow", "msg_levelLow"] + v["msg_levelLow:engager", "msg_levelLow:engager"] + 2 * v["msg_levelLow", "msg_levelLow:engager"])
pvalue = 2 * pt(-abs(m / stddev), df = fit.sadness_4lvl$df.residual)
c(m,stddev,m-1.96*stddev,m+1.96*stddev,pvalue)
m = fit.sadness_4lvl$coefficients["msg_levelMedium"] + fit.sadness_4lvl$coefficients["msg_levelMedium:engager"]
stddev = sqrt(v["msg_levelMedium", "msg_levelMedium"] + v["msg_levelMedium:engager", "msg_levelMedium:engager"] + 2 * v["msg_levelMedium", "msg_levelMedium:engager"])
pvalue = 2 * pt(-abs(m / stddev), df = fit.sadness_4lvl$df.residual)
c(m,stddev,m-1.96*stddev,m+1.96*stddev,pvalue)
m = fit.sadness_4lvl$coefficients["msg_levelHigh"] + fit.sadness_4lvl$coefficients["msg_levelHigh:engager"]
stddev = sqrt(v["msg_levelHigh", "msg_levelHigh"] + v["msg_levelHigh:engager", "msg_levelHigh:engager"] + 2 * v["msg_levelHigh", "msg_levelHigh:engager"])
pvalue = 2 * pt(-abs(m / stddev), df = fit.sadness_4lvl$df.residual)
c(m,stddev,m-1.96*stddev,m+1.96*stddev,pvalue)

fml = paste("Sadness_t_score ~ msg_binary * engager", paste(tv_vars, collapse = " + ") ,paste(bl_vars, collapse = " + "), sep = " + ")
fit.sadness_binary <- geeglm(
  as.formula(fml),
  # Sadness_t_score ~ msg_binary * engager + week + dem_sex + dem_age + dem_ethnicity + mrc_tbisev_new + Sadness_t_score_pre,
  data = tmp %>% filter(!is.na(Sadness_t_score_pre)),
  id = factor(subjectid),
  corstr = "i"
)
my_forest_plot(fit.sadness_binary, var_int = "msg_binaryHigh") %>% print()

