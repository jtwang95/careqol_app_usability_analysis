library(ggplot2)

source("functions.R")

## cs + race
my_reflection_plot_engage_race(fit_e = fit_results_4lvl_e_tinv[["cs_dem_race"]],
                               trt_name = "msg_level",
                               trt_levels = c("Low"),
                               mod_name = "dem_race",
                               xlab = "Race")
ggsave("~/University of Michigan Dropbox/Jitao Wang/CareQOL Project/MRT_analysis_TBI/figures/cs_race.png",width = 4,height = 4)

## cs + lengthcared
my_reflection_plot_engage(fit_e = fit_results_4lvl_e_tinv[["cs_dem_lengthcared"]],
                          trt_name = "msg_level",
                          trt_levels = c("Medium","High"),
                          mod_name = "dem_lengthcared",
                          xlab = "Years in caregiver role",
                          reflect_point = FALSE)
ggsave("~/University of Michigan Dropbox/Jitao Wang/CareQOL Project/MRT_analysis_TBI/figures/cs_lengthcared.png",width = 4,height = 4)

## cs + step
my_reflection_plot_engage(fit_e = fit_results_4lvl_e_raw[["Caregiver Stress_step_count"]],
                          trt_name = "msg_level",
                          trt_levels = c("High"),
                          mod_name = "step_count_pre",
                          xlab = "Prior step count",
                          reflect_point = FALSE)
ggsave("~/University of Michigan Dropbox/Jitao Wang/CareQOL Project/MRT_analysis_TBI/figures/cs_step.png",width = 4,height = 4)

## cs + sleep
my_reflection_plot_engage(fit_e = fit_results_4lvl_e_raw[["Caregiver Stress_sleep_count"]],
                   trt_name = "msg_level",
                   trt_levels = c("Medium"),
                   mod_name = "sleep_count_pre",
                   xlab = "Prior sleep hours",
                   reflect_point = FALSE)
ggsave("~/University of Michigan Dropbox/Jitao Wang/CareQOL Project/MRT_analysis_TBI/figures/cs_sleep.png",width = 4,height = 4)

## cs + anxiety
my_reflection_plot_engage(fit_e = fit_results_4lvl_e_raw[["Caregiver Stress_worry_pre"]],
                          trt_name = "msg_level",
                          trt_levels = c("Low","Medium","High"),
                          mod_name = "Worry_t_score_pre",
                          xlab = "Prior anxiety T score",
                          reflect_point = FALSE)
ggsave("~/University of Michigan Dropbox/Jitao Wang/CareQOL Project/MRT_analysis_TBI/figures/cs_anxiety.png",width = 4,height = 4)

## cs + depression
my_reflection_plot_engage(fit_e = fit_results_4lvl_e_raw[["Caregiver Stress_sadness_pre"]],
                          trt_name = "msg_level",
                          trt_levels = c("Low"),
                          mod_name = "Sadness_t_score_pre",
                          xlab = "Prior depression T score",
                          reflect_point = FALSE)
ggsave("~/University of Michigan Dropbox/Jitao Wang/CareQOL Project/MRT_analysis_TBI/figures/cs_sadness.png",width = 4,height = 4)

## worry + step
my_reflection_plot_engage(fit_e = fit_results_4lvl_e_raw[["Worry_step_count"]],
                          trt_name = "msg_level",
                          trt_levels = c("High"),
                          mod_name = "step_count_pre",
                          xlab = "Prior step count",
                          reflect_point = FALSE)
ggsave("~/University of Michigan Dropbox/Jitao Wang/CareQOL Project/MRT_analysis_TBI/figures/worry_step.png",width = 4,height = 4)

## worry + depression
my_reflection_plot_engage(fit_e = fit_results_4lvl_e_raw[["Worry_sadness_pre"]],
                          trt_name = "msg_level",
                          trt_levels = c("Low","High"),
                          mod_name = "Sadness_t_score_pre",
                          xlab = "Prior depression T score",
                          reflect_point = FALSE)
ggsave("~/University of Michigan Dropbox/Jitao Wang/CareQOL Project/MRT_analysis_TBI/figures/worry_sadness.png",width = 4,height = 4)

## sad + step
my_reflection_plot_engage(fit_e = fit_results_4lvl_e_raw[["Sadness_step_count"]],
                          trt_name = "msg_level",
                          trt_levels = c("Low"),
                          mod_name = "step_count_pre",
                          xlab = "Prior step count",
                          reflect_point = FALSE)
ggsave("~/University of Michigan Dropbox/Jitao Wang/CareQOL Project/MRT_analysis_TBI/figures/sadness_step.png",width = 4,height = 4)

## sad + sleep
my_reflection_plot_engage(fit_e = fit_results_4lvl_e_raw[["Sadness_sleep_count"]],
                          trt_name = "msg_level",
                          trt_levels = c("High"),
                          mod_name = "sleep_count_pre",
                          xlab = "Prior sleep hours",
                          reflect_point = FALSE)
ggsave("~/University of Michigan Dropbox/Jitao Wang/CareQOL Project/MRT_analysis_TBI/figures/sadness_sleep.png",width = 4,height = 4)

## sad + race
my_reflection_plot_engage_race(fit_e = fit_results_4lvl_e_tinv[["sadness_dem_race"]],
                               trt_name = "msg_level",
                               trt_levels = c("Medium","High"),
                               mod_name = "dem_race",
                               xlab = "Race")
ggsave("~/University of Michigan Dropbox/Jitao Wang/CareQOL Project/MRT_analysis_TBI/figures/sadness_race.png",width = 4,height = 4)

