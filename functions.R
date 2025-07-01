library(ggplot2)
library(tidyverse)

calc_se = function(fit, input_trt, input_bl) {
  X1 = as.matrix(model.matrix(delete.response(fit$terms), input_trt, xlev =
                                fit$xlevels))
  X0 = as.matrix(model.matrix(delete.response(fit$terms), input_bl, xlev =
                                fit$xlevels))
  ses = c()
  for (i in 1:dim(X1)[1]) {
    se = sqrt(t(matrix(X1[i, ] - X0)) %*% vcov(fit) %*% matrix(X1[i, ] - X0))
    ses = c(ses, se)
  }
  return(ses)
}

my_reflection_plot_engage = function(fit_e,
                                     trt_name,
                                     trt_levels,
                                     mod_name,
                                     xlab_name = NULL,
                                     log = FALSE,
                                     reflect_point = FALSE,
                                     q1 = 0.05,
                                     q2 = 0.95) {
  res = tibble()
  if (is.numeric(trt_levels[1])) {
    bl_level = 0
  } else {
    bl_level = "None"
  }
  hist_dat = fit_e$data %>% mutate(engage = 1)
  mod_range = seq(
    from = quantile(hist_dat %>% pull(!!mod_name), q1),
    to = quantile(hist_dat %>% pull(!!mod_name), q2),
    length.out = 100
  )
  # hist_dat %>% filter(!!mod_name >= mod_range[1]) %>% filter(!!mod_name <= mod_range[length(mod_range)])
  # hist_dat = hist_dat[hist_dat <= mod_range[length(mod_range)] &
  #                       hist_dat >= mod_range[1]]
  for (value in mod_range) {
    input_trt = expand_grid(
      !!trt_name := trt_levels,!!mod_name := value,
      week = 0,
      dem_sex = "female",
      dem_age = 60,
      dem_ethnicity = "hs",
      dem_race = "white",
      dem_lengthcared = 7,
      dem_timespent = factor("less than 2 hours"),
      mrc_tbisev_new = "severe",
      Caregiver_Stress_t_score_pre = 50,
      Sadness_t_score_pre = 50,
      Worry_t_score_pre = 50,
      step_count_pre = 20,
      sleep_count_pre = 7,
      resting_heart_rate_pre = 60,
      Caregiver_Stress_t_score_BL = 50,
      Sadness_t_score_BL = 50,
      Worry_t_score_BL = 50,
      .name_repair = "minimal"
    )
    input_bl = expand_grid(
      !!trt_name := bl_level,!!mod_name := value,
      week = 0,
      dem_sex = "female",
      dem_age = 60,
      dem_ethnicity = "hs",
      dem_race = "white",
      dem_lengthcared = 7,
      dem_timespent = factor("less than 2 hours"),
      mrc_tbisev_new = "severe",
      Caregiver_Stress_t_score_pre = 50,
      Sadness_t_score_pre = 50,
      Worry_t_score_pre = 50,
      step_count_pre = 20,
      sleep_count_pre = 7,
      resting_heart_rate_pre = 60,
      Caregiver_Stress_t_score_BL = 50,
      Sadness_t_score_BL = 50,
      Worry_t_score_BL = 50,
      .name_repair = "minimal"
    )
    
    
    effects_e = predict(fit_e, input_trt) - predict(fit_e, input_bl)
    ses = calc_se(fit_e, input_trt, input_bl)
    effects_e_ub = effects_e + 1.96 * ses
    effects_e_lb = effects_e - 1.96 * ses
    
    names(effects_e) = trt_levels
    
    res <- res %>% bind_rows(
      data.frame(
        effect = effects_e,
        effect_ub = effects_e_ub,
        effect_lb = effects_e_lb,
        engage = 1
      ) %>% rownames_to_column("trt_level") %>% mutate(mod_value = value)
    )
  }
  
  v = fit_e$data %>% pull(!!mod_name) %>% mean()
  
  res %>% ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_line(aes(x = mod_value, y = effect, color = trt_level), linewidth =
                0.5) +
    geom_ribbon(
      aes(
        x = mod_value,
        ymin = effect_lb,
        ymax = effect_ub,
        color = trt_level,
        group = interaction(trt_level, engage),
      ),
      alpha = 0.0,
      linewidth = 0.5,
      linetype = "dashed"
    ) +
    scale_y_continuous(name = "Message's effect", sec.axis = sec_axis( ~ . , name =
                                                                         "")) +
    theme_bw() +
    theme(axis.text.y.right = element_blank(),
          axis.ticks.y.right = element_blank()) +
    theme(legend.position = "bottom") +
    scale_color_discrete(name = "Message dosage") -> p
  if (is.null(xlab_name)) {
    p = p + xlab(mod_name)
  } else {
    p = p + xlab(xlab_name)
  }
  ylim_axis = layer_scales(p)$y$get_limits()
  p = p + geom_segment(
    aes(
      x = mod,
      xend = mod,
      y = ylim_axis[1],
      yend = 1 / 5 * diff(ylim_axis) / 4 + ylim_axis[1]
    ),
    data = data.frame(
      mod = hist_dat %>% pull(!!mod_name),
      engage = hist_dat %>% pull(engage)
    ),
    linewidth = 0.1
  ) + xlim(c(mod_range[1], mod_range[length(mod_range)]))
  if (reflect_point == TRUE) {
    v = exp(-coef(fit)[trt_name] / coef(fit)[paste(trt_name, ":log(", mod_name, ")", sep =
                                                     "")])
    p = p + geom_vline(xintercept = v, linetype = "dashed") + geom_text(
      data = data.frame(x = v, y = min(res$effect)),
      aes(x, y),
      label = floor(v),
      vjust = 1
    )
  }
  # p + theme(legend.position = c(0.12,0.92),legend.background = element_rect(fill = "white", color = "black"))
  # p + theme(legend.position = "none")
  p
}


my_reflection_plot_engage_race = function(fit_e,
                                          trt_name,
                                          trt_levels,
                                          mod_name,
                                          xlab_name = NULL) {
  res = tibble()
  if (is.numeric(trt_levels[1])) {
    bl_level = 0
  } else {
    bl_level = "None"
  }
  hist_dat = fit_e$data %>% mutate(engage = 1)
  mod_range = c("white", "others")
  for (value in mod_range) {
    input_trt = expand_grid(
      !!trt_name := trt_levels,
      week = 0,
      dem_sex = "female",
      dem_age = 60,
      dem_ethnicity = "hs",
      dem_race = value,
      dem_lengthcared = 7,
      dem_timespent = factor("less than 2 hours"),
      mrc_tbisev_new = "severe",
      Caregiver_Stress_t_score_pre = 50,
      Sadness_t_score_pre = 50,
      Worry_t_score_pre = 50,
      step_count_pre = 20,
      sleep_count_pre = 7,
      resting_heart_rate_pre = 60,
      Caregiver_Stress_t_score_BL = 50,
      Sadness_t_score_BL = 50,
      Worry_t_score_BL = 50,
      .name_repair = "minimal"
    )
    input_bl = expand_grid(
      !!trt_name := bl_level,
      week = 0,
      dem_sex = "female",
      dem_age = 60,
      dem_ethnicity = "hs",
      dem_race = value,
      dem_lengthcared = 7,
      dem_timespent = factor("less than 2 hours"),
      mrc_tbisev_new = "severe",
      Caregiver_Stress_t_score_pre = 50,
      Sadness_t_score_pre = 50,
      Worry_t_score_pre = 50,
      step_count_pre = 20,
      sleep_count_pre = 7,
      resting_heart_rate_pre = 60,
      Caregiver_Stress_t_score_BL = 50,
      Sadness_t_score_BL = 50,
      Worry_t_score_BL = 50,
      .name_repair = "minimal"
    )
    
    
    effects_e = predict(fit_e, input_trt) - predict(fit_e, input_bl)
    ses = calc_se(fit_e, input_trt, input_bl)
    effects_e_ub = effects_e + 1.96 * ses
    effects_e_lb = effects_e - 1.96 * ses
    
    names(effects_e) = trt_levels
    
    res <- res %>% bind_rows(
      data.frame(
        effect = effects_e,
        effect_ub = effects_e_ub,
        effect_lb = effects_e_lb,
        engage = 1
      ) %>% rownames_to_column("trt_level") %>% mutate(mod_value = value)
    )
  }
  pd <- position_dodge(width = 0.4)
  res %>% ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(aes(
      x = mod_value,
      y = effect,
      group = trt_level,
      color = trt_level
    ),
    position = pd) +
    geom_errorbar(
      aes(
        x = mod_value,
        y = effect,
        ymin = effect_lb,
        ymax = effect_ub,
        group = trt_level,
        color = trt_level
      ),
      position = pd,
      width = 0.2
    ) +
    scale_y_continuous(name = "Message's effect") +
    theme_bw() +
    theme(axis.text.y.right = element_blank(),
          axis.ticks.y.right = element_blank()) +
    theme(legend.position = "bottom") +
    scale_color_discrete(name = "Message dosage") -> p
  if (is.null(xlab_name)) {
    p = p + xlab(mod_name)
  } else {
    p = p + xlab(xlab_name)
  }
  ylim_axis = layer_scales(p)$y$get_limits()
  # p + theme(legend.position = c(0.12,0.92),legend.background = element_rect(fill = "white", color = "black"))
  # p + theme(legend.position = "none")
  p
}
