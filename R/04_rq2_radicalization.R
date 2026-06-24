# R/04_rq2_radicalization.R — Meso-level: CT transition models
# GAMM and GLMM for conspiracy theory community upshifts

#' Aggregate data to weekly user-level observations
#' @param data cleaned data frame with time column
aggregate_weekly <- function(data) {
  data %>%
    dplyr::mutate(week = lubridate::floor_date(as.Date(time), unit = "week")) %>%
    dplyr::group_by(user, week) %>%
    dplyr::summarise(
      n_comments = dplyr::n(),
      mean_score = mean(netvote, na.rm = TRUE),
      mean_trait = mean(trait, na.rm = TRUE),
      mean_group = mean(group, na.rm = TRUE),
      mean_svscore = mean(sv_score, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Prepare transition data with lagged predictors
#' @param weekly_data weekly aggregated data
#' @param habit_data optional habit classification data
prepare_transition_data <- function(weekly_data, habit_data = NULL) {
  if (is.null(habit_data)) {
    user_habit <- weekly_data %>% dplyr::distinct(user, habit_class)
  } else {
    user_habit <- habit_data %>% dplyr::distinct(user, habit_class)
  }
  
  weekly_data %>%
    dplyr::left_join(user_habit, by = "user") %>%
    dplyr::arrange(user, week) %>%
    dplyr::group_by(user) %>%
    dplyr::mutate(
      mean_group_lag = dplyr::lag(mean_group),
      mean_trait_lag = dplyr::lag(mean_trait),
      mean_svscore_lag = dplyr::lag(mean_svscore),
      mean_score_lag = dplyr::lag(mean_score),
      delta_sv = mean_svscore - mean_svscore_lag,
      transition_to_ct = ifelse(!is.na(delta_sv) & delta_sv > 0, 1, 0),
      transition_to_ct_lag = dplyr::lag(transition_to_ct)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(week) %>%
    dplyr::mutate(
      week_index = dplyr::row_number(),
      user = factor(user),
      habit_class = factor(
        habit_class,
        levels = c("occasional", "steady regular", "bursty")
      )
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c(mean_trait_lag, mean_group_lag, mean_score_lag, n_comments, week_index),
        ~as.numeric(scale(.x))
      )
    )
}

#' Fit generalized linear mixed model for CT transitions
#' @param transition_data prepared transition data
fit_transition_glmm <- function(transition_data) {
  lme4::glmer(
    transition_to_ct ~ transition_to_ct_lag + mean_trait_lag + mean_group_lag +
      mean_score_lag + habit_class + week_index + n_comments + (1 | user),
    data = transition_data,
    family = binomial(link = "logit"),
    na.action = na.exclude,
    control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
  )
}

#' Fit generalized additive mixed model for CT transitions
#' @param transition_data prepared transition data
fit_transition_gamm <- function(transition_data) {
  mgcv::bam(
    transition_to_ct ~ transition_to_ct_lag +
      s(mean_trait_lag) +
      s(mean_group_lag) +
      s(mean_score_lag) +
      s(week_index) +
      s(n_comments) +
      habit_class +
      s(user, bs = "re"),
    family = binomial,
    data = transition_data,
    discrete = TRUE
  )
}
