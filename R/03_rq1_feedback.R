# R/03_rq1_feedback.R — Micro-level: Posting latency models
# Linear mixed-effects regression for feedback sensitivity analysis

#' Prepare posting latency data with lagged features
#' @param data cleaned data frame with time and vote information
#' @param time_file_path optional path to separate time file
prepare_latency_data <- function(data, time_file_path = NULL) {
  if (!is.null(time_file_path) && file.exists(time_file_path)) {
    time <- readr::read_csv(time_file_path, show_col_types = FALSE) %>%
      dplyr::mutate(timehour = lubridate::ymd_hms(paste(date, time))) %>%
      dplyr::select(user, timehour, comment_id, submission_id)
    data <- data %>%
      dplyr::left_join(time, by = c("user", "comment_id", "submission_id"))
  }
  
  if (!"timehour" %in% names(data)) {
    data <- data %>% dplyr::mutate(timehour = time)
  }
  
  data %>%
    dplyr::arrange(user, timehour) %>%
    dplyr::group_by(user) %>%
    dplyr::mutate(
      post_latency = as.numeric(difftime(timehour, dplyr::lag(timehour), units = "hours")),
      post_latency_log = log1p(post_latency),
      lagged_net_votes = dplyr::lag(netvote),
      lagged_net_votes_z = as.numeric(scale(lagged_net_votes)),
      sv_growth_z = as.numeric(scale(sv_growth))
    ) %>%
    dplyr::ungroup()
}

#' Fit linear mixed-effects model for posting latency
#' @param latency_data prepared latency data frame
fit_latency_lmm <- function(latency_data) {
  if (is.null(latency_data)) return(NULL)
  lme4::lmer(
    post_latency_log ~ lagged_net_votes_z * habit_class +
      sv_growth_z +
      tenure_strata +
      (1 | user) + (1 | subverse),
    data = latency_data
  )
}

#' Diagnose LMM model fit (ICC, residuals)
#' @param model fitted LMM model
#' @param latency_data latency data frame
diagnose_latency <- function(model, latency_data) {
  if (is.null(model)) return(NULL)
  list(
    icc = performance::icc(model),
    residuals = residuals(model),
    fitted = fitted(model)
  )
}
