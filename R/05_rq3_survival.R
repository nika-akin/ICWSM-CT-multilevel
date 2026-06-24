# R/05_rq3_survival.R — Macro-level: Survival analysis
# Cox proportional hazards models for dropout/persistence

#' Prepare survival data: define dropout as >=60 days inactivity before shutdown
#' @param data cleaned data frame with user, subverse, time columns
prepare_survival_data <- function(data) {
  shutdown_date <- as.Date("2020-12-25")

  surv <- data %>%
    dplyr::group_by(user, subverse) %>%
    dplyr::summarise(
      first_comment_date = as.Date(min(time, na.rm = TRUE)),
      last_comment_date = as.Date(max(time, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      time_to_event = as.numeric(difftime(
        pmin(last_comment_date, shutdown_date),
        first_comment_date,
        units = "days"
      )),
      event_dropout = ifelse(
        difftime(shutdown_date, last_comment_date, units = "days") >= 60, 1, 0
      )
    )

  user_subverse_summary <- data %>%
    dplyr::group_by(user, subverse) %>%
    dplyr::summarise(
      sv_score = dplyr::first(sv_score),
      avg_upvotes = mean(upvotes, na.rm = TRUE),
      avg_downvotes = mean(downvotes, na.rm = TRUE),
      ever_had_privilege = dplyr::first(ever_had_privilege),
      total_days_with_privilege = dplyr::first(total_days_with_privilege),
      mean_CCP = dplyr::first(mean_CCP),
      sv_growth_factor = dplyr::first(sv_growth_factor),
      habit_class = dplyr::first(habit_class),
      controversy_group = dplyr::first(controversy_group),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      conspiracy_group = factor(
        ifelse(sv_score >= 0, "conspiracy", "non-conspiracy"),
        levels = c("conspiracy", "non-conspiracy")
      )
    )

  surv %>%
    dplyr::left_join(user_subverse_summary, by = c("user", "subverse")) %>%
    dplyr::select(
      user, subverse, first_comment_date, last_comment_date,
      time_to_event, event_dropout,
      ever_had_privilege, total_days_with_privilege, mean_CCP,
      sv_growth_factor, conspiracy_group, habit_class, controversy_group
    )
}

#' Prepare privilege trajectory for survival merge
#' @param data cleaned data frame
prepare_privilege_trajectory <- function(data) {
  comment_data <- data %>% dplyr::filter(!is.na(comment_id))
  plot_data <- comment_data %>%
    dplyr::arrange(user, time) %>%
    dplyr::group_by(user) %>%
    dplyr::mutate(
      net_votes = upvotes - downvotes,
      rolling_total = slider::slide_dbl(
        net_votes, sum, .before = 9, .after = 0, .complete = FALSE
      ),
      previous_total = dplyr::lag(rolling_total, default = 0),
      event_type = dplyr::case_when(
        previous_total < 10 & rolling_total >= 10 ~ "gain",
        previous_total >= 10 & rolling_total < 10 ~ "loss",
        TRUE ~ "no_change"
      )
    ) %>%
    dplyr::ungroup()

  plot_data %>%
    dplyr::group_by(user) %>%
    dplyr::summarise(
      n_gains = sum(event_type == "gain"),
      n_losses = sum(event_type == "loss")
    ) %>%
    dplyr::mutate(
      privilege = dplyr::case_when(
        n_gains == 0 ~ "never_had",
        n_gains >= 1 & n_losses == 0 ~ "steady",
        n_gains >= 1 & n_losses >= 1 ~ "fluctuating"
      ),
      privilege = factor(privilege, levels = c("never_had", "steady", "fluctuating"))
    )
}

#' Merge privilege trajectory with survival data
#' @param survival_data survival data frame
#' @param privilege_trajectory privilege trajectory data
merge_privilege_survival <- function(survival_data, privilege_trajectory) {
  survival_data %>%
    dplyr::left_join(privilege_trajectory, by = "user") %>%
    dplyr::mutate(
      privilege = factor(privilege, levels = c("never_had", "steady", "fluctuating"))
    )
}

#' Run log-rank tests across strata
#' @param survival_data survival data with stratification variables
run_logrank_tests <- function(survival_data) {
  list(
    ct = survival::survdiff(
      survival::Surv(time_to_event, event_dropout) ~ conspiracy_group, data = survival_data
    ),
    habit = survival::survdiff(
      survival::Surv(time_to_event, event_dropout) ~ habit_class, data = survival_data
    ),
    controversy = survival::survdiff(
      survival::Surv(time_to_event, event_dropout) ~ controversy_group, data = survival_data
    ),
    growth = survival::survdiff(
      survival::Surv(time_to_event, event_dropout) ~ sv_growth_factor, data = survival_data
    ),
    privilege = survival::survdiff(
      survival::Surv(time_to_event, event_dropout) ~ privilege, data = survival_data
    )
  )
}

#' Prepare long-format data for extended Cox model with time-varying coefficients
#' @param data cleaned data frame
#' @param survival_data survival data frame
prepare_cox_long_data <- function(data, survival_data) {
  shutdown_date <- as.Date("2020-12-25") - 30
  
  data <- data %>%
    dplyr::select(-dplyr::any_of(c("first_comment_date", "last_comment_date")))
  
  surv_cols <- survival_data %>%
    dplyr::select(user, subverse, first_comment_date, last_comment_date, event_dropout)
  
  data %>%
    dplyr::left_join(surv_cols, by = c("user", "subverse")) %>%
    dplyr::mutate(
      days_since_first = as.numeric(difftime(as.Date(time), first_comment_date, units = "days")),
      month = floor(days_since_first / 30) + 1
    ) %>%
    dplyr::group_by(user, subverse, first_comment_date, last_comment_date, event_dropout, 
                    month, habit_class, tenure_strata, conspiracy_group) %>%
    dplyr::summarise(
      avg_upvotes_window = mean(upvotes, na.rm = TRUE),
      avg_downvotes_window = mean(downvotes, na.rm = TRUE),
      habit_window = dplyr::n(),
      controversy_window = mean(controversy, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      start = as.Date(first_comment_date) + lubridate::days((month - 1) * 30),
      interval_stop_date = pmin(as.Date(start) + 30, shutdown_date, as.Date(last_comment_date), na.rm = TRUE),
      event = ifelse(as.Date(interval_stop_date) == as.Date(last_comment_date) & event_dropout == 1, 1, 0)
    ) %>%
    dplyr::group_by(user, subverse) %>%
    dplyr::mutate(
      baseline_date = min(as.Date(start)),
      start_numeric = as.numeric(difftime(as.Date(start), baseline_date, units = "days")),
      stop_numeric = as.numeric(difftime(as.Date(interval_stop_date), baseline_date, units = "days"))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(stop_numeric > start_numeric)
}

#' Fit extended Cox model with time-varying coefficients and frailty
#' @param long_data long-format survival data
fit_cox_timevarying <- function(long_data) {
  survival::coxph(
    survival::Surv(start_numeric, stop_numeric, event) ~
      avg_upvotes_window + tt(avg_upvotes_window) +
      avg_downvotes_window + tt(avg_downvotes_window) +
      habit_class +
      tenure_strata +
      conspiracy_group +
      controversy_window +
      survival::frailty(subverse),
    data = long_data,
    tt = function(x, t, ...) x * log1p(t)
  )
}

#' Cox model diagnostics (Schoenfeld residuals)
#' @param model fitted Cox model
#' @param long_data long-format data
diagnose_cox <- function(model, long_data) {
  tryCatch(
    survival::cox.zph(model),
    error = function(e) {
      list(
        message = "cox.zph not available for models with tt() terms",
        error = conditionMessage(e),
        table = NULL
      )
    }
  )
}
