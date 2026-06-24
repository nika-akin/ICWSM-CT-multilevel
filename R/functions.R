# R/functions.R — Pure functions for the targets pipeline
# Each function is documented with intent, inputs, and outputs.

library(data.table)

# =============================================================================
# 1. DATA I/O
# =============================================================================

#' Load main annotation file
#' @param path path to CSV
load_annotation <- function(path) {
  readr::read_csv(path, show_col_types = FALSE)
}

#' Load user info (registration dates)
load_user_info <- function(path) {
  readr::read_csv(path, show_col_types = FALSE)
}

#' Load subverse CT scores
load_sv_scores <- function(path) {
  df <- readr::read_csv(path, show_col_types = FALSE)
  # Original file has subverse as first column without name
  if ("...1" %in% names(df)) {
    df <- df %>% dplyr::rename(subverse = `...1`, sv_score = conspiracy)
  }
  df
}

#' Load subverse metadata
load_subverse <- function(path) {
  readr::read_csv(path, show_col_types = FALSE)
}

# =============================================================================
# 2. PREPROCESSING
# =============================================================================

#' Parse datetime from separate date and time columns
add_datetime <- function(data) {
  data %>%
    dplyr::mutate(
      time = lubridate::ymd_hms(paste(date, time))
    )
}

#' Join user registration info
join_user_info <- function(data, user_info) {
  dplyr::left_join(data, user_info, by = "user")
}

#' Join subverse conspiracy scores
join_sv_scores <- function(data, sv_scores) {
  dplyr::left_join(data, sv_scores, by = "subverse")
}

#' Join subverse metadata (subscriber count, creation date)
join_subverse_meta <- function(data, subverse_meta) {
  dplyr::left_join(data, subverse_meta, by = "subverse")
}

#' Calculate Reddit-style controversy score
#' magnitude = upvotes + downvotes; balance = min(up/down, down/up)
#' controversy = magnitude ^ balance (0 if no votes)
calculate_controversy <- function(data) {
  dt <- data.table::as.data.table(data)
  dt[, `:=`(
    magnitude = upvotes + downvotes,
    balance = fifelse(upvotes > downvotes, downvotes / upvotes, upvotes / downvotes)
  )]
  dt[downvotes <= 0 | upvotes <= 0, balance := 0]
  dt[downvotes <= 0 | upvotes <= 0, magnitude := 0]
  dt[, controversy := fifelse(magnitude == 0, 0, magnitude ^ balance)]
  dt[, controversy_group := factor(
    fifelse(controversy > 0, "high", "low"),
    levels = c("low", "high")
  )]
  as_tibble(dt)
}

#' Calculate CCP (Comment Contribution Points) privilege trajectory
#' Rolling window of 10 comments; privilege = rolling net votes >= 10
calculate_privilege <- function(data) {
  # User-subverse level cumulative privilege
  privilege <- data %>%
    dplyr::arrange(user, subverse, time) %>%
    dplyr::group_by(user, subverse) %>%
    dplyr::mutate(
      net_points = upvotes - downvotes,
      ccp = cumsum(net_points),
      has_privilege = dplyr::if_else(ccp >= 10, 1, 0),
      post_date = as.Date(time)
    ) %>%
    dplyr::ungroup()

  # Run-length encoding for privilege spells
  dt <- data.table::as.data.table(privilege)
  dt[, rleid_priv := data.table::rleid(has_privilege), by = .(user, subverse)]

  priv_runs <- dt[has_privilege == 1, .(
    start_date = min(post_date),
    end_date = max(post_date)
  ), by = .(user, subverse, rleid_priv)]
  priv_runs[, duration_days := as.numeric(end_date - start_date) + 1]

  total_priv_time <- priv_runs[, .(
    total_days_with_privilege = sum(duration_days)
  ), by = .(user, subverse)]

  privilege_summary <- privilege %>%
    dplyr::group_by(user, subverse) %>%
    dplyr::summarise(
      ever_had_privilege = as.numeric(max(has_privilege) == 1),
      mean_CCP = mean(ccp),
      first_comment_date = min(post_date),
      last_comment_date = max(post_date),
      .groups = "drop"
    ) %>%
    dplyr::left_join(total_priv_time, by = c("user", "subverse"))

  # Also compute user-level trajectory (gain/steady/loss)
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

  privilege_summary_user <- plot_data %>%
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

  # Merge back
  data <- data %>%
    dplyr::left_join(privilege_summary, by = c("user", "subverse")) %>%
    dplyr::left_join(privilege_summary_user, by = "user")

  data
}

#' Calculate posting habit classes (burstiness + frequency)
calculate_habit <- function(data) {
  test <- data.table::as.data.table(data)
  data.table::setorder(test, user, time)
  test[, post_gap := as.numeric(difftime(time, data.table::shift(time), units = "hours")), by = user]

  burstiness_stats <- test[!is.na(post_gap), .(
    mean_gap = mean(post_gap, na.rm = TRUE),
    sd_gap = sd(post_gap, na.rm = TRUE)
  ), by = user]
  burstiness_stats[, burstiness := (sd_gap - mean_gap) / (sd_gap + mean_gap)]
  burstiness_stats[is.nan(burstiness) | is.infinite(burstiness), burstiness := NA]

  active_days <- test[, .(active_days = uniqueN(as.Date(time))), by = user]
  total_posts <- test[, .N, by = user]

  habit_summary <- merge(burstiness_stats, active_days, by = "user")
  habit_summary <- merge(habit_summary, total_posts, by = "user")
  habit_summary[, posts_per_day := N / active_days]

  threshold <- quantile(habit_summary$posts_per_day, 0.75, na.rm = TRUE)
  habit_summary[, habit_class := fifelse(
    burstiness > 0.7, "bursty",
    fifelse(posts_per_day > threshold, "steady regular", "occasional")
  )]
  habit_summary[, habit_class := factor(
    habit_class, levels = c("occasional", "steady regular", "bursty")
  )]

  # Merge back
  data %>%
    dplyr::left_join(
      habit_summary[, .(user, habit_class, posts_per_day)],
      by = "user"
    )
}

#' Calculate user tenure relative to platform shutdown
calculate_tenure <- function(data) {
  shutdown_date <- as.Date("2020-12-25")
  data %>%
    dplyr::mutate(
      tenure_days = as.numeric(difftime(shutdown_date, reg_date, units = "days")),
      tenure_strata = cut(
        tenure_days,
        breaks = c(-Inf, 365, 1000, Inf),
        labels = c("short", "mid", "long"),
        right = TRUE
      )
    )
}

#' Calculate subverse growth rate (subscribers / age)
calculate_subverse_growth <- function(data) {
  shutdown_date <- as.Date("2020-12-25")
  data %>%
    dplyr::mutate(
      sv_age_days = as.numeric(difftime(shutdown_date, date_created, units = "days")),
      sv_growth = subscriber_count / sv_age_days,
      growth_quantile = dplyr::ntile(sv_growth, 4),
      growth_category = dplyr::case_when(
        growth_quantile == 4 ~ "fast",
        growth_quantile == 1 ~ "slow",
        TRUE ~ "medium"
      ),
      sv_growth_factor = factor(growth_category, levels = c("slow", "medium", "fast"))
    )
}

#' Final cleanup: remove unused columns, set factor levels
finalize_preprocessing <- function(data) {
  # Ensure upvotes/downvotes are numeric (NA → 0)
  data <- data %>%
    dplyr::mutate(
      upvotes = dplyr::if_else(is.na(upvotes), 0, as.numeric(upvotes)),
      downvotes = dplyr::if_else(is.na(downvotes), 0, as.numeric(downvotes)),
      netvote = upvotes - downvotes
    )
  
  # Create binary content features for RQ3 (if columns exist)
  if (all(c("Secrecy", "Pattern", "Threat") %in% names(data))) {
    data <- data %>% dplyr::mutate(
      trait = as.integer(Secrecy == TRUE & Pattern == TRUE & Threat == TRUE)
    )
  }
  if (all(c("Actor", "Action") %in% names(data))) {
    data <- data %>% dplyr::mutate(
      group = as.integer(Actor == TRUE & Action == TRUE)
    )
  }
  
  # Final cleanup
  data %>%
    dplyr::select(
      -dplyr::any_of(c("...1", "link", "domain", "date", "about", "title", "Tags", "body"))
    ) %>%
    dplyr::mutate(
      conspiracy_group = factor(
        ifelse(sv_score >= 0, "conspiracy", "non-conspiracy"),
        levels = c("conspiracy", "non-conspiracy")
      )
    )
}

# =============================================================================
# 3. DESCRIPTIVES & VALIDATION
# =============================================================================

make_descriptives <- function(data) {
  list(
    n_users = dplyr::n_distinct(data$user),
    n_subverses = dplyr::n_distinct(data$subverse),
    n_comments = sum(!is.na(data$comment_id)),
    n_submissions = sum(!is.na(data$submission_id)),
    date_range = range(data$time, na.rm = TRUE)
  )
}

#' Sample top/bottom subverses for manual validation
sample_subverses_for_validation <- function(data) {
  data %>%
    dplyr::group_by(subverse) %>%
    dplyr::summarise(avg_sv_score = mean(sv_score, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(avg_sv_score)) %>%
    dplyr::slice(c(1:30, dplyr::n() - 29:dplyr::n()))
}

#' Validate sv_score against manual labels (placeholder for user-supplied annotations)
validate_sv_scores <- function(sampled_subverses) {
  # Placeholder: user should merge manual labels here
  # Returns structure expected by downstream validation plots
  list(sampled_subverses = sampled_subverses, auc = NA, kappa = NA)
}

#' PCA validation of 5 CT content features
validate_features_pca <- function(data) {
  features <- data %>%
    dplyr::select(dplyr::any_of(c("Action", "Actor", "Threat", "Secrecy", "Pattern"))) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))

  if (ncol(features) < 5) return(NULL)

  pca_res <- psych::principal(features, nfactors = 2, rotate = "varimax")
  list(
    loadings = as.data.frame(unclass(pca_res$loadings)),
    variance = pca_res$Vaccounted,
    rmsr = pca_res$fit
  )
}

# =============================================================================
# 4. RQ1 — SURVIVAL
# =============================================================================

#' Prepare survival data: define dropout as >=60 days inactivity before shutdown
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

  # Summarise covariates at user-subverse level
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

merge_privilege_survival <- function(survival_data, privilege_trajectory) {
  survival_data %>%
    dplyr::left_join(privilege_trajectory, by = "user") %>%
    dplyr::mutate(
      privilege = factor(privilege, levels = c("never_had", "steady", "fluctuating"))
    )
}

#' Run log-rank tests across strata
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

prepare_cox_long_data <- function(data, survival_data) {
  shutdown_date <- as.Date("2020-12-25") - 30
  
  # Drop privilege-derived date columns to avoid collisions
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
      # EXAKTER Fensterstart: 30-Tage-Schritte seit erstem Kommentar in der Subverse
      start = as.Date(first_comment_date) + lubridate::days((month - 1) * 30),
      # Fensterende: frühestes von 30 Tage nach Start, Shutdown, oder letzter Kommentar
      interval_stop_date = pmin(as.Date(start) + 30, shutdown_date, as.Date(last_comment_date), na.rm = TRUE),
      # Event nur wenn Intervall exakt am letzten Kommentar endet UND Dropout vorliegt
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

#' Cox diagnostics (Schoenfeld residuals)
diagnose_cox <- function(model, long_data) {
  # cox.zph does not support tt() terms; catch the error and return a message
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

# =============================================================================
# 5. RQ2 — POSTING LATENCY
# =============================================================================

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

diagnose_latency <- function(model, latency_data) {
  if (is.null(model)) return(NULL)
  list(
    icc = performance::icc(model),
    residuals = residuals(model),
    fitted = fitted(model)
  )
}

# =============================================================================
# 6. RQ3 — CT TRANSITIONS
# =============================================================================

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

#' Export list of key objects for the Shiny dashboard
export_shiny_inputs <- function(...) {
  list(...)
}
