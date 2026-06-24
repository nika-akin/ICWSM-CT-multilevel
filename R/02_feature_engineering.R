# R/02_feature_engineering.R — Feature calculation functions
# Controversy, privilege, habit, tenure, and subverse growth features

library(data.table)

#' Calculate Reddit-style controversy score
#' magnitude = upvotes + downvotes; balance = min(up/down, down/up)
#' controversy = magnitude ^ balance (0 if no votes)
#' @param data data frame with upvotes and downvotes columns
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
#' @param data data frame with user, subverse, time, upvotes, downvotes
calculate_privilege <- function(data) {
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

  data <- data %>%
    dplyr::left_join(privilege_summary, by = c("user", "subverse")) %>%
    dplyr::left_join(privilege_summary_user, by = "user")

  data
}

#' Calculate posting habit classes (burstiness + frequency)
#' @param data data frame with user and time columns
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

  data %>%
    dplyr::left_join(
      habit_summary[, .(user, habit_class, posts_per_day)],
      by = "user"
    )
}

#' Calculate user tenure relative to platform shutdown
#' @param data data frame with reg_date column
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
#' @param data data frame with subscriber_count and date_created
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
