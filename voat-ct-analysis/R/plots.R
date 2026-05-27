# R/plots.R — Publication-ready visualizations
# All functions return ggplot objects (or lists of them) for targets storage.

library(ggplot2)

# =============================================================================
# 1. THEME UTILITIES
# =============================================================================

theme_apa <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.8),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(color = "black"),
      axis.text = ggplot2::element_text(color = "black"),
      axis.title = ggplot2::element_text(size = base_size, color = "black"),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.background = ggplot2::element_rect(color = "grey", fill = NA, linewidth = 0.3)
    )
}

# =============================================================================
# 2. PREPROCESSING PLOTS
# =============================================================================

plot_controversy_histogram <- function(data) {
  data %>%
    dplyr::filter(controversy > 0) %>%
    ggplot(aes(x = controversy)) +
    geom_histogram(bins = 100, fill = "#7fbf7b", alpha = 0.7) +
    scale_x_log10() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(x = "Controversiality score (log scale)", y = "Count") +
    theme_apa()
}

plot_privilege_histogram <- function(privilege_summary) {
  p1 <- ggplot(privilege_summary, aes(x = total_days_with_privilege)) +
    geom_histogram(bins = 50, fill = "#af8dc3", color = "white") +
    labs(x = "Total Days with Privilege", y = "Users",
         title = "Submission Rights (CCP >= 10)") +
    theme_apa()

  p2 <- ggplot(privilege_summary, aes(x = factor(ever_had_privilege))) +
    geom_bar(fill = "#af8dc3") +
    labs(x = "Ever Had Privilege", y = "Users") +
    theme_apa()

  patchwork::wrap_plots(p1, p2, ncol = 2)
}

plot_habit_boxplot <- function(habit_summary) {
  ggplot(habit_summary, aes(x = habit_class, y = posts_per_day, fill = habit_class)) +
    geom_boxplot(width = 0.6, outlier.shape = NA, alpha = 0.8) +
    geom_jitter(width = 0.2, size = 0.5, alpha = 0.2) +
    scale_y_log10() +
    labs(x = "Habit Class", y = "Posts per Active Day (log scale)") +
    scale_fill_brewer(palette = "Set2") +
    theme_apa() +
    theme(legend.position = "none")
}

# =============================================================================
# 3. RQ1 — SURVIVAL PLOTS
# =============================================================================

plot_kaplan_meier <- function(survival_data) {
  survival_data$time_to_event_years <- survival_data$time_to_event / 365
  split_data <- split(survival_data, survival_data$conspiracy_group)
  
  # --- PRIVILEGE ---
  plot_list_priv <- lapply(names(split_data), function(group) {
    subdata <- split_data[[group]]
    subfit <- survival::survfit(
      survival::Surv(time_to_event_years, event_dropout) ~ privilege,
      data = subdata
    )
    plot <- survminer::ggsurvplot(
      subfit, data = subdata,
      palette = c("#c97b63", "#009e73", "#56b4e9"),
      legend.labs = c("fluctuating", "steady", "never had"),
      ggtheme = theme_apa(), xlim = c(0, 6), break.time.by = 1,
      pval = TRUE, pval.method = TRUE, pval.size = 4,
      pval.coord = c(0, 0.05), pval.method.coord = c(0, 0.12),
      risk.table = FALSE, fontsize = 2, size = 0.2,
      conf.int = FALSE, censor = TRUE, censor.size = 2
    )
    plot$plot <- plot$plot +
      labs(title = group, x = "Time (years)") +
      scale_y_continuous(labels = scales::percent_format()) +
      theme(legend.position = "none")
    plot$plot
  })
  final_priv <- patchwork::wrap_plots(plot_list_priv, ncol = 2) +
    patchwork::plot_annotation(title = "Survival for Privilege Strata") +
    patchwork::plot_layout(guides = "collect")
  
  # --- CONTROVERSY ---
  plot_list_controv <- lapply(names(split_data), function(group) {
    subdata <- split_data[[group]]
    subfit <- survival::survfit(
      survival::Surv(time_to_event_years, event_dropout) ~ controversy_group,
      data = subdata
    )
    plot <- survminer::ggsurvplot(
      subfit, data = subdata,
      palette = c("#c97b63", "#009e73"),
      legend.labs = c("low controversy", "high controversy"),
      ggtheme = theme_apa(), xlim = c(0, 6), break.time.by = 1,
      pval = TRUE, pval.method = TRUE, pval.size = 4,
      risk.table = FALSE, fontsize = 2, size = 0.2,
      conf.int = FALSE, censor = TRUE, censor.size = 2
    )
    plot$plot <- plot$plot +
      labs(title = group, x = "Time (years)") +
      scale_y_continuous(labels = scales::percent_format()) +
      theme(legend.position = "none")
    plot$plot
  })
  final_controv <- patchwork::wrap_plots(plot_list_controv, ncol = 2) +
    patchwork::plot_annotation(title = "Survival for Controversy Strata") +
    patchwork::plot_layout(guides = "collect")
  
  # --- HABIT ---
  plot_list_habit <- lapply(names(split_data), function(group) {
    subdata <- split_data[[group]]
    subfit <- survival::survfit(
      survival::Surv(time_to_event_years, event_dropout) ~ habit_class,
      data = subdata
    )
    plot <- survminer::ggsurvplot(
      subfit, data = subdata,
      palette = c("#c97b63", "#009e73", "#56b4e9"),
      legend.labs = c("occasional", "regular", "bursty"),
      ggtheme = theme_apa(), xlim = c(0, 6), break.time.by = 1,
      pval = TRUE, pval.method = TRUE, pval.size = 4,
      risk.table = FALSE, fontsize = 2, size = 0.2,
      conf.int = FALSE, censor = TRUE, censor.size = 2
    )
    plot$plot <- plot$plot +
      labs(title = group, x = "Time (years)") +
      scale_y_continuous(labels = scales::percent_format()) +
      theme(legend.position = "none")
    plot$plot
  })
  final_habit <- patchwork::wrap_plots(plot_list_habit, ncol = 2) +
    patchwork::plot_annotation(title = "Survival for Habit Strata") +
    patchwork::plot_layout(guides = "collect")
  
  # --- GROWTH ---
  plot_list_growth <- lapply(names(split_data), function(group) {
    subdata <- split_data[[group]]
    subfit <- survival::survfit(
      survival::Surv(time_to_event_years, event_dropout) ~ sv_growth_factor,
      data = subdata
    )
    plot <- survminer::ggsurvplot(
      subfit, data = subdata,
      palette = c("#c97b63", "#009e73", "#56b4e9"),
      legend.labs = c("slow growth", "medium growth", "fast growth"),
      ggtheme = theme_apa(), xlim = c(0, 6), break.time.by = 1,
      pval = TRUE, pval.method = TRUE, pval.size = 4,
      risk.table = FALSE, fontsize = 2, size = 0.2,
      conf.int = FALSE, censor = TRUE, censor.size = 2
    )
    plot$plot <- plot$plot +
      labs(title = group, x = "Time (years)") +
      scale_y_continuous(labels = scales::percent_format()) +
      theme(legend.position = "none")
    plot$plot
  })
  final_growth <- patchwork::wrap_plots(plot_list_growth, ncol = 2) +
    patchwork::plot_annotation(title = "Survival for Subverse Growth Type") +
    patchwork::plot_layout(guides = "collect")
  
  list(
    privilege = final_priv,
    controversy = final_controv,
    habit = final_habit,
    growth = final_growth
  )
}

plot_cox_results <- function(cox_model, long_data) {
  if (is.null(cox_model)) return(NULL)
  
  cf <- coef(cox_model)
  vc <- vcov(cox_model)
  
  # Paper-Plot: 0 bis 2000 Tage
  time_seq <- seq(0, 2000, length.out = 300)
  
  get_idx <- function(main, tt) {
    c(which(names(cf) == main), which(names(cf) == tt))
  }
  
  compute_hr <- function(idx, t_seq) {
    cm <- cf[idx]
    vm <- vc[idx, idx]
    log_hr <- cm[1] + cm[2] * log1p(t_seq)
    var_log <- vm[1,1] + (log1p(t_seq)^2) * vm[2,2] + 2 * log1p(t_seq) * vm[1,2]
    se_log <- sqrt(max(var_log, 0))
    data.frame(
      time = t_seq,
      HR = exp(log_hr),
      lower = exp(log_hr - 1.96 * se_log),
      upper = exp(log_hr + 1.96 * se_log)
    )
  }
  
  idx_up   <- get_idx("avg_upvotes_window", "tt(avg_upvotes_window)")
  idx_down <- get_idx("avg_downvotes_window", "tt(avg_downvotes_window)")
  
  hr_up   <- compute_hr(idx_up, time_seq);   hr_up$Covariate   <- "Avg Upvotes"
  hr_down <- compute_hr(idx_down, time_seq); hr_down$Covariate <- "Avg Downvotes"
  
  plot_data <- rbind(hr_up, hr_down)
  
  # Figure 4: warm=Upvotes (#E69F00), cool=Downvotes (#56B4E9)
  p_time <- ggplot(plot_data, aes(x = time, y = HR, color = Covariate, fill = Covariate)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = c("Avg Upvotes" = "#E69F00", "Avg Downvotes" = "#56B4E9")) +
    scale_fill_manual(values  = c("Avg Upvotes" = "#E69F00", "Avg Downvotes" = "#56B4E9")) +
    labs(x = "Days since baseline", y = "Hazard Ratio") +
    coord_cartesian(xlim = c(0, 2000), ylim = c(0.95, 1.20)) +
    scale_x_continuous(breaks = seq(0, 2000, by = 500)) +
    theme_apa() +
    theme(legend.position = "bottom")
  
  # Main effects (Figure 9 / Appendix D)
  tidy_cox <- broom::tidy(cox_model, conf.int = TRUE, exponentiate = TRUE) %>%
    dplyr::filter(!stringr::str_detect(term, "frailty")) %>%
    dplyr::filter(!stringr::str_detect(term, "tt\\(")) %>%
    dplyr::mutate(
      term_clean = dplyr::case_when(
        term == "avg_upvotes_window"          ~ "Upvotes",
        term == "avg_downvotes_window"        ~ "Downvotes",
        term == "controversy_window"          ~ "Controversy",
        stringr::str_detect(term, "^habit_class") ~ stringr::str_replace(term, "habit_class", "Habit "),
        stringr::str_detect(term, "^tenure_strata") ~ "Tenure",
        stringr::str_detect(term, "^conspiracy_group") ~ "Non-CT sv",
        TRUE ~ term
      ),
      group_color = dplyr::case_when(
        stringr::str_detect(term_clean, "^Habit") ~ "Habit",
        term_clean %in% c("Upvotes", "Downvotes") ~ "Engagement",
        TRUE ~ "Other"
      )
    )
  
  p_main <- ggplot(tidy_cox, aes(x = estimate, y = reorder(term_clean, estimate), color = group_color)) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, linewidth = 1.2, alpha = 0.8) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
    scale_x_continuous(breaks = seq(0.5, 2.0, by = 0.25), limits = c(0.5, 2.0)) +
    scale_color_manual(values = c("Habit" = "#56B4E9", "Engagement" = "#E69F00", "Other" = "grey50")) +
    labs(x = "Hazard Ratio", y = NULL) +
    theme_apa() +
    theme(legend.position = "none", panel.grid.major.y = element_blank())
  
  list(main_effects = p_main, time_varying = p_time)
}
# =============================================================================
# 4. RQ2 — LATENCY PLOTS
# =============================================================================

plot_latency_results <- function(model, latency_data) {
  if (is.null(model)) return(NULL)

  # Interaction plot using ggeffects
  pred <- ggeffects::ggpredict(model, terms = c("lagged_net_votes_z", "habit_class"))

  p_interaction <- ggplot(pred, aes(x = x, y = predicted, color = group)) +
    geom_line(size = 1.2) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.15, color = NA) +
    labs(
      x = "Lagged Net Votes (z)",
      y = "log(1 + Posting Latency) (hours)",
      color = "Habit Class", fill = "Habit Class"
    ) +
    scale_color_manual(values = c("#af8dc3", "#7fbf7b", "grey")) +
    scale_fill_manual(values = c("#af8dc3", "#7fbf7b", "grey")) +
    theme_apa() +
    theme(legend.position = "bottom")

  # Partial residuals (hexbin)
  model_data <- model@frame
  resid_df <- data.frame(
    user = model_data$user,
    lagged_net_votes = model_data$lagged_net_votes,
    habit_class = model_data$habit_class,
    residuals = residuals(model)
  )

  p_hex <- ggplot(resid_df, aes(x = lagged_net_votes, y = residuals)) +
    ggplot2::geom_hex(bins = 50) +
    facet_wrap(~ habit_class) +
    labs(x = "Lagged net votes", y = "Partial residual") +
    theme_minimal()

  list(interaction = p_interaction, residuals = p_hex)
}

# =============================================================================
# 5. RQ3 — TRANSITION PLOTS
# =============================================================================

plot_glmm_results <- function(model, transition_data) {
  if (is.null(model)) return(NULL)

  coef_df <- as.data.frame(summary(model)$coefficients) %>%
    tibble::rownames_to_column("term") %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::mutate(
      lower = Estimate - 1.96 * `Std. Error`,
      upper = Estimate + 1.96 * `Std. Error`,
      Estimate_OR = exp(Estimate),
      lower_OR = exp(lower),
      upper_OR = exp(upper),
      stars = dplyr::case_when(
        `Pr(>|z|)` < 0.001 ~ "***",
        `Pr(>|z|)` < 0.01 ~ "**",
        `Pr(>|z|)` < 0.05 ~ "*",
        TRUE ~ ""
      ),
      label_OR = paste0(sprintf("%.2f", Estimate_OR), stars),
      effect_type = ifelse(grepl(":", term), "Interaction", "Main")
    )

  ggplot(coef_df, aes(x = reorder(term, Estimate_OR), y = Estimate_OR, color = effect_type)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = lower_OR, ymax = upper_OR), width = 0, size = 2) +
    geom_text(aes(label = label_OR), vjust = -1.2, size = 3.5, color = "#7fbf7b") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
    scale_color_manual(values = c("Main" = "#7fbf7b", "Interaction" = "#af8dc3")) +
    coord_flip() +
    labs(x = "", y = "Odds ratio (OR)") +
    theme_apa() +
    theme(legend.position = "none")
}

plot_gamm_smooths <- function(gamm_model) {
  if (is.null(gamm_model)) return(NULL)

  plot_smooth <- function(smooth_obj, x_var, x_label) {
    gratia::add_confint(smooth_obj) %>%
      ggplot(aes(y = .estimate, x = get(x_var))) +
      geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.2, fill = "#7fbf7b") +
      geom_line(color = "#7fbf7b", linewidth = 1.5) +
      labs(y = "Partial effect (log-odds)", x = x_label) +
      theme_apa()
  }

  sm_trait  <- gratia::smooth_estimates(gamm_model, smooth = "s(mean_trait_lag)")
  sm_group  <- gratia::smooth_estimates(gamm_model, smooth = "s(mean_group_lag)")
  sm_score  <- gratia::smooth_estimates(gamm_model, smooth = "s(mean_score_lag)")
  sm_week   <- gratia::smooth_estimates(gamm_model, smooth = "s(week_index)")
  sm_comm   <- gratia::smooth_estimates(gamm_model, smooth = "s(n_comments)")

  list(
    trait  = plot_smooth(sm_trait,  "mean_trait_lag",  "Trait identity (SD from mean)"),
    group  = plot_smooth(sm_group,  "mean_group_lag",  "Group identity (SD from mean)"),
    score  = plot_smooth(sm_score,  "mean_score_lag",  "Net votes (SD from mean)"),
    week   = plot_smooth(sm_week,   "week_index",      "Week index (SD from mean)"),
    comm   = plot_smooth(sm_comm,   "n_comments",      "Comments (SD from mean)")
  )
}

compute_gamm_marginals <- function(gamm_model, transition_data) {
  if (is.null(gamm_model)) return(NULL)

  sd_seq <- seq(-2, 2, length.out = 100)
  ref <- list(
    transition_to_ct_lag = 0,
    mean_trait_lag = 0,
    mean_group_lag = 0,
    mean_score_lag = 0,
    week_index = median(transition_data$week_index, na.rm = TRUE),
    n_comments = 0
  )

  make_grid <- function(focal_var, hc) {
    g <- expand.grid(
      habit_class = factor(hc, levels = levels(transition_data$habit_class)),
      sd_value = sd_seq,
      stringsAsFactors = FALSE
    )
    for (nm in names(ref)) g[[nm]] <- ref[[nm]]
    g[[focal_var]] <- g$sd_value
    g$user <- transition_data$user[1]  # dummy
    g
  }

  predict_gamm <- function(newdata) {
    pl <- predict(gamm_model, newdata = newdata, type = "link", se.fit = TRUE, exclude = "s(user)")
    newdata$pred <- plogis(pl$fit)
    newdata$lower <- plogis(pl$fit - 1.96 * pl$se.fit)
    newdata$upper <- plogis(pl$fit + 1.96 * pl$se.fit)
    newdata
  }

  bind_rows(
    predict_gamm(make_grid("mean_trait_lag", "occasional")) %>% mutate(effect = "Trait Content", habit_class = "occasional"),
    predict_gamm(make_grid("mean_trait_lag", "steady regular")) %>% mutate(effect = "Trait Content", habit_class = "steady regular"),
    predict_gamm(make_grid("mean_trait_lag", "bursty")) %>% mutate(effect = "Trait Content", habit_class = "bursty"),
    predict_gamm(make_grid("mean_group_lag", "occasional")) %>% mutate(effect = "Group Content", habit_class = "occasional"),
    predict_gamm(make_grid("mean_group_lag", "steady regular")) %>% mutate(effect = "Group Content", habit_class = "steady regular"),
    predict_gamm(make_grid("mean_group_lag", "bursty")) %>% mutate(effect = "Group Content", habit_class = "bursty"),
    predict_gamm(make_grid("mean_score_lag", "occasional")) %>% mutate(effect = "Net Votes", habit_class = "occasional"),
    predict_gamm(make_grid("mean_score_lag", "steady regular")) %>% mutate(effect = "Net Votes", habit_class = "steady regular"),
    predict_gamm(make_grid("mean_score_lag", "bursty")) %>% mutate(effect = "Net Votes", habit_class = "bursty")
  )
}

plot_gamm_marginal_lines <- function(marginal_data) {
  if (is.null(marginal_data)) return(NULL)

  custom_colors <- c("#af8dc3", "#7fbf7b", "grey")

  ggplot(marginal_data, aes(x = sd_value, y = pred, color = habit_class, fill = habit_class)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, color = NA) +
    geom_line(size = 1) +
    facet_wrap(~ effect) +
    scale_color_manual(values = custom_colors) +
    scale_fill_manual(values = custom_colors) +
    labs(
      x = "Predictor (standardized)",
      y = "Predicted probability of CT upshift",
      color = "Habit Class", fill = "Habit Class"
    ) +
    theme_apa() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
}

plot_gamm_contour <- function(gamm_model, transition_data) {
  if (is.null(gamm_model)) return(NULL)

  # Build 2D grid for group x comments interaction (if present in model)
  g_seq <- seq(-2, 2, length.out = 120)
  c_seq <- seq(-2, 2, length.out = 120)

  make_grid2d <- function(hc) {
    expand.grid(mean_group_lag = g_seq, n_comments = c_seq) %>%
      mutate(
        mean_trait_lag = 0,
        mean_score_lag = 0,
        transition_to_ct_lag = 0,
        week_index = median(transition_data$week_index, na.rm = TRUE),
        habit_class = factor(hc, levels = levels(transition_data$habit_class)),
        user = transition_data$user[1]
      )
  }

  grid_all <- bind_rows(
    make_grid2d("occasional"),
    make_grid2d("steady regular"),
    make_grid2d("bursty")
  )

  pl <- predict(gamm_model, newdata = grid_all, type = "link", se.fit = TRUE, exclude = "s(user)")
  grid_all$p_hat <- plogis(pl$fit)

  ggplot(grid_all, aes(x = mean_group_lag, y = n_comments, z = p_hat)) +
    geom_raster(aes(fill = p_hat), interpolate = TRUE) +
    geom_contour(color = "white", bins = 10, linewidth = 0.3) +
    facet_wrap(~ habit_class, nrow = 1) +
    scale_fill_viridis_c(name = "P(CT upshift)", limits = c(0, 1)) +
    labs(x = "Group identity (z)", y = "Weekly comments (z)") +
    theme_minimal(base_size = 11) +
    theme(
      panel.spacing = unit(8, "pt"),
      strip.text = element_text(face = "bold"),
      legend.position = "right"
    )
}

# =============================================================================
# 6. VALIDATION PLOTS
# =============================================================================

plot_validation_summary <- function(validation_results) {
  # Placeholder: would plot ROC or kappa from validation_sv_results
  NULL
}

plot_pca_loadings <- function(pca_result) {
  if (is.null(pca_result)) return(NULL)

  loadings <- pca_result$loadings %>%
    tibble::rownames_to_column("Feature") %>%
    tidyr::pivot_longer(-Feature, names_to = "Component", values_to = "Loading")

  ggplot(loadings, aes(x = Component, y = Feature, fill = Loading)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "#c97b63", mid = "white", high = "#009e73") +
    labs(x = "Principal Component", y = "Feature", title = "PCA Feature Loadings") +
    theme_apa() +
    theme(legend.position = "right")
}
