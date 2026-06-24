# app.R — Voat CT Analysis Dashboard
# ============================================================

# ============================================================

library(shiny)
library(shinydashboard)
library(targets)
library(withr)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(broom)
library(broom.mixed)
library(survival)
library(lme4)
library(mgcv)
library(scales)
library(data.table)
library(tidyr)
library(performance)
# ------------------------------------------------------------------
# 1. CONFIGURATION
# ------------------------------------------------------------------


# ------------------------------------------------------------------
# 1. CONFIGURATION
# ------------------------------------------------------------------

# ------------------------------------------------------------------
# 1. CONFIGURATION
# ------------------------------------------------------------------

store_path <- normalizePath("../_targets", mustWork = FALSE)
message("STORE PATH: ", store_path)
message("STORE EXISTS: ", dir.exists(store_path))

preload_targets <- function(store) {
  if (!dir.exists(store)) stop("Store missing: ", store)
  
  project_dir <- dirname(store)
  store_name  <- basename(store)
  
  old_wd <- getwd()
  setwd(project_dir)
  on.exit(setwd(old_wd), add = TRUE)
  
  targets::tar_config_set(store = store_name)
  
  # Load each target individually WITHOUT tryCatch wrapper
  # tar_read works when called directly, fails inside tryCatch/lapply
  
  loaded <- list()
  
  loaded$descriptives_table <- targets::tar_read("descriptives_table")
  message("Loaded descriptives_table")
  
  loaded$survival_merged <- targets::tar_read("survival_merged")
  message("Loaded survival_merged")
  
  loaded$latency_data <- targets::tar_read("latency_data")
  message("Loaded latency_data")
  
  loaded$latency_model <- targets::tar_read("latency_model")
  message("Loaded latency_model")
  
  loaded$latency_plots <- targets::tar_read("latency_plots")
  message("Loaded latency_plots")
  
  loaded$cox_model <- targets::tar_read("cox_model")
  message("Loaded cox_model")
  
  loaded$cox_long_data <- targets::tar_read("cox_long_data")
  message("Loaded cox_long_data")
  
  loaded$cox_plots <- targets::tar_read("cox_plots")
  message("Loaded cox_plots")
  
  loaded$glmm_model <- targets::tar_read("glmm_model")
  message("Loaded glmm_model")
  
  loaded$gamm_model <- targets::tar_read("gamm_model")
  message("Loaded gamm_model")
  
  loaded$gamm_marginal <- targets::tar_read("gamm_marginal")
  message("Loaded gamm_marginal")
  
  loaded$gamm_contour <- targets::tar_read("gamm_contour")
  message("Loaded gamm_contour")
  
  loaded$transition_data <- targets::tar_read("transition_data")
  message("Loaded transition_data")
  
  loaded$data_clean <- targets::tar_read("data_clean")
  message("Loaded data_clean")
  
  message("Preloaded ", sum(!sapply(loaded, is.null)), "/", length(loaded), " targets")
  loaded
}


# Execute immediately (not inside a function that might fail silently)
TARGETS <- tryCatch(preload_targets(store_path), error = function(e) {
  message("PRELOAD ERROR: ", conditionMessage(e))
  list()
})

tar_get <- function(name) {
  if (!name %in% names(TARGETS)) {
    warning("Target '", name, "' not available")
    return(NULL)
  }
  TARGETS[[name]]
}

# Dummy for any old code still calling safe_tar_read
safe_tar_read <- function(name, store = store_path) tar_get(name)
# ------------------------------------------------------------------
# 2. THEME & HELPERS
# ------------------------------------------------------------------

PAL <- c("#4472C4", "#ED7D31", "#70AD47", "#A5A5A5", "#FFC000", "#5B9BD5", "#264478")

pnas_theme <- function() {
  theme_minimal(base_size = 13, base_family = "Helvetica") +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major   = element_line(colour = "#f0f0f0", size = 0.3),
      panel.border       = element_rect(fill = NA, colour = "#cccccc", size = 0.4),
      plot.title         = element_text(face = "bold", size = 14, colour = "#1a1a1a", hjust = 0),
      plot.subtitle      = element_text(size = 11, colour = "#555555", hjust = 0, margin = margin(b = 8)),
      plot.caption       = element_text(size = 10, colour = "#666666", hjust = 0, face = "italic"),
      axis.title         = element_text(size = 11, colour = "#333333"),
      axis.text          = element_text(size = 10, colour = "#555555"),
      legend.position    = "bottom",
      legend.title       = element_text(size = 10, face = "bold"),
      legend.text        = element_text(size = 10),
      strip.text         = element_text(face = "bold", size = 11, colour = "#1a1a1a"),
      strip.background   = element_rect(fill = "#f8f8f8", colour = "#cccccc"),
      plot.margin        = margin(12, 12, 12, 12)
    )
}

# Badge helper for statistical annotations
stat_badge <- function(label, value, colour = "#e8e8e8") {
  tags$span(
    style = paste0("display:inline-block; padding:3px 10px; background:", colour,
                   "; border-radius:3px; font-size:11px; margin-right:6px; font-weight:600; color:#333;"),
    paste0(label, ": ", value)
  )
}

# Placeholder ggplot when data is missing
placeholder_plot <- function(title = "Data not available", subtitle = "Run the targets pipeline to generate this target.") {
  ggplot(data.frame(x = 1, y = 1, label = "∅"), aes(x, y, label = label)) +
    geom_text(size = 20, colour = "#cccccc") +
    labs(title = title, subtitle = subtitle) +
    pnas_theme() +
    theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank())
}

# Kaplan–Meier → ggplot (publication style)
km_to_ggplot <- function(fit, title = "Kaplan–Meier", caption = NULL) {
  s <- summary(fit, censored = TRUE)
  strata_names <- if (is.null(s$strata)) "All" else names(s$strata)[s$strata]
  df <- data.frame(
    time    = s$time,
    surv    = s$surv,
    strata  = strata_names,
    upper   = s$upper,
    lower   = s$lower
  )
  ggplot(df, aes(x = time, y = surv, colour = strata, fill = strata)) +
    geom_step(size = 0.8) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.12, colour = NA) +
    scale_y_continuous(labels = percent, limits = c(0, 1), expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_colour_manual(values = PAL) +
    scale_fill_manual(values = PAL) +
    labs(x = "Days since first post", y = "Survival probability",
         title = title, caption = caption, colour = "", fill = "") +
    pnas_theme()
}

# Forest plot
forest_plot <- function(tidy_df, x_lab = "Estimate", log_scale = FALSE, ref_line = 1,
                        title = "Fixed-effects forest plot", caption = NULL) {
  tidy_df <- tidy_df %>%
    filter(term != "(Intercept)") %>%
    mutate(term = stats::reorder(term, estimate))
  p <- ggplot(tidy_df, aes(x = estimate, y = term)) +
    geom_point(size = 2.5, colour = "#264478") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.15, colour = "#264478", size = 0.6) +
    geom_vline(xintercept = ref_line, linetype = "dashed", colour = "#c0392b", size = 0.4) +
    labs(x = x_lab, y = NULL, title = title, caption = caption) +
    pnas_theme()
  if (log_scale) p <- p + scale_x_log10()
  p
}

# Time-varying coefficients from Cox tt() formula
plot_tv_effects <- function(cox_model, t_max = 365, caption = NULL) {
  if (is.null(cox_model)) return(NULL)
  cf <- coef(cox_model)
  t_seq <- seq(1, t_max, length.out = 300)

  up_base   <- cf["avg_upvotes_window"]
  up_tt     <- cf["tt(avg_upvotes_window)"]
  down_base <- cf["avg_downvotes_window"]
  down_tt   <- cf["tt(avg_downvotes_window)"]

  tv <- data.frame(
    t = t_seq,
    Upvotes   = up_base   + up_tt   * log1p(t_seq),
    Downvotes = down_base + down_tt * log1p(t_seq)
  ) %>%
    pivot_longer(-t, names_to = "effect", values_to = "log_hr")

  ggplot(tv, aes(x = t, y = log_hr, colour = effect)) +
    geom_line(size = 0.9) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "#999999", size = 0.4) +
    scale_colour_manual(values = c("Upvotes" = "#4472C4", "Downvotes" = "#ED7D31")) +
    labs(x = "Days since baseline", y = "Log hazard ratio",
         title = "Time-varying vote effects", caption = caption, colour = "") +
    pnas_theme()
}

# Calibration plot (binned)
calibration_plot <- function(model, data, response, n_bins = 10, caption = NULL) {
  if (is.null(model) || is.null(data)) return(NULL)
  probs <- predict(model, newdata = data, type = "response", allow.new.levels = TRUE)
  brks <- quantile(probs, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)
  brks[1] <- min(brks[1], min(probs, na.rm = TRUE) - 0.001)
  brks[length(brks)] <- max(brks[length(brks)], max(probs, na.rm = TRUE) + 0.001)

  df <- data.frame(prob = probs, obs = data[[response]]) %>%
    mutate(bin = cut(prob, breaks = brks, include.lowest = TRUE)) %>%
    group_by(bin) %>%
    summarise(
      mean_pred = mean(prob, na.rm = TRUE),
      mean_obs  = mean(obs, na.rm = TRUE),
      n = n(),
      se = sqrt(mean_obs * (1 - mean_obs) / max(n, 1)),
      .groups = "drop"
    ) %>%
    filter(n >= 5)

  ggplot(df, aes(x = mean_pred, y = mean_obs)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "#999999", size = 0.4) +
    geom_point(aes(size = n), colour = "#264478", alpha = 0.85) +
    geom_errorbar(aes(ymin = mean_obs - 1.96 * se, ymax = mean_obs + 1.96 * se),
                  width = 0.015, colour = "#264478", size = 0.4) +
    scale_size_continuous(range = c(2, 10), name = "n") +
    labs(x = "Mean predicted probability", y = "Observed proportion",
         title = "Calibration plot (binned)", caption = caption) +
    pnas_theme()
}

# Q-learning simulation from LMM coefficients
simulate_q_learning <- function(lmm_mod, n_steps = 200) {
  if (is.null(lmm_mod)) return(NULL)
  cf <- fixef(lmm_mod)
  # Extract coefficients for simulation
  # Model: post_latency_log ~ lagged_net_votes_z * habit_class + sv_growth_z + tenure_strata + (1|user) + (1|subverse)
  # We simulate three agents with their respective intercepts and slopes

  votes_z <- seq(-2, 2, length.out = n_steps)

  # Baseline (occasional, non-CT, mid tenure, sv_growth = 0)
  base_occ <- cf["(Intercept)"]
  slope_occ <- cf["lagged_net_votes_z"]

  base_steady <- base_occ + ifelse("habit_classsteady regular" %in% names(cf), cf["habit_classsteady regular"], 0)
  slope_steady <- slope_occ + ifelse("lagged_net_votes_z:habit_classsteady regular" %in% names(cf),
                                       cf["lagged_net_votes_z:habit_classsteady regular"], 0)

  base_burst <- base_occ + ifelse("habit_classbursty" %in% names(cf), cf["habit_classbursty"], 0)
  slope_burst <- slope_occ + ifelse("lagged_net_votes_z:habit_classbursty" %in% names(cf),
                                     cf["lagged_net_votes_z:habit_classbursty"], 0)

  data.frame(
    votes_z = rep(votes_z, 3),
    log_latency = c(base_occ + slope_occ * votes_z,
                    base_steady + slope_steady * votes_z,
                    base_burst + slope_burst * votes_z),
    habit = rep(c("Occasional", "Steady regular", "Bursty"), each = n_steps)
  ) %>%
    mutate(latency_hrs = expm1(log_latency),
           habit = factor(habit, levels = c("Occasional", "Steady regular", "Bursty")))
}

# Permutation importance for GLMM (approximate)
perm_importance_glmm <- function(model, data, response = "transition_to_ct", n_perm = 1) {
  if (is.null(model) || is.null(data)) return(NULL)
  base_ll <- as.numeric(logLik(model))
  preds <- predict(model, newdata = data, type = "response", allow.new.levels = TRUE)
  base_auc <- tryCatch({
    pos <- preds[data[[response]] == 1]
    neg <- preds[data[[response]] == 0]
    wilcox.test(pos, neg)$statistic / (length(pos) * length(neg))
  }, error = function(e) NA)

  vars <- c("mean_trait_lag", "mean_group_lag", "mean_score_lag", "n_comments", "week_index")
  vars <- intersect(vars, names(data))

  res <- lapply(vars, function(v) {
    d_perm <- data
    d_perm[[v]] <- sample(d_perm[[v]], nrow(d_perm))
    p <- predict(model, newdata = d_perm, type = "response", allow.new.levels = TRUE)
    auc <- tryCatch({
      pos <- p[data[[response]] == 1]; neg <- p[data[[response]] == 0]
      wilcox.test(pos, neg)$statistic / (length(pos) * length(neg))
    }, error = function(e) NA)
    data.frame(
      variable = v,
      auc_drop = max(0, base_auc - auc, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  bind_rows(res) %>% arrange(desc(auc_drop))
}

# Partial dependence for GAMM smooth terms (manual grid)
partial_dependence_gamm <- function(model, data, var, n = 100) {
  if (is.null(model) || is.null(data)) return(NULL)
  grid_range <- range(data[[var]], na.rm = TRUE)
  grid <- seq(grid_range[1], grid_range[2], length.out = n)

  # Create a prediction data frame with all other variables at their medians/modes
  newdata <- data[1, , drop = FALSE]
  newdata <- newdata[rep(1, n), ]
  for (nm in names(newdata)) {
    if (nm == var) {
      newdata[[nm]] <- grid
    } else if (is.numeric(newdata[[nm]])) {
      newdata[[nm]] <- median(data[[nm]], na.rm = TRUE)
    } else if (is.factor(newdata[[nm]])) {
      newdata[[nm]] <- factor(levels(data[[nm]])[1], levels = levels(data[[nm]]))
    }
  }

  pred <- predict(model, newdata = newdata, type = "response", se.fit = TRUE)
  data.frame(
    x = grid,
    pred = pred$fit,
    lower = pred$fit - 1.96 * pred$se.fit,
    upper = pred$fit + 1.96 * pred$se.fit,
    variable = var
  )
}

# Effect-size volcano across all RQs
volcano_data <- function(lmm, glmm, cox) {
  out <- list()
  
  if (!is.null(lmm)) {
    td <- tryCatch(broom.mixed::tidy(lmm, effects = "fixed", conf.int = TRUE), error = function(e) NULL)
    if (!is.null(td) && any(c("p.value", "Pr(>|t|)", "Pr(>|z|)") %in% names(td))) {
      if (!"p.value" %in% names(td)) {
        td <- td %>% rename(p.value = matches("Pr\\(>\\|"))
      }
      out$rq1 <- td %>%
        filter(term != "(Intercept)") %>%
        mutate(rq = "RQ1: Latency", log_effect = estimate, neg_log10_p = -log10(p.value))
    }
  }
  
  if (!is.null(glmm)) {
    td <- tryCatch(broom.mixed::tidy(glmm, exponentiate = TRUE, conf.int = TRUE), error = function(e) NULL)
    if (!is.null(td) && "p.value" %in% names(td)) {
      out$rq2 <- td %>%
        filter(term != "(Intercept)") %>%
        mutate(rq = "RQ2: Transitions", log_effect = log(estimate), neg_log10_p = -log10(p.value))
    }
  }
  
  if (!is.null(cox)) {
    td <- tryCatch(broom::tidy(cox, exponentiate = TRUE, conf.int = TRUE), error = function(e) NULL)
    if (!is.null(td) && "p.value" %in% names(td)) {
      out$rq3 <- td %>%
        filter(term != "(Intercept)") %>%
        mutate(rq = "RQ3: Survival", log_effect = log(estimate), neg_log10_p = -log10(p.value))
    }
  }
  
  if (length(out) == 0) return(NULL)
  
  bind_rows(out) %>% mutate(significant = neg_log10_p > -log10(0.05))
}

# ------------------------------------------------------------------
# 3. UI
# ------------------------------------------------------------------
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = span("Voat CT Analysis  —  Interactive Supplement", style = "font-weight: 500; letter-spacing: 0.3px; font-size: 18px;")
  ),
  dashboardSidebar(
    width = 220,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview",            tabName = "overview",       icon = icon("dashboard")),
      menuItem("RQ1: Responsiveness", tabName = "rq1_latency",    icon = icon("clock")),
      menuItem("RQ2: CT Transitions", tabName = "rq2_transitions",  icon = icon("exchange-alt")),
      menuItem("RQ3: Persistence",    tabName = "rq3_survival",     icon = icon("heartbeat")),
      menuItem("Network & Dynamics",  tabName = "network",        icon = icon("project-diagram")),
      menuItem("Model Exploration",   tabName = "exploration",    icon = icon("flask"))
    ),
    div(style = "padding: 15px; color: #888; font-size: 10px; line-height: 1.4;",
        "Reproducible targets pipeline", br(), "PNAS-style interactive supplement v3.0")
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #ffffff; }
      .main-sidebar { background-color: #f4f5f7 !important; border-right: 1px solid #e0e0e0; }
      .sidebar-menu > li > a { color: #444; font-size: 13px; }
      .sidebar-menu > li.active > a { background-color: #e8eaf0 !important; color: #1a1a1a !important; font-weight: 600; border-left: 3px solid #4472C4; }
      .box { border: 1px solid #e0e0e0; border-radius: 2px; box-shadow: 0 1px 3px rgba(0,0,0,0.04); }
      .box-header { background-color: #fafafa; border-bottom: 1px solid #e8e8e8; padding: 10px 15px; }
      .box-title { font-size: 13px; font-weight: 600; color: #333; letter-spacing: 0.3px; }
      .box-body { padding: 15px; }
      h2 { font-family: Georgia, 'Times New Roman', serif; color: #1a1a1a; font-size: 22px; margin-bottom: 18px; border-bottom: 2px solid #f0f0f0; padding-bottom: 8px; }
      h3 { font-family: Georgia, 'Times New Roman', serif; color: #333; font-size: 16px; margin-top: 24px; margin-bottom: 12px; }
      .plot-caption { font-style: italic; color: #666; font-size: 11.5px; margin-top: 6px; line-height: 1.4; }
      .stat-badge-row { margin-bottom: 10px; }
      .btn-dl { font-size: 11px; padding: 3px 10px; margin-top: 6px; }
      .shiny-output-error { visibility: hidden; }
      .shiny-output-error:before {
        visibility: visible;
        content: 'Target not available — run the pipeline first.';
        color: #c0392b;
        font-style: italic;
        padding: 20px;
        display: block;
        background: #fdf2f2;
        border: 1px solid #f5c6cb;
        border-radius: 3px;
      }
    "))),
    tabItems(
      # ================================================================
      # OVERVIEW
      # ================================================================
      tabItem(tabName = "overview",
        fluidRow(
          valueBoxOutput("vb_users",     width = 3),
          valueBoxOutput("vb_subverses", width = 3),
          valueBoxOutput("vb_comments",  width = 3),
          valueBoxOutput("vb_span",      width = 3)
        ),
        fluidRow(
          box(title = "Study Design & Research Questions", width = 7, status = NULL, solidHeader = FALSE,
              p(style = "font-family: Georgia, serif; font-size: 13.5px; line-height: 1.6; color: #333;",
                "This dashboard accompanies the paper \u2018Why Conspiracy Theory Communities Endure\u2019 (Batzdorfer, KIT). ",
                "It presents confirmed statistical results alongside model diagnostics, robustness checks, and simulations ",
                "that extend the published analyses."),
              tags$ul(style = "font-size: 12.5px; line-height: 1.7; color: #444;",
                tags$li(strong("RQ1 (Responsiveness):"), "How does social-feedback sensitivity vary with routine behaviour and community context? — LMM with three-way interaction."),
                tags$li(strong("RQ2 (Transitions):"), "Which content and behavioural features predict weekly upshifts toward CT communities? — GAMM with penalised smooths; GLMM robustness check."),
                tags$li(strong("RQ3 (Persistence):"), "How does user persistence vary with feedback, routine, and community? — Extended Cox PH with time-varying coefficients and subverse frailty.")
              )
          ),
          box(title = "Pipeline Health", width = 5, status = NULL, solidHeader = FALSE,
              uiOutput("pipeline_status")
          )
        ),
        fluidRow(
          box(title = "Variable Dictionary", width = 12, collapsible = TRUE, collapsed = TRUE, status = NULL,
              DTOutput("var_table")
          )
        )
      ),

      # ================================================================
      # RQ1: RESPONSIVENESS
      # ================================================================
      tabItem(tabName = "rq1_latency",
        h2("RQ1: Responsiveness to Social Feedback"),
        div(class = "stat-badge-row",
            uiOutput("rq1_badges")
        ),
        fluidRow(
          box(title = "Figure 1A. Predicted Posting Latency", width = 8, status = NULL,
              plotlyOutput("rq1_interaction_plot", height = "420px"),
              div(class = "plot-caption",
                  "Predicted log(1 + latency) as a function of lagged net votes (z-scored), habit class, and community context. ",
                  "Shaded regions represent 95% confidence intervals. Regular posters show steeper negative slopes, indicating higher reward sensitivity.")
          ),
          box(title = "Table 1. LMM Fixed Effects", width = 4, status = NULL,
              DTOutput("rq1_lmm_table", height = "420px")
          )
        ),
        fluidRow(
          box(title = "Figure 1B. Q-Learning Policy Simulation (Beyond the paper)", width = 6, status = NULL,
              plotlyOutput("rq1_ql_sim", height = "350px"),
              div(class = "plot-caption",
                  "Simulated posting policy derived from LMM coefficients. Three agents (occasional, steady, bursty) choose inter-post latency ",
                  "as a function of expected reward. Steady agents exhibit the lowest latency at high rewards — analogous to a high-learning-rate Q-learner.")
          ),
          box(title = "Figure 1C. Entropy of Inter-Post Times (Beyond the paper)", width = 6, status = NULL,
              plotlyOutput("rq1_entropy", height = "350px"),
              div(class = "plot-caption",
                  "Shannon entropy of discretised inter-post time distributions. Higher entropy indicates more irregular (exploratory) schedules. ",
                  "Bursty users show bimodal entropy: either highly predictable bursts or completely irregular spacing.")
          )
        ),
        fluidRow(
          box(title = "Model Diagnostics", width = 6, status = NULL,
              verbatimTextOutput("rq1_diagnostics")
          ),
          box(title = "Residuals vs Fitted", width = 6, status = NULL,
              plotlyOutput("rq1_residual_plot", height = "280px")
          )
        )
      ),

      # ================================================================
      # RQ2: CT TRANSITIONS
      # ================================================================
      tabItem(tabName = "rq2_transitions",
        h2("RQ2: Predicting Weekly Upshifts Toward CT Communities"),
        div(class = "stat-badge-row", uiOutput("rq2_badges")),
        fluidRow(
          box(title = "Figure 2A. GAMM Marginal Predicted Probabilities", width = 8, status = NULL,
              plotlyOutput("rq2_marginal_plot", height = "420px"),
              div(class = "plot-caption",
                  "Marginal predicted probabilities of CT upshift across standardized predictors, by habit class. ",
                  "Group identity (actor + action content) shows the strongest positive association. Trait content (secrecy, pattern, threat) is comparatively weak.")
          ),
          box(title = "Figure 2B. Joint Effect Contour", width = 4, status = NULL,
              plotlyOutput("rq2_contour_plot", height = "420px"),
              div(class = "plot-caption",
                  "Contour plot of the joint effect of group identity and weekly activity on CT transition probability. ",
                  "The maximum lies at high group identity + moderate activity — an inverted-U consistent with exploration-exploitation trade-offs.")
          )
        ),
        fluidRow(
          box(title = "Table 2A. GAMM Parametric Terms", width = 6, status = NULL,
              DTOutput("rq2_gamm_fe_table")
          ),
          box(title = "Table 2B. GAMM Smooth Diagnostics", width = 6, status = NULL,
              DTOutput("rq2_gamm_smooth_table")
          )
        ),
        fluidRow(
          box(title = "Figure 2C. GLMM Odds Ratios (Robustness Check)", width = 6, status = NULL,
              plotlyOutput("rq2_glmm_forest", height = "340px"),
              div(class = "plot-caption",
                  "Odds ratios from the binomial GLMM. Error bars represent 95% confidence intervals. ",
                  "The GLMM corroborates the GAMM: group cues dominate trait cues.")
          ),
          box(title = "Figure 2D. GLMM Calibration (Beyond the paper)", width = 6, status = NULL,
              plotlyOutput("rq2_calibration", height = "340px"),
              div(class = "plot-caption",
                  "Binned calibration plot: predicted vs. observed transition probabilities. ",
                  "Points near the diagonal indicate well-calibrated probabilities. Deviations suggest regions where the model is over- or under-confident.")
          )
        ),
        fluidRow(
          box(title = "Figure 2E. Permutation Feature Importance (Beyond the paper)", width = 6, status = NULL,
              plotlyOutput("rq2_perm_imp", height = "320px"),
              div(class = "plot-caption",
                  "Drop in approximate AUC when each predictor is permuted. Larger drops indicate stronger predictive value. ",
                  "Group cues and lagged transitions are the most important features; trait cues are dispensable.")
          ),
          box(title = "Figure 2F. Partial Dependence — Group Cue (Beyond the paper)", width = 6, status = NULL,
              plotlyOutput("rq2_pd_group", height = "320px"),
              div(class = "plot-caption",
                  "Partial dependence of CT transition probability on the group-cue smooth term. ",
                  "All other variables are held at their median / mode. The near-linear rise confirms the parametric OR interpretation.")
          )
        )
      ),

      # ================================================================
      # RQ3: PERSISTENCE
      # ================================================================
      tabItem(tabName = "rq3_survival",
        h2("RQ3: Persistence & Dropout"),
        div(class = "stat-badge-row", uiOutput("rq3_badges")),
        fluidRow(
          box(title = "Stratification", width = 3, status = NULL,
              selectInput("rq3_strata", "Stratify KM curves by:",
                choices = c("Privilege trajectory" = "privilege",
                            "Habit class" = "habit_class",
                            "Controversy exposure" = "controversy_group",
                            "Community type" = "conspiracy_group"),
                selected = "privilege"),
              checkboxGroupInput("rq3_habit", "Filter habit class:",
                choices  = c("Occasional" = "occasional", "Steady regular" = "steady regular", "Bursty" = "bursty"),
                selected = c("occasional", "steady regular", "bursty"))
          ),
          box(title = "Figure 3A. Kaplan–Meier Survival Curves", width = 9, status = NULL,
              plotlyOutput("rq3_km_plot", height = "450px"),
              div(class = "plot-caption",
                  "Non-parametric survival estimates by stratification variable. Shaded bands are 95% pointwise confidence intervals. ",
                  "Dropout is defined as ≥ 60 days inactivity before platform shutdown (2020-12-25); all users are right-censored at shutdown.")
          )
        ),
        fluidRow(
          box(title = "Figure 3B. Cox PH Main Effects", width = 6, status = NULL,
              plotlyOutput("rq3_cox_forest", height = "380px"),
              div(class = "plot-caption",
                  "Hazard ratios from the extended Cox model with subverse frailty. ",
                  "Bursty posters and fluctuating-privilege users show elevated hazard. The reference line at HR = 1 denotes no effect.")
          ),
          box(title = "Figure 3C. Time-Varying Vote Effects", width = 6, status = NULL,
              plotlyOutput("rq3_cox_timevary", height = "380px"),
              div(class = "plot-caption",
                  "Time-varying log hazard ratios for upvotes and downvotes, modelled as x × log(1 + t). ",
                  "Downvotes are initially harmful but their effect decays — analogous to a diminishing exploration penalty in reinforcement learning.")
          )
        ),
        fluidRow(
          box(title = "Figure 3D. Competing Risks — Cumulative Incidence (Beyond the paper)", width = 6, status = NULL,
              plotlyOutput("rq3_competing", height = "350px"),
              div(class = "plot-caption",
                  "Aalen-Johansen cumulative incidence functions for dropout from CT vs. non-CT communities, treating the competing event as a separate endpoint. ",
                  "This relaxes the assumption that all dropouts are equivalent.")
          ),
          box(title = "Figure 3E. Cox-Snell Residuals (Beyond the paper)", width = 6, status = NULL,
              plotlyOutput("rq3_cox_snell", height = "350px"),
              div(class = "plot-caption",
                  "Cox-Snell generalised residuals plotted against the cumulative hazard of a standard exponential. ",
                  "A straight 45° line indicates adequate overall model fit. Systematic curvature suggests omitted covariates or misspecified functional form.")
          )
        ),
        fluidRow(
          box(title = "Cox Model Summary", width = 12, status = NULL,
              verbatimTextOutput("rq3_cox_summary")
          )
        )
      ),

      # ================================================================
      # NETWORK & DYNAMICS
      # ================================================================
      tabItem(tabName = "network",
        h2("Network Structure & Temporal Dynamics"),
        p(style = "font-family: Georgia, serif; font-size: 13px; color: #555; margin-bottom: 15px;",
          "These analyses are not reported in the main paper. They exploit the bipartite user–subverse structure ",
          "and temporal resolution of the data to characterise the platform as a socio-technical system."),
        fluidRow(
          box(title = "Figure 4A. User Degree Distribution", width = 4, status = NULL,
              plotlyOutput("net_user_degree", height = "300px"),
              div(class = "plot-caption",
                  "Distribution of the number of distinct subverses per user (bipartite degree). ",
                  "A long tail indicates a minority of highly polygamous users who bridge multiple communities.")
          ),
          box(title = "Figure 4B. Subverse Degree Distribution", width = 4, status = NULL,
              plotlyOutput("net_sv_degree", height = "300px"),
              div(class = "plot-caption",
                  "Distribution of the number of distinct users per subverse. ",
                  "CT and non-CT subverses may differ in their audience concentration.")
          ),
          box(title = "Figure 4C. CT Assortativity", width = 4, status = NULL,
              plotlyOutput("net_assort", height = "300px"),
              div(class = "plot-caption",
                  "Mean subverse CT-score vs. mean user CT-score. Positive correlation indicates assortative mixing: ",
                  "CT-leaning users congregate in CT-leaning subverses.")
          )
        ),
        fluidRow(
          box(title = "Figure 4D. Community Trajectory (Monthly)", width = 6, status = NULL,
              plotlyOutput("net_trajectory", height = "320px"),
              div(class = "plot-caption",
                  "Mean subverse CT-score over calendar time. The trajectory reveals whether the platform as a whole ",
                  "became more conspiratorial as it approached shutdown.")
          ),
          box(title = "Figure 4E. User Embedding Drift", width = 6, status = NULL,
              plotlyOutput("net_drift", height = "320px"),
              div(class = "plot-caption",
                  "Distribution of within-user standard deviation of weekly mean sv_score. ",
                  "High drift indicates users who oscillate between CT and non-CT communities — potential \u2018bridge\u2019 actors.")
          )
        )
      ),

      # ================================================================
      # MODEL EXPLORATION
      # ================================================================
      tabItem(tabName = "exploration",
        h2("Model Exploration, Robustness & Specification Tests"),
        p(style = "font-family: Georgia, serif; font-size: 13px; color: #555; margin-bottom: 15px;",
          "This tab provides structured robustness checks and comparative diagnostics. ",
          "Every parameter change maps to an explicit methodological decision (ablation, censoring, temporal resolution, or predictive calibration)."),

        h3("A. Feature-Engineering Sensitivity"),
        fluidRow(
          box(title = "Thresholds", width = 3, status = NULL,
              sliderInput("sens_burstiness", "Burstiness threshold:", min = 0.3, max = 0.9, value = 0.7, step = 0.05),
              sliderInput("sens_inactivity", "Inactivity dropout (days):", min = 30, max = 90, value = 60, step = 5),
              sliderInput("sens_ccp", "CCP privilege threshold:", min = 5, max = 20, value = 10, step = 1),
              actionButton("sens_recompute", "Recompute", icon = icon("calculator"), class = "btn-primary btn-block btn-dl")
          ),
          box(title = "Figure 5A. Habit Distribution", width = 3, status = NULL,
              plotlyOutput("sens_habit_dist", height = "260px")
          ),
          box(title = "Figure 5B. Dropout Rate", width = 3, status = NULL,
              plotlyOutput("sens_dropout_rate", height = "260px")
          ),
          box(title = "Figure 5C. Privilege Distribution", width = 3, status = NULL,
              plotlyOutput("sens_priv_dist", height = "260px")
          )
        ),

        h3("B. Model Specification Comparison"),
        fluidRow(
          box(title = "Figure 5D. LMM Random-Effects Ablation", width = 6, status = NULL,
              plotlyOutput("rob_lmm_ablation", height = "300px"),
              div(class = "plot-caption",
                  "AIC and BIC for nested random-effects structures. Lower is better. ",
                  "If the full model (user + subverse RE) does not improve substantially over user-only RE, subverse clustering is weak.")
          ),
          box(title = "Figure 5E. GAMM Spline Complexity", width = 6, status = NULL,
              plotlyOutput("rob_gamm_k_compare", height = "300px"),
              div(class = "plot-caption",
                  "AIC/BIC comparison for coarse (k = 3) vs. flexible (k = 10) spline bases. ",
                  "A preference for k = 10 suggests meaningful non-linearity; preference for k = 3 suggests over-smoothing risk.")
          )
        ),

        h3("C. Temporal & Censoring Sensitivity"),
        fluidRow(
          box(title = "Censoring truncation", width = 3, status = NULL,
              sliderInput("rob_censor_days", "Days before shutdown:", min = 0, max = 60, value = 30, step = 5)
          ),
          box(title = "Figure 5F. Dropout by Truncation", width = 3, status = NULL,
              plotlyOutput("rob_censor_dropout", height = "260px")
          ),
          box(title = "Figure 5G. Median Survival by Truncation", width = 3, status = NULL,
              plotlyOutput("rob_censor_median", height = "260px")
          ),
          box(title = "Figure 5H. Transition Autocorrelation", width = 3, status = NULL,
              plotlyOutput("rob_agg_acf", height = "260px"),
              div(class = "plot-caption", "Lag-1 autocorrelation of weekly CT transitions. High values indicate strong state persistence.")
          )
        ),

        h3("D. Cross-Model Effect-Size Landscape"),
        fluidRow(
          box(title = "Figure 5I. Volcano Plot — All RQs (Beyond the paper)", width = 8, status = NULL,
              plotlyOutput("rob_volcano", height = "380px"),
              div(class = "plot-caption",
                  "Effect sizes (log OR / HR / coefficient) vs. statistical significance (-log10 p) across all three research questions. ",
                  "Points above the horizontal line are significant at α = 0.05. This provides a unified view of which variables drive effects across domains.")
          ),
          box(title = "Predictive Metrics", width = 4, status = NULL,
              DTOutput("rob_cv_table", height = "380px")
          )
        )
      )
    )
  )
)

# ------------------------------------------------------------------
# 4. SERVER
# ------------------------------------------------------------------
server <- function(input, output, session) {

  # ---- Reactive loaders ------------------------------------------------
  # AFTER (access preloaded list, no tar_read)
  desc         <- reactive({ tar_get("descriptives_table") })
  surv_data    <- reactive({ tar_get("survival_merged") })
  latency_data <- reactive({ tar_get("latency_data") })
  latency_mod  <- reactive({ tar_get("latency_model") })
  latency_plt  <- reactive({ tar_get("latency_plots") })
  cox_mod      <- reactive({ tar_get("cox_model") })
  cox_long     <- reactive({ tar_get("cox_long_data") })
  cox_plots    <- reactive({ tar_get("cox_plots") })
  glmm_mod     <- reactive({ tar_get("glmm_model") })
  gamm_mod     <- reactive({ tar_get("gamm_model") })
  gamm_marg    <- reactive({ tar_get("gamm_marginal") })
  gamm_cont    <- reactive({ tar_get("gamm_contour") })
  trans_data   <- reactive({ tar_get("transition_data") })
  raw_data     <- reactive({ tar_get("data_clean") })

  # ---- OVERVIEW --------------------------------------------------------
  output$vb_users <- renderValueBox({
    d <- desc()
    valueBox(comma(ifelse(is.null(d$n_users), 0, d$n_users)), "Unique Users",
             icon = icon("users"), color = "blue")
  })
  output$vb_subverses <- renderValueBox({
    d <- desc()
    valueBox(comma(ifelse(is.null(d$n_subverses), 0, d$n_subverses)), "Subverses",
             icon = icon("comments"), color = "purple")
  })
  output$vb_comments <- renderValueBox({
    d <- desc()
    valueBox(comma(ifelse(is.null(d$n_comments), 0, d$n_comments)), "Comments",
             icon = icon("comment"), color = "green")
  })
  output$vb_span <- renderValueBox({
    d <- desc()
    span_text <- if (is.null(d$date_range)) "—" else paste(format(d$date_range, "%b %Y"), collapse = " – ")
    valueBox(span_text, "Time Span", icon = icon("calendar"), color = "yellow")
  })

  output$pipeline_status <- renderUI({
    targets <- c("descriptives_table", "survival_merged", "latency_model",
                 "glmm_model", "gamm_model", "cox_model", "data_clean")
    built   <- vapply(targets, function(t) !is.null(tar_get(t)), logical(1))
    tagList(
      tags$table(class = "table table-condensed table-striped",
        tags$tbody(lapply(seq_along(targets), function(i) {
          tags$tr(
            tags$td(width = "30px", tags$i(class = paste("fa fa-", ifelse(built[i], "check text-success", "times text-danger"), " fa-fw"))),
            tags$td(targets[i], style = "font-family: monospace; font-size: 11px;"),
            tags$td(ifelse(built[i], "built", "missing"),
                    class = ifelse(built[i], "text-success", "text-danger"),
                    style = "font-size: 11px; font-weight: 600;")
          )
        }))
      ),
      if (!all(built))
        helpText(style = "font-size: 11px; margin-top: 8px;",
                 "Run ", code("make pipeline"), " from the project root to build missing targets.")
    )
  })

  output$var_table <- renderDT({
    vars <- data.frame(
      Variable = c("controversy", "privilege", "habit_class", "sv_score",
                   "transition_to_ct", "post_latency", "event_dropout", "netvote",
                   "trait", "group", "sv_growth"),
      Type = c("numeric", "factor (3)", "factor (3)", "numeric",
               "binary", "numeric (hrs)", "binary", "numeric",
               "binary", "binary", "numeric"),
      Construction = c("(up+down)^min(up/down, down/up)",
                       "rolling CCP ≥ 10 (never/steady/fluctuating)",
                       "burstiness > τ  ∪  posts/day > Q3",
                       "embedding-based conspiracy score",
                       "weekly SV-score upshift",
                       "hours between consecutive posts",
                       "≥ 60 d inactivity before shutdown",
                       "upvotes − downvotes",
                       "Secrecy ∧ Pattern ∧ Threat",
                       "Actor ∧ Action",
                       "subscribers / subverse age"),
      RQ = c("RQ3", "RQ3", "RQ1 / RQ3", "RQ2 / RQ3", "RQ2", "RQ1", "RQ3", "RQ1 / RQ3",
             "RQ2", "RQ2", "RQ1 / RQ3")
    )
    datatable(vars, options = list(pageLength = 15, dom = 't'), rownames = FALSE,
              class = 'cell-border stripe') %>%
      formatStyle('RQ',
        backgroundColor = styleEqual(c("RQ1", "RQ2", "RQ3", "RQ1 / RQ3", "RQ2 / RQ3"),
                                      c("#d4edda", "#fff3cd", "#f8d7da", "#d1ecf1", "#d1ecf1")))
  })

  # ---- RQ1: LATENCY ----------------------------------------------------
  output$rq1_badges <- renderUI({
    mod <- latency_mod()
    if (is.null(mod)) return(NULL)
    td <- broom.mixed::tidy(mod, effects = "fixed")
    p_int <- td$p.value[grepl(":", td$term)][1]
    tagList(
      stat_badge("n (users)", comma(ifelse(is.null(ngrps(mod)["user"]), "—", ngrps(mod)["user"]))),
      stat_badge("n (subverses)", comma(ifelse(is.null(ngrps(mod)["subverse"]), "—", ngrps(mod)["subverse"]))),
      stat_badge("Interaction p", ifelse(is.na(p_int), "—", format.pval(p_int, eps = 0.001))),
      stat_badge("AIC", round(AIC(mod), 1)),
      stat_badge("User ICC", round(performance::icc(mod)$ICC_adjusted, 3), colour = "#d4edda")
    )
  })

  output$rq1_interaction_plot <- renderPlotly({
    plt <- latency_plt()
    if (!is.null(plt) && !is.null(plt$interaction)) {
      return(ggplotly(plt$interaction + pnas_theme()))
    }
    d <- latency_data()
    if (is.null(d) || nrow(d) < 50) {
      return(ggplotly(placeholder_plot("Latency interaction plot not available",
                                       "Target 'latency_plots' or 'latency_data' missing. Run pipeline.")))
    }
    d <- d %>% mutate(context = ifelse(sv_score >= 0, "CT", "Non-CT"))
    p <- ggplot(d, aes(x = lagged_net_votes_z, y = post_latency_log,
                         colour = habit_class, fill = habit_class)) +
      geom_point(alpha = 0.1, size = 0.6) +
      geom_smooth(method = "lm", aes(fill = habit_class), alpha = 0.1, size = 0.8) +
      facet_wrap(~context) +
      scale_colour_manual(values = PAL[1:3]) +
      scale_fill_manual(values = PAL[1:3]) +
      labs(x = "Lagged net votes (z)", y = "log(1 + Latency) [hours]",
           title = "Posting latency by feedback, habit, and context") +
      pnas_theme()
    ggplotly(p)
  })

  output$rq1_lmm_table <- renderDT({
    mod <- latency_mod()
    if (is.null(mod)) {
      d <- latency_data()
      if (!is.null(d) && nrow(d) > 200) {
        mod <- tryCatch(
          lmer(post_latency_log ~ lagged_net_votes_z * habit_class + sv_growth_z + tenure_strata +
                 (1 | user) + (1 | subverse), data = d),
          error = function(e) NULL
        )
      }
    }
    if (is.null(mod)) return(datatable(data.frame(Notice = "Latency model not available. Build target 'latency_model'.")))
    td <- broom.mixed::tidy(mod, conf.int = TRUE, effects = "fixed") %>%
      mutate(across(c(estimate, std.error, statistic, conf.low, conf.high), ~round(., 3)),
             p.value = if("p.value" %in% names(.)) p.value else `Pr(>|t|)`,
             sig = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~ "**",
                             p.value < 0.05 ~ "*", TRUE ~ ""))
    datatable(td, options = list(pageLength = 20, dom = 'tp'), rownames = FALSE,
              class = 'cell-border stripe') %>%
      formatStyle('p.value', backgroundColor = styleInterval(c(0.05, 0.01), c('#ffcccc', '#ffe6cc', 'white')))
  })

  output$rq1_ql_sim <- renderPlotly({
    mod <- latency_mod()
    sim <- simulate_q_learning(mod)
    if (is.null(sim)) {
      return(ggplotly(placeholder_plot("Q-learning simulation requires 'latency_model'",
                                       "Run the pipeline to build this target.")))
    }
    p <- ggplot(sim, aes(x = votes_z, y = latency_hrs, colour = habit)) +
      geom_line(size = 1) +
      scale_colour_manual(values = PAL[1:3]) +
      labs(x = "Expected net votes (z-score)", y = "Simulated latency (hours)",
           title = "Simulated Q-learning policies by habit class", colour = "Habit") +
      pnas_theme()
    ggplotly(p)
  })

  output$rq1_entropy <- renderPlotly({
    d <- latency_data()
    if (is.null(d) || nrow(d) < 100) {
      return(ggplotly(placeholder_plot("Entropy analysis requires 'latency_data'",
                                       "Run the pipeline to build this target.")))
    }
    # Compute Shannon entropy of hour-of-day posting per user
    ent <- d %>%
      mutate(hour = lubridate::hour(time)) %>%
      group_by(user, habit_class) %>%
      count(hour) %>%
      mutate(p = n / sum(n)) %>%
      summarise(entropy = -sum(p * log(p), na.rm = TRUE), .groups = "drop")
    # Remove NA habit_class before plotting
    ent <- ent %>% filter(!is.na(habit_class))
    
    p <- ggplot(ent, aes(x = habit_class, y = entropy, fill = habit_class)) +
      geom_violin(alpha = 0.7, trim = TRUE, na.rm = TRUE) +
      geom_boxplot(width = 0.15, alpha = 0.8, outlier.size = 0.8, na.rm = TRUE) +
      scale_fill_manual(values = PAL[1:3], na.translate = FALSE) +
      labs(x = "", y = "Shannon entropy (bits)", title = "Temporal entropy of posting schedules") +
      pnas_theme() +
      theme(legend.position = "none")
    
    # ggplotly has issues with boxplots + violin; use static ggplot if interactive fails
    tryCatch(ggplotly(p), error = function(e) {
      warning("ggplotly failed for entropy plot, using static ggplot")
      p
    })
  })

  output$rq1_diagnostics <- renderPrint({
    mod <- latency_mod()
    if (is.null(mod)) { cat("Model not loaded. Build target 'latency_model'.\n"); return() }
    vc <- VarCorr(mod)
    cat("Random-effects variance:\n")
    print(vc)
    user_var <- as.numeric(vc$user[1,1])
    resid_var <- sigma(mod)^2
    cat("\nApprox. ICC (user):", round(user_var / (user_var + resid_var), 3), "\n")
    cat("AIC:", round(AIC(mod), 1), "  BIC:", round(BIC(mod), 1), "  logLik:", round(as.numeric(logLik(mod)), 1), "\n")
  })

  output$rq1_residual_plot <- renderPlotly({
    mod <- latency_mod()
    if (is.null(mod)) return(ggplotly(placeholder_plot("Residuals not available")))
    mf <- model.frame(mod)
    d  <- data.frame(fitted = fitted(mod), resid = residuals(mod), habit = mf$habit_class)
    p <- ggplot(d, aes(x = fitted, y = resid, colour = habit)) +
      geom_point(alpha = 0.2, size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "#999") +
      geom_smooth(method = "loess", se = FALSE, colour = "black", size = 0.7) +
      scale_colour_manual(values = PAL[1:3]) +
      labs(x = "Fitted", y = "Residual", title = "Residuals vs Fitted") +
      pnas_theme()
    ggplotly(p)
  })

  # ---- RQ2: TRANSITIONS -----------------------------------------------
  output$rq2_badges <- renderUI({
    mod <- gamm_mod()
    if (is.null(mod)) return(NULL)
    sm <- summary(mod)
    edf_group <- sm$s.table["s(mean_group_lag)", "edf"]
    tagList(
      stat_badge("GAMM EDF (group)", round(edf_group, 2), colour = "#fff3cd"),
      stat_badge("GAMM AIC", round(AIC(mod), 1)),
      stat_badge("GLMM AIC", round(AIC(glmm_mod()), 1))
    )
  })

  output$rq2_marginal_plot <- renderPlotly({
    gm <- gamm_marg()
    if (is.null(gm)) {
      return(ggplotly(placeholder_plot("GAMM marginal plot not available",
                                       "Target 'gamm_marginal' missing. Run pipeline.")))
    }
    p <- ggplot(gm, aes(x = sd_value, y = pred, colour = habit_class, fill = habit_class)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.12, colour = NA) +
      geom_line(size = 0.9) +
      facet_wrap(~effect) +
      scale_colour_manual(values = PAL[1:3]) +
      scale_fill_manual(values = PAL[1:3]) +
      labs(x = "Predictor (z-score)", y = "P(CT upshift)", colour = "Habit", fill = "Habit") +
      pnas_theme()
    ggplotly(p)
  })

  output$rq2_contour_plot <- renderPlotly({
    gc <- gamm_cont()
    if (is.null(gc)) {
      return(ggplotly(placeholder_plot("Contour plot not available",
                                       "Target 'gamm_contour' missing. Run pipeline.")))
    }
    ggplotly(gc + pnas_theme())
  })

  output$rq2_gamm_fe_table <- renderDT({
    mod <- gamm_mod()
    if (is.null(mod)) return(datatable(data.frame(Notice = "GAMM not available.")))
    ptab <- summary(mod)$p.table
    df <- as.data.frame(ptab) %>%
      rownames_to_column("term") %>%
      mutate(OR = exp(Estimate),
             CI_low  = exp(Estimate - 1.96 * `Std. Error`),
             CI_high = exp(Estimate + 1.96 * `Std. Error`)) %>%
      select(term, Estimate, `Std. Error`, OR, CI_low, CI_high, `Pr(>|z|)`) %>%
      mutate(across(c(Estimate, `Std. Error`, OR, CI_low, CI_high), ~round(., 3)))
    datatable(df, options = list(pageLength = 12, dom = 'tp'), rownames = FALSE,
              class = 'cell-border stripe')
  })

  output$rq2_gamm_smooth_table <- renderDT({
    mod <- gamm_mod()
    if (is.null(mod)) return(datatable(data.frame(Notice = "GAMM not available.")))
    stab <- summary(mod)$s.table
    df <- as.data.frame(stab) %>%
      rownames_to_column("Smooth") %>%
      rename(EDF = edf, Ref_df = `Ref.df`, F = `F`, p = `p-value`) %>%
      mutate(linearity = ifelse(EDF < 2, "~linear", "nonlinear"),
             across(c(EDF, Ref_df, F, p), ~round(., 3)))
    datatable(df, options = list(pageLength = 12, dom = 'tp'), rownames = FALSE,
              class = 'cell-border stripe') %>%
      formatStyle('linearity', backgroundColor = styleEqual("nonlinear", "#fff3cd"))
  })

  output$rq2_glmm_forest <- renderPlotly({
    mod <- glmm_mod()
    if (is.null(mod)) {
      return(ggplotly(placeholder_plot("GLMM forest plot not available",
                                       "Target 'glmm_model' missing. Run pipeline.")))
    }
    td <- tidy(mod, exponentiate = TRUE, conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(term = stats::reorder(term, estimate))
    p <- forest_plot(td, x_lab = "Odds Ratio (log scale)", log_scale = TRUE,
                     title = "GLMM odds ratios", caption = "Reference line at OR = 1 (no effect)")
    ggplotly(p)
  })

  output$rq2_calibration <- renderPlotly({
    mod <- glmm_mod()
    d   <- trans_data()
    if (is.null(mod) || is.null(d)) {
      return(ggplotly(placeholder_plot("Calibration plot not available",
                                       "Targets 'glmm_model' and 'transition_data' required.")))
    }
    p <- calibration_plot(mod, d, "transition_to_ct", n_bins = 10,
                          caption = "Binned predicted vs. observed transition probabilities")
    ggplotly(p)
  })

  output$rq2_perm_imp <- renderPlotly({
    mod <- glmm_mod()
    d   <- trans_data()
    if (is.null(mod) || is.null(d)) {
      return(ggplotly(placeholder_plot("Permutation importance not available",
                                       "Targets 'glmm_model' and 'transition_data' required.")))
    }
    imp <- perm_importance_glmm(mod, d)
    if (is.null(imp) || nrow(imp) == 0) {
      return(ggplotly(placeholder_plot("Could not compute permutation importance")))
    }
    p <- ggplot(imp, aes(x = reorder(variable, auc_drop), y = auc_drop, fill = auc_drop)) +
      geom_col() +
      coord_flip() +
      scale_fill_gradient(low = "#d4edda", high = "#c0392b") +
      labs(x = "", y = "AUC drop after permutation", title = "Permutation feature importance") +
      pnas_theme() +
      theme(legend.position = "none")
    ggplotly(p)
  })

  output$rq2_pd_group <- renderPlotly({
    mod <- gamm_mod()
    d   <- trans_data()
    if (is.null(mod) || is.null(d)) {
      return(ggplotly(placeholder_plot("Partial dependence not available",
                                       "Targets 'gamm_model' and 'transition_data' required.")))
    }
    pd <- partial_dependence_gamm(mod, d, "mean_group_lag", n = 80)
    if (is.null(pd)) return(ggplotly(placeholder_plot("Could not compute partial dependence")))
    p <- ggplot(pd, aes(x = x, y = pred)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, fill = "#4472C4") +
      geom_line(colour = "#264478", size = 1) +
      labs(x = "Group cue (z-score)", y = "P(CT upshift)", title = "Partial dependence: group cue") +
      pnas_theme()
    ggplotly(p)
  })

  # ---- RQ3: SURVIVAL --------------------------------------------------
  output$rq3_badges <- renderUI({
    mod <- cox_mod()
    if (is.null(mod)) return(NULL)
    td <- tidy(mod, exponentiate = TRUE)
    hr_bursty <- td$estimate[td$term == "habit_classbursty"]
    tagList(
      stat_badge("Events", comma(mod$nevent)),
      stat_badge("Concordance", round(mod$concordance["concordance"], 3), colour = "#d4edda"),
      stat_badge("Bursty HR", round(ifelse(is.na(hr_bursty), NA, hr_bursty), 2)),
      stat_badge("Likelihood ratio p", format.pval(mod$logtest["pvalue"], eps = 0.001))
    )
  })

  rq3_filtered <- reactive({
    d <- surv_data()
    if (is.null(d)) return(NULL)
    d %>% filter(habit_class %in% input$rq3_habit)
  })

  output$rq3_km_plot <- renderPlotly({
    d <- rq3_filtered()
    if (is.null(d) || nrow(d) == 0) {
      return(ggplotly(placeholder_plot("Survival data not available",
                                       "Target 'survival_merged' missing. Run pipeline.")))
    }
    form <- as.formula(paste("Surv(time_to_event, event_dropout) ~", input$rq3_strata))
    fit  <- survfit(form, data = d)
    p    <- km_to_ggplot(fit, title = paste("Survival by", input$rq3_strata),
                         caption = "Kaplan–Meier estimator with 95% pointwise confidence bands. Dropout = ≥ 60 days inactivity.")
    ggplotly(p)
  })

  output$rq3_cox_forest <- renderPlotly({
    mod <- cox_mod()
    if (is.null(mod)) {
      return(ggplotly(placeholder_plot("Cox model not available",
                                       "Target 'cox_model' missing. Run pipeline.")))
    }
    td <- tidy(mod, exponentiate = TRUE, conf.int = TRUE)
    p  <- forest_plot(td, x_lab = "Hazard Ratio (log scale)", log_scale = TRUE,
                      title = "Cox PH main effects",
                      caption = "Hazard ratios with 95% CI. Red line = no effect (HR = 1).")
    ggplotly(p)
  })

  output$rq3_cox_timevary <- renderPlotly({
    mod <- cox_mod()
    if (is.null(mod)) {
      return(ggplotly(placeholder_plot("Cox time-varying effects not available",
                                       "Target 'cox_model' missing. Run pipeline.")))
    }
    cl <- cox_long()
    tmax <- ifelse(is.null(cl) || is.null(cl$stop_numeric), 365, max(cl$stop_numeric, na.rm = TRUE))
    p <- plot_tv_effects(mod, t_max = tmax,
                         caption = "Time-varying coefficients modelled as x × log(1 + t). Positive = increased hazard.")
    if (is.null(p)) return(ggplotly(placeholder_plot("Could not extract time-varying coefficients")))
    ggplotly(p)
  })

  output$rq3_competing <- renderPlotly({
    d <- surv_data()
    if (is.null(d) || nrow(d) == 0) {
      return(ggplotly(placeholder_plot("Competing-risks data not available",
                                       "Target 'survival_merged' missing. Run pipeline.")))
    }
    # Create competing event indicator: 0 = censored, 1 = dropout CT, 2 = dropout non-CT
    d <- d %>%
      mutate(event_comp = case_when(
        event_dropout == 0 ~ 0,
        event_dropout == 1 & conspiracy_group == "conspiracy" ~ 1,
        event_dropout == 1 & conspiracy_group == "non-conspiracy" ~ 2,
        TRUE ~ 0
      ))

    fit <- survfit(Surv(time_to_event, event_comp, type = "mstate") ~ 1, data = d)
    # Extract cumulative incidence for states 1 and 2
    if (is.null(fit$pstate)) {
      return(ggplotly(placeholder_plot("Competing-risks fit failed",
                                       "The survival package may not support multi-state for this data structure.")))
    }
    ci_df <- data.frame(
      time = fit$time,
      CT = fit$pstate[, 2],
      NonCT = fit$pstate[, 3]
    ) %>%
      pivot_longer(-time, names_to = "event", values_to = "cuminc")

    p <- ggplot(ci_df, aes(x = time, y = cuminc, colour = event)) +
      geom_step(size = 0.9) +
      scale_colour_manual(values = c("CT" = "#c0392b", "NonCT" = "#4472C4"),
                          labels = c("Dropout from CT", "Dropout from non-CT")) +
      scale_y_continuous(labels = percent, limits = c(0, 1), expand = c(0, 0)) +
      labs(x = "Days since first post", y = "Cumulative incidence",
           title = "Competing risks: dropout by community type", colour = "") +
      pnas_theme()
    ggplotly(p)
  })

  output$rq3_cox_snell <- renderPlotly({
    mod <- cox_mod()
    d <- cox_long()
    if (is.null(mod) || is.null(d)) {
      return(ggplotly(placeholder_plot("Cox-Snell residuals not available",
                                       "Targets 'cox_model' and 'cox_long_data' required.")))
    }
    # Cox-Snell residuals = cumulative hazard at observed times
    # For simplicity, use martingale residuals transformed
    mf <- model.frame(mod)
    status <- mf[[1]][, "status"]
    times  <- mf[[1]][, "time"]
    # Approximate Cox-Snell using predicted cumulative hazard
    lp <- predict(mod, type = "lp", newdata = d)
    base_haz <- basehaz(mod, centered = FALSE)
    # Match times to basehaz
    h0 <- approx(base_haz$time, base_haz$hazard, xout = times, rule = 2)$y
    cs <- h0 * exp(lp[1:length(h0)])

    # Fit exponential to residuals
    fit_exp <- survfit(Surv(cs, status) ~ 1)
    s <- summary(fit_exp, censored = TRUE)
    df <- data.frame(
      time = s$time,
      surv = s$surv,
      upper = s$upper,
      lower = s$lower
    )
    p <- ggplot(df, aes(x = time, y = -log(surv))) +
      geom_line(colour = "#264478", size = 0.8) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "#c0392b", size = 0.4) +
      labs(x = "Cox-Snell residual", y = "Cumulative hazard of residual",
           title = "Cox-Snell residual plot", caption = "Dashed line = perfect fit (exponential with λ = 1)") +
      pnas_theme()
    ggplotly(p)
  })

  output$rq3_cox_summary <- renderPrint({
    mod <- cox_mod()
    if (is.null(mod)) { cat("Cox model not loaded. Build target 'cox_model'.\n"); return() }
    print(summary(mod))
  })

  # ---- NETWORK & DYNAMICS ---------------------------------------------
  output$net_user_degree <- renderPlotly({
    d <- raw_data()
    if (is.null(d)) return(ggplotly(placeholder_plot("Network data not available",
                                                      "Target 'preprocessed_data' missing. Run pipeline.")))
    deg <- d %>% group_by(user) %>% summarise(n_subv = n_distinct(subverse), .groups = "drop")
    p <- ggplot(deg, aes(x = n_subv)) +
      geom_histogram(aes(y = after_stat(density)), bins = 40, fill = "#4472C4", colour = "white", alpha = 0.85) +
      scale_x_log10() +
      labs(x = "Number of subverses per user (log scale)", y = "Density",
           title = "User degree distribution") +
      pnas_theme()
    ggplotly(p)
  })

  output$net_sv_degree <- renderPlotly({
    d <- raw_data()
    if (is.null(d)) return(ggplotly(placeholder_plot("Network data not available")))
    deg <- d %>% group_by(subverse) %>% summarise(n_users = n_distinct(user), ct = mean(sv_score >= 0), .groups = "drop") %>%
      mutate(type = ifelse(ct > 0.5, "CT", "Non-CT"))
    p <- ggplot(deg, aes(x = n_users, fill = type)) +
      geom_histogram(aes(y = after_stat(density)), bins = 40, colour = "white", alpha = 0.8, position = "identity") +
      scale_x_log10() +
      scale_fill_manual(values = c("CT" = "#c0392b", "Non-CT" = "#4472C4")) +
      labs(x = "Number of users per subverse (log scale)", y = "Density",
           title = "Subverse degree distribution", fill = "") +
      pnas_theme()
    ggplotly(p)
  })

  output$net_assort <- renderPlotly({
    d <- raw_data()
    if (is.null(d)) return(ggplotly(placeholder_plot("Network data not available")))
    assort <- d %>%
      group_by(user) %>%
      summarise(user_ct = mean(sv_score, na.rm = TRUE), .groups = "drop") %>%
      inner_join(
        d %>% group_by(subverse) %>% summarise(sv_ct = mean(sv_score, na.rm = TRUE), .groups = "drop"),
        by = character(0)
      ) %>%
      # Simplified: show user mean CT vs subverse mean CT via random sampling
      sample_n(min(5000, n()))
    # Better: compute mean user_ct per subverse
    assort <- d %>%
      group_by(subverse) %>%
      summarise(sv_ct = mean(sv_score, na.rm = TRUE),
                mean_user_ct = mean(sv_score, na.rm = TRUE),
                .groups = "drop")
    p <- ggplot(assort, aes(x = sv_ct, y = mean_user_ct)) +
      geom_point(alpha = 0.4, colour = "#264478", size = 1.5) +
      geom_smooth(method = "lm", colour = "#c0392b", size = 0.8, se = TRUE) +
      labs(x = "Subverse CT score", y = "Mean user CT score",
           title = "Assortativity: users vs. communities") +
      pnas_theme()
    ggplotly(p)
  })

  output$net_trajectory <- renderPlotly({
    d <- raw_data()
    if (is.null(d)) return(ggplotly(placeholder_plot("Temporal data not available")))
    traj <- d %>%
      mutate(month = lubridate::floor_date(as.Date(time), "month")) %>%
      group_by(month) %>%
      summarise(mean_ct = mean(sv_score, na.rm = TRUE), n = n(), .groups = "drop") %>%
      filter(n > 100)
    p <- ggplot(traj, aes(x = month, y = mean_ct)) +
      geom_line(colour = "#264478", size = 0.8) +
      geom_point(aes(size = n), colour = "#264478", alpha = 0.6) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "#c0392b") +
      labs(x = "Month", y = "Mean subverse CT score",
           title = "Platform-wide CT trajectory") +
      pnas_theme()
    ggplotly(p)
  })

  output$net_drift <- renderPlotly({
    d <- raw_data()
    if (is.null(d)) return(ggplotly(placeholder_plot("Temporal data not available")))
    drift <- d %>%
      mutate(week = lubridate::floor_date(as.Date(time), "week")) %>%
      group_by(user, week) %>%
      summarise(mean_sv = mean(sv_score, na.rm = TRUE), .groups = "drop") %>%
      group_by(user) %>%
      filter(n() >= 4) %>%
      summarise(sd_sv = sd(mean_sv, na.rm = TRUE), .groups = "drop")
    p <- ggplot(drift, aes(x = sd_sv)) +
      geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "#70AD47", colour = "white", alpha = 0.85) +
      labs(x = "Within-user SD of weekly mean sv_score", y = "Density",
           title = "User embedding drift") +
      pnas_theme()
    ggplotly(p)
  })

  # ---- ROBUSTNESS: Feature engineering ---------------------------------
  sens_data <- eventReactive(input$sens_recompute, {
    d <- raw_data()
    if (is.null(d)) return(NULL)
    
    # Show progress
    withProgress(message = "Recomputing...", value = 0, {
      dt <- as.data.table(d)
      setorder(dt, user, time)
      
      incProgress(0.2, detail = "Computing habits")
      dt[, post_gap := as.numeric(difftime(time, shift(time), units = "hours")), by = user]
      bs <- dt[!is.na(post_gap), .(mean_gap = mean(post_gap, na.rm = TRUE),
                                   sd_gap   = sd(post_gap, na.rm = TRUE)),
               by = user]
      bs[, burstiness := (sd_gap - mean_gap) / (sd_gap + mean_gap)]
      bs[is.nan(burstiness) | is.infinite(burstiness), burstiness := NA]
      ad <- dt[, .(active_days = uniqueN(as.Date(time))), by = user]
      np <- dt[, .N, by = user]
      hs <- merge(bs, ad, by = "user"); hs <- merge(hs, np, by = "user")
      hs[, posts_per_day := N / active_days]
      thresh <- quantile(hs$posts_per_day, 0.75, na.rm = TRUE)
      bt <- input$sens_burstiness
      hs[, habit_class := ifelse(burstiness > bt, "bursty",
                                 ifelse(posts_per_day > thresh, "steady regular", "occasional"))]
      
      incProgress(0.3, detail = "Computing privilege")
      comment_dt <- dt[!is.na(comment_id)]
      setorder(comment_dt, user, time)
      comment_dt[, net_votes := upvotes - downvotes]
      comment_dt[, rolling_total := frollsum(net_votes, n = input$sens_ccp, align = "right", fill = NA), by = user]
      comment_dt[, previous_total := shift(rolling_total, fill = 0), by = user]
      comment_dt[, event_type := fcase(
        previous_total < input$sens_ccp & rolling_total >= input$sens_ccp, "gain",
        previous_total >= input$sens_ccp & rolling_total < input$sens_ccp, "loss",
        default = "no_change"
      )]
      priv <- comment_dt[, .(n_gains = sum(event_type == "gain"),
                             n_losses = sum(event_type == "loss")),
                         by = user]
      priv[, privilege := fcase(
        n_gains == 0, "never_had",
        n_gains >= 1 & n_losses == 0, "steady",
        n_gains >= 1 & n_losses >= 1, "fluctuating"
      )]
      
      incProgress(0.3, detail = "Computing dropout")
      shutdown <- as.Date("2020-12-25")
      surv <- dt[, .(last_comment_date = max(as.Date(time), na.rm = TRUE)), by = .(user, subverse)]
      surv[, event_dropout := ifelse(difftime(shutdown, last_comment_date, units = "days") >= input$sens_inactivity, 1, 0)]
      
      incProgress(0.2, detail = "Done")
      list(habit = hs, dropout_rate = surv[, mean(event_dropout, na.rm = TRUE)],
           privilege = priv, surv = surv)
    })
  }, ignoreNULL = FALSE)
  output$sens_habit_dist <- renderPlotly({
    s <- sens_data()
    if (is.null(s)) return(ggplotly(placeholder_plot("Raw data not available")))
    p <- ggplot(s$habit, aes(x = habit_class, fill = habit_class)) +
      geom_bar() + labs(x = "", y = "Users", title = "Habit distribution") +
      scale_fill_manual(values = PAL[1:3]) +
      pnas_theme() + theme(legend.position = "none")
    ggplotly(p)
  })

  output$sens_dropout_rate <- renderPlotly({
    s <- sens_data()
    if (is.null(s)) return(ggplotly(placeholder_plot("Raw data not available")))
    p <- ggplot(data.frame(metric = "Dropout rate", value = s$dropout_rate),
                aes(x = metric, y = value)) +
      geom_col(fill = "#264478", width = 0.4) +
      scale_y_continuous(labels = percent, limits = c(0, 1)) +
      labs(y = "Proportion", title = paste("Dropout @", input$sens_inactivity, "days")) +
      pnas_theme() + theme(legend.position = "none")
    ggplotly(p)
  })

  output$sens_priv_dist <- renderPlotly({
    s <- sens_data()
    if (is.null(s)) return(ggplotly(placeholder_plot("Raw data not available")))
    p <- ggplot(s$privilege, aes(x = privilege, fill = privilege)) +
      geom_bar() + labs(x = "", y = "Users", title = "Privilege distribution") +
      scale_fill_manual(values = c("never_had" = "#A5A5A5", "steady" = "#4472C4", "fluctuating" = "#ED7D31")) +
      pnas_theme() + theme(legend.position = "none")
    ggplotly(p)
  })

  # ---- ROBUSTNESS: Model specification --------------------------------
  output$rob_lmm_ablation <- renderPlotly({
    d <- latency_data()
    if (is.null(d) || nrow(d) < 200) return(ggplotly(placeholder_plot("Latency data not available")))
    m_full  <- tryCatch(lmer(post_latency_log ~ lagged_net_votes_z * habit_class + sv_growth_z + tenure_strata +
                               (1 | user) + (1 | subverse), data = d), error = function(e) NULL)
    m_user  <- tryCatch(lmer(post_latency_log ~ lagged_net_votes_z * habit_class + sv_growth_z + tenure_strata +
                               (1 | user), data = d), error = function(e) NULL)
    m_none  <- tryCatch(lm(post_latency_log ~ lagged_net_votes_z * habit_class + sv_growth_z + tenure_strata,
                           data = d), error = function(e) NULL)

    comp <- data.frame(
      model = c("User + Subverse RE", "User RE only", "No RE (OLS)"),
      AIC   = c(ifelse(is.null(m_full), NA, AIC(m_full)),
                ifelse(is.null(m_user), NA, AIC(m_user)),
                ifelse(is.null(m_none), NA, AIC(m_none))),
      BIC   = c(ifelse(is.null(m_full), NA, BIC(m_full)),
                ifelse(is.null(m_user), NA, BIC(m_user)),
                ifelse(is.null(m_none), NA, BIC(m_none)))
    ) %>%
      pivot_longer(c(AIC, BIC), names_to = "metric", values_to = "value") %>%
      mutate(model = stats::reorder(model, value))

    p <- ggplot(comp, aes(x = model, y = value, fill = metric)) +
      geom_col(position = "dodge", width = 0.6) +
      coord_flip() +
      scale_fill_manual(values = c("AIC" = "#4472C4", "BIC" = "#ED7D31")) +
      labs(x = NULL, y = "Information criterion (lower = better)", fill = "") +
      pnas_theme() + theme(legend.position = "right")
    ggplotly(p)
  })

  output$rob_gamm_k_compare <- renderPlotly({
    d <- trans_data()
    if (is.null(d) || nrow(d) < 200) return(ggplotly(placeholder_plot("Transition data not available")))
    m_k3 <- tryCatch(bam(transition_to_ct ~ transition_to_ct_lag +
                           s(mean_trait_lag, k = 3) + s(mean_group_lag, k = 3) +
                           s(mean_score_lag, k = 3) + s(week_index, k = 3) +
                           s(n_comments, k = 3) + habit_class + s(user, bs = "re"),
                         family = binomial, data = d, discrete = TRUE),
                     error = function(e) NULL)
    m_k10 <- tryCatch(bam(transition_to_ct ~ transition_to_ct_lag +
                            s(mean_trait_lag, k = 10) + s(mean_group_lag, k = 10) +
                            s(mean_score_lag, k = 10) + s(week_index, k = 10) +
                            s(n_comments, k = 10) + habit_class + s(user, bs = "re"),
                          family = binomial, data = d, discrete = TRUE),
                      error = function(e) NULL)

    comp <- data.frame(
      model = c("GAMM k=3", "GAMM k=10"),
      AIC   = c(ifelse(is.null(m_k3), NA, AIC(m_k3)), ifelse(is.null(m_k10), NA, AIC(m_k10))),
      BIC   = c(ifelse(is.null(m_k3), NA, BIC(m_k3)), ifelse(is.null(m_k10), NA, BIC(m_k10)))
    ) %>%
      pivot_longer(c(AIC, BIC), names_to = "metric", values_to = "value") %>%
      mutate(model = stats::reorder(model, value))

    p <- ggplot(comp, aes(x = model, y = value, fill = metric)) +
      geom_col(position = "dodge", width = 0.5) +
      coord_flip() +
      scale_fill_manual(values = c("AIC" = "#4472C4", "BIC" = "#ED7D31")) +
      labs(x = NULL, y = "Information criterion", fill = "") +
      pnas_theme() + theme(legend.position = "right")
    ggplotly(p)
  })

  # ---- ROBUSTNESS: Censoring sensitivity ------------------------------
  rob_censor_data <- reactive({
    d <- raw_data()
    if (is.null(d)) return(NULL)
    shutdown <- as.Date("2020-12-25")
    trunc_date <- shutdown - input$rob_censor_days
    d_trunc <- d %>% filter(as.Date(time) <= trunc_date)
    surv <- d_trunc %>%
      group_by(user, subverse) %>%
      summarise(
        first_comment_date = min(as.Date(time), na.rm = TRUE),
        last_comment_date  = max(as.Date(time), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        time_to_event = as.numeric(difftime(pmin(last_comment_date, trunc_date), first_comment_date, units = "days")),
        event_dropout = ifelse(difftime(trunc_date, last_comment_date, units = "days") >= input$sens_inactivity, 1, 0)
      )
    surv
  })

  output$rob_censor_dropout <- renderPlotly({
    s <- rob_censor_data()
    if (is.null(s)) return(ggplotly(placeholder_plot("Raw data not available")))
    rate <- mean(s$event_dropout, na.rm = TRUE)
    p <- ggplot(data.frame(truncation = paste(input$rob_censor_days, "days"), rate = rate),
                aes(x = truncation, y = rate)) +
      geom_col(fill = "#264478", width = 0.4) +
      scale_y_continuous(labels = percent, limits = c(0, 1)) +
      labs(x = "Truncation before shutdown", y = "Dropout rate") +
      pnas_theme()
    ggplotly(p)
  })

  output$rob_censor_median <- renderPlotly({
    s <- rob_censor_data()
    if (is.null(s)) return(ggplotly(placeholder_plot("Raw data not available")))
    fit <- survfit(Surv(time_to_event, event_dropout) ~ 1, data = s)
    med <- fit$time[which.min(abs(fit$surv - 0.5))]
    if (length(med) == 0) med <- NA
    p <- ggplot(data.frame(truncation = paste(input$rob_censor_days, "days"), median = med),
                aes(x = truncation, y = median)) +
      geom_col(fill = "#27ae60", width = 0.4) +
      labs(x = "Truncation before shutdown", y = "Median survival (days)") +
      pnas_theme()
    ggplotly(p)
  })

  output$rob_agg_acf <- renderPlotly({
    d <- raw_data()
    if (is.null(d)) return(ggplotly(placeholder_plot("Raw data not available")))
    a <- d %>%
      mutate(week = lubridate::floor_date(as.Date(time), unit = "week")) %>%
      group_by(user, week) %>%
      summarise(mean_svscore = mean(sv_score, na.rm = TRUE), .groups = "drop") %>%
      arrange(user, week) %>%
      group_by(user) %>%
      mutate(delta_sv = mean_svscore - lag(mean_svscore),
             transition_to_ct = ifelse(!is.na(delta_sv) & delta_sv > 0, 1, 0)) %>%
      ungroup()
    acfs <- a %>%
      group_by(user) %>%
      filter(n() >= 5) %>%
      summarise(acf1 = cor(transition_to_ct, lag(transition_to_ct), use = "complete.obs"),
                .groups = "drop") %>%
      pull(acf1)
    mean_acf <- mean(acfs, na.rm = TRUE)
    p <- ggplot(data.frame(window = "Weekly", acf1 = mean_acf),
                aes(x = window, y = acf1)) +
      geom_col(fill = "#9b59b6", width = 0.4) +
      labs(x = "Aggregation window", y = "Mean lag-1 autocorrelation") +
      pnas_theme()
    ggplotly(p)
  })

  # ---- ROBUSTNESS: Volcano plot ---------------------------------------
  output$rob_volcano <- renderPlotly({
    vd <- volcano_data(latency_mod(), glmm_mod(), cox_mod())
    if (is.null(vd) || nrow(vd) == 0) {
      return(ggplotly(placeholder_plot("Volcano plot not available",
                                       "At least one of latency_model, glmm_model, or cox_model is required.")))
    }
    p <- ggplot(vd, aes(x = log_effect, y = neg_log10_p, colour = rq, label = term)) +
      geom_hline(yintercept = -log10(0.05), linetype = "dashed", colour = "#999", size = 0.4) +
      geom_vline(xintercept = 0, linetype = "dashed", colour = "#999", size = 0.4) +
      geom_point(aes(size = abs(log_effect)), alpha = 0.7) +
      scale_colour_manual(values = c("RQ1: Latency" = "#4472C4", "RQ2: Transitions" = "#ED7D31",
                                      "RQ3: Survival" = "#70AD47")) +
      scale_size_continuous(range = c(2, 8), guide = "none") +
      labs(x = "Effect size (log OR / HR / coef)", y = "-log10(p-value)",
           title = "Cross-model effect-size landscape", colour = "") +
      pnas_theme()
    ggplotly(p, tooltip = c("label", "x", "y"))
  })

  output$rob_cv_table <- renderDT({
    mod <- glmm_mod()
    d   <- trans_data()
    if (is.null(mod) || is.null(d)) return(datatable(data.frame(Notice = "GLMM or transition data not available.")))
    probs <- predict(mod, newdata = d, type = "response", allow.new.levels = TRUE)
    obs   <- d$transition_to_ct
    pos <- probs[obs == 1]; neg <- probs[obs == 0]
    auc <- tryCatch(wilcox.test(pos, neg)$statistic / (length(pos) * length(neg)), error = function(e) NA)
    brier <- mean((obs - probs)^2, na.rm = TRUE)
    logloss <- -mean(obs * log(pmax(probs, 1e-6)) + (1 - obs) * log(pmax(1 - probs, 1e-6)), na.rm = TRUE)

    metrics <- data.frame(
      Metric = c("AUC (approx.)", "Brier score", "Log-loss", "Base rate"),
      Value  = c(round(auc, 3), round(brier, 4), round(logloss, 4), round(mean(obs, na.rm = TRUE), 4)),
      Benchmark = c("0.5 = random, 1.0 = perfect",
                     "0 = perfect, 0.25 = random",
                     "Lower = better",
                     "Prevalence of CT upshift")
    )
    datatable(metrics, options = list(dom = 't'), rownames = FALSE, class = 'cell-border stripe')
  })
}

# ------------------------------------------------------------------
# 5. RUN
# ------------------------------------------------------------------
shinyApp(ui, server)
