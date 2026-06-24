# app_exploration.R — Model Exploration tab only
# Standalone: loads targets at startup, no other tabs

library(shiny)
library(shinydashboard)
library(targets)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)

# ------------------------------------------------------------------
# 1. LOAD TARGETS
# ------------------------------------------------------------------

store_path <- normalizePath("../_targets", mustWork = FALSE)

old_wd <- getwd()
setwd(dirname(store_path))
targets::tar_config_set(store = "_targets")

TARGETS <- list(
  latency_model   = targets::tar_read("latency_model"),
  glmm_model      = targets::tar_read("glmm_model"),
  cox_model       = targets::tar_read("cox_model"),
  latency_data    = targets::tar_read("latency_data"),
  transition_data = targets::tar_read("transition_data"),
  data_clean      = targets::tar_read("data_clean")
)
setwd(old_wd)

tar_get <- function(name) TARGETS[[name]]

# ------------------------------------------------------------------
# 2. THEME & HELPERS
# ------------------------------------------------------------------

PAL <- c("#4472C4", "#ED7D31", "#70AD47", "#A5A5A5", "#FFC000", "#5B9BD5", "#264478")

pnas_theme <- function() {
  theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      panel.border = element_rect(fill = NA, colour = "#cccccc", size = 0.4),
      plot.title = element_text(face = "bold", size = 14, hjust = 0),
      axis.title = element_text(size = 11),
      legend.position = "bottom"
    )
}

# Volcano data helper
# Replace the volcano_data function and rob_volcano output in your app with this:

volcano_data <- function(lmm, glmm, cox) {
  out <- list()
  
  # Helper to safely extract p-values
  safe_tidy <- function(mod, exponentiate = FALSE) {
    if (is.null(mod)) return(NULL)
    td <- tryCatch(
      if (exponentiate) broom.mixed::tidy(mod, exponentiate = TRUE, conf.int = TRUE)
      else broom.mixed::tidy(mod, effects = "fixed", conf.int = TRUE),
      error = function(e) NULL
    )
    if (is.null(td)) return(NULL)
    
    # Find p-value column
    p_names <- c("p.value", "Pr(>|t|)", "Pr(>|z|)")
    p_col <- intersect(names(td), p_names)
    if (length(p_col) == 0) return(NULL)
    
    # Rename to standard
    names(td)[names(td) == p_col[1]] <- "p.value"
    
    # Remove intercept and NA p-values
    td %>%
      filter(term != "(Intercept)", !is.na(p.value), p.value > 0, p.value <= 1)
  }
  
  if (!is.null(lmm)) {
    td <- safe_tidy(lmm)
    if (!is.null(td) && nrow(td) > 0) {
      out$rq1 <- td %>%
        mutate(
          rq = "RQ1: Latency",
          log_effect = estimate,
          neg_log10_p = pmin(-log10(p.value), 10)  # Cap at 10 to avoid Inf
        )
    }
  }
  
  if (!is.null(glmm)) {
    td <- safe_tidy(glmm, exponentiate = TRUE)
    if (!is.null(td) && nrow(td) > 0) {
      out$rq2 <- td %>%
        mutate(
          rq = "RQ2: Transitions",
          log_effect = log(pmax(estimate, 0.01)),  # Avoid log(0)
          neg_log10_p = pmin(-log10(p.value), 10)
        )
    }
  }
  
  if (!is.null(cox)) {
    td <- tryCatch(broom::tidy(cox, exponentiate = TRUE, conf.int = TRUE), error = function(e) NULL)
    if (!is.null(td) && "p.value" %in% names(td)) {
      out$rq3 <- td %>%
        filter(term != "(Intercept)", !is.na(p.value), p.value > 0, p.value <= 1) %>%
        mutate(
          rq = "RQ3: Survival",
          log_effect = log(pmax(estimate, 0.01)),
          neg_log10_p = pmin(-log10(p.value), 10)
        )
    }
  }
  
  if (length(out) == 0) return(NULL)
  
  bind_rows(out) %>%
    mutate(significant = neg_log10_p > -log10(0.05))
}

# ------------------------------------------------------------------
# 3. UI
# ------------------------------------------------------------------

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Model Exploration"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Exploration", tabName = "exploration", icon = icon("flask"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "exploration",
              h2("Model Exploration, Robustness & Specification Tests"),
              p(style = "font-family: Georgia, serif; font-size: 13px; color: #555; margin-bottom: 15px;",
                "Cross-model comparison and feature-engineering sensitivity."),
              
              h3("A. Feature-Engineering Sensitivity"),
              fluidRow(
                box(title = "Thresholds", width = 3,
                    sliderInput("sens_burstiness", "Burstiness threshold:", min = 0.3, max = 0.9, value = 0.7, step = 0.05),
                    sliderInput("sens_inactivity", "Inactivity dropout (days):", min = 30, max = 90, value = 60, step = 5),
                    sliderInput("sens_ccp", "CCP privilege threshold:", min = 5, max = 20, value = 10, step = 1),
                    actionButton("sens_recompute", "Recompute", icon = icon("calculator"), class = "btn-primary btn-block")
                ),
                box(title = "Habit Distribution", width = 3,
                    plotOutput("sens_habit_dist", height = "260px")
                ),
                box(title = "Dropout Rate", width = 3,
                    plotOutput("sens_dropout_rate", height = "260px")
                ),
                box(title = "Privilege Distribution", width = 3,
                    plotOutput("sens_priv_dist", height = "260px")
                )
              ),
              
              h3("D. Cross-Model Effect-Size Landscape"),
              fluidRow(
                box(title = "Volcano Plot — All RQs", width = 8,
                    plotlyOutput("rob_volcano", height = "380px")
                ),
                box(title = "Predictive Metrics", width = 4,
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
  
  # ---- Reactive sensitivity computation ----
  sens_data <- eventReactive(input$sens_recompute, {
    d <- tar_get("data_clean")
    if (is.null(d)) return(NULL)
    
    dt <- data.table::as.data.table(d)
    data.table::setorder(dt, user, time)
    dt[, post_gap := as.numeric(difftime(time, data.table::shift(time), units = "hours")), by = user]
    
    bs <- dt[!is.na(post_gap), .(mean_gap = mean(post_gap, na.rm = TRUE),
                                 sd_gap = sd(post_gap, na.rm = TRUE)), by = user]
    bs[, burstiness := (sd_gap - mean_gap) / (sd_gap + mean_gap)]
    bs[is.nan(burstiness) | is.infinite(burstiness), burstiness := NA]
    
    ad <- dt[, .(active_days = data.table::uniqueN(as.Date(time))), by = user]
    np <- dt[, .N, by = user]
    hs <- merge(bs, ad, by = "user")
    hs <- merge(hs, np, by = "user")
    hs[, posts_per_day := N / active_days]
    
    thresh <- quantile(hs$posts_per_day, 0.75, na.rm = TRUE)
    bt <- input$sens_burstiness
    hs[, habit_class := ifelse(burstiness > bt, "bursty",
                               ifelse(posts_per_day > thresh, "steady regular", "occasional"))]
    
    # Privilege
    comment_dt <- dt[!is.na(comment_id)]
    data.table::setorder(comment_dt, user, time)
    comment_dt[, net_votes := upvotes - downvotes]
    comment_dt[, rolling_total := data.table::frollsum(net_votes, n = input$sens_ccp, align = "right", fill = NA), by = user]
    comment_dt[, previous_total := data.table::shift(rolling_total, fill = 0), by = user]
    comment_dt[, event_type := data.table::fcase(
      previous_total < input$sens_ccp & rolling_total >= input$sens_ccp, "gain",
      previous_total >= input$sens_ccp & rolling_total < input$sens_ccp, "loss",
      default = "no_change"
    )]
    priv <- comment_dt[, .(n_gains = sum(event_type == "gain"),
                           n_losses = sum(event_type == "loss")), by = user]
    priv[, privilege := data.table::fcase(
      n_gains == 0, "never_had",
      n_gains >= 1 & n_losses == 0, "steady",
      n_gains >= 1 & n_losses >= 1, "fluctuating"
    )]
    
    # Dropout
    shutdown <- as.Date("2020-12-25")
    surv <- dt[, .(last_comment_date = max(as.Date(time), na.rm = TRUE)), by = .(user, subverse)]
    surv[, event_dropout := ifelse(difftime(shutdown, last_comment_date, units = "days") >= input$sens_inactivity, 1, 0)]
    
    list(habit = hs, dropout_rate = surv[, mean(event_dropout, na.rm = TRUE)],
         privilege = priv)
  }, ignoreNULL = FALSE)
  
  # ---- Habit distribution ----
  output$sens_habit_dist <- renderPlot({
    s <- sens_data()
    if (is.null(s)) {
      return(ggplot(data.frame(x = 1, y = 1), aes(x, y)) + geom_text(label = "No data") + theme_void())
    }
    ggplot(s$habit, aes(x = habit_class, fill = habit_class)) +
      geom_bar() +
      scale_fill_manual(values = PAL[1:3]) +
      labs(x = "", y = "Users", title = "Habit distribution") +
      pnas_theme() +
      theme(legend.position = "none")
  })
  
  # ---- Dropout rate ----
  output$sens_dropout_rate <- renderPlot({
    s <- sens_data()
    if (is.null(s)) {
      return(ggplot(data.frame(x = 1, y = 1), aes(x, y)) + geom_text(label = "No data") + theme_void())
    }
    ggplot(data.frame(metric = "Dropout rate", value = s$dropout_rate),
           aes(x = metric, y = value)) +
      geom_col(fill = "#264478", width = 0.4) +
      scale_y_continuous(labels = percent, limits = c(0, 1)) +
      labs(y = "Proportion", title = paste("Dropout @", input$sens_inactivity, "days")) +
      pnas_theme() +
      theme(legend.position = "none")
  })
  
  # ---- Privilege distribution ----
  output$sens_priv_dist <- renderPlot({
    s <- sens_data()
    if (is.null(s)) {
      return(ggplot(data.frame(x = 1, y = 1), aes(x, y)) + geom_text(label = "No data") + theme_void())
    }
    ggplot(s$privilege, aes(x = privilege, fill = privilege)) +
      geom_bar() +
      scale_fill_manual(values = c("never_had" = "#A5A5A5", "steady" = "#4472C4", "fluctuating" = "#ED7D31")) +
      labs(x = "", y = "Users", title = "Privilege distribution") +
      pnas_theme() +
      theme(legend.position = "none")
  })
  
  # ---- Volcano plot ----
  output$rob_volcano <- renderPlotly({
    vd <- volcano_data(tar_get("latency_model"), tar_get("glmm_model"), tar_get("cox_model"))
    
    if (is.null(vd) || nrow(vd) == 0) {
      return(ggplotly(
        ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
          geom_text(label = "Volcano plot not available\n(models may be missing or have no p-values)", size = 5) +
          theme_void()
      ))
    }
    
    # Ensure no NA/Inf for plotly
    vd <- vd %>% filter(is.finite(log_effect), is.finite(neg_log10_p))
    if (nrow(vd) == 0) {
      return(ggplotly(
        ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
          geom_text(label = "No finite values to plot", size = 5) +
          theme_void()
      ))
    }
    
    p <- ggplot(vd, aes(x = log_effect, y = neg_log10_p, colour = rq)) +
      geom_hline(yintercept = -log10(0.05), linetype = "dashed", colour = "#999", linewidth = 0.4) +
      geom_vline(xintercept = 0, linetype = "dashed", colour = "#999", linewidth = 0.4) +
      geom_point(aes(size = abs(log_effect), text = term), alpha = 0.7) +
      scale_colour_manual(values = c("RQ1: Latency" = "#4472C4", "RQ2: Transitions" = "#ED7D31",
                                     "RQ3: Survival" = "#70AD47")) +
      scale_size_continuous(range = c(2, 8), guide = "none") +
      labs(x = "Effect size (log OR / HR / coef)", y = "-log10(p-value)",
           title = "Cross-model effect-size landscape", colour = "") +
      pnas_theme()
    
    ggplotly(p, tooltip = c("text", "x", "y"))
  })
  
  # ---- Predictive metrics table ----
  output$rob_cv_table <- renderDT({
    mod <- tar_get("glmm_model")
    d   <- tar_get("transition_data")
    if (is.null(mod) || is.null(d)) {
      return(datatable(data.frame(Notice = "GLMM or transition data not available.")))
    }
    
    probs <- predict(mod, newdata = d, type = "response", allow.new.levels = TRUE)
    obs   <- d$transition_to_ct
    
    pos <- probs[obs == 1]
    neg <- probs[obs == 0]
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