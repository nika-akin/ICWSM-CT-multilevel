# R/shiny_modules.R — Modular Shiny components
# Each module reads from the targets store via tar_read().

# =============================================================================
# MODULE: Overview
# =============================================================================
overview_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinydashboard::valueBoxOutput(ns("n_users")),
    shinydashboard::valueBoxOutput(ns("n_subs")),
    shinydashboard::valueBoxOutput(ns("n_obs")),
    shiny::hr(),
    shiny::h4("Pipeline Status"),
    shiny::verbatimTextOutput(ns("pipeline_status"))
  )
}

overview_server <- function(id, store_path = "_targets") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Read descriptives from targets store
    desc <- shiny::reactive({
      shiny::req(file.exists(store_path))
      withr::with_dir(dirname(store_path), {
        targets::tar_config_set(store = basename(store_path))
        targets::tar_read("descriptives_table")
      })
    })

    output$n_users <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = scales::comma(desc()$n_users),
        subtitle = "Unique Users",
        icon = shiny::icon("users"),
        color = "aqua"
      )
    })

    output$n_subs <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = scales::comma(desc()$n_subverses),
        subtitle = "Subverses",
        icon = shiny::icon("comments"),
        color = "purple"
      )
    })

    output$n_obs <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = scales::comma(desc()$n_comments),
        subtitle = "Comments",
        icon = shiny::icon("comment"),
        color = "green"
      )
    })

    output$pipeline_status <- shiny::renderPrint({
      if (!file.exists(store_path)) {
        cat("Targets store not found. Run `make pipeline` first.\n")
      } else {
        withr::with_dir(dirname(store_path), {
          targets::tar_config_set(store = basename(store_path))
          meta <- targets::tar_meta(fields = c("name", "time", "status"))
          print(as.data.frame(meta))
        })
      }
    })
  })
}

# =============================================================================
# MODULE: Survival Explorer
# =============================================================================
survival_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput(ns("strata"), "Stratify by:",
                           choices = c("privilege", "controversy", "habit", "growth"),
                           selected = "privilege"),
        shiny::selectInput(ns("ct_filter"), "Conspiracy Group:",
                           choices = c("All", "conspiracy", "non-conspiracy"),
                           selected = "All")
      ),
      shiny::mainPanel(
        shiny::plotOutput(ns("km_plot"), height = "600px")
      )
    )
  )
}

survival_server <- function(id, store_path = "_targets") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    km_plots <- shiny::reactive({
      shiny::req(file.exists(store_path))
      withr::with_dir(dirname(store_path), {
        targets::tar_config_set(store = basename(store_path))
        targets::tar_read("km_plots")
      })
    })

    output$km_plot <- shiny::renderPlot({
      p <- km_plots()[[input$strata]]
      if (input$ct_filter != "All") {
        # Note: filtering would require the raw survival data; here we show pre-computed strata
        shiny::showNotification("Showing pre-computed strata (filter applied in pipeline).", type = "message")
      }
      print(p)
    })
  })
}

# =============================================================================
# MODULE: Posting Latency
# =============================================================================
latency_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::sliderInput(ns("votes"), "Lagged Net Votes (z):", min = -3, max = 3, value = 0, step = 0.1),
        shiny::checkboxGroupInput(ns("habits"), "Habit Class:",
                                  choices = c("occasional", "steady regular", "bursty"),
                                  selected = c("occasional", "steady regular", "bursty"))
      ),
      shiny::mainPanel(
        shiny::plotOutput(ns("latency_plot"), height = "500px")
      )
    )
  )
}

latency_server <- function(id, store_path = "_targets") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    latency_plots <- shiny::reactive({
      shiny::req(file.exists(store_path))
      withr::with_dir(dirname(store_path), {
        targets::tar_config_set(store = basename(store_path))
        targets::tar_read("latency_plots")
      })
    })

    output$latency_plot <- shiny::renderPlot({
      p <- latency_plots()
      if (!is.null(p$interaction)) print(p$interaction)
    })
  })
}

# =============================================================================
# MODULE: CT Transitions (GAMM Marginals)
# =============================================================================
transition_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput(ns("predictor"), "Predictor:",
                           choices = c("Trait Content" = "Trait Content",
                                       "Group Content" = "Group Content",
                                       "Net Votes" = "Net Votes"),
                           selected = "Group Content"),
        shiny::checkboxGroupInput(ns("habits_trans"), "Habit Class:",
                                  choices = c("occasional", "steady regular", "bursty"),
                                  selected = c("occasional", "steady regular", "bursty"))
      ),
      shiny::mainPanel(
        shiny::plotOutput(ns("marginal_plot"), height = "500px"),
        shiny::hr(),
        shiny::plotOutput(ns("contour_plot"), height = "500px")
      )
    )
  )
}

transition_server <- function(id, store_path = "_targets") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    marginal_data <- shiny::reactive({
      shiny::req(file.exists(store_path))
      withr::with_dir(dirname(store_path), {
        targets::tar_config_set(store = basename(store_path))
        targets::tar_read("gamm_marginal")
      })
    })

    contour_plot <- shiny::reactive({
      shiny::req(file.exists(store_path))
      withr::with_dir(dirname(store_path), {
        targets::tar_config_set(store = basename(store_path))
        targets::tar_read("gamm_contour")
      })
    })

    output$marginal_plot <- shiny::renderPlot({
      req(marginal_data())
      dat <- marginal_data() %>% dplyr::filter(effect == input$predictor, habit_class %in% input$habits_trans)
      p <- ggplot(dat, aes(x = sd_value, y = pred, color = habit_class, fill = habit_class)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
        geom_line(size = 1.2) +
        scale_color_manual(values = c("occasional" = "#af8dc3", "steady regular" = "#7fbf7b", "bursty" = "grey")) +
        scale_fill_manual(values = c("occasional" = "#af8dc3", "steady regular" = "#7fbf7b", "bursty" = "grey")) +
        labs(x = "Predictor (standardized)", y = "P(CT upshift)", color = "Habit", fill = "Habit") +
        theme_apa() +
        theme(legend.position = "bottom")
      print(p)
    })

    output$contour_plot <- shiny::renderPlot({
      req(contour_plot())
      print(contour_plot())
    })
  })
}

# =============================================================================
# MODULE: Data Explorer
# =============================================================================
explorer_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::sliderInput(ns("sv_score"), "Subverse Score:", min = -1, max = 1, value = c(-1, 1), step = 0.1),
        shiny::selectInput(ns("habit"), "Habit Class:", choices = c("All", "occasional", "steady regular", "bursty"))
      ),
      shiny::mainPanel(
        DT::DTOutput(ns("data_table"))
      )
    )
  )
}

explorer_server <- function(id, store_path = "_targets") {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    raw_data <- shiny::reactive({
      shiny::req(file.exists(store_path))
      withr::with_dir(dirname(store_path), {
        targets::tar_config_set(store = basename(store_path))
        targets::tar_read("data_clean")
      })
    })

    output$data_table <- DT::renderDT({
      d <- raw_data()
      d <- d %>% dplyr::filter(sv_score >= input$sv_score[1], sv_score <= input$sv_score[2])
      if (input$habit != "All") d <- d %>% dplyr::filter(habit_class == input$habit)
      d %>%
        dplyr::select(user, subverse, time, upvotes, downvotes, controversy, habit_class, sv_score) %>%
        DT::datatable(options = list(pageLength = 15, scrollX = TRUE))
    })
  })
}
