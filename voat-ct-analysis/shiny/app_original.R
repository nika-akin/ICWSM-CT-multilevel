# shiny/app.R — Interactive dashboard for Voat CT Analysis
# Reads directly from the targets store; auto-updates when pipeline re-runs.

library(shiny)
library(shinydashboard)
library(targets)
library(withr)
library(dplyr)
library(ggplot2)

# Source modules
source("../R/shiny_modules.R")
source("../R/plots.R")

# Set targets store relative to app directory
store_path <- normalizePath("../_targets", mustWork = FALSE)

ui <- dashboardPage(
  dashboardHeader(title = "Voat CT Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Survival", tabName = "survival", icon = icon("heartbeat")),
      menuItem("Posting Latency", tabName = "latency", icon = icon("clock")),
      menuItem("CT Transitions", tabName = "transitions", icon = icon("exchange-alt")),
      menuItem("Data Explorer", tabName = "explorer", icon = icon("table"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(".content-wrapper { background-color: #f4f6f9; }"))),
    tabItems(
      tabItem(tabName = "overview",
              h2("Project Overview"),
              fluidRow(
                overview_ui("overview")
              )
      ),
      tabItem(tabName = "survival",
              h2("Survival Analysis (RQ1)"),
              survival_ui("survival")
      ),
      tabItem(tabName = "latency",
              h2("Posting Latency (RQ2)"),
              latency_ui("latency")
      ),
      tabItem(tabName = "transitions",
              h2("CT Transitions (RQ3)"),
              transition_ui("transitions")
      ),
      tabItem(tabName = "explorer",
              h2("Data Explorer"),
              explorer_ui("explorer")
      )
    )
  )
)

server <- function(input, output, session) {
  # Check store exists
  if (!file.exists(store_path)) {
    showNotification(
      "Targets store not found. Please run `make pipeline` from the project root first.",
      type = "error", duration = NULL
    )
  }

  overview_server("overview", store_path)
  survival_server("survival", store_path)
  latency_server("latency", store_path)
  transition_server("transitions", store_path)
  explorer_server("explorer", store_path)
}

shinyApp(ui, server)
