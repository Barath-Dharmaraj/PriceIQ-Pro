# ============================================================
# PriceIQ Pro — Enterprise Dynamic Pricing Platform
# Version 1.0.0  |  Professional Dark Theme
# ============================================================

library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)
library(DT)
library(randomForest)
library(forecast)
library(scales)
library(lubridate)

source("modules/data_management.R")
source("modules/eda_module.R")
source("modules/prediction_module.R")
source("modules/forecasting_module.R")
source("modules/optimization_module.R")
source("modules/insights_module.R")
source("modules/dashboard_module.R")

# ── UI ──────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "black",
  title = "PriceIQ Pro",

  dashboardHeader(
    title = tags$div(class = "logo-brand",
      tags$div(class = "logo-icon", "P"),
      tags$div(
        tags$div(class = "logo-text", "PriceIQ Pro"),
        tags$span(class = "logo-version", "ENTERPRISE v1.0")
      )
    ),
    titleWidth = 240
  ),

  dashboardSidebar(
    width = 240,
    tags$style(HTML("
      .sidebar-menu { margin-top: 0 !important; }
    ")),
    sidebarMenu(id = "sidebar_menu",
      tags$li(class = "header", "OVERVIEW"),
      menuItem("Dashboard",          tabName = "dashboard",    icon = icon("chart-line")),

      tags$li(class = "header", "DATA"),
      menuItem("Data Management",    tabName = "data",         icon = icon("database")),
      menuItem("Analytics",          tabName = "eda",          icon = icon("chart-bar")),

      tags$li(class = "header", "INTELLIGENCE"),
      menuItem("Demand Prediction",  tabName = "prediction",   icon = icon("robot")),
      menuItem("Forecasting",        tabName = "forecasting",  icon = icon("calendar-alt")),

      tags$li(class = "header", "OPTIMIZATION"),
      menuItem("Price Optimizer",    tabName = "optimization", icon = icon("sliders-h")),
      menuItem("What-If Simulator",  tabName = "whatif",       icon = icon("flask")),
      menuItem("AI Insights",        tabName = "insights",     icon = icon("lightbulb"))
    ),
    tags$div(class = "sidebar-footer-strip", "© 2024 PRICEIQ PRO")
  ),

  dashboardBody(
    tags$head(
      tags$title("PriceIQ Pro — Enterprise Pricing Platform"),
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800;900&family=IBM+Plex+Mono:wght@400;500;600&display=swap"
      ),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$style(HTML("
        /* Immediate paint — no FOUC */
        html,body,.wrapper,.content-wrapper { background:#080C14 !important; }
        .main-sidebar,.left-side { background:#0D1117 !important; }
        .main-header .navbar { background:#0D1117 !important; }
        .main-header .logo   { background:#0D1117 !important; }
        /* Fix content top overlap */
        .content-wrapper { padding-top: 28px !important; }
        /* Remove default box shadows/borders */
        .box { border-top: none !important; }
        /* Column gutters */
        .container-fluid { padding-left:0 !important; padding-right:0 !important; }
      "))
    ),

    tabItems(
      tabItem(tabName = "dashboard",   dashboardUI("exec_dash")),
      tabItem(tabName = "data",        dataManagementUI("data_mgmt")),
      tabItem(tabName = "eda",         edaUI("eda")),
      tabItem(tabName = "prediction",  predictionUI("ml_pred")),
      tabItem(tabName = "forecasting", forecastingUI("ts_fore")),
      tabItem(tabName = "optimization",optimizationUI("opt_engine")),
      tabItem(tabName = "whatif",      whatIfUI("whatif_sim")),
      tabItem(tabName = "insights",    insightsUI("ai_insights"))
    )
  )
)

# ── Server ──────────────────────────────────────────────────
server <- function(input, output, session) {
  app_data <- reactiveValues(
    df          = NULL,
    data_loaded = FALSE,
    rf_model    = NULL,
    arima_model = NULL
  )

  dashboardServer("exec_dash",    app_data)
  dataManagementServer("data_mgmt",   app_data)
  edaServer("eda",           app_data)
  predictionServer("ml_pred",      app_data)
  forecastingServer("ts_fore",      app_data)
  optimizationServer("opt_engine",   app_data)
  whatIfServer("whatif_sim",   app_data)
  insightsServer("ai_insights",  app_data)

  # Auto-load sample data on startup
  observe({
    isolate({
      tryCatch({
        if (file.exists("data/sample_dataset.csv")) {
          df <- read.csv("data/sample_dataset.csv", stringsAsFactors = FALSE)
          df$Date <- as.Date(df$Date)
          app_data$df          <- df
          app_data$data_loaded <- TRUE
        }
      }, error = function(e) message("Auto-load error: ", e$message))
    })
  })
}

shinyApp(ui = ui, server = server)
