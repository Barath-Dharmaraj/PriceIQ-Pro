# ── Data Management Module ───────────────────────────────────

dataManagementUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "page-header",
      div(class = "page-title", tags$span("Data", class = "page-accent"), " Management"),
      p("Upload, validate and explore your pricing dataset", class = "page-sub")
    ),

    fluidRow(
      column(3,
        div(class = "card",
          div(class = "card-title", "📂 Data Source"),
          fileInput(ns("csv_file"), "Upload CSV File", accept = ".csv",
            buttonLabel = "Choose File", placeholder = "No file selected"),
          div(style="text-align:center;color:var(--t4);font-size:11px;padding:4px 0;font-family:var(--mono);", "— or —"),
          actionButton(ns("load_sample"), "Load Sample Dataset",
            class="btn btn-info", style="width:100%;margin-bottom:8px;"),
          actionButton(ns("validate_data"), "Validate Data",
            class="btn btn-success", style="width:100%;margin-bottom:8px;"),
          actionButton(ns("clean_data"), "Auto-Clean Data",
            class="btn btn-warning", style="width:100%;"),
          div(class="section-divider"),
          uiOutput(ns("data_status"))
        )
      ),
      column(9,
        uiOutput(ns("stats_cards")),
        div(style="margin-top:16px;", class="card",
          div(class="card-title", "📋 Dataset Preview"),
          DT::dataTableOutput(ns("data_preview"))
        )
      )
    ),

    fluidRow(style="margin-top:16px;",
      column(6,
        div(class="card",
          div(class="card-title", "🔬 Data Quality"),
          uiOutput(ns("quality_report"))
        )
      ),
      column(6,
        div(class="card",
          div(class="card-title", "📊 Column Statistics"),
          uiOutput(ns("col_stats"))
        )
      )
    )
  )
}

dataManagementServer <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$load_sample, {
      tryCatch({
        df <- read.csv("data/sample_dataset.csv", stringsAsFactors = FALSE)
        df$Date <- as.Date(df$Date)
        app_data$df <- df; app_data$data_loaded <- TRUE
        showNotification("✅ Sample dataset loaded — 500 records", type="message", duration=4)
      }, error=function(e) showNotification(paste("❌", e$message), type="error"))
    })

    observeEvent(input$csv_file, {
      req(input$csv_file)
      tryCatch({
        df <- read.csv(input$csv_file$datapath, stringsAsFactors=FALSE)
        df$Date <- suppressWarnings(as.Date(df$Date))
        app_data$df <- df; app_data$data_loaded <- TRUE
        showNotification(paste0("✅ Uploaded: ", nrow(df), " rows"), type="message", duration=4)
      }, error=function(e) showNotification(paste("❌", e$message), type="error"))
    })

    observeEvent(input$validate_data, {
      req(app_data$data_loaded)
      req_cols <- c("Product_ID","Product_Name","Category","Cost_Price","Selling_Price",
                    "Demand","Revenue","Date","Inventory","Competitor_Price","Season")
      missing  <- setdiff(req_cols, names(app_data$df))
      nas      <- sum(is.na(app_data$df))
      if (length(missing)==0 && nas < nrow(app_data$df)*0.3)
        showNotification("✅ Validation passed", type="message", duration=4)
      else
        showNotification(paste("⚠️ Issues found — missing cols:", length(missing), "| NAs:", nas), type="warning")
    })

    observeEvent(input$clean_data, {
      req(app_data$data_loaded)
      df <- app_data$df
      n_before <- nrow(df)
      df <- df[rowSums(is.na(df)) < ncol(df)*0.5, ]
      for (col in names(df)[sapply(df, is.numeric)])
        df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm=TRUE)
      if ("Selling_Price" %in% names(df)) df <- df[!is.na(df$Selling_Price) & df$Selling_Price > 0, ]
      app_data$df <- df
      showNotification(paste0("🧹 Cleaned — removed ", n_before-nrow(df), " rows"), type="message")
    })

    output$data_status <- renderUI({
      if (!app_data$data_loaded)
        return(div(style="text-align:center;padding:16px;color:var(--t4);font-size:12px;", "No data loaded"))
      df <- app_data$df
      div(
        div(class="status-badge live", style="margin-bottom:10px;", "Live"),
        div(style="display:flex;gap:8px;",
          div(class="metric-mini", div(class="m-label","Rows"), div(class="m-value text-blue", format(nrow(df),big.mark=","))),
          div(class="metric-mini", div(class="m-label","Cols"), div(class="m-value", ncol(df)))
        )
      )
    })

    output$stats_cards <- renderUI({
      req(app_data$data_loaded, app_data$df); df <- app_data$df
      fluidRow(
        column(3, div(class="kpi-card blue",
          div(class="kpi-label","Total Revenue"),
          div(class="kpi-value", paste0("₹",format(round(sum(df$Revenue,na.rm=T)/1e6,1),nsmall=1),"M")),
          div(class="kpi-badge neutral","All Records"), tags$span("💰",class="kpi-icon")
        )),
        column(3, div(class="kpi-card green",
          div(class="kpi-label","Avg Selling Price"),
          div(class="kpi-value", paste0("₹",format(round(mean(df$Selling_Price,na.rm=T)),big.mark=","))),
          div(class="kpi-badge neutral","Mean"), tags$span("🏷",class="kpi-icon")
        )),
        column(3, div(class="kpi-card purple",
          div(class="kpi-label","Unique Products"),
          div(class="kpi-value", length(unique(df$Product_ID))),
          div(class="kpi-badge neutral","SKUs"), tags$span("📦",class="kpi-icon")
        )),
        column(3, div(class="kpi-card amber",
          div(class="kpi-label","Avg Demand"),
          div(class="kpi-value", round(mean(df$Demand,na.rm=T),1)),
          div(class="kpi-badge neutral","Per Record"), tags$span("📈",class="kpi-icon")
        ))
      )
    })

    output$data_preview <- DT::renderDataTable({
      req(app_data$data_loaded, app_data$df); df <- app_data$df
      if("Revenue" %in% names(df)) df$Revenue <- paste0("₹",format(round(df$Revenue),big.mark=","))
      if("Selling_Price" %in% names(df)) df$Selling_Price <- paste0("₹",format(df$Selling_Price,big.mark=","))
      if("Cost_Price" %in% names(df)) df$Cost_Price <- paste0("₹",format(df$Cost_Price,big.mark=","))
      if("Competitor_Price" %in% names(df)) df$Competitor_Price <- paste0("₹",format(df$Competitor_Price,big.mark=","))
      DT::datatable(df, options=list(pageLength=8,scrollX=TRUE,dom='frtip'), style='bootstrap', class='compact', rownames=FALSE)
    })

    output$quality_report <- renderUI({
      if (!app_data$data_loaded) return(p("Load data first.", style="color:var(--t3);font-size:12px;"))
      df <- app_data$df
      req_cols <- c("Product_ID","Product_Name","Category","Cost_Price","Selling_Price","Demand","Revenue","Date","Inventory","Competitor_Price","Season")
      checks <- list(
        list("Required Columns", if(length(setdiff(req_cols,names(df)))==0) "✅ All present" else paste("❌ Missing:", paste(setdiff(req_cols,names(df)),collapse=", ")), length(setdiff(req_cols,names(df)))==0),
        list("Missing Values",   if(sum(is.na(df))==0) "✅ None" else paste("⚠", sum(is.na(df)), "missing"), sum(is.na(df))==0),
        list("Duplicate Rows",   if(sum(duplicated(df))==0) "✅ None" else paste("⚠", sum(duplicated(df)), "found"), sum(duplicated(df))==0),
        list("Dataset Size",     paste0("✅ ", nrow(df), " × ", ncol(df)), TRUE)
      )
      tagList(lapply(checks, function(c) {
        div(style="display:flex;justify-content:space-between;align-items:center;padding:9px 0;border-bottom:1px solid var(--border);",
          span(c[[1]], style="font-size:12px;color:var(--t2);"),
          span(c[[2]], style=paste0("font-size:11px;font-family:var(--mono);color:",if(c[[3]])"var(--green)"else"var(--amber)",";"))
        )
      }))
    })

    output$col_stats <- renderUI({
      req(app_data$data_loaded, app_data$df); df <- app_data$df
      num_cols <- intersect(c("Cost_Price","Selling_Price","Demand","Revenue","Inventory","Competitor_Price"), names(df))
      tagList(lapply(num_cols, function(col) {
        v <- df[[col]][!is.na(df[[col]])]
        if (length(v)==0) return(NULL)
        pct <- min(100, round(mean(v)/max(v)*100))
        div(style="padding:9px 0;border-bottom:1px solid var(--border);",
          div(style="display:flex;justify-content:space-between;margin-bottom:4px;",
            span(col, style="font-size:12px;font-weight:600;color:var(--t1);"),
            span(paste0("Max: ",format(round(max(v)),big.mark=",")), style="font-size:10px;color:var(--t3);font-family:var(--mono);")
          ),
          div(style="display:flex;gap:14px;margin-bottom:4px;",
            span(paste0("Mean: ",format(round(mean(v)),big.mark=",")), style="font-size:10px;color:var(--blue-lt);font-family:var(--mono);"),
            span(paste0("Median: ",format(round(median(v)),big.mark=",")), style="font-size:10px;color:var(--t3);font-family:var(--mono);")
          ),
          div(class="feature-progress", div(class="feature-progress-fill", style=paste0("width:",pct,"%;")) )
        )
      }))
    })
  })
}
