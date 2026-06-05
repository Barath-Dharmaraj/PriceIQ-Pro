# ── Forecasting Module ───────────────────────────────────────

forecastingUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class="page-header",
      div(class="page-title", tags$span("Time Series", class="page-accent"), " Forecasting"),
      p("ARIMA-based demand and revenue forecasting with confidence intervals", class="page-sub")
    ),
    fluidRow(
      column(3,
        div(class="card",
          div(class="card-title","📅 Forecast Settings"),
          selectInput(ns("forecast_metric"),   "Metric",      choices=c("Revenue","Demand")),
          selectInput(ns("forecast_category"), "Category",    choices=c("All","Electronics","Fashion","Grocery","Home Appliances","Beauty","Sports","Books","Accessories")),
          selectInput(ns("forecast_horizon"),  "Horizon",     choices=c("7 Days"=7,"30 Days"=30,"90 Days"=90), selected=30),
          selectInput(ns("agg_freq"),          "Aggregation", choices=c("Daily"="day","Weekly"="week","Monthly"="month"), selected="week"),
          div(class="section-divider"),
          checkboxInput(ns("show_ci"),    "Show Confidence Intervals", value=TRUE),
          checkboxInput(ns("show_trend"), "Show Trend Line",           value=TRUE),
          div(class="section-divider"),
          actionButton(ns("run_forecast"), "Run Forecast", class="btn btn-primary", style="width:100%;")
        )
      ),
      column(9,
        uiOutput(ns("forecast_kpis")),
        div(class="card", style="margin-top:14px;",
          div(class="card-title", textOutput(ns("fc_subtitle"), inline=TRUE)),
          plotly::plotlyOutput(ns("forecast_plot"), height="320px")
        ),
        fluidRow(style="margin-top:14px;",
          column(6, div(class="card",
            div(class="card-title","Trend"),
            plotly::plotlyOutput(ns("trend_plot"), height="220px")
          )),
          column(6, div(class="card",
            div(class="card-title","Forecast Summary"),
            uiOutput(ns("forecast_summary"))
          ))
        )
      )
    )
  )
}

forecastingServer <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    fc_data  <- reactiveVal(NULL)
    ts_data  <- reactiveVal(NULL)

    output$fc_subtitle <- renderText({
      paste0("ARIMA forecast — next ", input$forecast_horizon, " days of ", tolower(input$forecast_metric))
    })

    pb <- function(p) p %>% plotly::layout(
      paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
      font=list(family="Inter",color="#475569",size=11),
      xaxis=list(gridcolor="rgba(255,255,255,0.04)"),
      yaxis=list(gridcolor="rgba(255,255,255,0.04)"),
      legend=list(font=list(color="#475569"),bgcolor="rgba(0,0,0,0)",orientation="h",y=-0.18,x=0.5,xanchor="center"),
      margin=list(l=44,r=10,t=10,b=54)
    ) %>% plotly::config(displayModeBar=FALSE, responsive=TRUE)

    observeEvent(input$run_forecast, {
      req(app_data$data_loaded, app_data$df)
      withProgress(message="Running ARIMA...", value=0, {
        tryCatch({
          df <- app_data$df; df$Date <- as.Date(df$Date)
          if (input$forecast_category != "All") df <- df[df$Category==input$forecast_category,]
          if (nrow(df)<14) { showNotification("❌ Not enough data",type="error"); return() }
          m <- input$forecast_metric; fr <- input$agg_freq
          incProgress(0.2, detail="Aggregating...")
          if (fr=="day")   { agg <- aggregate(as.formula(paste(m,"~Date")),df,sum); agg <- agg[order(agg$Date),] }
          else if (fr=="week") { df$W <- as.Date(cut(df$Date,"week")); agg <- aggregate(as.formula(paste(m,"~W")),df,sum); names(agg)[1]<-"Date"; agg<-agg[order(agg$Date),] }
          else { df$Mo <- as.Date(format(df$Date,"%Y-%m-01")); agg <- aggregate(as.formula(paste(m,"~Mo")),df,sum); names(agg)[1]<-"Date"; agg<-agg[order(agg$Date),] }
          ts_data(agg)
          incProgress(0.5, detail="Fitting ARIMA...")
          tsobj <- ts(agg[[m]], frequency=if(fr=="day")7 else if(fr=="week")4 else 12)
          arima_model <- tryCatch(forecast::auto.arima(tsobj,seasonal=TRUE,stepwise=TRUE,approximation=TRUE),
                                  error=function(e) forecast::Arima(tsobj,order=c(1,1,1)))
          h <- as.integer(input$forecast_horizon)
          if (fr=="week")  h <- max(2,round(h/7))
          if (fr=="month") h <- max(2,round(h/30))
          incProgress(0.7, detail="Forecasting...")
          fc <- forecast::forecast(arima_model, h=h, level=c(80,95))
          last_d   <- max(agg$Date); iv <- if(fr=="day")1 else if(fr=="week")7 else 30
          fut_dates <- seq(last_d+iv, by=iv, length.out=h)
          fdf <- data.frame(Date=fut_dates, Forecast=as.numeric(fc$mean),
            Lo80=as.numeric(fc$lower[,1]), Hi80=as.numeric(fc$upper[,1]),
            Lo95=as.numeric(fc$lower[,2]), Hi95=as.numeric(fc$upper[,2]))
          fdf[fdf<0] <- 0
          fc_data(fdf)
          saveRDS(arima_model,"models/arima_model.rds")
          showNotification("✅ Forecast complete",type="message",duration=3)
          incProgress(1)
        }, error=function(e) showNotification(paste("❌",e$message),type="error"))
      })
    })

    output$forecast_kpis <- renderUI({
      fr <- fc_data(); ts <- ts_data()
      if (is.null(fr)||is.null(ts))
        return(div(class="notif-strip",tags$span("🔮",class="icon"),"Select settings and click Run Forecast to generate ARIMA predictions."))
      m <- input$forecast_metric; is_rev <- m=="Revenue"
      fmt <- function(x) if(is_rev) paste0("₹",format(round(x/1e3,1),nsmall=1),"K") else format(round(x),big.mark=",")
      hist_avg <- mean(tail(ts[[m]],4),na.rm=TRUE); fc_avg <- mean(fr$Forecast,na.rm=TRUE)
      growth   <- round((fc_avg/hist_avg-1)*100,1)
      fluidRow(
        column(3, div(class="kpi-card blue",  div(class="kpi-label","Total Forecast"),div(class="kpi-value",fmt(sum(fr$Forecast,na.rm=TRUE))),div(class="kpi-badge neutral",paste(nrow(fr),"periods")),tags$span("🔮",class="kpi-icon"))),
        column(3, div(class="kpi-card green", div(class="kpi-label","Growth vs Recent"),div(class="kpi-value",paste0(if(growth>=0)"+"else"",growth,"%")),div(class=if(growth>=0)"kpi-badge up"else"kpi-badge down",if(growth>=0)"▲ Positive"else"▼ Decline"),tags$span("📈",class="kpi-icon"))),
        column(3, div(class="kpi-card purple",div(class="kpi-label","Peak Forecast"),  div(class="kpi-value",fmt(max(fr$Forecast,na.rm=TRUE))),div(class="kpi-badge up","Best Period"),tags$span("🏆",class="kpi-icon"))),
        column(3, div(class="kpi-card amber", div(class="kpi-label","Avg Per Period"), div(class="kpi-value",fmt(mean(fr$Forecast,na.rm=TRUE))),div(class="kpi-badge neutral","Mean"),tags$span("📊",class="kpi-icon")))
      )
    })

    output$forecast_plot <- plotly::renderPlotly({
      ts <- ts_data(); fr <- fc_data(); req(ts)
      m <- input$forecast_metric
      p <- plotly::plot_ly() %>%
        plotly::add_lines(x=ts$Date,y=ts[[m]],name="Historical",
          line=list(color="#2563EB",width=2),
          hovertemplate="<b>Historical</b><br>%{x}<br>%{y:,.0f}<extra></extra>")
      if (!is.null(fr)) {
        if (input$show_ci) {
          p <- p %>%
            plotly::add_trace(x=c(fr$Date,rev(fr$Date)),y=c(fr$Hi95,rev(fr$Lo95)),type="scatter",mode="none",fill="toself",fillcolor="rgba(37,99,235,0.06)",name="95% CI",hoverinfo="skip") %>%
            plotly::add_trace(x=c(fr$Date,rev(fr$Date)),y=c(fr$Hi80,rev(fr$Lo80)),type="scatter",mode="none",fill="toself",fillcolor="rgba(37,99,235,0.12)",name="80% CI",hoverinfo="skip")
        }
        p <- p %>% plotly::add_lines(x=fr$Date,y=fr$Forecast,name="Forecast",
          line=list(color="#10B981",width=2,dash="dot"),
          marker=list(color="#10B981",size=4),
          hovertemplate="<b>Forecast</b><br>%{x}<br>%{y:,.0f}<extra></extra>")
      }
      if (input$show_trend && nrow(ts)>3) {
        tdf <- data.frame(x=as.numeric(ts$Date),y=ts[[m]])
        tv  <- predict(lm(y~x,tdf),tdf)
        p <- p %>% plotly::add_lines(x=ts$Date,y=tv,name="Trend",line=list(color="rgba(245,158,11,0.5)",width=1.5,dash="dash"),hoverinfo="skip")
      }
      pb(p %>% plotly::layout(xaxis=list(title=""),yaxis=list(title=m)))
    })

    output$trend_plot <- plotly::renderPlotly({
      req(ts_data()); ts <- ts_data(); m <- input$forecast_metric
      n  <- max(3,floor(nrow(ts)*0.15)); sm <- stats::filter(ts[[m]],rep(1/n,n),sides=2)
      pb(plotly::plot_ly(ts,x=~Date) %>%
        plotly::add_lines(y=ts[[m]],name="Raw",line=list(color="rgba(37,99,235,0.3)",width=1.5)) %>%
        plotly::add_lines(y=sm,name="Trend",line=list(color="#7C3AED",width=2)) %>%
        plotly::layout(xaxis=list(title=""),yaxis=list(title=m)))
    })

    output$forecast_summary <- renderUI({
      fr <- fc_data(); if (is.null(fr)) return(p("Run forecast first.",style="color:var(--t3);font-size:12px;"))
      m <- input$forecast_metric; is_rev <- m=="Revenue"
      fmt <- function(x) if(is_rev) paste0("₹",format(round(x),big.mark=",")) else format(round(x),big.mark=",")
      items <- list(
        list("Periods",       nrow(fr)),
        list("Peak",          fmt(max(fr$Forecast,na.rm=TRUE))),
        list("Trough",        fmt(min(fr$Forecast,na.rm=TRUE))),
        list("Average",       fmt(mean(fr$Forecast,na.rm=TRUE))),
        list("Avg CI Width",  fmt(mean(fr$Hi95-fr$Lo95,na.rm=TRUE)))
      )
      tagList(lapply(items, function(it)
        div(style="display:flex;justify-content:space-between;align-items:center;padding:9px 0;border-bottom:1px solid var(--border);",
          span(it[[1]],style="font-size:12px;color:var(--t2);"),
          span(it[[2]],style="font-family:var(--mono);font-size:11px;font-weight:600;color:var(--blue-lt);")
        )
      ))
    })
  })
}
