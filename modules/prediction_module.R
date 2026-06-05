# ── ML Prediction Module ─────────────────────────────────────

predictionUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class="page-header",
      div(class="page-title", tags$span("Demand", class="page-accent"), " Prediction"),
      p("Random Forest model — predict demand from price, competitor, inventory, category and season", class="page-sub")
    ),
    fluidRow(
      column(3,
        div(class="card",
          div(class="card-title", "⚙ Model Config"),
          sliderInput(ns("n_trees"),    "Number of Trees",  min=50, max=500, value=200, step=50),
          sliderInput(ns("train_split"),"Training Split %", min=60, max=90,  value=80,  step=5),
          selectInput(ns("ml_cat_filter"), "Category Filter",
            choices=c("All","Electronics","Fashion","Grocery","Home Appliances","Beauty","Sports","Books","Accessories")),
          actionButton(ns("train_model"), "Train Model", class="btn btn-primary", style="width:100%;margin-top:4px;"),
          div(class="section-divider"),
          uiOutput(ns("model_status"))
        ),
        div(class="card", style="margin-top:14px;",
          div(class="card-title", "🎯 Single Prediction"),
          numericInput(ns("pred_price"),     "Selling Price (₹)",    value=1500, min=100, max=200000),
          numericInput(ns("pred_comp"),      "Competitor Price (₹)", value=1400, min=100, max=200000),
          numericInput(ns("pred_inventory"), "Inventory",            value=150,  min=1,   max=10000),
          selectInput(ns("pred_category"),   "Category",
            choices=c("Electronics","Fashion","Grocery","Home Appliances","Beauty","Sports","Books","Accessories")),
          selectInput(ns("pred_season"),     "Season",
            choices=c("Spring","Summer","Autumn","Winter")),
          actionButton(ns("predict_demand"), "Predict Demand", class="btn btn-success", style="width:100%;")
        )
      ),
      column(9,
        uiOutput(ns("model_metrics_ui")),
        fluidRow(style="margin-top:14px;",
          column(6, div(class="card", div(class="card-title","Feature Importance"), plotly::plotlyOutput(ns("feature_importance"),height="260px"))),
          column(6, div(class="card", div(class="card-title","Actual vs Predicted"), plotly::plotlyOutput(ns("actual_vs_pred"),height="260px")))
        ),
        div(class="card", style="margin-top:14px;",
          div(class="card-title","Prediction Result"),
          uiOutput(ns("prediction_result"))
        )
      )
    )
  )
}

predictionServer <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    model_trained <- reactiveVal(FALSE)
    model_obj     <- reactiveVal(NULL)
    model_metrics <- reactiveVal(NULL)
    test_results  <- reactiveVal(NULL)
    pred_result   <- reactiveVal(NULL)

    encode <- function(df) {
      cl <- c("Electronics","Fashion","Grocery","Home Appliances","Beauty","Sports","Books","Accessories")
      sl <- c("Spring","Summer","Autumn","Winter")
      df$Cat_Num    <- match(df$Category, cl);  df$Cat_Num[is.na(df$Cat_Num)]    <- 1
      df$Season_Num <- match(df$Season,   sl);  df$Season_Num[is.na(df$Season_Num)] <- 1
      df
    }

    observeEvent(input$train_model, {
      req(app_data$data_loaded, app_data$df)
      withProgress(message="Training Random Forest...", value=0, {
        tryCatch({
          df <- app_data$df
          if (input$ml_cat_filter != "All") df <- df[df$Category==input$ml_cat_filter,]
          if (nrow(df)<30) { showNotification("❌ Need at least 30 rows",type="error"); return() }
          df <- encode(df)
          feats <- c("Selling_Price","Competitor_Price","Inventory","Cat_Num","Season_Num")
          dm <- df[,c(feats,"Demand")]; dm <- dm[complete.cases(dm),]
          incProgress(0.3, detail="Splitting data...")
          set.seed(42); idx <- sample(nrow(dm), floor(input$train_split/100*nrow(dm)))
          tr <- dm[idx,]; te <- dm[-idx,]
          incProgress(0.5, detail="Fitting model...")
          rf <- randomForest::randomForest(Demand~., data=tr, ntree=input$n_trees, importance=TRUE)
          incProgress(0.85, detail="Evaluating...")
          preds <- predict(rf, te); acts <- te$Demand
          mae  <- mean(abs(preds-acts))
          rmse <- sqrt(mean((preds-acts)^2))
          r2   <- 1 - sum((acts-preds)^2)/sum((acts-mean(acts))^2)
          model_obj(rf); model_metrics(list(mae=mae,rmse=rmse,r2=r2))
          test_results(data.frame(Actual=acts,Predicted=preds))
          model_trained(TRUE); app_data$rf_model <- rf
          saveRDS(rf,"models/random_forest_model.rds")
          showNotification("✅ Model trained successfully",type="message",duration=4)
          incProgress(1)
        }, error=function(e) showNotification(paste("❌",e$message),type="error"))
      })
    })

    observeEvent(input$predict_demand, {
      if (!model_trained()) { showNotification("⚠️ Train the model first",type="warning"); return() }
      tryCatch({
        cl <- c("Electronics","Fashion","Grocery","Home Appliances","Beauty","Sports","Books","Accessories")
        sl <- c("Spring","Summer","Autumn","Winter")
        nd <- data.frame(Selling_Price=input$pred_price, Competitor_Price=input$pred_comp,
          Inventory=input$pred_inventory, Cat_Num=match(input$pred_category,cl),
          Season_Num=match(input$pred_season,sl))
        rf <- model_obj(); p <- predict(rf, nd)
        ti <- predict(rf, nd, predict.all=TRUE)$individual
        pred_result(list(demand=round(p), ci_low=round(quantile(ti,0.10)),
          ci_high=round(quantile(ti,0.90)),
          revenue=round(p)*input$pred_price,
          profit=round(p)*(input$pred_price-input$pred_price*0.65)))
      }, error=function(e) showNotification(paste("❌",e$message),type="error"))
    })

    pb <- function(p) p %>% plotly::layout(
      paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
      font=list(family="Inter",color="#475569",size=11),
      xaxis=list(gridcolor="rgba(255,255,255,0.04)"),
      yaxis=list(gridcolor="rgba(255,255,255,0.04)"),
      margin=list(l=40,r=10,t=10,b=40), showlegend=FALSE
    ) %>% plotly::config(displayModeBar=FALSE,responsive=TRUE)

    output$model_status <- renderUI({
      if (!model_trained())
        return(div(style="text-align:center;padding:14px;color:var(--t4);font-size:12px;","Train model to see status"))
      m <- model_metrics()
      div(
        div(class="status-badge live",style="margin-bottom:10px;","Model Active"),
        lapply(list(
          list("R² Score",paste0(round(m$r2*100,1),"%"),"var(--green)"),
          list("RMSE",round(m$rmse,2),"var(--blue-lt)"),
          list("MAE",round(m$mae,2),"var(--purple)")
        ), function(x) div(style="display:flex;justify-content:space-between;font-size:12px;margin-bottom:6px;",
          span(x[[1]],style="color:var(--t3);"),
          span(x[[2]],style=paste0("font-family:var(--mono);font-weight:600;color:",x[[3]],";"))
        ))
      )
    })

    output$model_metrics_ui <- renderUI({
      if (!model_trained())
        return(div(class="notif-strip", tags$span("🤖",class="icon"), "Configure and click Train Model to see performance metrics."))
      m <- model_metrics()
      fluidRow(
        column(4, div(class="kpi-card green",
          div(class="kpi-label","R² Score"), div(class="kpi-value",paste0(round(m$r2*100,1),"%")),
          div(class="kpi-badge up","Variance Explained"), tags$span("🎯",class="kpi-icon")
        )),
        column(4, div(class="kpi-card blue",
          div(class="kpi-label","RMSE"), div(class="kpi-value",round(m$rmse,2)),
          div(class="kpi-badge neutral","Root Mean Sq Error"), tags$span("📉",class="kpi-icon")
        )),
        column(4, div(class="kpi-card purple",
          div(class="kpi-label","MAE"), div(class="kpi-value",round(m$mae,2)),
          div(class="kpi-badge neutral","Mean Abs Error"), tags$span("📊",class="kpi-icon")
        ))
      )
    })

    output$feature_importance <- plotly::renderPlotly({
      req(model_trained(), model_obj())
      imp <- randomForest::importance(model_obj())
      df  <- data.frame(
        Feature    = c("Selling Price","Competitor Price","Inventory","Category","Season"),
        Importance = as.numeric(imp[,"%IncMSE"])
      )
      df <- df[order(df$Importance),]
      pb(plotly::plot_ly(df, x=~Importance, y=~Feature, type="bar", orientation="h",
        marker=list(color=c("#334155","#334155","#7C3AED","#2563EB","#10B981"),line=list(width=0)),
        hovertemplate="<b>%{y}</b><br>%IncMSE: %{x:.2f}<extra></extra>"
      ) %>% plotly::layout(xaxis=list(title="% Increase in MSE"), yaxis=list(title="")))
    })

    output$actual_vs_pred <- plotly::renderPlotly({
      req(model_trained(), test_results()); td <- test_results()
      lm <- max(c(td$Actual,td$Predicted),na.rm=TRUE)
      pb(plotly::plot_ly(td,x=~Actual,y=~Predicted,type="scatter",mode="markers",
        marker=list(color="#2563EB",size=5,opacity=0.55),
        hovertemplate="Actual: %{x}<br>Pred: %{y:.0f}<extra></extra>"
      ) %>% plotly::add_lines(x=c(0,lm),y=c(0,lm),line=list(color="#10B981",dash="dash",width=1.5)) %>%
        plotly::layout(xaxis=list(title="Actual"),yaxis=list(title="Predicted")))
    })

    output$prediction_result <- renderUI({
      if (is.null(pred_result()))
        return(div(style="text-align:center;padding:28px;color:var(--t4);","Enter parameters above and click Predict Demand"))
      pr <- pred_result()
      div(style="text-align:center;",
        div(style="font-size:48px;font-weight:800;color:#10B981;letter-spacing:-2px;font-variant-numeric:tabular-nums;margin-bottom:4px;",
          format(pr$demand,big.mark=",")),
        div(style="font-family:var(--mono);font-size:9px;letter-spacing:1.5px;color:var(--t3);margin-bottom:12px;","PREDICTED UNITS"),
        div(style="font-size:12px;color:var(--t3);margin-bottom:16px;",
          paste0("90% CI: ",format(pr$ci_low,big.mark=",")," – ",format(pr$ci_high,big.mark=",")," units")),
        div(style="display:flex;gap:12px;justify-content:center;flex-wrap:wrap;",
          div(class="metric-mini", div(class="m-label","Predicted Revenue"), div(class="m-value text-blue", paste0("₹",format(round(pr$revenue/1e3,1),nsmall=1),"K"))),
          div(class="metric-mini", div(class="m-label","Predicted Profit"),  div(class="m-value text-green",paste0("₹",format(round(pr$profit/1e3,1),nsmall=1),"K"))),
          div(class="metric-mini", div(class="m-label","CI Width"),          div(class="m-value", paste0("±",round((pr$ci_high-pr$ci_low)/max(1,pr$demand)*50),"%")))
        )
      )
    })
  })
}
