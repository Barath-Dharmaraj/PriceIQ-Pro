# ── Revenue Optimization Engine ──────────────────────────────

optimizationUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class="page-header",
      div(class="page-title", tags$span("Price", class="page-accent"), " Optimization Engine"),
      p("AI-driven scenario analysis to find revenue-maximizing and profit-maximizing prices", class="page-sub")
    ),
    fluidRow(
      column(3,
        div(class="card",
          div(class="card-title","🎛 Settings"),
          selectInput(ns("opt_product"), "Select Product", choices=c("Loading..."="")),
          uiOutput(ns("current_price_display")),
          div(class="section-divider"),
          sliderInput(ns("price_range_min"), "Min Multiplier", min=0.5, max=1.0, value=0.7, step=0.05),
          sliderInput(ns("price_range_max"), "Max Multiplier", min=1.0, max=3.0, value=2.0, step=0.05),
          numericInput(ns("n_scenarios"), "Scenarios", value=50, min=20, max=200, step=10),
          div(class="section-divider"),
          sliderInput(ns("elasticity"), "Price Elasticity", min=-3.0, max=-0.1, value=-1.2, step=0.1),
          p("More negative = more price-sensitive", style="font-size:10px;color:var(--t4);margin-top:-8px;font-family:var(--mono);"),
          div(class="section-divider"),
          actionButton(ns("run_optimization"), "Optimize Price", class="btn btn-primary", style="width:100%;")
        )
      ),
      column(9,
        uiOutput(ns("optimization_result")),
        div(class="card", style="margin-top:14px;",
          div(class="card-title","Price Scenario Analysis"),
          plotly::plotlyOutput(ns("scenario_chart"), height="300px")
        ),
        div(class="card", style="margin-top:14px;",
          div(class="card-title","Top Scenarios"),
          DT::dataTableOutput(ns("scenarios_table"))
        )
      )
    ),
    div(class="card", style="margin-top:14px;",
      div(class="card-title","Portfolio — Top Product Recommendations"),
      uiOutput(ns("portfolio_optimization"))
    )
  )
}

optimizationServer <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    scenarios  <- reactiveVal(NULL)
    opt_result <- reactiveVal(NULL)

    observe({
      req(app_data$data_loaded, app_data$df)
      df <- app_data$df
      prods <- sort(unique(paste0(df$Product_Name," (",df$Product_ID,")")))
      updateSelectInput(session,"opt_product",choices=prods[1:min(50,length(prods))])
    })

    sel_prod <- reactive({
      req(app_data$data_loaded, app_data$df, input$opt_product)
      df <- app_data$df
      pid <- gsub(".*\\((.*)\\)$","\\1",input$opt_product)
      df[df$Product_ID==pid,][1,]
    })

    output$current_price_display <- renderUI({
      p <- tryCatch(sel_prod(), error=function(e) NULL)
      if (is.null(p)||nrow(p)==0) return(NULL)
      div(style="background:var(--bg-2);border:1px solid var(--border);border-radius:var(--r2);padding:12px;margin-top:4px;",
        lapply(list(
          list("Current Price", paste0("₹",format(p$Selling_Price,big.mark=",")), "var(--blue-lt)"),
          list("Cost Price",    paste0("₹",format(p$Cost_Price,big.mark=",")),    "var(--t2)"),
          list("Category",     p$Category,                                         "var(--t2)")
        ), function(x) div(style="display:flex;justify-content:space-between;margin-bottom:5px;",
          span(x[[1]],style="font-size:11px;color:var(--t3);"),
          span(x[[2]],style=paste0("font-family:var(--mono);font-size:11px;font-weight:600;color:",x[[3]],";"))
        ))
      )
    })

    observeEvent(input$run_optimization, {
      req(app_data$data_loaded, app_data$df)
      tryCatch({
        p <- sel_prod(); if(is.null(p)||nrow(p)==0){showNotification("❌ No product selected",type="error");return()}
        cp <- p$Selling_Price; cost <- p$Cost_Price; bd <- p$Demand; elas <- input$elasticity
        prices <- seq(cp*input$price_range_min, cp*input$price_range_max, length.out=input$n_scenarios)
        sc <- data.frame(Price=prices)
        sc$Demand_Predicted <- pmax(1, bd*(sc$Price/cp)^elas)
        sc$Revenue  <- sc$Price*sc$Demand_Predicted
        sc$Profit   <- (sc$Price-cost)*sc$Demand_Predicted
        sc$Rev_Growth  <- round((sc$Revenue/(cp*bd)-1)*100,1)
        sc$Prof_Growth <- round((sc$Profit/((cp-cost)*bd)-1)*100,1)
        ri <- which.max(sc$Revenue); pi <- which.max(sc$Profit)
        opt_result(list(
          current_price=cp, rev_opt_price=sc$Price[ri], prof_opt_price=sc$Price[pi],
          current_revenue=cp*bd, opt_revenue=sc$Revenue[ri],
          current_profit=(cp-cost)*bd, opt_profit=sc$Profit[pi],
          rev_growth=sc$Rev_Growth[ri], prof_growth=sc$Prof_Growth[pi],
          product_name=p$Product_Name
        ))
        scenarios(sc)
        showNotification("✅ Optimization complete",type="message",duration=3)
      }, error=function(e) showNotification(paste("❌",e$message),type="error"))
    })

    output$optimization_result <- renderUI({
      or <- opt_result()
      if (is.null(or))
        return(div(class="notif-strip",tags$span("🚀",class="icon"),"Select a product and click Optimize Price to see recommendations."))
      div(class="opt-result-card",
        div(style="font-size:14px;font-weight:700;color:var(--t1);margin-bottom:2px;", paste("Results —",or$product_name)),
        div(style="font-size:11px;color:var(--t3);margin-bottom:14px;font-family:var(--mono);","Revenue-maximizing recommendation"),
        div(class="price-arrow",
          div(class="price-box current",  div(class="label","CURRENT PRICE"), div(class="value",paste0("₹",format(round(or$current_price),big.mark=",")))),
          tags$span("→",class="arrow-icon"),
          div(class="price-box recommended",div(class="label","RECOMMENDED"),   div(class="value",paste0("₹",format(round(or$rev_opt_price),big.mark=","))))
        ),
        div(class="gain-badges",
          div(class="gain-badge",div(class="badge-label","REVENUE INCREASE"),paste0(if(or$rev_growth>=0)"+"else"",or$rev_growth,"%")),
          div(class="gain-badge",div(class="badge-label","PROFIT INCREASE"), paste0(if(or$prof_growth>=0)"+"else"",or$prof_growth,"%")),
          div(class="gain-badge",style="background:var(--purple-bg);border-color:rgba(124,58,237,0.2);color:var(--purple);",
            div(class="badge-label","PROFIT-MAX PRICE"), paste0("₹",format(round(or$prof_opt_price),big.mark=",")))
        )
      )
    })

    output$scenario_chart <- plotly::renderPlotly({
      sc <- scenarios(); or <- opt_result(); req(sc)
      p <- plotly::plot_ly(sc) %>%
        plotly::add_lines(x=~Price,y=~Revenue,name="Revenue",line=list(color="#2563EB",width=2),yaxis="y1",hovertemplate="₹%{x:,.0f}<br>Rev: ₹%{y:,.0f}<extra></extra>") %>%
        plotly::add_lines(x=~Price,y=~Profit, name="Profit", line=list(color="#10B981",width=2),yaxis="y1",hovertemplate="₹%{x:,.0f}<br>Prof: ₹%{y:,.0f}<extra></extra>") %>%
        plotly::add_lines(x=~Price,y=~Demand_Predicted,name="Demand",line=list(color="#F59E0B",width=1.5,dash="dot"),yaxis="y2",hovertemplate="₹%{x:,.0f}<br>Dem: %{y:.0f}<extra></extra>")
      if (!is.null(or)) {
        mx <- max(sc$Revenue,na.rm=TRUE)
        p <- p %>%
          plotly::add_lines(x=c(or$rev_opt_price,or$rev_opt_price),y=c(0,mx),line=list(color="rgba(37,99,235,0.3)",dash="dash",width=1),name="Rev Max",yaxis="y1",hoverinfo="skip") %>%
          plotly::add_lines(x=c(or$prof_opt_price,or$prof_opt_price),y=c(0,mx),line=list(color="rgba(16,185,129,0.3)",dash="dash",width=1),name="Profit Max",yaxis="y1",hoverinfo="skip")
      }
      p %>% plotly::layout(
        paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
        font=list(family="Inter",color="#475569",size=11),
        xaxis=list(title="Price (₹)",gridcolor="rgba(255,255,255,0.04)"),
        yaxis=list(title="Revenue / Profit (₹)",gridcolor="rgba(255,255,255,0.04)",side="left"),
        yaxis2=list(title="Demand",overlaying="y",side="right",showgrid=FALSE,tickfont=list(color="#F59E0B")),
        legend=list(font=list(color="#475569"),bgcolor="rgba(0,0,0,0)",orientation="h",y=-0.18,x=0.5,xanchor="center"),
        margin=list(l=60,r=60,t=10,b=56)
      ) %>% plotly::config(displayModeBar=FALSE,responsive=TRUE)
    })

    output$scenarios_table <- DT::renderDataTable({
      sc <- scenarios(); req(sc)
      top <- sc[order(sc$Revenue,decreasing=TRUE)[1:12],]
      out <- data.frame(
        Price=paste0("₹",format(round(top$Price),big.mark=",")),
        Demand=round(top$Demand_Predicted),
        Revenue=paste0("₹",format(round(top$Revenue),big.mark=",")),
        Profit=paste0("₹",format(round(top$Profit),big.mark=",")),
        `Rev Growth`=paste0(ifelse(top$Rev_Growth>=0,"+",""),top$Rev_Growth,"%"),
        check.names=FALSE
      )
      DT::datatable(out,options=list(pageLength=6,dom='tip',scrollX=TRUE),style='bootstrap',class='compact',rownames=FALSE)
    })

    output$portfolio_optimization <- renderUI({
      req(app_data$data_loaded, app_data$df); df <- app_data$df
      pa <- aggregate(cbind(Revenue,Demand,Selling_Price,Cost_Price)~Product_Name+Category,df,mean)
      pa <- pa[order(pa$Revenue,decreasing=TRUE)[1:8],]
      pa$Opt_Price   <- round(pa$Selling_Price*1.10)
      pa$Opt_Demand  <- round(pa$Demand*(1.10^(-1.2)))
      pa$Rev_Change  <- round((pa$Opt_Price*pa$Opt_Demand/(pa$Selling_Price*pa$Demand)-1)*100,1)
      fluidRow(lapply(1:min(4,nrow(pa)), function(i) {
        r <- pa[i,]; up <- r$Rev_Change>=0
        column(3, div(class="card",
          style=paste0("border-top:2px solid ",if(up)"var(--green)"else"var(--red)","!important;"),
          div(style="font-family:var(--mono);font-size:9px;color:var(--t3);letter-spacing:1px;margin-bottom:5px;",r$Category),
          div(style="font-size:12.5px;font-weight:600;color:var(--t1);margin-bottom:10px;",r$Product_Name),
          lapply(list(
            list("Current", paste0("₹",format(r$Selling_Price,big.mark=",")), "var(--t2)"),
            list("Suggested",paste0("₹",format(r$Opt_Price,big.mark=",")),   if(up)"var(--green)"else"var(--red)")
          ), function(x) div(style="display:flex;justify-content:space-between;font-size:11px;margin-bottom:4px;",
            span(x[[1]],style="color:var(--t3);"), span(x[[2]],style=paste0("font-family:var(--mono);font-weight:600;color:",x[[3]],";"))
          )),
          div(class=if(up)"kpi-badge up"else"kpi-badge down",style="margin-top:8px;display:inline-flex;",
            paste0(if(up)"▲ +"else"▼ ",r$Rev_Change,"% Revenue"))
        ))
      }))
    })
  })
}
