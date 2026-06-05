# ── Insights & What-If Module ────────────────────────────────

# ── What-If Simulator ────────────────────────────────────────
whatIfUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class="page-header",
      div(class="page-title", tags$span("What-If", class="page-accent"), " Simulator"),
      p("Real-time impact modeling — adjust sliders to instantly recalculate demand, revenue and profit", class="page-sub")
    ),
    fluidRow(
      column(3,
        div(class="card",
          div(style="display:flex;align-items:center;justify-content:space-between;margin-bottom:14px;",
            div(class="card-title",style="margin-bottom:0;","Controls"),
            div(class="status-badge live","Live")
          ),
          sliderInput(ns("sim_price"),     "Product Price (₹)",    min=100, max=100000, value=5000, step=100),
          sliderInput(ns("sim_comp"),      "Competitor Price (₹)", min=100, max=100000, value=4800, step=100),
          sliderInput(ns("sim_inventory"), "Inventory Level",      min=10,  max=5000,   value=500,  step=10),
          selectInput(ns("sim_category"), "Category",
            choices=c("Electronics","Fashion","Grocery","Home Appliances","Beauty","Sports","Books","Accessories")),
          selectInput(ns("sim_season"),    "Season", choices=c("Spring","Summer","Autumn","Winter")),
          div(class="section-divider"),
          sliderInput(ns("sim_cost"),       "Cost Price (₹)",    min=100, max=80000, value=3000, step=100),
          sliderInput(ns("sim_elasticity"), "Price Elasticity",  min=-3.0,max=-0.1,  value=-1.2, step=0.05)
        )
      ),
      column(9,
        uiOutput(ns("sim_kpis")),
        fluidRow(style="margin-top:14px;",
          column(6, div(class="card", div(class="card-title","Revenue Sensitivity"), plotly::plotlyOutput(ns("sim_chart"),  height="240px"))),
          column(6, div(class="card", div(class="card-title","Profit Landscape"),    plotly::plotlyOutput(ns("profit_chart"),height="240px")))
        ),
        div(class="card",style="margin-top:14px;",
          div(class="card-title","Competitive Position Analysis"),
          uiOutput(ns("competitive_position"))
        )
      )
    )
  )
}

whatIfServer <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    calc_dem <- function(price, comp, inventory, cat, season, elas) {
      base_map <- c(Electronics=120,Fashion=80,Grocery=250,`Home Appliances`=60,Beauty=100,Sports=90,Books=150,Accessories=110)
      base <- base_map[cat]; if(is.na(base)) base <- 100
      sm   <- switch(season,Winter=1.3,Summer=1.2,Spring=1.1,Autumn=1.0)
      pmax(5, round(base * sm * (comp/price)^abs(elas)))
    }

    pb <- function(p) p %>% plotly::layout(
      paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
      font=list(family="Inter",color="#475569",size=11),
      xaxis=list(gridcolor="rgba(255,255,255,0.04)"),
      yaxis=list(gridcolor="rgba(255,255,255,0.04)"),
      showlegend=FALSE, margin=list(l=44,r=10,t=10,b=40)
    ) %>% plotly::config(displayModeBar=FALSE,responsive=TRUE)

    output$sim_kpis <- renderUI({
      d   <- calc_dem(input$sim_price,input$sim_comp,input$sim_inventory,input$sim_category,input$sim_season,input$sim_elasticity)
      rev <- input$sim_price*d; profit <- (input$sim_price-input$sim_cost)*d
      margin <- round((input$sim_price-input$sim_cost)/input$sim_price*100,1)
      cd     <- round((input$sim_price/input$sim_comp-1)*100,1)
      sd     <- if(d>0) round(input$sim_inventory/d) else Inf
      fluidRow(
        column(2, div(class="kpi-card blue",  div(class="kpi-label","Demand"),    div(class="kpi-value",format(d,big.mark=",")),   div(class="kpi-badge neutral","Units"), tags$span("📦",class="kpi-icon"))),
        column(2, div(class="kpi-card purple", div(class="kpi-label","Revenue"),   div(class="kpi-value",paste0("₹",if(rev>1e6)paste0(round(rev/1e6,1),"M")else paste0(round(rev/1e3,1),"K"))), div(class="kpi-badge neutral","Projected"), tags$span("💰",class="kpi-icon"))),
        column(2, div(class="kpi-card green",  div(class="kpi-label","Profit"),    div(class="kpi-value",paste0("₹",if(abs(profit)>1e6)paste0(round(profit/1e6,1),"M")else paste0(round(profit/1e3,1),"K"))), div(class=if(profit>0)"kpi-badge up"else"kpi-badge down",if(profit>0)"▲ Pos"else"▼ Loss"), tags$span("📈",class="kpi-icon"))),
        column(2, div(class="kpi-card amber",  div(class="kpi-label","Margin"),    div(class="kpi-value",paste0(margin,"%")), div(class=if(margin>20)"kpi-badge up"else"kpi-badge down",if(margin>20)"▲ OK"else"▼ Low"), tags$span("🎯",class="kpi-icon"))),
        column(2, div(class="kpi-card cyan",   div(class="kpi-label","vs Comp"),   div(class="kpi-value",paste0(if(cd>0)"+"else"",cd,"%")), div(class=if(abs(cd)<10)"kpi-badge up"else"kpi-badge down",if(abs(cd)<10)"Competitive"else"Far off"), tags$span("🏁",class="kpi-icon"))),
        column(2, div(class="kpi-card red",    div(class="kpi-label","Stock Days"),div(class="kpi-value",if(is.infinite(sd))"∞"else sd), div(class=if(is.infinite(sd)||sd>7)"kpi-badge up"else"kpi-badge down",if(is.infinite(sd)||sd>7)"Sufficient"else"▼ Low"), tags$span("📊",class="kpi-icon")))
      )
    })

    output$sim_chart <- plotly::renderPlotly({
      bp <- input$sim_price; prices <- seq(bp*0.5,bp*2,length.out=60)
      revs <- sapply(prices,function(p) p*calc_dem(p,input$sim_comp,input$sim_inventory,input$sim_category,input$sim_season,input$sim_elasticity))
      pb(plotly::plot_ly(x=prices,y=revs,type="scatter",mode="lines",line=list(color="#2563EB",width=2),fill="tozeroy",fillcolor="rgba(37,99,235,0.06)",hovertemplate="₹%{x:,.0f}<br>Rev: ₹%{y:,.0f}<extra></extra>") %>%
        plotly::add_lines(x=c(bp,bp),y=c(0,max(revs,na.rm=TRUE)),line=list(color="#F59E0B",dash="dash",width=1.5),hoverinfo="skip") %>%
        plotly::layout(xaxis=list(title="Price (₹)"),yaxis=list(title="Revenue (₹)")))
    })

    output$profit_chart <- plotly::renderPlotly({
      bp <- input$sim_price; cost <- input$sim_cost; prices <- seq(bp*0.5,bp*2,length.out=60)
      profs <- sapply(prices,function(p) (p-cost)*calc_dem(p,input$sim_comp,input$sim_inventory,input$sim_category,input$sim_season,input$sim_elasticity))
      pb(plotly::plot_ly(x=prices,y=profs,type="scatter",mode="lines",line=list(color="#10B981",width=2),fill="tozeroy",fillcolor="rgba(16,185,129,0.06)",hovertemplate="₹%{x:,.0f}<br>Profit: ₹%{y:,.0f}<extra></extra>") %>%
        plotly::add_lines(x=c(bp,bp),y=c(min(profs,na.rm=TRUE),max(profs,na.rm=TRUE)),line=list(color="#F59E0B",dash="dash",width=1.5),hoverinfo="skip") %>%
        plotly::layout(xaxis=list(title="Price (₹)"),yaxis=list(title="Profit (₹)")))
    })

    output$competitive_position <- renderUI({
      price <- input$sim_price; comp <- input$sim_comp; cost <- input$sim_cost
      margin <- (price-cost)/price*100; gap <- round((price/comp-1)*100,1)
      d <- calc_dem(price,comp,input$sim_inventory,input$sim_category,input$sim_season,input$sim_elasticity)
      sd <- if(d>0) round(input$sim_inventory/d) else Inf

      recs <- list()
      if (price>comp*1.10) recs[[length(recs)+1]] <- list(icon="⚠️",type="warning",title="Overpriced",desc=paste0(gap,"% above competitor. Consider reducing price to recapture demand."))
      else if (price<comp*0.90) recs[[length(recs)+1]] <- list(icon="💡",type="info",title="Price Increase Opportunity",desc=paste0(abs(gap),"% below competitor. You have room to raise price."))
      else recs[[length(recs)+1]] <- list(icon="✅",type="success",title="Competitive Pricing",desc="Your price is well-aligned with the competitor.")
      if (margin<15) recs[[length(recs)+1]] <- list(icon="🔴",type="danger",title="Low Margin",desc=paste0(round(margin,1),"% margin. Raise price or reduce cost."))
      else if (margin>40) recs[[length(recs)+1]] <- list(icon="🏆",type="success",title="Strong Margin",desc=paste0(round(margin,1),"% margin — room for competitive pricing."))
      if (!is.infinite(sd) && sd<7) recs[[length(recs)+1]] <- list(icon="📦",type="warning",title="Inventory Alert",desc="Less than 7 days of stock at current demand. Restock soon.")

      tagList(lapply(recs, function(r) div(class="insight-card",
        div(class=paste("insight-icon",r$type),tags$span(r$icon,style="font-size:16px;")),
        div(div(class="insight-title",r$title),div(class="insight-desc",r$desc))
      )))
    })
  })
}

# ── AI Insights ──────────────────────────────────────────────
insightsUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class="page-header",
      div(class="page-title", tags$span("AI Business", class="page-accent"), " Insights"),
      p("Automated strategic recommendations generated from data analysis", class="page-sub")
    ),
    fluidRow(
      column(3,
        div(class="card",
          div(class="card-title","Generate"),
          actionButton(ns("generate_insights"),"Generate Insights",class="btn btn-primary",style="width:100%;"),
          p("Analyzes dataset for pricing risks, revenue opportunities and operational alerts.",
            style="font-size:11px;color:var(--t3);margin-top:10px;line-height:1.6;")
        )
      ),
      column(9, uiOutput(ns("insight_summary_bar")))
    ),
    fluidRow(style="margin-top:14px;",
      column(6, div(class="card",
        div(class="card-title","Strategic Recommendations"),
        uiOutput(ns("insights_cards"))
      )),
      column(6,
        div(class="card",
          div(class="card-title","Category Rankings"),
          uiOutput(ns("category_rankings"))
        ),
        div(class="card",style="margin-top:14px;",
          div(class="card-title","Top Products by Revenue"),
          uiOutput(ns("top_products_list"))
        )
      )
    )
  )
}

insightsServer <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    insights_list <- reactiveVal(NULL)

    observeEvent(input$generate_insights, {
      req(app_data$data_loaded, app_data$df)
      df <- app_data$df; df$Date <- as.Date(df$Date)
      df$Month  <- format(df$Date,"%Y-%m")
      df$Margin <- (df$Selling_Price-df$Cost_Price)/df$Selling_Price*100
      ins <- list()

      months <- sort(unique(df$Month))
      if (length(months)>=2) {
        l2<-tail(months,2); r1<-sum(df$Revenue[df$Month==l2[1]],na.rm=T); r2<-sum(df$Revenue[df$Month==l2[2]],na.rm=T)
        g <- round((r2/r1-1)*100,1)
        ins[[length(ins)+1]] <- list(icon=if(g>=0)"📈"else"📉",type=if(g>=0)"success"else"danger",
          title=paste0("Revenue ",if(g>=0)"Growth"else"Decline",": ",if(g>=0)"+"else"",g,"%"),
          desc=paste0("MoM change. ",if(g>5)"Strong momentum."else if(g>0)"Steady growth."else"Review pricing strategy."))
      }

      lm <- df[df$Margin<15,]; if(nrow(lm)>0) ins[[length(ins)+1]] <- list(icon="⚠️",type="warning",
        title=paste0(length(unique(lm$Product_Name))," Products with Low Margin (<15%)"),
        desc=paste0("Including: ",paste(head(unique(lm$Product_Name),2),collapse=", "),". Increase price or reduce cost."))

      ls <- df[df$Inventory<df$Demand*7,]; if(nrow(ls)>0) ins[[length(ins)+1]] <- list(icon="📦",type="danger",
        title=paste0("Stock Risk: ",length(unique(ls$Product_Name))," Products"),
        desc=paste0("Under 7 days stock. At risk: ",paste(head(unique(ls$Product_Name),2),collapse=", ")))

      op <- df[df$Selling_Price>df$Competitor_Price*1.10,]; if(nrow(op)>0) ins[[length(ins)+1]] <- list(icon="🏷️",type="warning",
        title=paste0(length(unique(op$Product_Name))," Products >10% Above Competitor"),
        desc=paste0("May be losing market share. Review: ",paste(head(unique(op$Product_Name),2),collapse=", ")))

      up <- df[df$Selling_Price<df$Competitor_Price*0.90,]; if(nrow(up)>0) {
        pot <- sum((up$Competitor_Price*0.95-up$Selling_Price)*up$Demand,na.rm=T)
        ins[[length(ins)+1]] <- list(icon="💡",type="info",
          title=paste0("Price Increase Opportunity: ₹",format(round(pot/1e3,1),nsmall=1),"K"),
          desc=paste0(length(unique(up$Product_Name))," products underpriced vs competitor. Raising to 95% of competitor could boost revenue significantly."))
      }

      sr <- aggregate(Revenue~Season,df,sum); bs <- sr$Season[which.max(sr$Revenue)]
      ins[[length(ins)+1]] <- list(icon="🌟",type="success",title=paste0("Peak Season: ",bs),
        desc=paste0(bs," generates peak revenue. Plan inventory and campaigns ahead."))

      cr <- aggregate(Demand~Category,df,mean); tc <- cr$Category[which.max(cr$Demand)]
      ins[[length(ins)+1]] <- list(icon="🏆",type="success",title=paste0("Highest Demand: ",tc),
        desc=paste0(tc," shows strongest demand. Prioritize stock and competitive pricing here."))

      insights_list(ins)
      showNotification("✅ Insights generated",type="message",duration=3)
    })

    output$insight_summary_bar <- renderUI({
      il <- insights_list(); if(is.null(il)) return(div(class="notif-strip",tags$span("⚡",class="icon"),"Click Generate Insights to analyze your data."))
      ns <- sum(sapply(il,function(i)i$type%in%c("success","info")))
      nw <- sum(sapply(il,function(i)i$type=="warning"))
      nd <- sum(sapply(il,function(i)i$type=="danger"))
      div(style="display:flex;gap:12px;flex-wrap:wrap;",
        div(class="kpi-card green",style="flex:1;min-width:100px;",div(class="kpi-label","Opportunities"),div(class="kpi-value",ns)),
        div(class="kpi-card amber",style="flex:1;min-width:100px;",div(class="kpi-label","Warnings"),div(class="kpi-value",nw)),
        div(class="kpi-card red",  style="flex:1;min-width:100px;",div(class="kpi-label","Risks"),    div(class="kpi-value",nd)),
        div(class="kpi-card blue", style="flex:1;min-width:100px;",div(class="kpi-label","Total"),    div(class="kpi-value",length(il)))
      )
    })

    output$insights_cards <- renderUI({
      il <- insights_list()
      if (is.null(il)) return(div(style="text-align:center;padding:28px;color:var(--t4);","Click Generate Insights to begin."))
      tagList(lapply(il, function(r) div(class="insight-card",
        div(class=paste("insight-icon",r$type),tags$span(r$icon,style="font-size:16px;")),
        div(div(class="insight-title",r$title),div(class="insight-desc",r$desc))
      )))
    })

    output$category_rankings <- renderUI({
      req(app_data$data_loaded, app_data$df); df <- app_data$df
      cv  <- aggregate(Revenue~Category,df,sum); cv <- cv[order(cv$Revenue,decreasing=TRUE),]
      tot <- sum(cv$Revenue)
      COLS <- c("#2563EB","#7C3AED","#10B981","#F59E0B","#EF4444")
      MEDALS <- c("🥇","🥈","🥉","4th","5th")
      tagList(lapply(seq_len(min(5,nrow(cv))), function(i) {
        r <- cv[i,]; pct <- round(r$Revenue/tot*100,1)
        div(style="margin-bottom:10px;",
          div(style="display:flex;justify-content:space-between;margin-bottom:3px;",
            div(style="display:flex;align-items:center;gap:7px;",
              tags$span(MEDALS[i]),span(r$Category,style="font-size:12px;font-weight:600;color:var(--t1);")),
            span(paste0("₹",format(round(r$Revenue/1e6,1),nsmall=1),"M"),
              style=paste0("font-family:var(--mono);font-size:11px;font-weight:600;color:",COLS[i],";"))
          ),
          div(class="feature-progress",div(class="feature-progress-fill",style=paste0("width:",pct,"%;background:",COLS[i],";"))),
          div(style="text-align:right;font-size:9px;color:var(--t4);font-family:var(--mono);margin-top:1px;",paste0(pct,"%"))
        )
      }))
    })

    output$top_products_list <- renderUI({
      req(app_data$data_loaded, app_data$df); df <- app_data$df
      pr <- aggregate(Revenue~Product_Name+Category,df,sum)
      pr <- pr[order(pr$Revenue,decreasing=TRUE)[1:5],]
      tagList(lapply(seq_len(nrow(pr)), function(i) {
        r <- pr[i,]
        div(style="display:flex;align-items:center;gap:10px;padding:9px 0;border-bottom:1px solid var(--border);",
          div(style="width:24px;height:24px;background:var(--bg-3);border-radius:4px;display:flex;align-items:center;justify-content:center;font-size:10px;font-weight:700;color:var(--blue-lt);font-family:var(--mono);flex-shrink:0;",paste0("#",i)),
          div(style="flex:1;",
            div(style="font-size:12.5px;font-weight:600;color:var(--t1);",r$Product_Name),
            div(style="font-size:10px;color:var(--t3);",r$Category)
          ),
          div(style="font-family:var(--mono);font-size:11px;font-weight:600;color:var(--green);",paste0("₹",format(round(r$Revenue/1e3,1),nsmall=1),"K"))
        )
      }))
    })
  })
}
