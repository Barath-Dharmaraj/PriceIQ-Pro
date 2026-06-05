# ── Executive Dashboard Module ───────────────────────────────

dashboardUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class="page-header",
      div(style="display:flex;align-items:center;justify-content:space-between;flex-wrap:wrap;gap:10px;",
        div(
          div(class="page-title", tags$span("Executive", class="page-accent"), " Dashboard"),
          p("Revenue intelligence overview", class="page-sub")
        ),
        div(class="status-badge live", "Live Analytics")
      )
    ),

    uiOutput(ns("exec_kpis")),
    div(class="section-divider"),

    fluidRow(
      column(8,
        div(class="card",
          div(style="display:flex;align-items:center;justify-content:space-between;margin-bottom:14px;",
            div(class="card-title",style="margin-bottom:0;", "Revenue Trend"),
            selectInput(ns("dash_period"), NULL,
              choices=c("Last 30 Days"=30,"Last 7 Days"=7,"Last 90 Days"=90,"All Time"=9999),
              selected=30, width="140px")
          ),
          plotly::plotlyOutput(ns("dash_revenue"), height="280px")
        )
      ),
      column(4,
        div(class="card",
          div(class="card-title", "Category Mix"),
          plotly::plotlyOutput(ns("dash_category"), height="280px")
        )
      )
    ),

    fluidRow(style="margin-top:16px;",
      column(4, div(class="card",
        div(class="card-title","Seasonal Performance"),
        plotly::plotlyOutput(ns("dash_season"), height="220px")
      )),
      column(4, div(class="card",
        div(class="card-title","Top Products"),
        plotly::plotlyOutput(ns("dash_top_products"), height="220px")
      )),
      column(4, div(class="card",
        div(class="card-title","Category Bubble"),
        plotly::plotlyOutput(ns("dash_bubble"), height="220px")
      ))
    ),

    fluidRow(style="margin-top:16px;",
      column(6, div(class="card",
        div(class="card-title","Key Metrics"),
        uiOutput(ns("secondary_kpis"))
      )),
      column(6, div(class="card",
        div(class="card-title","Quick Insights"),
        uiOutput(ns("quick_insights"))
      ))
    ),

    div(style="margin-top:16px;", class="card",
      div(class="card-title","Product Performance Summary"),
      DT::dataTableOutput(ns("top_products_table"))
    )
  )
}

dashboardServer <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    COLORS <- c("#2563EB","#7C3AED","#10B981","#F59E0B","#EF4444","#06B6D4","#EC4899","#F97316")

    pb <- function(p) p %>% plotly::layout(
      paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
      font=list(family="Inter",color="#475569",size=11),
      xaxis=list(gridcolor="rgba(255,255,255,0.04)",zerolinecolor="rgba(255,255,255,0.04)",tickfont=list(size=10)),
      yaxis=list(gridcolor="rgba(255,255,255,0.04)",zerolinecolor="rgba(255,255,255,0.04)",tickfont=list(size=10)),
      legend=list(font=list(color="#475569"),bgcolor="rgba(0,0,0,0)"),
      margin=list(l=40,r=10,t=10,b=40)
    ) %>% plotly::config(displayModeBar=FALSE, responsive=TRUE)

    df_r <- reactive({
      req(app_data$data_loaded, app_data$df)
      df <- app_data$df
      df$Date   <- as.Date(df$Date)
      df$Month  <- format(df$Date,"%Y-%m")
      df$Margin <- round((df$Selling_Price - df$Cost_Price)/df$Selling_Price*100,1)
      df$Profit <- (df$Selling_Price - df$Cost_Price)*df$Demand
      df
    })

    df_filtered <- reactive({
      df <- df_r(); h <- as.integer(input$dash_period)
      if (h < 9999) { cut <- Sys.Date()-h; sub <- df[df$Date>=cut,]; if(nrow(sub)>0) return(sub) }
      df
    })

    output$exec_kpis <- renderUI({
      df <- df_r()
      tot_rev  <- sum(df$Revenue,na.rm=T)
      tot_prof <- sum(df$Profit,na.rm=T)
      n_prod   <- length(unique(df$Product_ID))
      avg_mar  <- round(mean(df$Margin,na.rm=T),1)
      cat_rev  <- aggregate(Revenue~Category,df,sum)
      best_cat <- cat_rev$Category[which.max(cat_rev$Revenue)]
      months   <- sort(unique(df$Month))
      mom <- if(length(months)>=2){
        l2<-tail(months,2); r1<-sum(df$Revenue[df$Month==l2[1]],na.rm=T); r2<-sum(df$Revenue[df$Month==l2[2]],na.rm=T)
        round((r2/r1-1)*100,1)
      } else 0

      fluidRow(
        column(2, div(class="kpi-card blue",
          div(class="kpi-label","Total Revenue"),
          div(class="kpi-value",paste0("₹",format(round(tot_rev/1e6,1),nsmall=1),"M")),
          div(class="kpi-badge neutral","All Time"), tags$span("💰",class="kpi-icon")
        )),
        column(2, div(class="kpi-card green",
          div(class="kpi-label","Total Profit"),
          div(class="kpi-value",paste0("₹",format(round(tot_prof/1e6,1),nsmall=1),"M")),
          div(class=if(tot_prof>0)"kpi-badge up"else"kpi-badge down",if(tot_prof>0)"▲ Positive"else"▼ Loss"),
          tags$span("📈",class="kpi-icon")
        )),
        column(2, div(class="kpi-card purple",
          div(class="kpi-label","Products"),
          div(class="kpi-value",format(n_prod,big.mark=",")),
          div(class="kpi-badge neutral","Unique SKUs"), tags$span("📦",class="kpi-icon")
        )),
        column(2, div(class="kpi-card amber",
          div(class="kpi-label","Avg Margin"),
          div(class="kpi-value",paste0(avg_mar,"%")),
          div(class=if(avg_mar>25)"kpi-badge up"else"kpi-badge neutral",if(avg_mar>25)"▲ Strong"else"Moderate"),
          tags$span("💎",class="kpi-icon")
        )),
        column(2, div(class="kpi-card cyan",
          div(class="kpi-label","Best Category"),
          div(class="kpi-value",style="font-size:16px;letter-spacing:-0.3px;",best_cat),
          div(class=if(mom>=0)"kpi-badge up"else"kpi-badge down",paste0(if(mom>=0)"▲ +"else"▼ ",mom,"% MoM")),
          tags$span("🏆",class="kpi-icon")
        )),
        column(2, div(class="kpi-card red",
          div(class="kpi-label","Data Records"),
          div(class="kpi-value",format(nrow(df),big.mark=",")),
          div(class="kpi-badge neutral","Total Rows"), tags$span("📊",class="kpi-icon")
        ))
      )
    })

    output$dash_revenue <- plotly::renderPlotly({
      df <- df_filtered()
      daily <- aggregate(cbind(Revenue,Profit)~Date, df, sum)
      daily <- daily[order(daily$Date),]
      n <- min(7,nrow(daily)); daily$MA <- stats::filter(daily$Revenue,rep(1/n,n),sides=1)
      pb(plotly::plot_ly(daily) %>%
        plotly::add_bars(x=~Date,y=~Revenue,name="Revenue",marker=list(color="rgba(37,99,235,0.25)",line=list(width=0))) %>%
        plotly::add_lines(x=~Date,y=~MA,name="7-Day MA",line=list(color="#2563EB",width=2)) %>%
        plotly::add_lines(x=~Date,y=~Profit,name="Profit",line=list(color="#10B981",width=1.5,dash="dot")) %>%
        plotly::layout(barmode="overlay",xaxis=list(title=""),yaxis=list(title="₹"),
          legend=list(orientation="h",y=-0.18,x=0.5,xanchor="center"))
      )
    })

    output$dash_category <- plotly::renderPlotly({
      df <- df_r(); cv <- aggregate(Revenue~Category,df,sum)
      pb(plotly::plot_ly(cv,labels=~Category,values=~Revenue,type="pie",hole=0.5,
        marker=list(colors=COLORS,line=list(color="#080C14",width=2)),
        textinfo="label+percent",textfont=list(size=10,color="#94A3B8"),
        hovertemplate="<b>%{label}</b><br>₹%{value:,.0f}<extra></extra>"
      ) %>% plotly::layout(showlegend=FALSE,margin=list(l=0,r=0,t=10,b=0)))
    })

    output$dash_season <- plotly::renderPlotly({
      df <- df_r()
      sea <- aggregate(Revenue~Season,df,mean)
      sea$Season <- factor(sea$Season,levels=c("Spring","Summer","Autumn","Winter"))
      sea <- sea[order(sea$Season),]
      pb(plotly::plot_ly(sea,x=~Season,y=~Revenue,type="bar",
        marker=list(color=c("#10B981","#2563EB","#F59E0B","#7C3AED"),line=list(width=0)),
        hovertemplate="<b>%{x}</b><br>₹%{y:,.0f}<extra></extra>"
      ) %>% plotly::layout(xaxis=list(title=""),yaxis=list(title="Avg Rev (₹)"),showlegend=FALSE))
    })

    output$dash_top_products <- plotly::renderPlotly({
      df <- df_r(); pr <- aggregate(Revenue~Product_Name,df,sum)
      top <- pr[order(pr$Revenue,decreasing=TRUE)[1:10],]
      top$SN <- substr(top$Product_Name,1,16)
      pb(plotly::plot_ly(top,x=~Revenue,y=~reorder(SN,Revenue),type="bar",orientation="h",
        marker=list(color=colorRampPalette(c("#1e3a5f","#2563EB"))(10),line=list(width=0)),
        hovertemplate="<b>%{y}</b><br>₹%{x:,.0f}<extra></extra>"
      ) %>% plotly::layout(xaxis=list(title="Revenue"),yaxis=list(title=""),showlegend=FALSE))
    })

    output$dash_bubble <- plotly::renderPlotly({
      df <- df_r(); pr <- aggregate(cbind(Selling_Price,Revenue,Demand)~Category,df,mean)
      pb(plotly::plot_ly(pr,x=~Selling_Price,y=~Revenue,size=~Demand,color=~Category,colors=COLORS,
        type="scatter",mode="markers",marker=list(opacity=0.75,sizemode="diameter",sizeref=0.5),
        hovertemplate="<b>%{text}</b><br>₹%{x:,.0f}<extra></extra>",text=~Category
      ) %>% plotly::layout(xaxis=list(title="Avg Price (₹)"),yaxis=list(title="Avg Revenue"),showlegend=FALSE))
    })

    output$secondary_kpis <- renderUI({
      df <- df_r()
      bm <- aggregate(Margin~Category,df,mean); bmc <- bm$Category[which.max(bm$Margin)]
      items <- list(
        list("Best Margin Category", paste0(bmc," (",round(max(bm$Margin),1),"%)"), "var(--purple)"),
        list("Total Inventory",      format(sum(df$Inventory,na.rm=T),big.mark=","), "var(--blue-lt)"),
        list("Low Stock Products",   sum(df$Inventory<df$Demand*7,na.rm=T), "var(--amber)"),
        list("Overpriced vs Market", sum(df$Selling_Price>df$Competitor_Price*1.10,na.rm=T), "var(--red)"),
        list("Categories Tracked",  length(unique(df$Category)), "var(--green)"),
        list("Date Range",          paste(format(min(df$Date,na.rm=T),"%b %d"),"-",format(max(df$Date,na.rm=T),"%b %d, %Y")), "var(--cyan)")
      )
      tagList(lapply(items, function(it)
        div(style="display:flex;align-items:center;justify-content:space-between;padding:9px 0;border-bottom:1px solid var(--border);",
          span(it[[1]], style="font-size:12px;color:var(--t2);"),
          span(it[[2]], style=paste0("font-family:var(--mono);font-size:12px;font-weight:600;color:",it[[3]],";"))
        )
      ))
    })

    output$quick_insights <- renderUI({
      df <- df_r()
      months <- sort(unique(df$Month))
      mom <- if(length(months)>=2){l2<-tail(months,2);round((sum(df$Revenue[df$Month==l2[2]],na.rm=T)/sum(df$Revenue[df$Month==l2[1]],na.rm=T)-1)*100,1)}else 0
      am <- round(mean(df$Margin,na.rm=T),1)
      sr <- aggregate(Revenue~Season,df,sum); bs <- sr$Season[which.max(sr$Revenue)]
      tagList(
        div(class="insight-card",
          div(class=paste("insight-icon",if(mom>=0)"success"else"danger"),tags$span(if(mom>=0)"📈"else"📉",style="font-size:16px;")),
          div(div(class="insight-title","MoM Revenue"),div(class="insight-desc",paste0(if(mom>=0)"Growth of +"else"Decline of ",mom,"% vs previous month")))
        ),
        div(class="insight-card",
          div(class=paste("insight-icon",if(am>25)"success"else"warning"),tags$span("💎",style="font-size:16px;")),
          div(div(class="insight-title","Margin Health"),div(class="insight-desc",paste0(am,"% avg — ",if(am>30)"Excellent"else if(am>20)"Good"else"Needs attention")))
        ),
        div(class="insight-card",
          div(class="insight-icon info",tags$span("🌤️",style="font-size:16px;")),
          div(div(class="insight-title",paste0("Peak Season: ",bs)),div(class="insight-desc","Highest revenue season — plan inventory ahead."))
        )
      )
    })

    output$top_products_table <- DT::renderDataTable({
      df <- df_r()
      ps <- aggregate(cbind(Revenue,Demand,Selling_Price,Margin)~Product_Name+Category,df,mean)
      ps$TotRev <- aggregate(Revenue~Product_Name+Category,df,sum)$Revenue
      ps <- ps[order(ps$TotRev,decreasing=TRUE)[1:20],]
      out <- data.frame(
        Product=ps$Product_Name, Category=ps$Category,
        `Avg Price`=paste0("₹",format(round(ps$Selling_Price),big.mark=",")),
        `Avg Demand`=round(ps$Demand),
        `Margin`=paste0(round(ps$Margin,1),"%"),
        `Total Revenue`=paste0("₹",format(round(ps$TotRev/1e3,1),nsmall=1),"K"),
        check.names=FALSE
      )
      DT::datatable(out, options=list(pageLength=10,dom='tip',scrollX=TRUE), style='bootstrap', class='compact', rownames=FALSE)
    })
  })
}
