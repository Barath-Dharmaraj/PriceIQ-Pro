# ── EDA Module ───────────────────────────────────────────────

edaUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class="page-header",
      div(class="page-title", tags$span("Exploratory", class="page-accent"), " Analytics"),
      p("Interactive visualizations across revenue, demand, category, and pricing dimensions", class="page-sub")
    ),
    tabsetPanel(type="tabs",
      tabPanel("Revenue & Demand", br(),
        fluidRow(
          column(8, div(class="card", div(class="card-title","Revenue Trend"), plotly::plotlyOutput(ns("revenue_trend"),height="300px"))),
          column(4, div(class="card", div(class="card-title","Demand Distribution"), plotly::plotlyOutput(ns("demand_hist"),height="300px")))
        ),
        fluidRow(style="margin-top:16px;",
          column(6, div(class="card", div(class="card-title","Monthly Revenue Growth"), plotly::plotlyOutput(ns("monthly_growth"),height="260px"))),
          column(6, div(class="card", div(class="card-title","Price vs Demand"), plotly::plotlyOutput(ns("price_demand"),height="260px")))
        )
      ),
      tabPanel("Category Analysis", br(),
        fluidRow(
          column(6, div(class="card", div(class="card-title","Revenue by Category"), plotly::plotlyOutput(ns("category_revenue"),height="300px"))),
          column(6, div(class="card", div(class="card-title","Market Share"), plotly::plotlyOutput(ns("category_pie"),height="300px")))
        ),
        fluidRow(style="margin-top:16px;",
          column(12, div(class="card", div(class="card-title","Price Comparison by Category"), plotly::plotlyOutput(ns("category_price"),height="240px")))
        )
      ),
      tabPanel("Seasonal & Inventory", br(),
        fluidRow(
          column(6, div(class="card", div(class="card-title","Seasonal Demand by Category"), plotly::plotlyOutput(ns("seasonal_demand"),height="300px"))),
          column(6, div(class="card", div(class="card-title","Inventory vs Demand"), plotly::plotlyOutput(ns("inventory_demand"),height="300px")))
        ),
        fluidRow(style="margin-top:16px;",
          column(12, div(class="card", div(class="card-title","Revenue Heatmap — Month × Category"), plotly::plotlyOutput(ns("heatmap"),height="280px")))
        )
      ),
      tabPanel("Pricing Intelligence", br(),
        fluidRow(
          column(6, div(class="card", div(class="card-title","Price Competitiveness"), plotly::plotlyOutput(ns("price_competition"),height="280px"))),
          column(6, div(class="card", div(class="card-title","Margin Distribution"), plotly::plotlyOutput(ns("margin_dist"),height="280px")))
        ),
        fluidRow(style="margin-top:16px;",
          column(12, div(class="card", div(class="card-title","Correlation Heatmap"), plotly::plotlyOutput(ns("correlation"),height="320px")))
        )
      )
    )
  )
}

edaServer <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    COLORS <- c("#2563EB","#7C3AED","#10B981","#F59E0B","#EF4444","#06B6D4","#EC4899","#F97316")

    pb <- function(p) p %>% plotly::layout(
      paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
      font=list(family="Inter",color="#475569",size=11),
      xaxis=list(gridcolor="rgba(255,255,255,0.04)",zerolinecolor="rgba(255,255,255,0.04)"),
      yaxis=list(gridcolor="rgba(255,255,255,0.04)",zerolinecolor="rgba(255,255,255,0.04)"),
      legend=list(font=list(color="#475569"),bgcolor="rgba(0,0,0,0)"),
      margin=list(l=40,r=10,t=10,b=40), hovermode="closest"
    ) %>% plotly::config(displayModeBar=FALSE,responsive=TRUE)

    df_r <- reactive({
      req(app_data$data_loaded,app_data$df)
      df <- app_data$df; df$Date <- as.Date(df$Date)
      df$Month  <- format(df$Date,"%Y-%m")
      df$Margin <- round((df$Selling_Price-df$Cost_Price)/df$Selling_Price*100,1)
      df
    })

    output$revenue_trend <- plotly::renderPlotly({
      df <- df_r(); d <- aggregate(Revenue~Date,df,sum); d <- d[order(d$Date),]
      pb(plotly::plot_ly(d,x=~Date,y=~Revenue,type="scatter",mode="lines",
        line=list(color="#2563EB",width=2),fill="tozeroy",fillcolor="rgba(37,99,235,0.06)",
        hovertemplate="<b>%{x}</b><br>₹%{y:,.0f}<extra></extra>"))
    })

    output$demand_hist <- plotly::renderPlotly({
      df <- df_r()
      pb(plotly::plot_ly(df,x=~Demand,type="histogram",nbinsx=28,
        marker=list(color="#7C3AED",line=list(color="rgba(124,58,237,0.2)",width=1)),
        hovertemplate="Demand: %{x}<br>Count: %{y}<extra></extra>"))
    })

    output$monthly_growth <- plotly::renderPlotly({
      df <- df_r(); m <- aggregate(Revenue~Month,df,sum); m <- m[order(m$Month),]
      m$G <- c(0,diff(m$Revenue)/head(m$Revenue,-1)*100)
      pb(plotly::plot_ly(m,x=~Month,y=~G,type="bar",
        marker=list(color=ifelse(m$G>=0,"#10B981","#EF4444"),line=list(width=0)),
        hovertemplate="%{x}<br>Growth: %{y:.1f}%<extra></extra>"
      ) %>% plotly::layout(yaxis=list(ticksuffix="%")))
    })

    output$price_demand <- plotly::renderPlotly({
      df <- df_r()
      pb(plotly::plot_ly(df,x=~Selling_Price,y=~Demand,color=~Category,colors=COLORS,
        type="scatter",mode="markers",marker=list(size=5,opacity=0.6),
        text=~Product_Name,hovertemplate="<b>%{text}</b><br>₹%{x:,.0f}<br>Demand: %{y}<extra></extra>"
      ) %>% plotly::layout(xaxis=list(title="Selling Price (₹)"),yaxis=list(title="Demand")))
    })

    output$category_revenue <- plotly::renderPlotly({
      df <- df_r(); cv <- aggregate(Revenue~Category,df,sum); cv <- cv[order(cv$Revenue,decreasing=TRUE),]
      pb(plotly::plot_ly(cv,x=~reorder(Category,Revenue),y=~Revenue,type="bar",
        marker=list(color=COLORS[1:nrow(cv)],line=list(width=0)),
        hovertemplate="<b>%{x}</b><br>₹%{y:,.0f}<extra></extra>"
      ) %>% plotly::layout(xaxis=list(title=""),yaxis=list(title="Revenue (₹)"),showlegend=FALSE))
    })

    output$category_pie <- plotly::renderPlotly({
      df <- df_r(); cv <- aggregate(Revenue~Category,df,sum)
      pb(plotly::plot_ly(cv,labels=~Category,values=~Revenue,type="pie",hole=0.5,
        marker=list(colors=COLORS,line=list(color="#080C14",width=2)),
        textinfo="label+percent",textfont=list(size=10,color="#94A3B8"),
        hovertemplate="<b>%{label}</b><br>₹%{value:,.0f}<extra></extra>"
      ) %>% plotly::layout(showlegend=FALSE))
    })

    output$category_price <- plotly::renderPlotly({
      df <- df_r(); cp <- aggregate(cbind(Selling_Price,Cost_Price,Competitor_Price)~Category,df,mean)
      p <- plotly::plot_ly(cp,x=~Category)
      p <- plotly::add_bars(p,y=~Cost_Price,name="Cost",marker=list(color="#334155"))
      p <- plotly::add_bars(p,y=~Selling_Price,name="Selling",marker=list(color="#2563EB"))
      p <- plotly::add_bars(p,y=~Competitor_Price,name="Competitor",marker=list(color="#F59E0B"))
      pb(p %>% plotly::layout(barmode="group",yaxis=list(title="Avg Price (₹)")))
    })

    output$seasonal_demand <- plotly::renderPlotly({
      df <- df_r(); sd <- aggregate(Demand~Season+Category,df,mean)
      p <- plotly::plot_ly()
      for (i in seq_along(unique(sd$Category))) {
        cat <- unique(sd$Category)[i]; sub <- sd[sd$Category==cat,]
        p <- plotly::add_lines(p,x=sub$Season,y=sub$Demand,name=cat,line=list(color=COLORS[i%%8+1],width=1.5),marker=list(size=5))
      }
      pb(p %>% plotly::layout(xaxis=list(title="Season",categoryorder="array",categoryarray=c("Spring","Summer","Autumn","Winter")),yaxis=list(title="Avg Demand")))
    })

    output$inventory_demand <- plotly::renderPlotly({
      df <- df_r(); df$S <- ifelse(df$Inventory<df$Demand*7,"Low Stock","Adequate")
      pb(plotly::plot_ly(df,x=~Demand,y=~Inventory,color=~S,colors=c("Low Stock"="#EF4444","Adequate"="#10B981"),
        type="scatter",mode="markers",marker=list(size=5,opacity=0.6),
        hovertemplate="Demand: %{x}<br>Inventory: %{y}<extra></extra>"
      ) %>% plotly::layout(xaxis=list(title="Demand"),yaxis=list(title="Inventory")))
    })

    output$heatmap <- plotly::renderPlotly({
      df <- df_r(); df$MN <- format(df$Date,"%b")
      heat <- aggregate(Revenue~MN+Category,df,sum)
      hw <- reshape(heat,idvar="Category",timevar="MN",direction="wide"); hw[is.na(hw)] <- 0
      mc <- names(hw)[-1]; zm <- as.matrix(hw[,mc])
      pb(plotly::plot_ly(x=gsub("Revenue\\.","",mc),y=hw$Category,z=zm,type="heatmap",
        colorscale=list(c(0,"#0D1117"),c(0.5,"#1e3a5f"),c(1,"#2563EB")),
        hovertemplate="Month: %{x}<br>Category: %{y}<br>₹%{z:,.0f}<extra></extra>"))
    })

    output$price_competition <- plotly::renderPlotly({
      df <- df_r()
      pb(plotly::plot_ly(df,x=~Competitor_Price,y=~Selling_Price,color=~Category,colors=COLORS,
        type="scatter",mode="markers",marker=list(size=5,opacity=0.6),
        hovertemplate="Comp: ₹%{x:,.0f}<br>Ours: ₹%{y:,.0f}<extra></extra>"
      ) %>% plotly::layout(xaxis=list(title="Competitor (₹)"),yaxis=list(title="Our Price (₹)"),
        shapes=list(list(type="line",x0=min(df$Competitor_Price,na.rm=T),x1=max(df$Competitor_Price,na.rm=T),
          y0=min(df$Competitor_Price,na.rm=T),y1=max(df$Competitor_Price,na.rm=T),
          line=list(color="rgba(255,255,255,0.12)",dash="dash",width=1)))))
    })

    output$margin_dist <- plotly::renderPlotly({
      df <- df_r()
      pb(plotly::plot_ly(df,x=~Margin,color=~Category,colors=COLORS,type="box",boxpoints=FALSE,
        hovertemplate="%{x:.1f}%<extra></extra>"
      ) %>% plotly::layout(xaxis=list(title="Profit Margin (%)",ticksuffix="%"),showlegend=FALSE))
    })

    output$correlation <- plotly::renderPlotly({
      df <- df_r()
      nc <- intersect(c("Cost_Price","Selling_Price","Demand","Revenue","Inventory","Competitor_Price"),names(df))
      cm <- round(cor(df[,nc],use="pairwise.complete.obs"),2)
      pb(plotly::plot_ly(x=colnames(cm),y=rownames(cm),z=cm,type="heatmap",
        colorscale=list(c(0,"#EF4444"),c(0.5,"#0D1117"),c(1,"#2563EB")),zmin=-1,zmax=1,
        text=matrix(as.character(cm),nrow=nrow(cm)),texttemplate="%{text}",
        hovertemplate="%{x} × %{y}<br>r = %{z:.2f}<extra></extra>"))
    })
  })
}
