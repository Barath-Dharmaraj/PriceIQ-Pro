# PriceIQ Pro

**Enterprise Dynamic Pricing & Revenue Optimization Platform**

Built entirely in R using Shiny — combines machine learning demand prediction, ARIMA time-series forecasting, and mathematical price optimization into a single production-ready analytics dashboard.

![R](https://img.shields.io/badge/R-4.2%2B-276DC3?style=flat-square&logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-1.7%2B-4A90D9?style=flat-square)
![License](https://img.shields.io/badge/License-MIT-22C55E?style=flat-square)
![Status](https://img.shields.io/badge/Status-Production%20Ready-22C55E?style=flat-square)

---

## Overview

PriceIQ Pro helps retail businesses stop guessing on pricing. It ingests historical sales data, trains a Random Forest model on demand signals, forecasts future revenue using ARIMA, and then runs a price scenario optimizer to find the exact price point that maximizes revenue or profit.

The UI is built with a professional dark theme — clean, minimal, inspired by tools like Vercel, Linear, and Bloomberg Terminal.

---

## Features

| Module | Description |
|--------|-------------|
| **Executive Dashboard** | CEO-level KPIs, revenue trend, category mix, seasonal performance |
| **Data Management** | CSV upload, auto-validation, data cleaning, quality report |
| **Exploratory Analytics** | 16 interactive Plotly charts across revenue, category, seasonal and pricing tabs |
| **Demand Prediction** | Random Forest with feature importance, R² / RMSE / MAE metrics, confidence intervals |
| **Time Series Forecasting** | ARIMA with 80% and 95% confidence bands, 7 / 30 / 90 day horizons |
| **Price Optimizer** | Elasticity-based scenario engine, finds revenue-max and profit-max price points |
| **What-If Simulator** | Real-time sliders — instantly recalculates demand, revenue, profit, margin |
| **AI Insights** | Automated strategic recommendations — stock risks, pricing gaps, growth signals |

---

## Tech Stack

- **Language** — R 4.2+
- **Framework** — Shiny + shinydashboard
- **ML** — randomForest (demand prediction)
- **Forecasting** — forecast / auto.arima (time series)
- **Visualization** — plotly, ggplot2
- **Tables** — DT
- **Data** — dplyr, lubridate, scales

---

## Project Structure

```
PriceIQ-Pro/
├── app.R                    # Main entry point
├── install_packages.R       # One-time dependency installer
├── requirements.txt         # Package list
├── data/
│   └── sample_dataset.csv   # 500-row synthetic retail dataset
├── models/
│   ├── random_forest_model.rds   # Saved after training
│   └── arima_model.rds           # Saved after forecasting
├── modules/
│   ├── dashboard_module.R
│   ├── data_management.R
│   ├── eda_module.R
│   ├── prediction_module.R
│   ├── forecasting_module.R
│   ├── optimization_module.R
│   └── insights_module.R
└── www/
    └── custom.css           # Full dark theme
```

---

## Getting Started

### 1. Clone the repository

```bash
git clone https://github.com/YOUR_USERNAME/PriceIQ-Pro.git
cd PriceIQ-Pro
```

### 2. Install dependencies

```r
source("install_packages.R")
```

### 3. Run the app

```r
shiny::runApp("app.R")
```

The app auto-loads the sample dataset on startup. No extra steps needed.

---

## Dataset

The built-in dataset contains 500 synthetic retail records across 8 categories:

`Electronics · Fashion · Grocery · Home Appliances · Beauty · Sports · Books · Accessories`

| Column | Type | Description |
|--------|------|-------------|
| Product_ID | String | Unique SKU |
| Product_Name | String | Product name |
| Category | Factor | Product category |
| Cost_Price | Numeric | Wholesale cost (₹) |
| Selling_Price | Numeric | Retail price (₹) |
| Demand | Integer | Units sold |
| Revenue | Numeric | Selling_Price × Demand |
| Date | Date | Transaction date |
| Inventory | Integer | Stock on hand |
| Competitor_Price | Numeric | Market competitor price (₹) |
| Season | Factor | Spring / Summer / Autumn / Winter |

You can also upload your own CSV — just match the column names above.

---

## How the Price Optimizer Works

The optimizer uses a price elasticity demand model:

```
Demand_new = Demand_base × (Price_new / Price_current) ^ elasticity
```

It generates up to 200 price scenarios between a configurable min/max range, calculates revenue and profit at each point, and identifies the peak. The result is a clear recommendation:

```
Current Price: ₹1,200  →  Recommended Price: ₹1,390
Revenue Increase: +18.5%
Profit Increase: +12.7%
```

Elasticity is configurable:
- `-1.0` → Unit elastic (standard)
- `-2.0` → Highly elastic (electronics, fashion)
- `-0.5` → Inelastic (groceries, essentials)

---

## Screenshots

> Add screenshots to a `/screenshots` folder after running the app

| Screen | Description |
|--------|-------------|
| `screenshots/dashboard.png` | Executive Dashboard |
| `screenshots/eda.png` | Exploratory Analytics |
| `screenshots/ml.png` | Demand Prediction metrics |
| `screenshots/optimizer.png` | Price optimization result |
| `screenshots/whatif.png` | What-If Simulator |

---

## Skills Demonstrated

This project was built to showcase a full-stack data science engineering skillset:

- **Machine Learning** — Random Forest regression, hyperparameter config, feature importance
- **Time Series Analysis** — ARIMA with automatic order selection, seasonal decomposition
- **Data Visualization** — 20+ interactive Plotly charts, custom theming
- **Software Architecture** — Modular Shiny app with shared reactive state across 8 modules
- **UI Engineering** — Custom CSS dark theme, professional SaaS design system
- **Business Intelligence** — KPI dashboards, automated insight generation
- **Revenue Science** — Price elasticity modeling, scenario-based optimization

---

## License

MIT License — free to use, modify, and distribute.

---

## Author

Built by **Barath** as a portfolio project demonstrating enterprise R Shiny development, machine learning, and business intelligence in a single production-ready application.

> *Connect on [LinkedIn](https://linkedin.com) | [GitHub](https://github.com/YOUR_USERNAME)*
