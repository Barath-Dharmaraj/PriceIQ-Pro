# ============================================================
# PriceIQ Pro — Package Installer
# Run this script once before launching the app
# ============================================================

cat("Installing PriceIQ Pro dependencies...\n\n")

packages <- c(
  "shiny",
  "shinydashboard",
  "bslib",
  "plotly",
  "ggplot2",
  "dplyr",
  "randomForest",
  "forecast",
  "caret",
  "DT",
  "tidyverse",
  "lubridate",
  "scales"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(paste0("  Installing: ", pkg, "\n"))
    install.packages(pkg, repos = "https://cran.r-project.org", quiet = TRUE)
  } else {
    cat(paste0("  ✓ Already installed: ", pkg, "\n"))
  }
}

for (pkg in packages) {
  tryCatch(
    install_if_missing(pkg),
    error = function(e) cat(paste0("  ✗ Failed to install: ", pkg, " — ", e$message, "\n"))
  )
}

cat("\n✅ All packages processed. You can now run: shiny::runApp('app.R')\n")
