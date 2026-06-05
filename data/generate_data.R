# PriceIQ Pro - Synthetic Dataset Generator
# Generates realistic retail pricing data for demonstration

set.seed(42)

n <- 500

categories <- c("Electronics", "Fashion", "Grocery", "Home Appliances", 
                 "Beauty", "Sports", "Books", "Accessories")

seasons <- c("Spring", "Summer", "Autumn", "Winter")

products <- list(
  Electronics    = c("Smartphone X12", "Laptop Pro", "Wireless Earbuds", "Smart Watch", "Tablet Z", "Gaming Console", "Bluetooth Speaker", "LED Monitor"),
  Fashion        = c("Designer Jacket", "Denim Jeans", "Summer Dress", "Formal Shirt", "Sneakers Pro", "Leather Bag", "Silk Scarf", "Winter Coat"),
  Grocery        = c("Organic Rice 5kg", "Premium Coffee", "Olive Oil 1L", "Basmati Rice", "Green Tea Pack", "Protein Granola", "Almond Butter", "Coconut Milk"),
  `Home Appliances` = c("Air Purifier", "Robot Vacuum", "Instant Pot", "Coffee Maker", "Stand Mixer", "Air Fryer", "Induction Cooktop", "Water Purifier"),
  Beauty         = c("Vitamin C Serum", "Moisturizer SPF50", "Hair Growth Oil", "Perfume Luxe", "Foundation Pro", "Lipstick Set", "Eye Cream", "Body Lotion"),
  Sports         = c("Yoga Mat Pro", "Resistance Bands", "Dumbbell Set", "Running Shoes", "Fitness Tracker", "Protein Powder", "Jump Rope", "Cycling Helmet"),
  Books          = c("Data Science Handbook", "Python ML Guide", "Business Strategy", "Self Help Master", "History of AI", "Finance Freedom", "Leadership 101", "Creative Thinking"),
  Accessories    = c("Sunglasses UV400", "Leather Wallet", "Smart Ring", "Travel Pillow", "Phone Case Pro", "Desk Organizer", "Minimalist Watch", "Key Finder")
)

cost_ranges <- list(
  Electronics    = c(5000, 80000),
  Fashion        = c(500, 15000),
  Grocery        = c(100, 2000),
  `Home Appliances` = c(2000, 50000),
  Beauty         = c(200, 5000),
  Sports         = c(300, 10000),
  Books          = c(150, 800),
  Accessories    = c(200, 8000)
)

# Generate dates over 2 years
dates <- seq(as.Date("2023-01-01"), as.Date("2024-12-31"), by = "day")
sample_dates <- sample(dates, n, replace = TRUE)

category_vec <- sample(categories, n, replace = TRUE, 
                        prob = c(0.20, 0.15, 0.12, 0.15, 0.10, 0.10, 0.08, 0.10))

# Build dataset
data <- data.frame(
  Product_ID = paste0("PRD-", sprintf("%04d", 1:n)),
  stringsAsFactors = FALSE
)

data$Category <- category_vec

data$Product_Name <- mapply(function(cat) {
  prods <- products[[cat]]
  sample(prods, 1)
}, data$Category)

data$Cost_Price <- mapply(function(cat) {
  range_vals <- cost_ranges[[cat]]
  round(runif(1, range_vals[1], range_vals[2] * 0.7), 0)
}, data$Category)

# Selling price is 20-80% markup over cost
data$Selling_Price <- round(data$Cost_Price * runif(n, 1.2, 1.8), 0)

data$Competitor_Price <- round(data$Selling_Price * runif(n, 0.85, 1.15), 0)

data$Season <- sapply(sample_dates, function(d) {
  m <- as.integer(format(d, "%m"))
  if (m %in% c(3,4,5)) "Spring"
  else if (m %in% c(6,7,8)) "Summer"
  else if (m %in% c(9,10,11)) "Autumn"
  else "Winter"
})

# Demand influenced by season, price competitiveness
season_multiplier <- ifelse(data$Season == "Winter", 1.3,
                     ifelse(data$Season == "Summer", 1.2,
                     ifelse(data$Season == "Spring", 1.1, 1.0)))

price_ratio <- data$Competitor_Price / data$Selling_Price
demand_base <- runif(n, 20, 300)
data$Demand <- pmax(1, round(demand_base * season_multiplier * price_ratio * runif(n, 0.85, 1.15), 0))

data$Revenue <- data$Selling_Price * data$Demand
data$Date <- format(sample_dates, "%Y-%m-%d")
data$Inventory <- round(data$Demand * runif(n, 0.8, 2.5), 0)

# Reorder columns
data <- data[, c("Product_ID", "Product_Name", "Category", "Cost_Price", 
                  "Selling_Price", "Demand", "Revenue", "Date", 
                  "Inventory", "Competitor_Price", "Season")]

write.csv(data, "/home/claude/PriceIQ-Pro/data/sample_dataset.csv", row.names = FALSE)
cat("Dataset generated:", nrow(data), "rows\n")
