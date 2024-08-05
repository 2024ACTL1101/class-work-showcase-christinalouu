
## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```r
# Load data from CSV file
amd_df <- read.csv("AMD.csv")
# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)
amd_df <- amd_df[, c("date", "close")]
```

#### Plotting the Data
Plot the closing prices over time to visualize the price movement.
```r
plot(xlab = 'Date', ylab = 'Close Price', main = 'AMD Stock Closing Prices Over Time', amd_df$date, amd_df$close,'l')
```

### Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```r
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
avg_purchase_price <- 0

for (i in 1:nrow(amd_df)) {
    if (previous_price == 0) {     # If previous price = 0
        amd_df[i, "trade_type"] <- "buy"
        amd_df[i, "costs_proceeds"] <- -amd_df[i, "close"] * share_size
        accumulated_shares <- 100    
    } else if (amd_df[i, "close"] < previous_price) {    # If price of the current day is less than that of the previous day
        amd_df[i, "trade_type"] <- "buy"
        amd_df[i, "costs_proceeds"] <- -amd_df[i, "close"] * share_size
        accumulated_shares <- accumulated_shares + share_size
    } else if (i == nrow(amd_df)) {    # Last day of trading
        amd_df[i, "trade_type"] <- "sell"
        amd_df[i, "costs_proceeds"] <- accumulated_shares * amd_df[i, "close"]
        break
    } 
    previous_price <- amd_df[i, "close"]
    amd_df[i, "accumulated_shares"] = accumulated_shares
}
print(amd_df)
```


### Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```r

# Chosen Trading Period of 2022
start_date <- as.Date('2022-01-03')
end_date <- as.Date('2022-12-30')


restricted_df <- subset(amd_df, date >= start_date & date <= end_date)
rownames(restricted_df) <- NULL
restricted_df$trade_type <- NA
restricted_df$costs_proceeds <- NA
restricted_df$accumulated_shares <- 0

plot(restricted_df$date, restricted_df$close, type = 'l', xlab = 'Date', ylab = 'Close Price', main = 'AMD Stock Closing Prices Over Time')
```


### Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```r
restricted_df$trade_type <- NA
restricted_df$costs_proceeds <- NA
restricted_df$accumulated_shares <- 0 

previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(restricted_df)) {
    # If previous price = 0
    if (previous_price == 0) {
        restricted_df[i, "trade_type"] <- "buy"
        restricted_df[i, "costs_proceeds"] <- -restricted_df[i, "close"] * share_size
        accumulated_shares <- 100    
    # If price of the current day is less than that of the previous day
    } else if (restricted_df[i, "close"] < previous_price) {
        restricted_df[i, "trade_type"] <- "buy"
        restricted_df[i, "costs_proceeds"] <- -restricted_df[i, "close"] * share_size
        accumulated_shares <- accumulated_shares + share_size
    } 
    # Last day of trading
    if (i == nrow(restricted_df)) {
        restricted_df[i, "trade_type"] <- "sell"
        restricted_df[i, "costs_proceeds"] <- accumulated_shares * restricted_df[i, "close"]
        accumulated_shares <- 0
    } 
    previous_price <- restricted_df[i, "close"]
    restricted_df[i, "accumulated_shares"] <- accumulated_shares
}

# Total Profit/Loss Calculation
profit_loss <- sum(restricted_df$costs_proceeds, na.rm = TRUE)
print(restricted_df)
cat("Total Profit/Loss:", profit_loss, "\n")

# Invested Capital
capital <- 0 
for (i in 1:nrow(restricted_df)) {
    if (restricted_df[i, "trade_type"] == "buy" && !is.na(restricted_df[i, "trade_type"])) {
        capital <- capital + restricted_df[i, "costs_proceeds"]
    }
}
capital <- -capital
cat("Invested Capital:", capital, "\n")

# Return On Investment
cat("Return on Investment (ROI) (%):", profit_loss / capital * 100, "\n")
```

### Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```r
# Stop-Loss Mechanism Implementation
# Activates if the stock falls by 30% from the average purchase price
total_cost <- 0 # Total cost of shares purchased
stop_loss <- 0.7 # When stock falls 30% from the average purchase price
accumulated_shares <- 0

for (i in 1:nrow(restricted_df)) {
  if (accumulated_shares > 0) {
    avg_purchase_price <- total_cost / accumulated_shares
  } 
  
  if (restricted_df[i, "close"] < (stop_loss * avg_purchase_price)) {    # If Stop-Loss mechanism activates, set trade_type to "sell"
    restricted_df[i, "trade_type"] <- "sell"
    shares_to_sell <- accumulated_shares / 2
    restricted_df[i, "costs_proceeds"] <- -restricted_df[i, "close"] * shares_to_sell
    accumulated_shares <- accumulated_shares - shares_to_sell
    total_cost <- total_cost - shares_to_sell * avg_purchase_price
  } else if (previous_price == 0) {    # If previous price = 0, set trade_type to "buy"
      restricted_df[i, "trade_type"] <- "buy"
      restricted_df[i, "costs_proceeds"] <- -restricted_df[i, "close"] * share_size
      accumulated_shares <- 100  
      total_cost <- total_cost - restricted_df[i, "costs_proceeds"]
  } else if (restricted_df[i, "close"] < previous_price) {    # If price of the current day is less than that of the previous day, set trade_type to "buy"
      restricted_df[i, "trade_type"] <- "buy"
      restricted_df[i, "costs_proceeds"] <- -restricted_df[i, "close"] * share_size
      accumulated_shares <- accumulated_shares + share_size
      total_cost <- total_cost - restricted_df[i, "costs_proceeds"]
  } 
  
  if (i == nrow(restricted_df)) {    # Last day of trading
      restricted_df[i, "trade_type"] <- "sell"
      restricted_df[i, "costs_proceeds"] <- accumulated_shares * restricted_df[i, "close"]
      accumulated_shares <- 0
  } 
    previous_price <- restricted_df[i, "close"]
    restricted_df[i, "accumulated_shares"] <- accumulated_shares
}

print(restricted_df)
```


### Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```r
# For the 2022 trading period when the Stop-Loss mechanism is activated
for (i in 1:nrow(restricted_df)) {
  if (accumulated_shares > 0) {
    avg_purchase_price <- total_cost / accumulated_shares
  } 
  
  if (restricted_df[i, "close"] < (stop_loss * avg_purchase_price)) {    # If Stop-Loss mechanism activates, set trade_type to "sell"
    restricted_df[i, "trade_type"] <- "sell"
    shares_to_sell <- accumulated_shares / 2
    restricted_df[i, "costs_proceeds"] <- -restricted_df[i, "close"] * shares_to_sell
    accumulated_shares <- accumulated_shares - shares_to_sell
    total_cost <- total_cost - shares_to_sell * avg_purchase_price
  } else if (previous_price == 0) {    # If previous price = 0, set trade_type to "buy"
      restricted_df[i, "trade_type"] <- "buy"
      restricted_df[i, "costs_proceeds"] <- -restricted_df[i, "close"] * share_size
      accumulated_shares <- 100  
      total_cost <- total_cost - restricted_df[i, "costs_proceeds"]
  } else if (restricted_df[i, "close"] < previous_price) {    # If price of the current day is less than that of the previous day, set trade_type to "buy"
      restricted_df[i, "trade_type"] <- "buy"
      restricted_df[i, "costs_proceeds"] <- -restricted_df[i, "close"] * share_size
      accumulated_shares <- accumulated_shares + share_size
      total_cost <- total_cost - restricted_df[i, "costs_proceeds"]
  } 
  
  if (i == nrow(restricted_df)) {    # Last day of trading
      restricted_df[i, "trade_type"] <- "sell"
      restricted_df[i, "costs_proceeds"] <- accumulated_shares * restricted_df[i, "close"]
      accumulated_shares <- 0
  } 
    previous_price <- restricted_df[i, "close"]
    restricted_df[i, "accumulated_shares"] <- accumulated_shares
}

# Initial Profit/Loss at the start of the trading period
initial_PL <- restricted_df$costs_proceeds[1]
cat("Initial Profit/Loss:", initial_PL, "\n")

# Total Profit/Loss Calculation
profit_loss <- sum(restricted_df$costs_proceeds, na.rm = TRUE)
print(restricted_df)

cat("Total Profit/Loss:", profit_loss, "\n")

# Invested Capital
capital <- 0 
for (i in 1:nrow(restricted_df)) {
    if (restricted_df[i, "trade_type"] == "buy" && !is.na(restricted_df[i, "trade_type"])) {
        capital <- capital + restricted_df[i, "costs_proceeds"]
    }
}
capital <- -capital

# Return On Investment
cat("Return on Investment (ROI) (%):", profit_loss / capital * 100, "\n")
```


During the 2022 period, AMD experienced a significant decline in both Profit/Loss and Return on Investment (ROI), with figures falling to -\$1, 656, 584 and -148.7304%, respectively. This was driven by macroeconomic factors such as inflation and geopolitical tensions. The Reserve Bank of Australia (RBA) reported that inflation increased to 7.8% in December from 3.5% the previous year, largely due to rising interest rates during the global pandemic, supply chain disruptions and increased demand for goods and services.

Disruptions in the supply chain and global conflicts, such as the Russia-Ukraine war and the collapse of Brazil’s coal mines, significantly raised the costs of electricity and production inputs. The geopolitical tensions following the Russian invasion resulted in many countries turning to other economies, like Saudi Arabia, for oil and fuel. As a result, in 2022, fuel prices rose by 35% throughout the year, contributing to cost-push inflationary pressures as firms sought to preserve profit margins by passing these increased costs onto consumers.

For AMD, higher fuel and oil prices increased costs of production and transportation of the manufacturing of semiconductor devices used in computer processors. This increased operational costs, combined with reduced consumer purchasing power, deteriorated investor confidence, as evident by the drastic drop in AMD's share price from \$150.24 to \$64.77 by year-end.

Hence, for AMD, these economic factors resulted in the drop in share price, worsening total profit/loss and ROI as the company was forced to sell half its holdings when the Stop-Loss mechanism was activated, leading to an unfavourable financial position.




