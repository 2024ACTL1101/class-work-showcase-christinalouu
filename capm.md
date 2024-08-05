
# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```r
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```r
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

$$
E(R_i) = R_f + \beta_i (E(R_m) - R_f)
$$

Where:

- $E(R_i)$ is the expected return on the capital asset,
- $R_f$ is the risk-free rate,
- $\beta_i$ is the beta of the security, which represents the systematic risk of the security,
- $E(R_m)$ is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
  
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```r
# Calculate the daily returns for AMD and the S&P 500
AMD_Return <- (df$AMD - lag(df$AMD, 1)) / lag(df$AMD, 1)
GSPC_Return <- (df$GSPC - lag(df$GSPC, 1)) / lag(df$GSPC, 1)

# Add the daily return values as new columns of the dataframe 'df'
df$AMD_Return <- AMD_Return
df$GSPC_Return <- GSPC_Return
```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
  
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```r
df$Daily_RF <- (1 + df$RF / 100)^(1 / 360) - 1
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```r
# Create new columns in the dataframe for the excess returns
df$Excess_AMD_Returns <- df$AMD_Return - df$Daily_RF
df$Excess_GSPC_Returns <- df$GSPC_Return - df$Daily_RF
```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```r
model <- lm(Excess_AMD_Returns ~ Excess_GSPC_Returns, data = df)
summary(model)
```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:**
From the analysis above, the $\beta$ value of AMD relative to the S&P 500 is the coefficient of the regression line and measures the sensitivity of the stock's returns to fluctuations in the market. The beta value of 1.5699987 indicates that AMD is more volatile than the market as it is greater than a value of 1. This suggests that AMD is expected to move 1.5699987 times as much as the market and indicates that AMD shares are more risky but are also likely to give more rewards.

#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```r
ggplot(df, aes(x = Excess_GSPC_Returns, y = Excess_AMD_Returns)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Relationship between AMD and S&P Excess Returns",
       x = "Excess Returns of S&P", 
       y = "Excess Returns of AMD") + 
         theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) 
```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.



**Answer:**

```r
# Calculate the number of observations in the Excess_GSPC_Returns column
n <- length(na.omit(df$'Excess_GSPC_Returns'))                
RF_rate <- 0.05     # Given Risk-free rate 
annual_expected_return <- 0.133
daily_excess_return <- (annual_expected_return - RF_rate) / 252

# Calculate the mean of 'Excess_GSPC_Returns 
mean <- mean(df$'Excess_GSPC_Returns', na.rm = TRUE)

# Calculate the sum of squared differences between 'Excess_AMD_Returns' and the mean
SST <- sum((df$'Excess_AMD_Returns' - mean)^2, na.rm = TRUE)

# Calculate the standard error of the residuals with (n-2) degrees of freedom
se <- sqrt((sum((residuals(model))^2))/(n-2))

# Calculate the standard error of the forecast, accounting for the prediction interval
se_forecast <- se * sqrt((1 + (1/n) + (daily_excess_return - mean)^2 / SST))

# Annualise the standard error of the forecast
se_forecast <- se_forecast * sqrt(252)

# Set the alpha (significance level) to 10%
alpha <- 0.10  

# Calculate the critical t-value with (n-2) degrees of freedom
t_value <- qt(1 - alpha/2, df = n - 2)

# Calculate the expected return using the CAPM
capm_return <- 0.05 + 1.5699987 * (annual_expected_return - RF_rate)

# Calculate the lower and upper bound of the confidence interval for the CAPM return
lower_bound <- capm_return - t_value * se_forecast
upper_bound <- capm_return + t_value * se_forecast

print(lower_bound)
print(upper_bound)
```
