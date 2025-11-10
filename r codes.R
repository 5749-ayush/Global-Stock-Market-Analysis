#-----------------------------------------------------------------------
# SECTION 0: SETUP AND LIBRARY LOADING
#-----------------------------------------------------------------------
# All necessary libraries are loaded here at the beginning.

library(dplyr)
library(data.table)
library(psych)
library(tidyr)
library(ggplot2)
library(corrplot)
library(forecast)
library(tseries)
library(lmtest)
library(vars)
library(urca)

#-----------------------------------------------------------------------
# SECTION 1: DATA LOADING, CLEANING, AND PREPARATION
#-----------------------------------------------------------------------

# --- 1.1. Read the Data ---
# Ensure these file paths are correct for your computer.
sp500 <- fread("C:/Users/ayush/OneDrive/文档/documents/r_folder/S&P 500 Historical Data.csv")
nikkei <- fread("C:/Users/ayush/OneDrive/文档/documents/r_folder/Nikkei 225 Historical Data.csv")
ftse <- fread("C:/Users/ayush/OneDrive/文档/documents/r_folder/FTSE 100 Historical Data.csv")
dax <- fread("C:/Users/ayush/OneDrive/文档/documents/r_folder/DAX Historical Data.csv")
asx <- fread("C:/Users/ayush/OneDrive/文档/documents/r_folder/S&P_ASX 200 Historical Data.csv")

# --- 1.2. Data Cleaning Function ---
# This function cleans and calculates log returns for each dataset.
clean_and_filter_data <- function(data, name) {
  data %>%
    mutate(Date = as.Date(Date, format="%d-%m-%Y"),
           Price = as.numeric(gsub(",", "", Price))) %>%
    filter(Date >= as.Date("2018-01-01") & Date <= as.Date("2024-12-31")) %>%
    arrange(Date) %>%
    filter(!is.na(Date) & !is.na(Price)) %>%
    distinct() %>%
    # Using transmute to create the final columns needed
    transmute(Date,
              !!paste0("Price_", name) := Price,
              !!paste0("Return_", name) := c(NA, diff(log(Price))))
}

# --- 1.3. Apply the Cleaning Function to ALL Datasets ---
# *** CORRECTION: Added the missing cleaning step for ASX data ***
sp500_clean <- clean_and_filter_data(sp500, "SP500")
nikkei_clean <- clean_and_filter_data(nikkei, "Nikkei225")
ftse_clean <- clean_and_filter_data(ftse, "FTSE100")
dax_clean <- clean_and_filter_data(dax, "DAX")
asx_clean <- clean_and_filter_data(asx, "ASX200") # This line was missing

# --- 1.4. Combine ALL Datasets into Final Data Frames ---
# *** CORRECTION: Combined all five indices to prevent errors later. ***
all_data_combined <- sp500_clean %>%
  inner_join(ftse_clean, by = "Date") %>%
  inner_join(dax_clean, by = "Date") %>%
  inner_join(nikkei_clean, by = "Date") %>%
  inner_join(asx_clean, by = "Date") # This join was missing

# Create the final, clean data frames for prices and returns.
# These will be the ONLY data frames used for the rest of the analysis.
all_prices <- all_data_combined %>% dplyr::select(Date, starts_with("Price")) %>% na.omit()
all_returns <- all_data_combined %>% dplyr::select(Date, starts_with("Return")) %>% na.omit()

cat("Final clean dataset has", nrow(all_returns), "observations from", 
    min(all_returns$Date), "to", max(all_returns$Date), "\n")


#-----------------------------------------------------------------------
# SECTION 2: DESCRIPTIVE STATISTICS AND VISUALIZATION
#-----------------------------------------------------------------------

# --- 2.1. Summary Statistics ---
descriptive_stats <- describe(all_returns[, -1]) # Using describe() from the 'psych' package
print("--- Descriptive Statistics of Daily Returns ---")
print(descriptive_stats)

# --- 2.2. Reshape Data for ggplot2 ---
long_prices <- all_prices %>% gather(key = "Index", value = "Price", -Date)
long_returns <- all_returns %>% gather(key = "Index", value = "Return", -Date)

# --- 2.3. Visualization Plots ---

# GRAPH 1: Historical Price Series
price_plot <- ggplot(long_prices, aes(x = Date, y = Price, color = Index)) +
  geom_line(linewidth = 0.8) +
  labs(title = "Historical Closing Prices (2018-2024)", x = "Year", y = "Index Price Level") +
  theme_bw() + theme(legend.position = "bottom", legend.title = element_blank())
print(price_plot)

# GRAPH 2: Daily Returns (Volatility Clustering)
returns_plot <- ggplot(long_returns, aes(x = Date, y = Return)) +
  geom_line(aes(color = Index), alpha = 0.7, linewidth = 0.5) +
  facet_wrap(~Index, scales = "free_y", ncol = 1) +
  labs(title = "Daily Log Returns (2018-2024)", x = "Year", y = "Log Return") +
  theme_bw() + theme(legend.position = "none")
print(returns_plot)

# GRAPH 3: Distribution of Returns
dist_plot <- ggplot(long_returns, aes(x = Return)) +
  geom_histogram(aes(y = ..density..), bins = 100, fill = "lightblue", color = "grey") +
  geom_density(color = "darkred", linewidth = 1) +
  facet_wrap(~Index, scales = "free") +
  labs(title = "Distribution of Daily Returns", x = "Log Return", y = "Density") +
  theme_bw()
print(dist_plot)

# GRAPH 4: Correlation Matrix Heatmap
cor_matrix <- cor(all_returns[, -1])
print("--- Correlation Matrix of Returns ---")
print(round(cor_matrix, 3))
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         title = "\nCorrelation Matrix of Daily Returns", mar=c(0,0,1,0))


#-----------------------------------------------------------------------------
# SECTION 3: COMPREHENSIVE HYPOTHESIS TESTING
#-----------------------------------------------------------------------------
# This section tests eight formal hypotheses about the behavior of the
# stock market indices using t-tests, ANOVA, and Chi-Square tests.
#-----------------------------------------------------------------------------

# --- Data Preparation for Hypothesis Tests ---
# This code creates necessary helper columns (like Weekday, Month) and data subsets.
library(lubridate) # Helps with dates

# Create a new dataframe with date components
returns_with_dates <- all_returns %>%
  mutate(
    Weekday = wday(Date, label = TRUE, week_start = 1), # Mon=1
    Month = month(Date, label = TRUE)
  )

# Create subsets for the Pre-COVID vs. During-COVID analysis
pre_covid_returns <- filter(returns_with_dates, Date >= "2018-01-01" & Date <= "2019-12-31")
during_covid_returns <- filter(returns_with_dates, Date >= "2020-01-01" & Date <= "2020-12-31")


#-----------------------------------------------------------------------
# HYPOTHESIS 1: T-TEST (S&P 500 vs. DAX Mean Return)
#-----------------------------------------------------------------------
# Question: Is there a significant difference in the average daily return between the US and German markets?
# H0: The mean daily returns are equal. (μ_SP500 = μ_DAX)
# H1: The mean daily returns are not equal.
cat("\n\n--- Hypothesis 1: T-Test (S&P 500 vs. DAX Mean Return) ---\n")
t_test_sp500_dax <- t.test(all_returns$Return_SP500, all_returns$Return_DAX)
print(t_test_sp500_dax)


#-----------------------------------------------------------------------
# HYPOTHESIS 2: T-TEST (S&P 500 Pre-COVID vs. During-COVID)
#-----------------------------------------------------------------------
# Question: Did the average daily performance of the S&P 500 change during the pandemic?
# H0: The mean daily return was the same before and during COVID. (μ_pre = μ_during)
# H1: The mean daily returns were not equal.
cat("\n\n--- Hypothesis 2: T-Test (S&P 500 Pre-COVID vs. During-COVID) ---\n")
t_test_covid <- t.test(pre_covid_returns$Return_SP500, during_covid_returns$Return_SP500)
print(t_test_covid)


#-----------------------------------------------------------------------
# HYPOTHESIS 3 (NEW): T-TEST (Comparing Mean Volatility)
#-----------------------------------------------------------------------
# Question: Is there a significant difference in the average daily volatility (price movement)
# between the Japanese (Nikkei) and Australian (ASX) markets?
# We measure volatility using the mean of absolute daily returns.
#
# H0: The mean absolute daily return (volatility) of the Nikkei is EQUAL to that of the ASX.
# H1: The mean absolute daily returns (volatility) are NOT equal.
#
# What will happen: This test tells us if one market is inherently more "jumpy" or
# volatile on a day-to-day basis than the other, regardless of direction.

cat("\n\n--- Hypothesis 3 (NEW): T-Test (Nikkei vs. ASX Mean Volatility) ---\n")
t_test_volatility <- t.test(abs(all_returns$Return_Nikkei225), abs(all_returns$Return_ASX200))
print(t_test_volatility)

# --- How to Interpret in Your Report ---
# "To compare the average daily volatility between markets, a two-sample t-test was
# performed on the absolute daily returns of the Nikkei 225 and the S&P/ASX 200.
# The p-value was [insert p-value]. Since this is [less than / greater than] 0.05, we
# [reject / fail to reject] the null hypothesis. This provides evidence that there
# [is / is not] a statistically significant difference in the average magnitude of
# daily price movements between the Japanese and Australian markets."


#-----------------------------------------------------------------------
# HYPOTHESIS 4: ANOVA (Nikkei "Day-of-the-Week" Effect)
#-----------------------------------------------------------------------
# Question: Is there a "Day of the Week" effect on the Nikkei's returns?
# H0: The mean daily return is the same for all five weekdays.
# H1: At least one day has a different mean return.
cat("\n\n--- Hypothesis 4: ANOVA (Nikkei 225 Day-of-the-Week Effect) ---\n")
anova_weekday <- aov(Return_Nikkei225 ~ Weekday, data = returns_with_dates)
print(summary(anova_weekday))


#-----------------------------------------------------------------------
# HYPOTHESIS 5 (NEW): ANOVA (Monthly Seasonality)
#-----------------------------------------------------------------------
# Question: Does the German market (DAX) exhibit monthly seasonality (e.g., a "January Effect")?
# In other words, is the average daily return different depending on the month of the year?
#
# H0: The mean daily return of the DAX is the SAME for all twelve months.
# H1: At least one month has a different mean daily return.
#
# What will happen: This tests for well-known calendar anomalies. A significant result
# suggests that systematic, predictable patterns may exist in the DAX's performance
# based on the time of year.

cat("\n\n--- Hypothesis 5 (NEW): ANOVA (DAX Monthly Seasonality Effect) ---\n")
anova_month <- aov(Return_DAX ~ Month, data = returns_with_dates)
print(summary(anova_month))

# If the ANOVA p-value (Pr(>F)) is significant, run a post-hoc test to see WHICH months differ.
if (summary(anova_month)[[1]][["Pr(>F)"]][1] < 0.05) {
  cat("\n--- ANOVA result is significant. Running Tukey HSD Post-Hoc Test ---\n")
  print(TukeyHSD(anova_month))
}

# --- How to Interpret in Your Report ---
# "To test for monthly seasonality, a one-way ANOVA was conducted on the DAX's daily
# returns, grouped by month. The p-value of [insert p-value] is [greater than / less than]
# 0.05. Therefore, we [fail to reject / reject] the null hypothesis, concluding that
# there is [no / a] statistically significant 'month-of-the-year' effect on the
# average returns of the German market."


#-----------------------------------------------------------------------
# HYPOTHESIS 6: CHI-SQUARE (Directional Co-movement S&P 500 vs FTSE 100)
#-----------------------------------------------------------------------
# Question: Are the daily UP/DOWN movements of the US and UK markets independent?
# H0: The direction of the S&P 500's return is INDEPENDENT of the FTSE 100's direction.
# H1: The directions are DEPENDENT.
cat("\n\n--- Hypothesis 6: Chi-Square Test (Directional Independence S&P 500 vs FTSE 100) ---\n")
returns_with_direction <- all_returns %>%
  mutate(
    Direction_SP500 = ifelse(Return_SP500 > 0, "Up", "Down"),
    Direction_FTSE100 = ifelse(Return_FTSE100 > 0, "Up", "Down")
  )
contingency_table_direction <- table(returns_with_direction$Direction_SP500, returns_with_direction$Direction_FTSE100)
print(contingency_table_direction)
chi_test_direction <- chisq.test(contingency_table_direction)
print(chi_test_direction)


#-----------------------------------------------------------------------
# HYPOTHESIS 7: CHI-SQUARE (Volatility Independence Across All Indices)
#-----------------------------------------------------------------------
# Question: Is the frequency of "large price swings" the same across all 5 markets?
# H0: The frequency of large moves is INDEPENDENT of the index.
# H1: The frequency is DEPENDENT on the index.
cat("\n\n--- Hypothesis 7: Chi-Square Test (Volatility Independence Across Indices) ---\n")
long_returns_for_q <- all_returns %>% gather(key = "Index", value = "Return", -Date)
volatility_threshold <- quantile(abs(long_returns_for_q$Return), 0.90)
returns_with_volatility <- all_returns %>%
  mutate(across(starts_with("Return"), ~ifelse(abs(.) > volatility_threshold, "Large Move", "Normal Move"), .names = "Vol_{.col}"))
volatility_long <- returns_with_volatility %>%
  dplyr::select(Date, starts_with("Vol_")) %>%
  gather(key = "Index", value = "Move_Type", -Date)
contingency_table_volatility <- table(volatility_long$Index, volatility_long$Move_Type)
print(contingency_table_volatility)
chi_test_volatility <- chisq.test(contingency_table_volatility)
print(chi_test_volatility)


#-----------------------------------------------------------------------
# HYPOTHESIS 8 (NEW): CHI-SQUARE (Direction vs. Weekday)
#-----------------------------------------------------------------------
# Question: Is the S&P 500 more likely to go UP on certain days of the week than others?
#
# H0: The direction of the S&P 500's daily return (Up/Down) is INDEPENDENT of the weekday.
# H1: The direction is DEPENDENT on the weekday.
#
# What will happen: This tests for behavioral patterns. A significant result
# could mean that, for example, the market is systematically more optimistic
# (more UP days) on Fridays than it is on Mondays.

cat("\n\n--- Hypothesis 8 (NEW): Chi-Square Test (S&P 500 Direction vs. Weekday) ---\n")
# Add a direction column to our dataframe that has the weekday information
returns_with_dates_and_direction <- returns_with_dates %>%
  mutate(Direction_SP500 = ifelse(Return_SP500 > 0, "Up", "Down"))

# Create a contingency table and run the test
contingency_table_weekday_dir <- table(returns_with_dates_and_direction$Direction_SP500,
                                       returns_with_dates_and_direction$Weekday)
print(contingency_table_weekday_dir)
chi_test_weekday_dir <- chisq.test(contingency_table_weekday_dir)
print(chi_test_weekday_dir)

# --- How to Interpret in Your Report ---
# "A Chi-Square Test of Independence was used to investigate whether the directional
# bias of the S&P 500 is consistent across the days of the week. The test compares
# the frequency of 'Up' and 'Down' days for each weekday. The p-value was
# [insert p-value]. As this value is [greater than / less than] 0.05, we
# [fail to reject / reject] the null hypothesis. This implies that there is
# [no / a] statistically significant association between the day of the week and
# the likelihood of the S&P 500 finishing the day with a positive or negative return."
#-----------------------------------------------------------------------------
# SECTION 4: ARIMA PREDICTIONS (FOR NEXT 90 DAYS)
#-----------------------------------------------------------------------------

# --- 4.a. S&P 500 ARIMA Forecast ---
# *** CORRECTION: Using the correct Price column 'Price_SP500' from the clean dataframe ***
sp500_ts_price <- ts(sp500_clean$Price_SP500)
model_arima_sp500 <- auto.arima(sp500_ts_price)
forecast_sp500 <- forecast(model_arima_sp500, h = 90)
autoplot(forecast_sp500) + labs(title = "S&P 500 Price Forecast (90 Days)")
print(summary(model_arima_sp500))

# --- 4.b. Nikkei 225 ARIMA Forecast ---
# *** CORRECTION: Using correct column 'Price_Nikkei225' ***
nikkei_ts_price <- ts(nikkei_clean$Price_Nikkei225)
model_arima_nikkei <- auto.arima(nikkei_ts_price)
forecast_nikkei <- forecast(model_arima_nikkei, h = 90)
autoplot(forecast_nikkei) + labs(title = "Nikkei 225 Price Forecast (90 Days)")
print(summary(model_arima_nikkei))

# --- 4.c. FTSE 100 ARIMA Forecast ---
# *** CORRECTION: Using correct column 'Price_FTSE100' ***
ftse_ts_price <- ts(ftse_clean$Price_FTSE100)
model_arima_ftse <- auto.arima(ftse_ts_price)
forecast_ftse <- forecast(model_arima_ftse, h = 90)
autoplot(forecast_ftse) + labs(title = "FTSE 100 Price Forecast (90 Days)")
print(summary(model_arima_ftse))

# --- 4.d. DAX ARIMA Forecast ---
# *** CORRECTION: Using correct column 'Price_DAX' ***
dax_ts_price <- ts(dax_clean$Price_DAX)
model_arima_dax <- auto.arima(dax_ts_price)
forecast_dax <- forecast(model_arima_dax, h = 90)
autoplot(forecast_dax) + labs(title = "DAX Price Forecast (90 Days)")
print(summary(model_arima_dax))

# --- 4.e. S&P/ASX 200 ARIMA Forecast ---
# *** CORRECTION: Using correct column 'Price_ASX200' ***
asx_ts_price <- ts(asx_clean$Price_ASX200)
model_arima_asx <- auto.arima(asx_ts_price)
forecast_asx <- forecast(model_arima_asx, h = 90)
autoplot(forecast_asx) + labs(title = "S&P/ASX 200 Price Forecast (90 Days)")
print(summary(model_arima_asx))


#-----------------------------------------------------------------------------
# GRANGER CAUSALITY AND VAR MODEL ANALYSIS
#-----------------------------------------------------------------------------
# This script systematically tests for causal relationships between the
# returns of every unique pair of the five indices.
#
# For each pair, it performs two key analyses:
# 1. Granger Causality Test: A statistical test to see if past values of one
#    series can help predict the future values of another. This assesses
#    "long-run" predictive causality.
# 2. Vector Autoregression (VAR): A model that captures the short-run dynamic
#    relationship between the series. We analyze it with Impulse Response
#    Functions (IRFs) to see how a shock in one market affects the other.
#-----------------------------------------------------------------------------

# --- Step 1: Prepare the necessary libraries and data ---
# Ensure these libraries are loaded from the top of your script.
library(lmtest) # For grangertest()
library(vars)   # For VARselect(), VAR(), and irf()

# We use the 'all_returns' data frame, which contains the stationary return series.
# We will select only the return columns for the analysis.
return_cols <- all_returns[, -1] # Exclude the 'Date' column
col_names <- names(return_cols)  # Get the names of the return columns

cat("\n\n==========================================================")
cat("\n--- Starting Granger and VAR Analysis for All Pairs ---")
cat("\n==========================================================\n")


# --- Step 2: Loop through every unique pair of indices ---
# This structure ensures we test each pair only once.
for (i in 1:(length(col_names) - 1)) {
  for (j in (i + 1):length(col_names)) {
    
    # Get the names and time series data for the current pair
    name1 <- col_names[i]
    name2 <- col_names[j]
    ts1 <- ts(return_cols[[name1]])
    ts2 <- ts(return_cols[[name2]])
    
    # Print a clear header for the current pair's analysis
    cat(paste("\n\n-------------------------------------------------"))
    cat(paste("\n--- Analyzing Pair:", name1, "vs.", name2, "---"))
    cat(paste("\n-------------------------------------------------\n"))
    
    # Combine the two series into a single data frame for the models
    combined_ts_df <- data.frame(Series1 = ts1, Series2 = ts2)
    names(combined_ts_df) <- c(name1, name2) # Ensure column names are correct
    
    
    # --- Part A: Optimal Lag Selection ---
    # Before we can test for causality, we must find the optimal number of
    # past periods (lags) to include in our model.
    cat("--- A: Selecting Optimal Lag Order ---\n")
    lag_selection <- VARselect(combined_ts_df, lag.max = 8, type = "const")
    lag_order <- lag_selection$selection["AIC(n)"] # We use the AIC criterion
    cat(paste("Selected optimal lag order (AIC):", lag_order, "\n"))
    
    
    # --- Part B: Granger Causality Test Code ---
    cat("\n--- B: Granger Causality Test ---\n")
    
    # Test 1: Does name2 Granger-cause name1?
    # H0 (Null Hypothesis): Past values of name2 do NOT help predict name1.
    cat(paste("Test 1 -> H0:", name2, "does NOT Granger-cause", name1, "\n"))
    granger_test_2_to_1 <- grangertest(get(name1) ~ get(name2), order = lag_order, data = combined_ts_df)
    print(granger_test_2_to_1)
    
    # Test 2: Does name1 Granger-cause name2?
    # H0 (Null Hypothesis): Past values of name1 do NOT help predict name2.
    cat(paste("\nTest 2 -> H0:", name1, "does NOT Granger-cause", name2, "\n"))
    granger_test_1_to_2 <- grangertest(get(name2) ~ get(name1), order = lag_order, data = combined_ts_df)
    print(granger_test_1_to_2)
    
    # INTERPRETATION: Look at the p-value ('Pr(>F)'). If it is less than 0.05,
    # you reject the null hypothesis (H0) and conclude that a causal
    # relationship exists in that direction.
    
    
    # --- Part C: Vector Autoregression (VAR) Model Code ---
    cat("\n--- C: VAR Model for Short-Run Dynamics ---\n")
    
    # We build the VAR model using the optimal lag order we found earlier.
    var_model <- VAR(combined_ts_df, p = lag_order, type = "const")
    
    # Print the detailed summary of the model.
    cat("VAR Model Summary:\n")
    print(summary(var_model))
    
    # INTERPRETATION: In the summary, look at the p-values for the coefficients.
    # For the 'Equation for name1', if the lagged terms of name2 (e.g., name2.l1)
    # have significant p-values (< 0.05), it implies a short-run effect
    # from name2 to name1.
    
    
    # --- Part D: Impulse Response Functions (IRF) ---
    # This is a key part of VAR analysis. It shows how a "shock" (a sudden
    # unexpected move) in one index affects the other over the next 20 days.
    cat("\n--- D: Generating Impulse Response Functions ---\n")
    
    irf_results <- irf(var_model, n.ahead = 20, boot = TRUE)
    
    # Plot the IRF results for visualization.
    plot(irf_results, main = paste("Impulse Response: Shocks between", name1, "&", name2))
    
    cat(paste("Check the 'Plots' tab to see the IRF graphs for this pair.\n"))
    
  } # End of inner loop
} # End of outer loop




cat("\n\n--- Granger and VAR analysis for all country pairs is complete. ---\n")

