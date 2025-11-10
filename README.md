# Global-Stock-Market-Analysis
An econometric study of 5 major indices (S&P 500, FTSE 100, DAX 40, Nikkei 225, and S&P/ASX 200.) to model performance, risk, and causality. Analyzes volatility, correlation, and shock transmission between the S&amp;P 500, DAX, FTSE 100, Nikkei 225, and ASX 200 from 2018-2024.


## 📖 Project Overview
This project analyzes the short-run and long-run relationships between five major global stock market indices from January 1, 2018, to December 31, 2024. Using daily closing price data, the study employs a comprehensive econometric framework to model and understand market behavior.


## The analysis covers:

Descriptive statistics and return distributions 

Volatility clustering and correlation analysis 

Statistical hypothesis testing (T-tests, ANOVA, Chi-Square) 

90-day price forecasting using ARIMA models 

Causality and shock transmission using Granger Causality and Vector Autoregression (VAR) 


## 📊 Key Findings
Primary Finding: S&P 500 Dominance
The analysis revealed a "highly interconnected, yet US-centric, financial landscape". The S&P 500 was identified as the primary driver of global market returns.

S&P 500 (US): This was the only index to exhibit a statistically significant positive "drift," identified as 1.81 points per day. It is classified as a "random walk with drift," and its 90-day forecast projects a continued upward trend.

Other Indices (Europe, Asia, Australia): The DAX, FTSE 100, Nikkei 225, and ASX 200 all behaved as "random walks" without a statistically significant drift. Their 90-day forecasts are consequently flat, with the best statistical guess for a future price being the last known price.

### Causality & Information Flow
The S&P 500's performance is the most dominant predictive force, with its closing performance strongly influencing other markets on their next trading day (the "overnight" effect).

S&P 500 ➔ Nikkei 225: A clear unidirectional (one-way) relationship. The S&P 500's past returns are a powerful predictor of the Nikkei's future returns , but the reverse is not true.

S&P 500 ⇔ FTSE 100 & DAX: Strong bidirectional (two-way) predictive relationships exist. Shocks from the S&P 500 are transmitted immediately to the German and UK markets.

S&P 500 ⇔ ASX 200: A complex bidirectional relationship was found. It includes the standard positive "overnight" effect from the US to Australia and a unique, immediate negative correlation from an ASX shock to the S&P 500.

### Volatility & Risk

Fat Tails: All indices exhibit high kurtosis and sharply peaked distributions. This indicates that extreme price swings ("fat tails") are far more common than a normal distribution would predict.

Volatility Clustering: All markets experienced a simultaneous, dramatic spike in volatility around 2020, corresponding with the COVID-19 pandemic.

Correlation: The European markets (DAX and FTSE 100) are very highly correlated at 0.83. During global crises, correlations across all markets rise, significantly diminishing the benefits of diversification.

### Other Key Hypotheses

No Mean Difference: There was no significant difference in the average daily returns between the S&P 500 and the DAX or for the S&P 500 pre- vs. during-COVID.

"Day-of-the-Week" Effect: A statistically significant "Day-of-the-Week" effect was found for the Nikkei 225.

No "January Effect": No significant monthly seasonality (like a "January Effect") was found for the DAX.

Co-Movement: The daily UP/DOWN movements of the S&P 500 and FTSE 100 are dependent (they tend to move together).

## ⚙️ Technologies & Methods

Language: R 

Key Packages: ggplot2 , psych (for describe()) , corrplot 

Statistical Models:

1) ARIMA (Autoregressive Integrated Moving Average) 

2) VAR (Vector Autoregression) 

3) Granger Causality Test 

4) T-Test, ANOVA, Chi-Square Test 

## 🗂️ Data Sources
  1) Investing.com 
  
  2) Yahoo Finance 

## ✍️ Authors
   Ayush Jain 
   
   Pilla Poushya Meghana 
   
   Bhavya 
   
   Sachin Shanmugam 
   
   Malay Aditya
