# Master-thesis
Master’s thesis analysing ESG ratings' impact on stock returns after earnings announcements using R
# 📘 Master’s Thesis – ESG and Earnings Announcements

This repository contains the R scripts, analysis outputs, and documentation for my master’s thesis:  
**“Market Reactions to Earnings Announcements: The Role of ESG Ratings in Nasdaq-Listed Firms (2014–2024)”**

## 📊 Summary

This thesis examines the impact of firms’ ESG (Environmental, Social, and Governance) ratings on stock price reactions following earnings announcements, focusing on Nasdaq-listed companies over 10 years. Using R, I performed an event study and multiple regression models to analyze short-term abnormal returns and post-earnings announcement drift (PEAD).

### Key Objectives:
- Explore whether high ESG-rated firms experience different abnormal returns around earnings events
- Analyze PEAD patterns across ESG quartiles
- Examine how firm characteristics (e.g., size, liquidity) influence ESG’s predictive power

## 🧪 Methods

- **Event Study** using short-term window: _t−1, t, t+1_
- **Long-Term Drift Analysis:** _t+2 to t+25_
- **Abnormal Return Estimation:**
  - CAPM
  - Fama-French three-factor model
- **Regression Models:**
  - OLS and fixed-effects panel regressions
  - Interaction terms for ESG x SUE (Standardized Unexpected Earnings)

## 🛠 Tools & Packages

- R, RStudio  
- `dplyr`, `ggplot2`, `plm`, `xts`, `lmtest`, `sandwich`  
- Data visualization using `ggplot2`  
- Data source: Refinitiv Eikon


