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


## 📄 File Descriptions

- `Final_3.R`: Main R script for full data processing pipeline, regression models, and visualization of ESG effects.
- `Reg_Long_effect.R`: Focuses on long-term abnormal returns and post-earnings announcement drift.
- `Announcement Return with SUE.html`: HTML report showing short-term stock return responses across SUE quartiles.
- `Announcement Return with QSUE.html`: Similar analysis with standardized unexpected earnings (QSUE).
- `Announcement Return long term effect.html`: Examines long-term cumulative abnormal returns (t+2 to t+25).
- `summary_table.html`: Summary of the main variables.


## 📌 Highlights

- ESG ratings do **not** significantly impact immediate 3-day market reactions
- Evidence of PEAD exists, but only in small-cap and less-traded stocks
- ESG effects are muted after controlling for firm-specific frictions

## 🔗 Author

**Rumana Islam**  
📫 [LinkedIn Profile](https://www.linkedin.com/in/rumanaislam1997/)  
📂 [Portfolio](https://github.com/rumanaislam1)  
📬 Email: rumanaislammahin@gmail.com

