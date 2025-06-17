load("/Volumes/Files/Thesis/Price_data.RData")
load("/Volumes/Files/Thesis/shares.RData")
load("/Volumes/Files/Thesis/announcement.RData")
setwd("/Volumes/Files/Thesis")
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
# -------------------------------
# Data Preparation
# -------------------------------
announcement <- announcement %>% select(-c(1:4))

event_data <- announcement %>%
  filter(`Company Event Type` %in% c("EarningsPresentation", "EarningsReleases", "EarningsPressConference"))

event_data <- event_data %>%
  rename(
    event = "Event Start Date",
    event_time = "Event Start Time"
  ) %>%
  mutate(event = as.Date(event, format = "%Y-%m-%d"))

# -------------------------------
# Prepare Price Data
# -------------------------------
Price_data <- Price_data %>%
  arrange(Instrument, Date) %>%
  mutate(`Price Close` = as.numeric(`Price Close`)) %>%  # ensure numeric
  group_by(Instrument) %>%
  arrange(Date) %>%
  mutate(stock_return = (`Price Close` / lag(`Price Close`)) - 1) %>%
  ungroup()

# Extract market index data (where Instrument equals ".IXIC")
market_data <- Price_data %>%
  filter(Instrument == ".IXIC") %>%
  select(Date, market_return = stock_return) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  distinct(Date, .keep_all = TRUE)

# Remove market data from Price_data to get firm data
firm_data <- Price_data %>%
  filter(Instrument != ".IXIC") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

# Join market return into firm_data by Date
firm_data <- firm_data %>%
  left_join(market_data, by = "Date")

# Function for the long event window: t+2 to t+25
calculate_CAR_long <- function(inst, event_date, event_time, Price_data, est_window_days = 250) {
  # Convert event_date to Date if needed
  event_date <- as.Date(event_date, format = "%Y-%m-%d")
  
  # Adjust event_date: if event_time is after 4pm, shift event date to the next trading day
  if (!is.null(event_time) && !is.na(event_time) && event_time > "16:00:00") {
    event_date <- event_date + days(1)
  }
  
  # Filter Price_data for the given Instrument
  inst_data <- Price_data %>% 
    filter(Instrument == inst) %>% 
    mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
  
  # Define the estimation window: from (event_date - est_window_days) up to (but not including) event_date
  est_data <- inst_data %>% 
    filter(Date < event_date, Date >= event_date - days(est_window_days))
  
  # Estimate the market model: stock_return ~ market_return
  model <- tryCatch({
    lm(stock_return ~ market_return, data = est_data)
  }, error = function(e) {
    NULL
  })
  
  # If model estimation fails, return NA for CAR_long
  if (is.null(model)) {
    return(data.frame(Instrument = inst, event = event_date, CAR_long = NA))
  }
  
  # Define the event window: from t+2 to t+25 relative to the adjusted event_date
  event_window <- inst_data %>% 
    filter(Date >= event_date + days(2),
           Date <= event_date + days(25))
  
  # Calculate expected returns and abnormal returns in the event window
  event_window <- event_window %>% 
    mutate(expected_return = coef(model)[1] + coef(model)[2] * market_return,
           abnormal_return = stock_return - expected_return)
  
  # Calculate the cumulative abnormal return (CAR) over the event window
  CAR_long <- sum(event_window$abnormal_return, na.rm = TRUE)
  
  # Return the result as a one-row data frame
  return(data.frame(Instrument = inst, event = event_date, CAR_long = CAR_long))
}

# Apply the function to each event in your event_data and combine the results
results_long <- event_data %>% 
  rowwise() %>% 
  mutate(result_long = list(calculate_CAR_long(
    inst = Instrument,
    event_date = event,
    event_time = event_time,
    Price_data = firm_data,
    est_window_days = 250
  ))) %>% 
  ungroup() %>% 
  unnest(cols = c(result_long), names_sep = "_")
all_data <- results_long

#--------------------------------------------------------------------------------
# calculating SUE
#-------------------------------------------------------------------------------------
quarterly_data <- read.csv("Quarterly_data.csv", stringsAsFactors = FALSE)
head(quarterly_data)

# Clean and transform the data
quarterly_data <- quarterly_data %>%
  select(
    Instrument,
    Date, 
    Actual_EPS = `Earnings.Per.Share.Reported...Mean`,
    Forecast_EPS = `Earnings.Per.Share...SmartEstimate.`,
    ROA = `Return.On.Assets...Mean`,
    Analyst_Number = `Number.of.Analysts`,
    Beta
  ) %>%
  mutate(
    Date = as.Date(Date)
  )

# -------------------------------
# Step 1. Prepare quarterly_data
# -------------------------------
quarterly_data <- quarterly_data %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"),
         Quarter = paste0("Q", quarter(Date)),
         Year = year(Date)) %>%
  distinct(Instrument, Quarter, Year, .keep_all = TRUE)

# -------------------------------
# Step 2. Prepare event_data
# -------------------------------

event_data <- event_data %>%
  mutate(event = as.Date(event, format = "%Y-%m-%d"),
         quarter = quarter(event),
         year = year(event))

# -------------------------------
# Step 3. Join quarterly_data to event_data (for current quarter EPS)
# -------------------------------
quarterly_data <- quarterly_data %>%
  mutate(Quarter = if_else(grepl("^Q", Quarter),
                           as.integer(str_remove(Quarter, "Q")),
                           as.integer(Quarter)))
event_data <- event_data %>%
  left_join(quarterly_data %>%
              select(Instrument, Date, Analyst_Number, Actual_EPS, Forecast_EPS, Quarter, Year),
            by = c("Instrument", "quarter" = "Quarter", "year" = "Year"))

# -------------------------------
# Step 4. Compute Previous Quarter Information
# -------------------------------

event_data <- event_data %>%
  mutate(prev_Quarter = case_when(
    quarter == 1 ~ 4,
    quarter == 2 ~ 1,
    quarter == 3 ~ 2,
    quarter == 4 ~ 3
  ),
  prev_Year = if_else(quarter == 1, year - 1, year))

# -------------------------------
# Step 5. Obtain Previous Quarter End Date from quarterly_data
# -------------------------------

prev_quarter_data <- quarterly_data %>%
  select(Instrument, Quarter, Year, Date) %>%
  rename(prev_Date = Date)

event_data <- event_data %>%
  left_join(prev_quarter_data, by = c("Instrument", "prev_Quarter" = "Quarter", "prev_Year" = "Year"))

# -------------------------------
# Step 6. Get the Previous Quarter-End Closing Price from Price_data
# -------------------------------

prev_price_data <- firm_data %>%
  select(Instrument, Date, `Price Close`) %>%
  distinct()

event_data <- event_data %>%
  left_join(prev_price_data, by = c("Instrument", "prev_Date" = "Date"))

# -------------------------------
# Step 7. Calculate SUE
# -------------------------------
# SUE = (Actual_EPS - Forecast_EPS) / (Price Close at previous quarter end)
event_data <- event_data %>%
  mutate(SUE = (Actual_EPS - Forecast_EPS) / `Price Close`)

event_data <- event_data %>%
  distinct(Instrument, event, .keep_all = TRUE)

all_data <- all_data %>%
  left_join(event_data %>% select(Instrument, event, SUE),
            by = c("Instrument", "event"))
#-------------------------------------------------------------------------------
# Size Calculation
#-------------------------------------------------------------------------------
all_data <- all_data %>%
  mutate(prev_month_end = floor_date(event, unit = "month") - days(1))

size_data <- Price_data %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  distinct(Instrument, Date, .keep_all = TRUE) %>%
  select(Instrument, Date, `Company Market Capitalization`)

all_data <- all_data %>%
  left_join(size_data, by = c("Instrument", "prev_month_end" = "Date"))

all_data <- all_data %>%
  mutate(Size = log(`Company Market Capitalization`))
#------------------------------
# share turnover
#-----------------------

share_outstanding <- share_outstanding %>%
  rename(Company_Shares = `Company Shares `)

calculate_turnover <- function(inst, quarter_end, Price_data, share_outstanding) {
  quarter_end <- as.Date(quarter_end)
  start_date <- quarter_end - years(1) + days(1)
  period_data <- Price_data %>%
    filter(Instrument == inst,
           Date >= start_date,
           Date <= quarter_end)
  
  if(nrow(period_data) == 0) return(NA)
  
  monthly_data <- period_data %>%
    mutate(YearMonth = floor_date(Date, unit = "month")) %>%
    group_by(YearMonth) %>%
    summarise(avg_volume = mean(Volume, na.rm = TRUE),
              .groups = "drop")
  
  overall_avg_volume <- mean(monthly_data$avg_volume, na.rm = TRUE)
  shares_data <- share_outstanding %>%
    filter(Instrument == inst,
           Date >= start_date,
           Date <= quarter_end)
  
  if(nrow(shares_data) == 0) return(NA)
  
  monthly_shares <- shares_data %>%
    mutate(YearMonth = floor_date(Date, unit = "month")) %>%
    group_by(YearMonth) %>%
    summarise(avg_shares = mean(as.numeric(Company_Shares), na.rm = TRUE),
              .groups = "drop")
  
  overall_avg_shares <- mean(monthly_shares$avg_shares, na.rm = TRUE)
  
  turnover <- overall_avg_volume / overall_avg_shares
  return(turnover)
}

all_data <- all_data %>%
  mutate(event = as.Date(event, format = "%Y-%m-%d"),
         quarter_end = floor_date(event, "quarter") - days(1))

all_data <- all_data %>%
  rowwise() %>%
  mutate(ShareTurnover = calculate_turnover(Instrument, quarter_end, Price_data, share_outstanding)) %>%
  ungroup()

#-----------------------------------
# joining Analyst number to all_data
#-----------------------------------------
all_data <- all_data %>%
  left_join(
    event_data %>% select(Instrument, event, Analyst_Number),
    by = c("Instrument", "event")
  ) %>%
  mutate(Analyst_following = log(1 + Analyst_Number))

#-------------------------------------------------------------------------------
# Report Lag: Number of days from quarter-end to earnings announcement date 
#--------------------------------------------------------------------------
all_data <- all_data %>%
  mutate(Rptlag = as.numeric(event - quarter_end))

#---------------------------------------------
#ESG Score
#---------------------------------------
ESG_data <- read.csv("/Volumes/Files/Thesis/Yearly_data.csv")

ESG_data <- ESG_data %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%dT%H:%M:%SZ")) %>%
  rename(
    overall_ESG = `ESG.Score`,
    environment_grade = `Environmental.Pillar.Score.Grade`,
    social_grade = `Social.Pillar.Score.Grade`,
    governance_grade = 'Governance.Pillar.Score.Grade',
    environment_score = `Environmental.Pillar.Score`,
    social_score = 'Social.Pillar.Score',
    governance_score = 'Governance.Pillar.Score'
  )

get_latest_esg <- function(inst, event_date, ESG_data) {
  event_date <- as.Date(event_date)
  record <- ESG_data %>%
    filter(Instrument == inst, Date <= event_date) %>%
    arrange(desc(Date)) %>%
    slice(1)
  
  if(nrow(record) == 0) {
    return(data.frame(
      overall_ESG = NA,
      environment_score = NA,
      social_score = NA,
      governance_score = NA,
      ESG_Date = NA
    ))
  }
  record <- record %>%
    rename(ESG_Date = Date) %>%
    select(overall_ESG, environment_score, social_score, governance_score, ESG_Date)
  
  return(record)
}

all_data <- all_data %>%
  rowwise() %>%
  mutate(esg_info = list(get_latest_esg(Instrument, event, ESG_data))) %>%
  ungroup() %>%
  unnest(cols = c(esg_info))

#------------------------------------------------------------------------
# Equity
#------------------------------------------------------------------------

load("/Volumes/Files/Thesis/equity.RData")
equity <- equity %>% select(-c(1:3))

equity <- equity %>%
  rename(report_date = `Report Date`,
         shareholders_equity = `Shareholders Equity - Actual`) %>%
  mutate(report_date = as.Date(report_date, format = "%Y-%m-%d"))

equity_unique <- equity %>%
  distinct(Instrument, report_date, .keep_all = TRUE)

all_data <- all_data %>%
  left_join(equity_unique, by = c("Instrument", "event" = "report_date"))

library(tidyr)

all_data <- all_data %>%
  mutate(year = year(event)) %>%  # create the year variable from the event date
  group_by(Instrument, year) %>%
  fill(shareholders_equity, .direction = "downup") %>%
  ungroup()

#--------------------------
#book to market ratio
#-------------------------------

all_data <- all_data %>%
  mutate(shareholders_equity = as.numeric(gsub(",", "", shareholders_equity)),
         book_to_market = shareholders_equity / `Company Market Capitalization`)

#------------------------------
#quantile
#-----------------------------

library(quantreg)

all_data$QSUE <- scale(all_data$SUE)
all_data$QESG <- scale(all_data$overall_ESG)

#--------------------------------------
# Summmary table
#-----------------------------------
all_data <- all_data %>% 
  rename(announcement_return = result_CAR)


library(dplyr)
library(zoo)

all_data <- na.omit(all_data)

regression_data <- all_data %>%
  select(Instrument, event, announcement_return, SUE, overall_ESG,environment_score,social_score,governance_score,
         Size, Rptlag, ShareTurnover, Analyst_following, book_to_market, year, QSUE, QESG)

library(kableExtra)
summary_table <- regression_data %>%
  select(where(is.numeric), -year) %>%
  summarise(
    across(
      everything(),
      list(
        'No. of observation' = ~sum(!is.na(.)),
        Min    = ~min(., na.rm = TRUE),
        P25    = ~quantile(., 0.25, na.rm = TRUE),
        Median = ~median(., na.rm = TRUE),
        P75    = ~quantile(., 0.75, na.rm = TRUE),
        Max    = ~max(., na.rm = TRUE),
        SD     = ~sd(., na.rm = TRUE)
      )
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", ".value"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  select(Variable, 'No. of observation', Min, P25, Median, P75, Max, SD)

kable(summary_table, "html") %>%
  kable_styling(full_width = FALSE) %>%
  save_kable("summary_table.html")


#-----------------------------------------------
# Regression
#-----------------------------------------------------
library(fixest)
library(modelsummary)
class(regression_data$QSUE)
regression_data <- regression_data %>%
  mutate(QSUE = as.numeric(as.character(QSUE)))

regression_data <- regression_data %>%
  mutate(QESG = as.numeric(as.character(QESG)))


regression_data <- regression_data %>%
  rename(Environment = environment_score,
         Social = social_score,
         Governance = governance_score)

load("/Volumes/Files/Thesis/regmodel.RData")

all_data %>% 
  slice_min(SUE, n = 10) %>% 
  select(SUE)

all_data %>% 
  slice_max(SUE, n = 10) %>% 
  select(SUE)

k <- 1
k1<- 1
idxs_low  <- head(order(all_data$SUE), k)
idxs_high <- head(order(-all_data$SUE), k1)
to_remove <- unique(c(idxs_low, idxs_high))
all_data <- all_data[-to_remove, ]
summary(all_data$SUE) 

library(psych)
all_data$SUE2 <- -all_data$SUE  
summary(all_data$SUE2)

btm_bounds <- quantile(all_data$book_to_market, c(.01,.99), na.rm = TRUE)

all_data <- all_data %>%
  mutate(booktomarket = pmin(pmax(book_to_market, btm_bounds[1]), btm_bounds[2]),
         booktomarket = log1p(booktomarket))

all_data <- all_data %>% 
  mutate(across(everything(), ~ ifelse(is.infinite(.x), NA, .x)))
# 1) estimate a density
dens <- density(all_data$SUE2)

# 2) build a smooth CDF via a cumulative sum + interpolation
xs     <- dens$x
ys     <- cumsum(dens$y) / sum(dens$y)
cdf_fn <- approxfun(xs, ys, yleft=0, yright=1)    # linear interpolation

# 3) apply to your data
all_data$QSUE_smooth <- cdf_fn(all_data$SUE2)

reg_data<- na.omit(all_data)

reg_data <- reg_data %>%
  mutate(
    Environment = percent_rank(environment_score),
    Social = percent_rank(social_score),
    Governance = percent_rank(governance_score),
    QSUE = percent_rank(SUE),
    QESG = percent_rank(overall_ESG))
library(fixest)
Model1_return_sue<- feols(
  announcement_return ~ SUE2 +
    QESG +
    Size +
    Rptlag +
    ShareTurnover +
    Analyst_following +
    booktomarket | Instrument + year,
  data = reg_data,
  cluster = "Instrument"
)

summary(Model1_return_sue)

Model2_return_sue <- feols(
  announcement_return ~ SUE2 +
    Environment+
    Social+
    Governance+
    Size +
    Rptlag +
    ShareTurnover +
    Analyst_following +
    booktomarket | Instrument + year,
  data = reg_data,
  cluster = "Instrument"
)

summary(Model2_return_sue)





#------------------------------------------
Model1_return<- feols(
  announcement_return ~ QSUE +
    QESG +
    Size +
    Rptlag +
    ShareTurnover +
    Analyst_following +
    booktomarket | Instrument + year,
  data = reg_data,
  cluster = "Instrument"
)

summary(Model1_return)

Model2_return <- feols(
  announcement_return ~ QSUE_smooth +
    Environment+
    Social+
    Governance+
    Size +
    Rptlag +
    ShareTurnover +
    Analyst_following +
    booktomarket | Instrument + year,
  data = reg_data,
  cluster = "Instrument"
)

summary(Model2_return)


summary(reg_data$QSUE_smooth)
summary(reg_data$QSUE)

#-----------------------------------
#output
#---------------------
main_order <- c(
  "SUE","QSUE", "Size", "booktomarket", "ShareTurnover", 
  "Analyst_following", "Rptlag", 
  "QESG", "Environment", "Social", "Governance"
)

var_order_all <- c(main_order)


# 3) Pretty labels for both sets
var_labels_main <- c(
  SUE2               = "SUE",
  QSUE_smooth      = "QSUE",
  Size              = "Size",
  booktomarket      = "Book-to-Market",
  ShareTurnover     = "Share Turnover",
  Analyst_following = "Analyst Following",
  Rptlag            = "Reporting Lag",
  QESG              = "ESG",
  Environment       = "Environmental",
  Social            = "Social",
  Governance        = "Governance"
)


var_labels_all <- c(var_labels_main)

# this will give you exactly:
# "SUE", "Size", "Book-to-Market", …, "Governance",
# then "SUE×Size", …, "SUE×Governance"
order_labels <- c(
  var_labels_all[ main_order ]
)

etable_df<- etable(
  `Model- 1` = Model1_return_sue,
  `Model- 2` = Model2_return_sue,
  `Model- 3` = Model1_return,
  `Model- 4` = Model2_return,   # <-- your pretty labels, in exact sequence
  dict    = var_labels_all,         # <-- raw→pretty lookup
  fitstat = c("n", "r2"),
  tex     = FALSE
)


library(kableExtra)  
library(htmltools)

tbl <- kable(
  etable_df, 
  format        = "html", 
  escape        = FALSE,
  caption       = "Announcement Return Regressions"
) %>%
  kable_styling(full_width = TRUE)

html_str <- as.character(tbl)

html_content <- HTML(html_str)

save_html(html_content, "Announcement Return long term effect.html")

setwd("/Volumes/Files/Thesis")
getwd()
list.files(pattern = "\\.html$")
