load("/Volumes/Files/Thesis/Price_data.RData")
load("/Volumes/Files/Thesis/shares.RData")
load("/Volumes/Files/Thesis/announcement.RData")
quarterly_data <- read.csv(
  "/Volumes/Files/Thesis/Quarterly_data.csv",
  stringsAsFactors = FALSE
)

setwd("/Volumes/Files/Thesis")
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
announcement <- announcement %>% select(-c(1:4))

event_data <- announcement %>%
  filter(`Company Event Type` %in% c("EarningsPresentation", "EarningsReleases", "EarningsPressConference"))

event_data <- event_data %>%
  rename(
    event = "Event Start Date",
    event_time = "Event Start Time"
  ) %>%
  mutate(event = as.Date(event, format = "%Y-%m-%d"))


Price_data <- data.frame(Price_data)

Price_data <- Price_data[!duplicated(Price_data[, c(1,2,4)]), ]

Price_data <- Price_data[!is.na(Price_data$Price.Close), ]

Price_data <- Price_data[!is.na(Price_data$Instrument), ]

Price_data <- Price_data[!is.na(Price_data$Date), ]

Price_data <- Price_data[!duplicated(Price_data[, c(1,2)]), ]

# -------------------------------
# Prepare Price Data
# -------------------------------
Price_data <- Price_data %>%
  arrange(Instrument, Date) %>%
  mutate(Price.Close = as.numeric(Price.Close)) %>% 
  group_by(Instrument) %>%
  arrange(Date) %>%
  mutate(stock_return = (Price.Close / lag(Price.Close)) - 1) %>%
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

# -------------------------------
# Function to Calculate CAR
# -------------------------------
calculate_CAR <- function(inst, event_date, event_time, Price_data, est_window_days = 250) {
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
    filter(Date < event_date,
           Date >= event_date - days(est_window_days))
  
  # Estimate the market model
  model <- tryCatch({
    lm(stock_return ~ market_return, data = est_data)
  }, error = function(e) {
    NULL
  })
  
  # If model estimation fails, return NA for CAR
  if (is.null(model)) {
    return(data.frame(Instrument = inst, event = event_date, CAR = NA))
  }
  
  # Define the event window: t-1, t, and t+1 relative to the adjusted event_date
  event_window <- inst_data %>%
    filter(Date >= event_date - days(1),
           Date <= event_date + days(1))
  
  # Calculate expected returns and abnormal returns in the event window
  event_window <- event_window %>%
    mutate(expected_return = coef(model)[1] + coef(model)[2] * market_return,
           abnormal_return = stock_return - expected_return)
  
  # Calculate the cumulative abnormal return over the three-day event window
  CAR <- sum(event_window$abnormal_return, na.rm = TRUE)
  
  # Return the result as a one-row data frame
  return(data.frame(Instrument = inst, event = event_date, CAR = CAR))
}
# -------------------------------
# Apply the Function to Each Event
# -------------------------------
all_data <- event_data %>%
  rowwise() %>%
  mutate(result = list(calculate_CAR(inst = Instrument,
                                     event_date = event,
                                     event_time = event_time,
                                     Price_data = firm_data,
                                     est_window_days = 250))) %>%
  ungroup() %>%
  unnest(cols = c(result), names_sep = "_")

##################
#Sue calculation with price close 
##############################

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
library(dplyr)
library(readr)
quarterly_data <- quarterly_data %>%
  mutate(Quarter = parse_number(as.character(Quarter)))


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
  select(Instrument, Date, Price.Close) %>%
  distinct()

event_data <- event_data %>%
  left_join(prev_price_data, by = c("Instrument", "prev_Date" = "Date"))

# -------------------------------
# Step 7. Calculate SUE
# -------------------------------
# SUE = (Actual_EPS - Forecast_EPS) / (Price Close at previous quarter end)
event_data <- event_data %>%
  mutate(SUE = (Actual_EPS - Forecast_EPS) / Price.Close)

event_data <- event_data %>%
  distinct(Instrument, event, .keep_all = TRUE)

all_data <- all_data %>%
  left_join(event_data %>% select(Instrument, event, SUE),
            by = c("Instrument", "event"))

summary(all_data$SUE)

#-------------------------------------------------------------------------------
# Size Calculation
#-------------------------------------------------------------------------------
all_data <- all_data %>%
  mutate(prev_month_end = floor_date(event, unit = "month") - days(1))

size_data <- firm_data %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  distinct(Instrument, Date, .keep_all = TRUE) %>%
  select(Instrument, Date, `Company Market Capitalization`)

all_data <- all_data %>%
  left_join(firm_data, by = c("Instrument", "prev_month_end" = "Date"))

all_data <- all_data %>%
  mutate(Size = log(Company.Market.Capitalization))
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

####################################################
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
         book_to_market = shareholders_equity / Company.Market.Capitalization)

btm_bounds <- quantile(all_data$book_to_market, c(.01,.99), na.rm = TRUE)

all_data <- all_data %>%
  mutate(booktomarket = pmin(pmax(book_to_market, btm_bounds[1]), btm_bounds[2]),
         booktomarket = log1p(booktomarket))
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

#-----------------------------------------------------
load("/Volumes/Files/Thesis/datapreparation.RData")
#-----------------------------------------------------

#---------------------------------
#fixing
#____________________________

all_data <- all_data %>% 
  rename(announcement_return = result_CAR)

all_data %>% 
  slice_min(announcement_return, n = 10) %>% 
  select(announcement_return)

all_data %>% 
  slice_max(announcement_return, n = 10) %>% 
  select(announcement_return)

k <- 5
idxs_low  <- head(order(all_data$announcement_return), k)
to_remove <- unique(c(idxs_low))
all_data <- all_data[-to_remove, ]

all_data %>% 
  slice_min(SUE, n = 10) %>% 
  select(SUE)

all_data %>% 
  slice_max(SUE, n = 10) %>% 
  select(SUE)

k <- 8
k1<- 2
idxs_low  <- head(order(all_data$SUE), k)
idxs_high <- head(order(-all_data$SUE), k1)
to_remove <- unique(c(idxs_low, idxs_high))
all_data <- all_data[-to_remove, ]
summary(all_data$SUE) 

library(psych)
all_data$SUE2 <- -all_data$SUE  
summary(all_data$SUE2)
all_data <- all_data %>% 
  mutate(across(everything(), ~ ifelse(is.infinite(.x), NA, .x)))
reg_data<- na.omit(all_data)

#----------------------------
#summary
#-----------------------------
summary(reg_data[, c("announcement_return", "SUE" , "SUE2","QSUE","overall_ESG", "Size","Rptlag", "ShareTurnover", "Analyst_following","booktomarket" )])

library(dplyr)
library(tidyr)
library(kableExtra)

# specify the order you want
esg_first <- c(
  "announcement_return",
  "SUE2",
  "Size", 
  "booktomarket",
  "ShareTurnover", 
  "Analyst_following",
  "Rptlag",
  "overall_ESG", "environment_score",
  "social_score", "governance_score"
)

summary_table <- reg_data %>%
  select(where(is.numeric),
         -year, -event,-Analyst_Number,-book_to_market,-Company.Market.Capitalization,
         -ESG_Date,-market_return,-prev_month_end ,-Price.Open,
         -Price.Close,-quarter_end,-result_event,-shareholders_equity,-stock_return,-SUE,-Volume,-QSUE,
         -QESG,-Environment,
         -Social,
         -Governance
  ) %>%
  summarise(
    across(
      everything(),
      list(
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
  select(Variable, Min, P25, Median, P75, Max, SD) %>%
  mutate(
    Variable = factor(
      Variable,
      levels = c(esg_first, sort(setdiff(Variable, esg_first)))
    )
  ) %>%
  arrange(Variable)

library(kableExtra)

setwd("/Volumes/Files/Thesis")

kable(summary_table, "html", digits = 3) %>%
  kable_styling(full_width = FALSE) %>%
  save_kable(file = "summary_table.html")


#----------------------------
#regression
#---------------------
reg_data <- reg_data %>%
  mutate(
    Environment = percent_rank(environment_score),
    Social = percent_rank(social_score),
    Governance = percent_rank(governance_score),
    QSUE = percent_rank(SUE2),
    QESG = percent_rank(overall_ESG))
#----------------
# plot with esg and sue
#-------------------
summary(reg_data$overall_ESG)
summary(reg_data$announcement_return)
summary(reg_data$SUE2)
summary(reg_data$Size)

# install.packages(c("dplyr","ggplot2"))  # if needed
library(dplyr)
library(ggplot2)
library(dplyr)
library(ggplot2)
reg_data2 <- reg_data %>%
  mutate(
    ESG_cat = ntile(overall_ESG, 2),              
    ESG_cat = factor(ESG_cat,
                      levels = 1:2,
                      labels = c("Low", "High"))
  )


ggplot(reg_data2,
       aes(x     = SUE2,
           y     = announcement_return,
           color = ESG_cat,
           group = ESG_cat)) +
  geom_smooth(method   = "lm",
              se       = FALSE,
              linewidth = 1) +           # solid lines by default
  
  scale_color_manual(values = c(
    Low  = "lightblue",
    High = "darkblue"
  )) +
  
  geom_vline(xintercept = 0, linetype = "longdash", color = "grey50") +
  geom_hline(yintercept = 0, linetype = "dotted",   color = "grey50") +
  
  labs(title    = "ESG Ratings vs. Earnings Surprise Reactions",
       x        = "Standardized Unexpected Earnings (SUE)",
       y        = "Cumulative Abnormal Return (CAR)",
       color    = "ESG Rating Level") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
#------------------------------------
library(dplyr)
library(ggplot2)

# 1. Cut Size into three equal‐count bins and label them
reg_data3 <- reg_data %>%
  mutate(
    Size_cat = ntile(Size, 2),            
    Size_cat = factor(Size_cat,
                      levels = 1:2,
                      labels = c("Small", "Large"))
  )

# 2. Plot straight lm‐lines for all three
ggplot(reg_data3, aes(
  x     = SUE,
  y     = announcement_return,
  color = Size_cat,
  group = Size_cat
)) +
  geom_smooth(
    method    = "lm",
    se        = FALSE,
    linewidth = 1
  ) +
  scale_color_manual(values = c(
    Small   = "lightblue",
    Large   = "darkblue"
  )) +
  geom_vline(xintercept = 0,
             linetype   = "longdash",
             color      = "grey50") +
  geom_hline(yintercept = 0,
             linetype   = "dotted",
             color      = "grey50") +
  labs(
    title = "Size vs. Earnings Surprise Reactions",
    x     = "Standardized Unexpected Earnings (SUE)",
    y     = "Cumulative Abnormal Return (CAR)",
    color = "Size Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

#--------------------------
#regression with SUE
#-------------------
library(fixest)
Model1_return_sue<- feols(
  announcement_return ~ SUE +
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
  announcement_return ~ SUE +
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

#--------------------------------------------------------
#interaction term
#-------------------------------------------------------
Model1_withinteraction_sue<- feols(
  announcement_return ~ SUE +
    QESG +
    Size +
    Rptlag +
    ShareTurnover +
    Analyst_following +
    booktomarket +
    SUE:QESG +
    SUE:Size +
    SUE:Rptlag +
    SUE:ShareTurnover +
    SUE:Analyst_following +
    SUE:booktomarket  | Instrument + year,
  data = reg_data,
  cluster = "Instrument"
)

summary(Model1_withinteraction_sue)


Model2_withinteraction_sue <- feols(
  announcement_return ~ SUE +
    QESG +
    Size +
    Rptlag +
    ShareTurnover +
    Analyst_following +
    booktomarket +
    SUE:Environment +
    SUE:Social +
    SUE:Governance +
    SUE:Size +
    SUE:Rptlag +
    SUE:ShareTurnover +
    SUE:Analyst_following +
    SUE:booktomarket | Instrument + year,
  data = reg_data,
  cluster = "Instrument"
)

summary(Model2_withinteraction_sue)
#-----------------------------------
#output
#---------------------------

main_order <- c(
  "SUE", "Size", "booktomarket", "ShareTurnover", 
  "Analyst_following", "Rptlag", 
  "QESG", "Environment", "Social", "Governance"
)

# **FIX**: build interactions **without** re-including "SUE:SUE"
int_order <- paste0("SUE:", main_order[-1])

var_order_all <- c(main_order, int_order)


# 3) Pretty labels for both sets
var_labels_main <- c(
  SUE               = "SUE",
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

var_labels_int <- setNames(
  paste0("SUE×", var_labels_main[-1]),
  int_order
)

var_labels_all <- c(var_labels_main, var_labels_int)

# this will give you exactly:
# "SUE", "Size", "Book-to-Market", …, "Governance",
# then "SUE×Size", …, "SUE×Governance"
order_labels <- c(
  var_labels_all[ main_order ],
  var_labels_all[ int_order ]
)

etable(
  `Model- 1` = Model1_return_sue,
  `Model- 2` = Model2_return_sue,
  `Model- 3` = Model1_withinteraction_sue,
  `Model- 4` = Model2_withinteraction_sue,
  order   = unname(order_labels),   # <-- your pretty labels, in exact sequence
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

save_html(html_content, "Announcement Return with SUE.html")

setwd("/Volumes/Files/Thesis")
getwd()
list.files(pattern = "\\.html$")

#--------------------------
#regression with QSUE
#-------------------

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
  announcement_return ~ QSUE +
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




#--------------------------------------------------------
#interaction term
#-------------------------------------------------------
Model1_withinteraction<- feols(
  announcement_return ~ QSUE +
    QESG +
    Size +
    Rptlag +
    ShareTurnover +
    Analyst_following +
    booktomarket +
    QSUE:QESG +
    QSUE:Size +
    QSUE:Rptlag +
    QSUE:ShareTurnover +
    QSUE:Analyst_following +
    QSUE:booktomarket  | Instrument + year,
  data = reg_data,
  cluster = "Instrument"
)

summary(Model1_withinteraction)


Model2_withinteraction <- feols(
  announcement_return ~ QSUE +
    QESG +
    Size +
    Rptlag +
    ShareTurnover +
    Analyst_following +
    booktomarket +
    QSUE:Environment +
    QSUE:Social +
    QSUE:Governance +
    QSUE:Size +
    QSUE:Rptlag +
    QSUE:ShareTurnover +
    QSUE:Analyst_following +
    QSUE:booktomarket | Instrument + year,
  data = reg_data,
  cluster = "Instrument"
)

summary(Model2_withinteraction)

##############
main_order <- c(
  "QSUE", "Size", "booktomarket", "ShareTurnover", 
  "Analyst_following", "Rptlag", 
  "QESG", "Environment", "Social", "Governance"
)

int_order <- paste0("QSUE:", main_order[-1])

var_order_all <- c(main_order, int_order)


# 3) Pretty labels for both sets
var_labels_main <- c(
  QSUE               = "QSUE",
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

var_labels_int <- setNames(
  paste0("QSUE×", var_labels_main[-1]),
  int_order
)

var_labels_all <- c(var_labels_main, var_labels_int)

# 3) One table, with your exact ordering and pretty labels
etable_df <- etable(
  `Model- 1`    = Model1_return,
  `Model- 2`   = Model2_return,
  `Model- 3`     = Model1_withinteraction,
  `Model- 4`   = Model2_withinteraction,
  order  = var_order_all,
  dict   = var_labels_all,
  fitstat = c("n", "r2"),
  tex    = FALSE 
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

save_html(html_content, "Announcement Return with QSUE.html")

setwd("/Volumes/Files/Thesis")
getwd()
list.files(pattern = "\\.html$")


#-------------------------
#Plot
#____________________________

library(ggplot2)
ggplot(reg_data, aes(x = SUE2, y = announcement_return)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(
    x     = "Standardized Unexpected Earnings (SUE)",
    y     = "Announcement Return",
    title = "Announcement Return vs. SUE (event window t-1, t, t+1)"
  ) +
  theme_minimal() +
  theme(
    plot.title     = element_text(size = 14, face = "bold"),
    axis.title     = element_text(size = 12)
  )










