# BUSINESS SCIENCE UNIVERSITY
# DS4B 103-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: FEATURE ENGINEERING

# GOAL ----
# - FIND ENGINEERED FEATURES BEFORE MODELING

# OBJECTIVES:
# - Time-Based Features - Trend-Based & Seasonal Features
# - Interactions
# - Fourier Series
# - Autocorrelated Lags
# - Special Events
# - External Regressor Lags

# LIBRARIES & DATA ----

# Core
library(tidyverse)
library(lubridate)
library(timetk)
library(plotly)

# Data
google_analytics_summary_tbl <- read_rds("00_data/google_analytics_summary_hourly.rds")

learning_labs_tbl <- read_rds("00_data/learning_labs.rds") 

subscribers_tbl   <- read_rds("00_data/mailchimp_users.rds")


# DATA PREPARATION ----
# - Apply Preprocessing to Target
limit_lower <-  0
offset <-  1

data_prepared_tbl <- subscribers_tbl %>% 
  summarize_by_time(optin_time, .by = 'day', optins = n()) %>% 
  pad_by_time(.pad_value = 0) %>% 
  
  #limiting
  mutate(optin_trans = log_interval_vec(optins, limit_lower = limit_lower,
                                            offset = offset)) %>% 
  
  #scaling
  mutate(optin_trans_stand = standardize_vec(optin_trans)) %>% 
  
  # removing first dates
  filter_by_time(.start_date = '2018-07-03') %>% 
  
  # cleaning outliers
  mutate(optin_trans_clean = ts_clean_vec(optin_trans_stand, period = 7)) %>% 
  mutate(optin_trans_stand = ifelse(optin_time %>% between_time('2018-11-18', '2018-11-20'),
                                    optin_trans_clean, optin_trans_stand))


data_prepared_tbl %>% 
  select(-optins, - optin_trans, - optin_trans_clean) %>% 
  pivot_longer(contains('tran')) %>% 
  plot_time_series(optin_time, value, name) 


# FEATURE INVESTIGATION ----

# 1.0 TIME-BASED FEATURES ----
# - tk_augment_timeseries_signature()

# * Time Series Signature ----
data_prep_signature_tbl <- data_prepared_tbl %>% 
  tk_augment_timeseries_signature() %>% 
  select(-diff, 
         -contains('iso'),
         - contains('xts'),
         -contains("hour"),
         -contains("minute"), 
         -contains("second"),
         -contains('am.pm' ))

# * Trend-Based Features ----
data_prep_signature_tbl %>% glimpse()
# ** Linear Trend
data_prep_signature_tbl %>% 
  plot_time_series_regression( optin_time, 
                               .formula = optin_trans_stand ~ index.num)

# ** Nonlinear Trend - Basis Splines
data_prep_signature_tbl %>% 
  plot_time_series_regression(
    optin_time,
    .formula = optin_trans_stand ~splines::bs(index.num, degree = 3),
    .show_summary = TRUE
  )

data_prep_signature_tbl %>% 
  plot_time_series_regression(
    optin_time,
    .formula = optin_trans_stand ~splines::ns(index.num, 
                                              knots = quantile(index.num, probs = c(0.33, 0.5, 0.8))),
    .show_summary = TRUE
  )

# * Seasonal Features ----

# Weekly Seasonality
data_prep_signature_tbl %>% 
  plot_time_series_regression(
    optin_time,
    .formula = optin_trans_stand ~ wday.lbl,
    .show_summary = TRUE
  )



# ** Monthly Seasonality
data_prep_signature_tbl %>% 
  plot_time_series_regression(
    optin_time,
    .formula = optin_trans_stand ~ month.lbl,
    .show_summary = TRUE
  )


# ** Together with Trend
model_formula_sea <- as.formula(
  optin_trans_stand ~ splines::ns(index.num,
                                  knots = quantile(index.num,probs =  c(0.25, 0.5))) + 
    wday.lbl +
    month.lbl + 
    . -
    optin_trans -
    optin_trans_clean -
    optin_trans_stand
    )


data_prep_signature_tbl %>% 
  plot_time_series_regression(optin_time, 
                              .formula = model_formula_sea, 
                              .show_summary = TRUE)




# 2.0 INTERACTIONS ----


  model_formula_interact <- as.formula(
    optin_trans_stand ~ splines::ns(index.num,
                                    knots = quantile(index.num,probs =  c(0.25, 0.5))) 
    + (as.factor(week2) * wday.lbl) + 
      . -
      optin_trans -
      optin_trans_clean -
      optin_trans_stand
  )

data_prep_signature_tbl %>% 
  plot_time_series_regression(optin_time, .formula =model_formula_interact, .show_summary = TRUE)
  

# 3.0 FOURIER SERIES ----
# - tk_augment_fourier

# Data Prep
data_prep_signature_tbl %>% 
  plot_acf_diagnostics(optin_time, optin_trans_stand)

data_prep_fourier_tbl <- data_prep_signature_tbl %>% 
  tk_augment_fourier(optin_time, .periods = c(7,30,90,365.25), .K = c(1,2,5,10,15))

data_prep_fourier_tbl %>% glimpse()

# Model
model_formula_fourier <- as.formula(
  optin_trans_stand ~ splines::ns(index.num,
                                  knots = quantile(index.num,probs =  c(0.25, 0.5))) 
  + (as.factor(week2) * wday.lbl) + 
    . -
    optin_trans -
    optins-
    optin_trans_clean -
    optin_trans_stand
)

data_prep_fourier_tbl %>% 
  plot_time_series_regression(optin_time, .formula =model_formula_fourier, .show_summary = TRUE)

# 4.0 LAGS ----
# - tk_augment_lags()

# Data Prep
data_prep_fourier_tbl %>% 
  plot_acf_diagnostics(
    optin_time, .value = optin_trans_stand, .lags = (8*7):600
  )

data_prep_lags_tbl <- data_prep_fourier_tbl %>% 
  tk_augment_lags(optin_trans_stand, .lags = c(63,70)) %>% 
  drop_na()
  

# Model
model_formula_lag <- as.formula(
  optin_trans_stand ~ splines::ns(index.num,
                                  knots = quantile(index.num,probs =  c(0.20))) 
  + (as.factor(week2) * wday.lbl) + 
    . -
    optin_trans -
    optins-
    optin_trans_clean -
    optin_trans_stand
)

# Visualize
data_prep_lags_tbl %>% 
  plot_time_series_regression(optin_time, 
                              .formula = model_formula_lag,
                              .show_summary = TRUE)

# 5.0 SPECIAL EVENTS ----

# Data Prep
learning_lab_daily_tbl <- learning_labs_tbl %>% 
  mutate(event_date = lubridate::ymd_hms(event_date)) %>% 
  summarize_by_time(event_date, .by = 'day', event = n())

data_prep_events_tbl <- data_prep_lags_tbl %>%
  left_join(learning_lab_daily_tbl, by = c('optin_time'='event_date')) %>% 
  mutate(event = ifelse(is.na(event), 0, event))

data_prep_events_tbl %>% glimpse()

g <- data_prep_events_tbl %>% 
  plot_time_series(optin_time, optin_trans_stand, .interactive = FALSE) +
  geom_point(color = 'red' , data = . %>% filter(event == 1))

ggplotly(g)


# Model
model_formula_event <- as.formula(
  optin_trans_stand ~ splines::ns(index.num,
                                  knots = quantile(index.num,probs =  c(0.3, 0.6, 0.7, 0.9))) 
  + (as.factor(week2) * wday.lbl) + 
    . -
    optin_trans -
    optins-
    optin_trans_clean -
    optin_trans_stand
)

data_prep_events_tbl %>% 
  plot_time_series_regression(optin_time, 
                              .formula = model_formula_event,
                              .show_summary = TRUE)


# Visualize

# 6.0 EXTERNAL LAGGED REGRESSORS ----
# - xregs

# Data Prep
google_analytics_prep_tbl <- google_analytics_summary_tbl %>% 
  mutate(date = ymd_h(dateHour)) %>% 
  summarize_by_time(date, .by = 'day', across(pageViews:sessions, .fns = sum)) %>% 
  mutate(across(pageViews:sessions, .fns = log1p)) %>% 
  mutate(across(pageViews:sessions, .fns = standardize_vec))

data_prep_google_tbl <- data_prep_events_tbl %>% 
  left_join(google_analytics_prep_tbl , by = c('optin_time'='date')) %>% 
  mutate(pageViews_lag1 = lag_vec(pageViews, lag = 1)) %>% 
  drop_na()

data_prep_google_tbl %>% 
  plot_acf_diagnostics(optin_time, optin_trans_stand, .ccf_vars = pageViews:sessions, .show_ccf_vars_only = TRUE)

# Model
model_formula_event <- as.formula(
  optin_trans_stand ~ splines::ns(index.num,
                                  knots = quantile(index.num,probs =  c(0.3, 0.6, 0.7, 0.9))) 
  + (as.factor(week2) * wday.lbl) + 
    . -
    optin_trans -
    optins-
    optin_trans_clean -
    optin_trans_stand-
    pageViews 
)



# Visualize

data_prep_google_tbl %>% 
  plot_time_series_regression(optin_time, 
                              .formula = model_formula_event,
                              .show_summary = TRUE)


data_prep_google_tbl %>% 
  select(optin_time, optin_trans_stand, pageViews) %>% 
  pivot_longer(-optin_time) %>% 
  plot_time_series(optin_time, value, name, .smooth = FALSE)

#linear model
model_formula_event <- as.formula(
  optin_trans_stand ~ splines::ns(index.num,
                                  knots = quantile(index.num,probs =  c(0.3, 0.6, 0.7, 0.9))) 
  + (as.factor(week2) * wday.lbl) + 
    . -
    optin_trans -
    optins-
    optin_trans_clean -
    optin_trans_stand
)


model_fit_best_lm <- lm(model_formula_event, data = data_prep_events_tbl)

model_fit_best_lm$terms %>% formula()

write_rds(model_fit_best_lm, file = '00_models/model_fit_best_lm.rds')

# 7.0 RECOMMENDATION ----
# - Best model: 
# - Best Model Formula:


