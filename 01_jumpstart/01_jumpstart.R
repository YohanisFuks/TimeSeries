# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: ADVANCED TIME SERIES FORECASTING FOR BUSINESS
# MODULE: TIME SERIES JUMPSTART 

# GOAL: Forecast Daily Email Users - Next 8-WEEKS

# OBJECTIVES ----
# - Dive into a time-series analysis project
# - Experience Frameworks: modeltime
# - Experience 2 Algorithms:
#   1. Prophet
#   2. LM w/ Engineered Features

# LIBRARIES ----
#time series machine learning
library(tidymodels)
library(modeltime)

#EDA
library(DataExplorer)

#core
library(tidyverse)
library(timetk)
library(lubridate)

# DATA -----
mailchimp_users_tbl <- read_rds('00_data/mailchimp_users.rds')


# 1.0 EDA & DATA PREP ----
# * DAILY SUBSCRIBERS INCREASES
mailchimp_users_tbl %>% glimpse()

#count of optins by day
optins_day_tbl <- mailchimp_users_tbl %>% 
  summarize_by_time(
    .date_var = optin_time,
    .by = 'day',
    optins = n()
  ) 

#summary diagnostics ----

optins_day_tbl %>% tk_summary_diagnostics(.date_var = optin_time)

optin_day_prepared_tbl <- optins_day_tbl %>% pad_by_time(.by = 'day',
                               .date_var = optin_time ,
                               .pad_value = 0)

optin_day_prepared_tbl %>% 
  plot_time_series(.date_var = optin_time,
                   optins)

# 2.0 EVALUATION PERIOD ----

evalutaion_tbl <- optins_day_tbl %>% 
  filter_by_time(.date_var = optin_time,
                 .start_date = '2018-11-20',
                 .end_date = 'end') 


evalutaion_tbl %>% 
  plot_time_series(optin_time, optins)

#train/test

splits <- evalutaion_tbl %>% 
  time_series_split(date_var = optin_time,
                    assess = '8 week', 
                    cumulative = TRUE)

splits %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(optin_time, optins)


# 3.0 PROPHET FORECASTING ----

#prophet model using ModelTime/Parsnip

model_prophet_fit <- prophet_reg() %>% 
  set_engine('prophet') %>% 
  fit(optins ~ optin_time, data = training(splits))


# modeltime process

model_tbl <- modeltime_table(model_prophet_fit)

#calibration
calibration_tbl <- model_tbl %>% modeltime_calibrate(new_data = testing(splits))

#visualize forecast
calibration_tbl %>%
  modeltime_forecast(actual_data = evalutaion_tbl) %>% 
  plot_modeltime_forecast()

#get accuracy metrics
calibration_tbl %>% 
  modeltime_accuracy()

# 4.0 FORECASTING WITH FEATURE ENGINEERING ----

# identify possible features

evalutaion_tbl %>% 
  plot_seasonal_diagnostics(optin_time, log(optins))


# * recipes spec
training(splits)
recipes_spec <- recipe(optins ~ . ,data = training(splits)) %>% 
  #time signatures
step_timeseries_signature(optin_time) %>% 
  step_rm(ends_with('.iso'), 
          ends_with('.xts'), 
          contains('hour'), 
          contains('minute'),
          contains('second'),
          contains('am.pm')) %>% 
  step_normalize(ends_with('index.num'),
                 ends_with('year')) %>% 
  step_dummy(all_nominal())

recipes_spec %>% prep() %>% 
  juice() 

# machine learning specs
model_spec <- linear_reg() %>% 
  set_engine('lm') 

workflow_fit_lm <- workflow() %>% 
  add_model(model_spec) %>% 
  add_recipe(recipes_spec) %>% 
  fit(training(splits))

# modeltime process

calibration_tbl <- modeltime_table(
  model_prophet_fit,
  workflow_fit_lm
) %>% modeltime_calibrate(new_data = testing(splits))

calibration_tbl %>% 
  modeltime_accuracy()

calibration_tbl %>% 
  modeltime_forecast(new_data = testing(splits),
                     actual_data = evalutaion_tbl) %>% 
  plot_modeltime_forecast()


# 5.0 SUMMARY & NEXT STEPS ----

# * What you've learned ----
# - You've been exposed to:
#   - Tidymodels / Modeltime Framework
# - You've seen 2 modeling approaches:
#   - Prophet - Univariate, Automatic
#   - Linear Regression Model - Many recipe steps
# - You've experienced Feature Engineering
#   - Visualizations: ACF, Seasonality
#   - Feature Engineering from Date Variables
#
# * Where you are going! ----
# - You still need to learn:
#   - New algorithms
#   - Machine Learning - How to tune parameters
#   - Feature Engineering Strategies
#   - Ensembling - Competition winning strategy
#   - and a lot more!

