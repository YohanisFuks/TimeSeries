# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: EXPONENTIAL SMOOTHING

# GOAL: Understand Exponential Smoothing

# OBJECTIVES ----
# - ETS - Exponential Smoothing
# - TBATS - Multiple Seasonality Models
# - Seasonal Decomposition - Multiple Seasonality Models

# LIBRARIES & SETUP ----

# Time Series ML
library(tidymodels)
library(modeltime)
library(forecast)

# Core 
library(tidyverse)
library(lubridate)
library(timetk)


# DATA ----

artifacts_list <- read_rds("00_models/feature_engineering_artifacts_list.rds") 

data_prepared_tbl <- artifacts_list$data$data_prepared_tbl 
data_prepared_tbl

# TRAIN / TEST SPLITS ----

splits <- time_series_split(data_prepared_tbl, assess = "8 weeks", cumulative = TRUE)

splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(optin_time, optins_trans)

train_tbl <- training(splits) %>%
    select(optin_time, optins_trans)

# EXPONENTIAL SMOOTHING (ETS) -----
# - Error, Trend, Seasonal Model - Holt-Winters Seasonal
# - Automatic forecasting method based on Exponential Smoothing
# - Single Seasonality
# - Cannot use Xregs (purely univariate)
a <- 0.4

exp_smoother <- function(i){
  a * (1-a)^i
}

# * Exponential Smoothing ----
train_tbl %>% 
  slice_tail(n = 8) %>% 
  arrange(desc(optin_time)) %>% 
  mutate(id = 1:n()-1) %>% 
  mutate(wt = exp_smoother(id)) %>% 
  mutate(value = wt * optins_trans) %>% 
  summarise(value = sum(value))
linear_reg() %>% 
  set_engine('lm')


# * ETS Model ----
?exp_smoothing
model_fit_ets <- exp_smoothing(error = 'additive',
              trend = 'none',
              season = 'none') %>% 
  set_engine('ets') %>% 
  fit(optins_trans ~ optin_time, data = training(splits))

# * Modeltime ----
modeltime_table(
  model_fit_ets
) %>% 
  modeltime_calibrate(testing(splits)) %>% 
  modeltime_forecast(new_data = testing(splits),actual_data = data_prepared_tbl) %>% 
  plot_modeltime_forecast()


# TBATS ----
# - Multiple Seasonality Model
# - Extension of ETS for complex seasonality
# - Automatic
# - Does not support XREGS

# * TBATS Model ----
model_fit_tbats <- seasonal_reg(seasonal_period_1 = 7,
             seasonal_period_2 = 30,
             seasonal_period_3 = 365) %>% 
  set_engine('tbats') %>% 
  fit(optins_trans ~ optin_time, data = training(splits))

# * Modeltime ----
modeltime_table(
  model_fit_ets,
  model_fit_tbats) %>% 
  modeltime_calibrate(new_data = testing(splits)) %>% 
  modeltime_forecast(new_data = testing(splits), 
                     actual_data = data_prepared_tbl) %>% 
  plot_modeltime_forecast()

# SEASONAL DECOMPOSITION ----
# - Uses seasonal decomposition to model 
#   trend & seasonality separately
#   - Trend modeled with ARIMA or ETS
#   - Seasonality modeled with Seasonal Naive (SNAIVE)
# - Can handle multiple seasonality
# - ARIMA version accepts XREGS, ETS does not

# Multiple Seasonal Decomposition
?msts()
msts(train_tbl$optins_trans, seasonal.periods = c(7,30,364/2)) %>% 
  mstl() %>% 
  autoplot()


# * STLM ETS Model ----
model_fit_stlm <- seasonal_reg(seasonal_period_1 = 7,seasonal_period_2 = 30,seasonal_period_3 = 365/2 ) %>% 
  set_engine('stlm_ets') %>% 
  fit(optins_trans ~ optin_time, data = training(splits)) %>% 
  autoplot()



# * STLM ARIMA Model ----
model_fit_stlm_arima <- seasonal_reg(seasonal_period_1 = 7,seasonal_period_2 = 30,seasonal_period_3 = 365/2) %>% 
  set_engine('stlm_arima') %>% 
  fit(optins_trans ~ optin_time, data = training(splits)) 

model_fit_stlm_arima_xreg <- seasonal_reg(seasonal_period_1 = 7,seasonal_period_2 = 30,seasonal_period_3 = 365/2) %>% 
  set_engine('stlm_arima') %>% 
  fit(optins_trans ~ optin_time + lab_event, data = training(splits)) 

# * Modeltime ----

model_tbl <- modeltime_table(
  model_fit_ets,
  model_fit_stlm,
  model_fit_stlm_arima,
  model_fit_tbats,
  model_fit_stlm_arima_xreg
) 


# EVALUATION ----

# * Calibration ----
calibration_tbl <- model_tbl%>% 
  modeltime_calibrate(testing(splits))

# * Forecast Test ----
calibration_tbl  %>% 
  modeltime_forecast(new_data = testing(splits),
                     actual_data = data_prepared_tbl) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


# * Accuracy Test ----
calibration_tbl %>% 
  modeltime_accuracy()

# * Refit ----
refit_tbl <- calibration_tbl %>% 
  modeltime_refit(data = data_prepared_tbl)

refit_tbl %>% 
  modeltime_forecast(artifacts_list$data$forecast_tbl, actual_data = data_prepared_tbl) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)



#saving
calibration_tbl %>% write_rds('00_models/calibration_tbl_ets_tbats.rds')




