# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: ADVANCED TIME SERIES FORECASTING FOR BUSINESS
# MODULE: VISUALIZATION TOOLS ----

# OBJECTIVES ----
# Introduction to:
# - Time Plot
# - Autocorrelation
# - Seasonality 
# - Anomalies
# - Seasonal Decomposition (STL)
# - Time Series Regression (TSLM)

# LIBRARIES ----

library(tidyverse)
library(timetk)
library(lubridate)

# DATA ----

google_analytics_tbl <- read_rds("00_data/google_analytics_summary_hourly.rds")
google_analytics_tbl 

mailchimp_users_tbl <- read_rds("00_data/mailchimp_users.rds")
mailchimp_users_tbl

# DATA PREPARATION ---- 

google_analytics_long_hour_tbl <- google_analytics_tbl %>% 
  mutate(data = ymd_h(dateHour)) %>% 
  select(-dateHour) %>% 
  pivot_longer(cols = pageViews:sessions)

subscriber_day_tbl <- mailchimp_users_tbl %>% 
  summarize_by_time(.by       = 'day', 
                    .date_var = optin_time, 
                    optins    = n()) %>% 
  pad_by_time(.by = 'day' ,
              .pad_value = 0)

# 1.0 TIME SERIES PLOT ----
# - Primary Visualization Tool
# - Spot issues and understand one or more time series

?plot_time_series

# * Basics ----
subscriber_day_tbl %>% 
  plot_time_series(optin_time, 
                   optins)

google_analytics_long_hour_tbl %>% 
  plot_time_series(data, value)

# * Facets/Groups ----
google_analytics_long_hour_tbl %>% 
  plot_time_series(data, value, .color_var = name)

google_analytics_long_hour_tbl %>% 
  group_by(name)%>% 
  plot_time_series(data, value, .color_var = name)

google_analytics_long_hour_tbl %>% 
  #group_by(name)%>% 
  plot_time_series(data, value, .color_var = name, .facet_vars = name)


# * Mutations/Transformations ----
subscriber_day_tbl %>% 
  plot_time_series(optin_time, log(optins+1))


google_analytics_long_hour_tbl %>% 
  group_by(name) %>% 
plot_time_series(
  .date_var = data,
  .value = log(value +1)
)


# * Smoother Adjustment
subscriber_day_tbl %>% 
  plot_time_series(optin_time, log(optins+1),
                   .smooth_period = '30 days',
                   .smooth_degree = 2,
                   .smooth_span = 0.05)


# * Static ggplot ----





# 2.0 ACF Diagnostics ----
# - Detecting Lagged Features
?plot_acf_diagnostics

# * ACF / PACF -----
# - Date Features & Fourier Series 
subscriber_day_tbl %>% 
  plot_acf_diagnostics(optin_time, log(optins +1))



# * CCF ----
# - Lagged External Regressors

google_analytics_day_tbl <- google_analytics_long_hour_tbl %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  summarize_by_time(.date_var = data,
                    .by = 'day',
                    across(pageViews:sessions, .fns = sum))


subscriber_ga_day_tbl <- subscriber_day_tbl %>% 
  left_join(google_analytics_day_tbl, by = c('optin_time'='data')) 

subscriber_ga_day_tbl %>%
  drop_na() %>% 
  plot_acf_diagnostics(optin_time, optins, 
                       .ccf_vars = pageViews:sessions)

# 3.0 SEASONALITY ----
# - Detecting Time-Based Features

?plot_seasonal_diagnostics

google_analytics_long_hour_tbl %>% 
  group_by(name) %>% 
  plot_seasonal_diagnostics(.date_var = data,
                            .value = log(value+1),
                            .feature_set = c('hour', 'wday.lbl'),
                            .geom = 'violin')




# 4.0 ANOMALIES ----
# - Detecting Events & Possible Data Issues

?plot_anomaly_diagnostics

subscriber_day_tbl %>% 
  plot_anomaly_diagnostics(.date_var = optin_time, 
                           .value = optins,
                           .alpha = 0.01,
                           .max_anomalies = 0.01)

subscriber_day_tbl %>% 
  tk_anomaly_diagnostics(.date_var = optin_time, 
                         .value = optins,
                         .alpha = 0.01,
                         .max_anomalies = 0.01)
  


google_analytics_long_hour_tbl %>% 
  group_by(name) %>% 
  plot_anomaly_diagnostics(.date_var = data,
                           .value = value)

# 5.0 SEASONAL DECOMPOSITION ----
# - Detecting Trend and Seasonal Cycles

?plot_stl_diagnostics
subscriber_day_tbl %>% 
  plot_stl_diagnostics(.date_var = optin_time, .value = log(optins+1),
                       .frequency = '1 month',
                       .trend = '1 year')



google_analytics_long_hour_tbl %>% 
  group_by(name) %>% 
  plot_stl_diagnostics(.date_var = data, .value = log(value+1))


# 6.0 TIME SERIES REGRESSION PLOT ----
# - Finding features

?plot_time_series_regression

subscriber_day_tbl %>% 
  plot_time_series_regression(.date_var = optin_time, 
                              log(optins+1) ~ as.numeric(optin_time) + wday(optin_time,
                                                                     label = TRUE) +
                                month(optin_time, label = TRUE),
                              .show_summary = TRUE)

google_analytics_long_hour_tbl %>% 
  group_by(name) %>% 
  plot_time_series_regression(
    .date_var = data,
    log(value + 1) ~ as.numeric(data) +
      as.factor(hour(data))+
      wday(data, label = TRUE)+
      month(data, label = TRUE),
    .show_summary = TRUE
  ) 


