# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: TIME SERIES DATA WRANGLING ----


# GOAL ----
# - Gain exposure to timetk data wrangling functionality

# OBJECTIVES ----
# - Summarize/Pad - Manipulate Data to different periodicities (scales, intervals)
# - Filter - Zoom in & Slice Time Series
# - Mutate - Apply mutations by time groups
# - Joining Time Series
# - Index Operations
# - Future Frame - Forecasting Exposure



# LIBRARIES ----

library(tidyverse)
library(timetk)
library(lubridate)
library(DataExplorer)

# DATA ----

google_analytics_summary_tbl <- read_rds("00_data/google_analytics_summary_hourly.rds")
google_analytics_summary_tbl 

mailchimp_users_tbl  <- read_rds("00_data/mailchimp_users.rds")
mailchimp_users_tbl 

transactions_tbl  <- read_rds("00_data/transactions_weekly.rds")
transactions_tbl 


# 1.0 SUMMARIZE BY TIME  ----
# - APPLY COMMON AGGREGATIONS
# - HIGH TO LOW FREQ

# * To Daily - Subscribers ----
#ungrouped
subscribers_daily_tbl <- mailchimp_users_tbl %>% 
  summarize_by_time(optin_time,
                    by = 'day', 
                    optins = n())

#grouped
mailchimp_users_tbl %>% 
  group_by(member_rating) %>% 
  summarize_by_time(optin_time,
                    by = 'day', 
                    optins = n()) %>% 
  plot_time_series(optin_time, optins)


# * To Daily - GA Summary ----
google_analytics_day_tbl <- google_analytics_summary_tbl %>% 
  mutate(DateHour = ymd_h(dateHour)) %>% 
  summarise_by_time(DateHour,
                .by = 'day',
                across(pageViews:sessions, .fns = sum))


google_analytics_day_tbl %>% 
  pivot_longer(pageViews:sessions) %>% 
  plot_time_series(DateHour, 
                   value, 
                   .facet_var = name)
# * To Weekly - Subscribers ----

subscribers_week_tbl <- mailchimp_users_tbl %>% 
  summarize_by_time(optin_time, 
                    .by = '1 week', 
                    optins = n())


# * To Monthly - Transactions ----
transactions_month_tbl  <- transactions_tbl %>% 
  summarize_by_time(purchased_at, 
                    .by = 'month', 
                    revenue = sum(revenue),
                    .type = 'ceiling') %>% 
  mutate(purchased_at = purchased_at %-time% '1 day')


# 2.0 PAD BY TIME ----
# - Filling in Gaps
# - Going from Low to High Frequency (un-aggregating)

# * Fill Daily Gaps ----
subscribers_daily_tbl %>% 
  pad_by_time(.start_date = '2018-07-01',
              .by = 'day',
              .pad_value = 0)



# * Weekly to Daily ----
transactions_tbl %>% 
  pad_by_time(purchased_at, .by = 'day', 
              .start_date = '2018-06') %>% 
  mutate_by_time(.by = 'week', revenue_spread = sum(revenue, na.rm = T)/7)




# 3.0 FILTER BY TIME ----
# - Pare data down before modeling

# * Slicing - Everything after the BIG anomaly ----
subscribers_daily_tbl %>% 
  filter_by_time(.start_date = '2018-11-20') %>% 
  plot_time_series(optin_time, optins , .plotly_slider = T)




# * Zooming In - Just December 2018 ----
subscribers_daily_tbl %>% 
  filter_by_time(.start_date = '2018-12', .end_date = '2018-12') %>% 
  plot_time_series(optin_time, optins , .plotly_slider = T)


# * Offsetting - Using plus-time and minus-time offsets to get things just right ----


# 4.0 MUTATING BY TIME -----
# - Get change from beginning/end of period

# * First, Last, Mean, Median by Period ----
transactions_tbl %>% 
  mutate_by_time(purchased_at, 
                 .by = '3 month', 
                 revenue_mean = mean(revenue),
                 revenue_median = median(revenue),
                 revenue_max = max(revenue),
                 revenue_min = min(revenue)) %>%
  pivot_longer(contains('revenue' )) %>% 
  plot_time_series(purchased_at, value, name, .smooth = F)




# 5.0 JOINING BY TIME ----
# - Investigating Relationships
# - Identify External Regressors

# * Subscribers + GA Summary Web Traffic ----
subscriber_daily_padded_tbl <- subscribers_daily_tbl %>% 
  pad_by_time(.pad_value = 0 , .start_date = '2018-06')

subscribers_google_joined_tbl <- subscriber_daily_padded_tbl %>% 
  left_join(google_analytics_day_tbl, by = c('optin_time'='DateHour')) %>% 
  select(-by)


# * Inspect Join -----
subscribers_google_joined_tbl %>% 
  plot_missing()

google_analytics_day_tbl %>% 
  tk_summary_diagnostics()

subscribers_google_joined_tbl %>% 
  tk_summary_diagnostics()

subscribers_google_joined_tbl %>% 
  pivot_longer(-optin_time) %>% 
  plot_time_series(optin_time, value, .color_var = name )

# * Visualization Techniques (Relationships) ----
log_standardize_tbl <- subscribers_google_joined_tbl %>% 
  drop_na() %>% 
  mutate(across(optins:sessions, .fns = log1p)) %>% 
  mutate(across(optins:sessions, .fns = standardize_vec))

log_standardize_tbl %>%  
  pivot_longer(optins:sessions) %>% 
  plot_time_series(optin_time, value, .color_var = name, .smooth = FALSE)

log_standardize_tbl %>% 
  plot_acf_diagnostics(optin_time, 
                       optins,
                       .ccf_vars = pageViews:sessions , 
                       .show_ccf_vars_only = TRUE)
## cross-correlation

# 6.0 WORKING WITH THE INDEX ----
# - Index Manipulations

# * Making an index ----
tibble(date = tk_make_timeseries('2011', 
                   length_out = 100,
                   by = 'month'))

# * Holiday Sequence ----
tk_make_holiday_sequence('2011','2021' ,calendar = 'NYSE') %>% 
  tk_get_holiday_signature()

# * Offsetting time ----
'2011-01-01' %-time% '1 month'

# * Extending an index ----
tk_make_timeseries('2011-01') %>% 
  tk_make_future_timeseries(length_out = '1 month')



# 7.0 FUTURE FRAME ----
# - Forecasting helper
google_analytics_day_tbl %>% 
  plot_time_series(DateHour, pageViews)


# * Future Frame ----
google_analytics_day_tbl %>% 
  future_frame(.length_out = '2 months')


# * Modeling ----
model_fit_lm <- lm(pageViews ~ as.numeric(DateHour) + wday(DateHour, label = TRUE),
   data = google_analytics_day_tbl)

future_tbl <- google_analytics_day_tbl %>% 
  future_frame(.length_out = '2 months') 

predictions_vec <- predict(model_fit_lm, newdata = future_tbl) %>% as.vector()
# * Visualizing ----

google_analytics_day_tbl %>% 
  select(pageViews, DateHour) %>% 
  add_column(type = 'actual') %>% 
  bind_rows(
    future_tbl  %>% 
      mutate(pageViews = predictions_vec,
             type = 'prediction')
  ) %>% 
  plot_time_series(DateHour, pageViews, .color_var = type)


