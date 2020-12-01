# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: ADVANCED TIME SERIES FORECASTING FOR BUSINESS
# MODULE: INSPECT COURSE DATASETS


# LIBRARIES ----
library(tidyverse)


# DATA -----

# * Establish Relationships ----
#   - Website traffic (Page Views, Sessions, Organic Traffic)
#   - Top 20 Pages

#Google Analytics - Hourly data
read_rds('00_data/google_analytics_summary_hourly.rds')

#Google Analytics - Top 20 pages Daily
read_rds('00_data/google_analytics_by_page_daily.rds')

# * Build Relationships ----
#   - Collect emails
#   - Host Events

#MailChimp data
read_rds('00_data/mailchimp_users.rds')

#Learning labs
read_rds('00_data/learning_labs.rds')


# * Generate Course Revenue ----
#   - Revenue data (aggregated at weekly interval)
#   - Product Events

#transactions weekly
read_rds('00_data/transactions_weekly.rds')

#product events
read_rds('00_data/product_events.rds')

