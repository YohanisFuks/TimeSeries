# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: BOOSTED ALGORITHMS

# GOAL: Understand Boosting Errors

# OBJECTIVES ----
# - Learn about modeling residual errors
# - Apply Prophet + XGBoost

# LIBRARIES & SETUP ----

# Time Series ML
library(tidymodels)
library(modeltime)

# Core 
library(tidyverse)
library(lubridate)
library(timetk)

# DATA ----

artifacts_list <- read_rds("00_models/feature_engineering_artifacts_list_checkpoint.rds") 

data_prepared_tbl <- artifacts_list$data$data_prepared_tbl 
data_prepared_tbl

# FUNCTIONS & MODELS ----
source("00_scripts/01_calibrate_and_plot.R")

model_fit_best_prophet <- read_rds("00_models/model_fit_best_prophet_checkpoint.rds")
model_fit_best_arima   <- read_rds("00_models/model_fit_best_arima.rds")

# TRAIN / TEST SPLITS ----

splits <- time_series_split(data_prepared_tbl, assess = "8 weeks", cumulative = TRUE)

splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(optin_time, optins_trans)


# 1.0 PROPHET BOOST ----

# * Best Prophet Model ----
model_fit_best_prophet$preproc$terms %>% formula()

calibrate_and_plot(model_fit_best_prophet)

# Error - Broken Model


# Fixing a broken model
model_tlb_best_prophet <- modeltime_table(model_fit_best_prophet) %>% 
                           modeltime_refit(training(splits))


model_tlb_best_prophet

model_fit_best_prophet_refited <- model_tlb_best_prophet$.model[[1]]

calibrate_and_plot(model_fit_best_prophet_refited)



# * Boosting Prophet Models ----

# Recipes
recipe_spec_base_no_lag <- artifacts_list$recipes$recipe_spec_base %>% 
  step_rm(starts_with('lag'))

recipe_spec_base_no_lag %>% 
  prep() %>% 
  juice() %>% 
  glimpse()


# Model Spec
?prophet_boost()

model_spec_prophet_boost <- prophet_boost(changepoint_num = 25,
              changepoint_range = 0.8,
              min_n = 22,
              tree_depth = 3,
              learn_rate  = 0.22,
              trees = 200,
              mtry = 0.75,
              loss_reduction = 0.15,
              seasonality_yearly = FALSE,
              seasonality_weekly = FALSE,
              seasonality_daily = FALSE) %>% 
set_engine('prophet_xgboost')

# Workflow
set.seed(42)
workflow_fit_prophet_boost <- workflow() %>% 
  add_model(model_spec_prophet_boost) %>% 
  add_recipe(recipe_spec_base_no_lag) %>% 
  fit(training(splits))

calibrate_and_plot(
  model_fit_best_prophet_refited,
  workflow_fit_prophet_boost
)

modeltime_table(workflow_fit_prophet_boost) %>% 
  modeltime_residuals(new_data = training(splits)) %>% 
  plot_modeltime_residuals()

# 2.0 ARIMA BOOST ----


# * Best ARIMA Model ----
calibrate_and_plot( model_fit_best_arima)


# * Boosting ARIMA -----
?arima_boost()

set.seed(42)
model_spec_arima_boost <- arima_boost(mode = 'regression',
            #seasonal_period = ,
            tree_depth = 4,
            learn_rate = 0.31,
            min_n = 15,
            loss_reduction = 1,
            mtry = 0.85,
            trees = 200) %>% 
  set_engine('auto_arima_xgboost')

workflow_fit_arima_boost <- workflow() %>% 
  add_model(model_spec_arima_boost) %>% 
  add_recipe(recipe_spec_base_no_lag) %>% 
  fit(training(splits))

calibrate_and_plot(workflow_fit_arima_boost,
                   workflow_fit_prophet_boost)

# 3.0 MODELTIME EVALUATION ----

# * Modeltime ----
model_tbl <- modeltime_table(
  workflow_fit_arima_boost,
  workflow_fit_prophet_boost,
  model_fit_best_arima,
  model_fit_best_prophet_refited
) 
  


# * Calibration ----
calibration_tbl <- model_tbl %>% 
  modeltime_calibrate(testing(splits))

calibration_tbl



# * Accuracy Test ----
calibration_tbl %>% 
  modeltime_accuracy()


# * Forecast Test ----
calibration_tbl %>% 
  modeltime_forecast(new_data = testing(splits),
                     actual_data = data_prepared_tbl) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)


# * Refit ----
refit_tbl <- calibration_tbl %>% 
  modeltime_refit(data_prepared_tbl)

refit_tbl %>% 
  modeltime_forecast(new_data = artifacts_list$data$forecast_tbl,
                     actual = data_prepared_tbl) %>% 
  plot_modeltime_forecast(.conf_interval_show = FALSE)




# 4.0 SAVE ARTIFACTS ----
calibration_tbl %>% 
  write_rds('00_models/calibration_tlb_boosted_models.rds')






