# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: MACHINE LEARNING

# GOAL: Understand Machine Learning Algorithms

# OBJECTIVES ----
# - Exposure to key ML algorithms
# - Inspect Key Parameters
# - Show Modeltime Workflow

# LIBRARIES & SETUP ----

# Time Series ML
library(tidymodels)
library(modeltime)
library(rules)

# Core 
library(tidyverse)
library(lubridate)
library(timetk)

# DATA ----

artifacts_list <- read_rds("00_models/feature_engineering_artifacts_list_checkpoint.rds") 
data_prepared_tbl <- artifacts_list$data$data_prepared_tbl 

data_prepared_tbl

# TRAIN / TEST SPLITS ----

splits <- time_series_split(data_prepared_tbl, assess = "8 weeks", cumulative = TRUE)

splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(optin_time, optins_trans)

# RECIPES ----

recipe_spec_base     <- artifacts_list$recipes$recipe_spec_base
recipe_spec_base

recipe_spec_1_spline <- artifacts_list$recipes$recipe_spec_1
recipe_spec_1_spline

recipe_spec_2_lag    <- artifacts_list$recipes$recipe_spec_2
recipe_spec_2_lag

training(splits) %>% glimpse()

recipe_spec_1_spline %>% prep() %>% juice() %>% glimpse()

# 1.0 ELASTIC NET REGRESSION ----
# - Strengths: Very good for trend
# - Weaknesses: Not as good for complex patterns (i.e. seasonality)

# Spline
model_spec_glmnet <- linear_reg(penalty = 0.01 ,
           mixture = 0.5 ) %>% 
  set_engine('glmnet') 

workflow_fit_glmnet_splines <- workflow() %>% 
  add_model(model_spec_glmnet) %>% 
  add_recipe(recipe_spec_1_spline) %>% 
  fit(training(splits))


# Lag
workflow_fit_glmnet_lag <- workflow() %>% 
  add_model(model_spec_glmnet) %>% 
  add_recipe(recipe_spec_2_lag) %>% 
  fit(training(splits))

# Calibrate & Plot
calibration_tbl <- modeltime_table(
  workflow_fit_glmnet_splines,
  workflow_fit_glmnet_lag
) %>% 
  update_model_description(1., 'GLMNET splits') %>% 
  update_model_description(2, 'GLMNET Lag') %>% 
  modeltime_calibrate(testing(splits))

calibration_tbl %>% 
  modeltime_accuracy()


calibration_tbl %>% 
  modeltime_forecast(new_data = testing(splits), actual_data = data_prepared_tbl) %>% 
  plot_modeltime_forecast()

# *** PLOTTING UTILITY *** ----
# - Calibrate & Plot

calibrate_and_plot <- function(..., type = 'testing') {
  if (type == 'testing'){
    new_data <- testing(splits) 
  } else{
    new_data = training(splits) %>% drop_na()
  }
  
  calibration_tbl <-  modeltime_table(...) %>% 
    modeltime_calibrate(new_data) 
    print(calibration_tbl %>% modeltime_accuracy())
  calibration_tbl %>% 
    modeltime_forecast(
      new_data = new_data,
      actual_data = data_prepared_tbl
    ) %>% 
    plot_modeltime_forecast(.conf_interval_show  = FALSE)
}

calibrate_and_plot(
  workflow_fit_glmnet_splines,
  workflow_fit_glmnet_lag,
  type = 'testing'
)

# 2.0 MARS ----
# Multiple Adaptive Regression Splines
# - Strengths: Best algorithm for modeling trend
# - Weaknesses: 
#   - Not good for complex patterns (i.e. seasonality)
#   - Don't combine with splines! MARS makes splines.
# - Key Concept: Can combine with xgboost (better seasonality detection)
#   - prophet_reg: uses a technique similar to mars for modeling trend component
#   - prophet_boost: Uses prophet for trend, xgboost for features

model_spec_mars <- mars(mode = 'regression',
                        num_terms = 10) %>% 
  set_engine('earth', endspan = 150) 


# Simple Numeric
workflow_fit_mars_simple <- workflow() %>% 
  add_model(model_spec_mars) %>% 
  add_formula(optins_trans ~ as.numeric(optin_time)) %>% 
  fit(training(splits))


workflow_fit_mars_spline <- workflow() %>% 
  add_model(model_spec_mars) %>% 
  add_recipe(recipe_spec_1_spline) %>% 
  fit(training(splits))


workflow_fit_mars_lag <- workflow() %>% 
  add_model(model_spec_mars) %>% 
  add_recipe(recipe_spec_2_lag) %>% 
  fit(training(splits))

calibrate_and_plot(workflow_fit_mars_simple,
                   workflow_fit_mars_spline,
                   workflow_fit_mars_lag,
                   type = 'testing')


# Spline


# Lag


# Calibrate & Plot


# 3.0 SVM POLY ----
# Strengths: Well-rounded algorithm
# Weaknesses: Needs tuned or can overfit
model_spec_svm_poly <- svm_poly(mode = 'regression',
                                cost = 5,
                                degree = 1,
                                scale_factor = 3) %>% 
  set_engine('kernlab')


# Spline
set.seed(123)
workflow_fit_svm_splines <-  workflow() %>% 
  add_model(model_spec_svm_poly) %>% 
  add_recipe(recipe_spec_1_spline) %>% 
  fit(training(splits))

calibrate_and_plot(workflow_fit_svm_splines,
                   type = 'testing')

# Lag
set.seed(123)
workflow_fit_svm_lag <-  workflow() %>% 
  add_model(model_spec_svm_poly) %>% 
  add_recipe(recipe_spec_2_lag) %>% 
  fit(training(splits))


# Calibrate & Plot
calibrate_and_plot(workflow_fit_svm_lag,
                   workflow_fit_svm_splines,
                   type = 'testing')



# 4.0 SVM RADIAL BASIS ----
# Strengths: Well-rounded algorithm
# Weaknesses: Needs tuned or can overfit


# Spline
model_spec_svm_rbf <- svm_rbf(mode = 'regression',
                              cost = 2,
                              rbf_sigma = 0.01) %>% 
  set_engine('kernlab')

workflow_fit_rbf_spline <-   workflow() %>% 
  add_model(model_spec_svm_rbf) %>% 
  add_recipe(recipe_spec_1_spline) %>% 
  fit(training(splits))


# Lag
workflow_fit_rbf_lag <-   workflow() %>% 
  add_model(model_spec_svm_rbf) %>% 
  add_recipe(recipe_spec_2_lag) %>% 
  fit(training(splits))

# Calibrate & Plot
calibrate_and_plot(workflow_fit_rbf_lag,
                   workflow_fit_rbf_spline,
                   type = 'testing')


# 5.0 K-NEAREST NEIGHBORS ----
# - Strengths: Uses neighboring points to estimate 
# - Weaknesses: Cannot predict beyond the maximum/minimum target (e.g. increasing trend)
# - Solution: Model trend separately (if needed). 
#   - Can combine with ARIMA, Linear Regression, Mars, or Prophet

# Show issue when trend extends beyond maximum value ----

sample_data_tbl <- tibble(
  date = tk_make_timeseries('2011', by = 'quarter', length_out = 10),
  value = 1:10
)

sample_data_tbl %>% 
  plot_time_series(date, value, .smooth = FALSE)

model_spec_knn <- nearest_neighbor(mode = 'regression') %>% 
  set_engine('kknn')

simple_fit_knn <-  model_spec_knn %>% 
  fit(value ~ as.numeric(date), sample_data_tbl)

model_spec_glmnet_simple <- linear_reg(mode = 'regression', penalty = 0.1, mixture = 0.5) %>% 
  set_engine('glmnet')

simple_fit_glmnet <- model_spec_glmnet_simple %>% 
  fit(value ~ as.numeric(date) + quarter(date), sample_data_tbl)

modeltime_table(
  simple_fit_knn,
  simple_fit_glmnet
) %>% 
  modeltime_forecast(new_data = bind_rows(
    sample_data_tbl,
    future_frame(sample_data_tbl, .length_out = '2 year')
  ),
  actual_data = sample_data_tbl) %>% 
  plot_modeltime_forecast()

# Implementation ----
model_spec_knn <- parsnip::nearest_neighbor(mode = 'regression',
                                            neighbors = 30,
                                            dist_power = 10) %>% 
  set_engine('kknn')


# Spline
  
  set.seed(123)
workflow_fit_knn_spline <- workflow() %>% 
  add_model(model_spec_knn) %>% 
  add_recipe(recipe_spec_1_spline) %>% 
  fit(training(splits))


# Lag
set.seed(123)
workflow_fit_knn_lag <- workflow() %>% 
  add_model(model_spec_knn) %>% 
  add_recipe(recipe_spec_2_lag) %>% 
  fit(training(splits))

# Calibrate & Plot
calibrate_and_plot(
  workflow_fit_knn_lag,
  workflow_fit_knn_spline
)
# 6.0 RANDOM FOREST ----
# - Strengths: Can model seasonality very well
# - Weaknesses: 
#   - Cannot predict beyond the maximum/minimum target (e.g. increasing trend)
# - Solution: Model trend separately (if needed). 
#   - Can combine with ARIMA, Linear Regression, Mars, or Prophet

# Implementation
model_spec_rf <- rand_forest(mode = 'regression',
                             trees = 1000,
                             min_n = 30) %>% 
  set_engine('randomForest')

# Spline
workflow_fit_rf_spline <- workflow() %>% 
  add_model(model_spec_rf) %>% 
  add_recipe(recipe_spec_1_spline) %>% 
  fit(training(splits))

# Lag
workflow_fit_rf_lag <- workflow() %>% 
  add_model(model_spec_rf) %>% 
  add_recipe(recipe_spec_2_lag) %>% 
  fit(training(splits))

# Calibrate & Plot
calibrate_and_plot(
  workflow_fit_rf_lag,
  workflow_fit_rf_spline
)
# 7.0 XGBOOST ----
# - Strengths: Best for seasonality & complex patterns
# - Weaknesses: 
#   - Cannot predict beyond the maximum/minimum target (e.g. increasing trend)
# - Solution: Model trend separately (if needed). 
#   - Can combine with ARIMA, Linear Regression, Mars, or Prophet
#   - prophet_boost & arima_boost: Do this

# Implementation
model_spec_boost <- parsnip::boost_tree(mode = 'regression',
                                        mtry = 23,
                                        learn_rate = 0.29,
                                        tree_depth = 10,
                                        trees = 1000,
                                        #loss_reduction = 0.1,
                                        min_n = 2) %>% 
  set_engine('xgboost')
# Spline
set.seed(42)
workflow_fit_xgboost_spline <- workflow() %>% 
  add_model(model_spec_boost) %>% 
  add_recipe(recipe_spec_1_spline) %>% 
  fit(training(splits))

# Lag
set.seed(42)
workflow_fit_xgboost_lag <- workflow() %>% 
  add_model(model_spec_boost) %>% 
  add_recipe(recipe_spec_2_lag) %>% 
  fit(training(splits))

# Calibrate & Plot
calibrate_and_plot(workflow_fit_xgboost_lag,
                   workflow_fit_xgboost_spline)

# 8.0 CUBIST ----
# - Like XGBoost, but the terminal (final) nodes are fit using linear regression
# - Does better than tree-based algorithms when time series has trend
# - Can predict beyond maximum



# Implementation 
model_spec_cubist <- cubist_rules() %>% 
  set_engine('Cubist')

# Spline
set.seed(42)
workflow_fit_cubist_spline <- workflow() %>% 
  add_model(model_spec_cubist) %>% 
  add_recipe(recipe_spec_1_spline) %>% 
  fit(training(splits))

# Lag
workflow_fit_cubist_lag <- workflow_fit_cubist_spline %>% 
  update_recipe(recipe_spec_2_lag) %>% 
  fit(training(splits))


# Calibrate & Plot



# 9.0 NEURAL NET ----
# - Single Layer Multi-layer Perceptron Network
# - Simple network - Like linear regression
# - Can improve learning by adding more hidden units, epochs, etc

# Spline


# Lag


# Calibrate & Plot


# 10.0 NNETAR ----
# - NNET with Lagged Features (AR)
# - Is a sequential model (comes from the forecast package)
# - Must include date feature

# Base Model
model_spec_nnet <- mlp(mode = 'regression',
                       activation = 'linear',
                       penalty = 5,
                       #dropout = 0.1,
                       epochs = 75) %>% 
  set_engine('nnet')

workflow_fit_nnet_spline <- workflow() %>% 
add_model(model_spec_nnet) %>% 
add_recipe(recipe_spec_1_spline) %>% 
  fit(training(splits))

workflow_fit_nnet_lag <- workflow() %>% 
  add_model(model_spec_nnet) %>% 
  add_recipe(recipe_spec_2_lag) %>% 
  fit(training(splits))



# Calibrate & Plot
calibrate_and_plot(workflow_fit_nnet_lag,
                   workflow_fit_nnet_spline)


model_spec_nnetar <- nnetar_reg(num_networks = 30,
                                epochs = 75,
                                penalty = 10) %>% 
  set_engine('nnetar') 
  
set.seed(42)
worflow_fit_nnetar_base <- workflow() %>% 
  add_model(model_spec_nnetar) %>% 
  add_recipe(recipe_spec_base) %>% 
  fit(training(splits) %>% drop_na())

calibrate_and_plot(worflow_fit_nnetar_base)


# 11.0 Modeltime Forecasting Workflow -----
# - Compare model performance

# * Modeltime Table ----
model_tbl  <- modeltime_table(
  workflow_fit_glmnet_lag,
  workflow_fit_glmnet_splines,
  
  workflow_fit_mars_lag,
  workflow_fit_mars_spline,
  
  workflow_fit_svm_lag,
  workflow_fit_svm_splines,
  
  workflow_fit_knn_lag,
  workflow_fit_knn_spline,
  
  workflow_fit_xgboost_lag,
  workflow_fit_xgboost_spline,
  
  workflow_fit_nnet_lag,
  workflow_fit_nnet_spline,
  
  worflow_fit_nnetar_base
  
) %>% 
  mutate(.model_desc_2 = str_c(.model_desc, rep_along(.model_desc, c(' - lag', '- spline'))))%>% 
  mutate(.model_desc = ifelse(.model_id == 13, .model_desc, .model_desc_2))
# * Calibration Table ----
calibration_tbl <- model_tbl %>% 
  modeltime_calibrate(testing(splits))

# * Obtain Test Forecast Accuracy ----
calibration_tbl %>% 
  modeltime_accuracy() %>% 
  table_modeltime_accuracy(resizable = TRUE)

# * Visualize Test Forecast ----
forecast_test_tbl <- calibration_tbl %>% 
  modeltime_forecast(new_data = testing(splits),
                     actual_data = data_prepared_tbl)


forecast_test_tbl %>% 
  plot_modeltime_forecast(.conf_interval_show = F)
# * Refit ----
  
set.seet(42)
refit_tbl <- calibration_tbl %>% 
  modeltime_refit(data_prepared_tbl)


forecast_future_tbl <- refit_tbl %>% 
  modeltime_forecast(
    new_data = artifacts_list$data$forecast_tbl,
    actual_data = data_prepared_tbl
  )

forecast_future_tbl %>% 
  plot_modeltime_forecast(.conf_interval_show = F)


#saving artifacts
calibration_tbl %>% 
  write_rds('00_models/machine_learning_artifacts_tbl.rds')

dump(c('calibrate_and_plot'), file = '00_scripts/pq_calibrate_and_plot.R')
