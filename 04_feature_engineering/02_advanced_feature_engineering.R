# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: ADVANCED FEATURE ENGINEERING

# GOALS ----
# - LEARN ADVANCED FEATURE ENGINEERING TECHNIQUES & WORKFLOW

# OBJECTIVES ----
# - IMPLEMENT RECIPES PIPELINE
# - APPLY MODELTIME WORKFLOW & VISUALIZE RESULTS
# - COMPARE SPLINE MODEL VS LAG MODEL

# LIBRARIES & DATA ----

# Time Series ML
library(tidymodels)
library(modeltime)

# Core
library(tidyverse)
library(lubridate)
library(timetk)

# Data
subscribers_tbl   <- read_rds("00_data/mailchimp_users.rds")
learning_labs_tbl <- read_rds("00_data/learning_labs.rds") 

# Data Wrangle
subscribers_prep_tbl <- subscribers_tbl %>%
    summarise_by_time(optin_time, .by = "day", optins = n()) %>%
    pad_by_time(.pad_value = 0)
subscribers_prep_tbl

learning_labs_prep_tbl <- learning_labs_tbl %>%
    mutate(event_date = ymd_hms(event_date)) %>%
    summarise_by_time(event_date, .by = "day", event = n())
learning_labs_prep_tbl

# Data Transformation
subscribers_transformed_tbl <- subscribers_prep_tbl %>%
    
    # Preprocess Target
    mutate(optins_trans = log_interval_vec(optins, limit_lower = 0, offset = 1)) %>%
    mutate(optins_trans = standardize_vec(optins_trans)) %>%
    select(-optins) %>%
    
    # Fix missing values at beginning of series
    filter_by_time(.start_date = "2018-07-03") %>%
    
    # Cleaning
    mutate(optins_trans_cleaned = ts_clean_vec(optins_trans, period = 7)) %>%
    mutate(optins_trans = ifelse(optin_time %>% between_time("2018-11-18", "2018-11-20"), 
                                 optins_trans_cleaned,
                                 optins_trans)) %>%
    select(-optins_trans_cleaned)

subscribers_transformed_tbl

# Save Key Params
limit_lower <- 0
limit_upper <- 3650.8
offset      <- 1
std_mean    <- -5.25529020756467
std_sd      <- 1.1109817111334


horizon <- 8 * 7
lag_period <- 8 * 7

rolling_periods <- c(30,60,90)

subscribers_transformed_tbl

data_prepared_full_tbl <- subscribers_transformed_tbl %>% 
    
    #adding features
    bind_rows(future_frame(. , .date_var = optin_time, .length_out = horizon))  %>% 
    
    #add autocorrelated lags
    tk_augment_lags(optins_trans, .lags = lag_period) %>% 
    
    #adding rolling features
    tk_augment_slidify(.value = optins_trans_lag56,
                       .f = mean,
                       .period = rolling_periods,
                       .align = 'center',
                       .partial = TRUE) %>% 
    
    #adding events
    left_join(learning_labs_prep_tbl, by = c('optin_time'='event_date')) %>% 
    mutate(event = ifelse(is.na(event), 0, event)) %>% 
    
    
    #format columns
    rename(lab_event = event) %>% 
    rename_with(.cols = contains('lag'), .fn = ~ str_c('lag_',.))
    


data_prepared_full_tbl 

data_prepared_full_tbl %>% 
    pivot_longer(-optin_time) %>%
    plot_time_series(optin_time, value, name, .smooth = FALSE)


# 1.0 STEP 1 - CREATE FULL DATA SET ----
# - Extend to Future Window
# - Add any lags to full dataset
# - Add any external regressors to full dataset



# 2.0 STEP 2 - SEPARATE INTO MODELING & FORECAST DATA ----
data_prepared_tbl <- data_prepared_full_tbl %>% 
    filter(!is.na(optins_trans))

data_prepared_tbl

forecast_tbl <- data_prepared_full_tbl %>% tail(horizon)

# 3.0 TRAIN/TEST (MODEL DATASET) ----
splits <- data_prepared_tbl %>% 
    time_series_split(assess = horizon,
                      cumulative = TRUE)

splits %>% 
    tk_time_series_cv_plan() %>% 
    plot_time_series_cv_plan(optin_time, optins_trans)



# 4.0 RECIPES ----

model_fit_best_lm <- read_rds(file = '00_models/model_fit_best_lm.rds')

model_fit_best_lm %>% summary()
model_fit_best_lm %>% formula()


# - Time Series Signature - Adds bulk time-based features
# - Spline Transformation to index.num
# - Interaction: wday.lbl:week2
# - Fourier Features

recipe_spec_base <- recipe(optins_trans ~. ,data = training(splits)) %>% 
    
    #timeseries signature
    step_timeseries_signature(optin_time) %>% 
    #remove columns
    step_rm(matches('(iso)|(xts)|(minute)|(hour)|(second)|(am.pm)')) %>% 
    
    step_normalize(matches('(index.num)|(year)|(yday)')) %>% 
    
    #dummy encoding / one hot encoding
    step_dummy(all_nominal(), one_hot = TRUE) %>% 
    
    #interaction
    step_interact(~ matches('week2') * matches('wday.lbl')) %>% 
    
    #fourier
    step_fourier(optin_time, period = c(7,14,30,90,365), K = 10)

    
    #checking the recipe
    recipe_spec_base %>% prep() %>% juice() %>% glimpse()
# 5.0 SPLINE MODEL ----

# * LM Model Spec ----
model_spec_lm <- linear_reg() %>% 
        set_engine('lm')

# * Spline Recipe Spec ----
    recipe_spec_1 <- recipe_spec_base %>% 
        step_rm(optin_time) %>% 
        step_ns(deg_free = 2, contains('index.num')) %>% 
        step_rm(starts_with('lag_'))

# * Spline Workflow  ----
workflow_fit_lm_1 <- workflow() %>% 
        add_model(model_spec_lm) %>% 
        add_recipe(recipe_spec_1) %>% 
        fit(training(splits))

    
# 6.0 MODELTIME  ----
calibration_tbl <- modeltime_table(workflow_fit_lm_1) %>% 
        modeltime_calibrate(new_data = testing(splits))

    
calibration_tbl %>% 
    modeltime_forecast(new_data = testing(splits), actual_data = data_prepared_tbl) %>% 
    plot_modeltime_forecast()


calibration_tbl %>% 
    modeltime_accuracy()
    

workflow_fit_lm_1 %>% 
    pull_workflow_fit() %>% 
    pluck('fit') %>% 
    summary()

# 7.0 LAG MODEL ----

# * Lag Recipe ----
recipe_spec_2 <- recipe_spec_base %>% 
    step_rm(optin_time) %>% 
    
    #remove nas
    step_naomit(starts_with('lag_'))

    


recipe_spec_2 %>% prep() %>% juice() %>% glimpse()

# * Lag Workflow ----
workflow_fit_lm_2_lag <- workflow() %>% 
    add_model(model_spec_lm) %>% 
    add_recipe(recipe_spec_2) %>% 
    fit(training(splits))

workflow_fit_lm_2_lag %>% 
    pull_workflow_fit() %>% 
    pluck('fit') %>% 
    summary()

workflow_fit_lm_2_lag %>% 
    modeltime_calibrate(new_data = testing(splits)) %>% 
    modeltime_forecast(new_data = testing(splits),actual_data = data_prepared_tbl) %>% 
    plot_modeltime_forecast()

# * Compare with Modeltime -----
calibration_tbl <- modeltime_table(
    workflow_fit_lm_1,
    workflow_fit_lm_2_lag
) %>% 
    modeltime_calibrate(new_data = testing(splits))

calibration_tbl %>% 
    modeltime_forecast(new_data = testing(splits), 
                       actual_data = data_prepared_tbl) %>% 
    plot_modeltime_forecast()

calibration_tbl %>% 
    modeltime_accuracy()


# 8.0 FUTURE FORECAST ----
refit_tbl <- calibration_tbl %>% 
    #refit
    modeltime_refit(data = data_prepared_tbl)

refit_tbl %>% 
    modeltime_forecast(new_data =  forecast_tbl,
                       actual_data = data_prepared_tbl) %>% 
    
    #inverse_transformation
    mutate(across(.value:.conf_hi, .fns = ~standardize_inv_vec(
        x = .,
        mean = std_mean,
        sd = std_sd
    ))) %>% 
    mutate(across(.value:.conf_hi, .fns = ~ log_interval_inv_vec(
        x = .,
        limit_lower = limit_lower,
        limit_upper = limit_upper,
        offset = offset
    ))) %>% 
    plot_modeltime_forecast()

# 9.0 SAVE ARTIFACTS ----
feature_engineering_artifacts <- list(
    # data
    data = list(
    data_prepared_tbl = data_prepared_tbl,
    forecast_tbl = forecast_tbl),
    
    #recipes
    recipes = list (
    recipe_spec_base = recipe_spec_base,
    recipe_spec_1 = recipe_spec_1,
    recipe_spec_2 =  recipe_spec_2),
    
    #models/workflows
    workflows = list(
    workflow_fit_lm_1 = workflow_fit_lm_1,
    workflow_fit_lm_2_lag = workflow_fit_lm_2_lag),
    
    #inversion
    standardize = list(
        std_mean = std_mean,
        std_sd = std_sd
    ),
    log_invertal_vec = list(
        limit_lower = limit_lower,
        limit_upper = limit_upper,
        offset = offset
        
    )
    
)


feature_engineering_artifacts %>% write_rds('00_models/feature_engineering_artifacts_list.rds')




