# Libraries ---------------------------------------------------------------
# Install - load tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}
# Install - load ggside                                                       
if(require(ggside) == FALSE){                                                
  install.packages('ggside')                                                 
  library(ggside)                                                            
}else{                                                                          
  library(ggside)                                                            
}
# Install - AER                                        
if(require(AER) == FALSE){                                                
  install.packages('AER')                                                 
}

# Data --------------------------------------------------------------------
data("CigarettesSW", package = "AER")

# Description
?AER::CigarettesSW 
# EDA
CigarettesSW %>% 
  select_if(is.numeric) %>% 
  pairs()

# Data wrangling
cigarettes = CigarettesSW %>% 
  # Transmute date
  transmute(
    # Create an ID
    id = paste0(state, year), 
    # Keep year and packs
    year, packs,
    # Deflact real values
    real_price = price / cpi,
    real_local_tax =  tax / cpi,
    real_sales_tax =  taxs / cpi,
    real_income_percapita = income / population / cpi
  )

# Result
cigarettes %>% 
  select_if(is.numeric) %>% 
  pairs()


# The machine learning framework ------------------------------------------
# Install - load tidymodels                                                       
if(require(tidymodels) == FALSE){                                                
  install.packages('tidymodels')                                                 
  library(tidymodels)                                                            
}else{                                                                          
  library(tidymodels)                                                            
}
# Data splitting for samples
set.seed(123)
smoke_split = initial_split(
  # Data to split
  data = cigarettes,
  # Proportions
  prop = 0.80,
  # Variable of interest
  strata = packs
)

# Get training and testing samples
smoke_training = training(smoke_split)
smoke_testing = testing(smoke_split)

# Create cross-validation folds
set.seed(123)
smoke_folds = vfold_cv(smoke_training, v = 3)

# Create a recipe
recipe = recipe(
  # Set a formula
  formula = packs ~ .,
  # Set input data
  data = smoke_testing
) %>% 
  update_role(id, new_role = 'id') %>% 
  step_log(all_numeric()) %>% 
  step_dummy(year)

set.seed(123)
linear_regression = linear_reg() %>% 
  set_engine('lm') %>% 
  set_mode('regression')

# Create a workflow
lr_workflow = workflow() %>% 
  # Add your recipe
  add_recipe(recipe) %>% 
  # Add your model
  add_model(linear_regression)

# Get parameters to tune
lr_parameters = lr_workflow %>% 
  extract_parameter_set_dials() 

# Create a tune grid
lr_tuning = tune_grid(
  object = lr_workflow,
  resamples = smoke_folds,
#  param_info = lr_parameters,
  metrics = metric_set(yardstick::rmse),
  control = control_grid(verbose = TRUE),
  grid = 1
)

# Collect metrics
tunning_metrics = lr_tuning %>%
  collect_metrics() %>% 
  # Arrange results from higher to lower
  arrange(desc(mean))

# Select the best model
best_lr = lr_tuning %>%
  select_best("rmse")

# Finalize the workflow
final_lr = lr_workflow %>% 
  finalize_workflow(best_lr)

# Make a final fit
results_lr = final_lr %>% 
  last_fit(
    split = smoke_split,
    metrics = metric_set(yardstick::rmse)
  )

# Asses testing metrics
metrics_lr = results_lr %>% 
  collect_metrics()

print(metrics_lr)


# Working with the trained model ------------------------------------------
# Extract the trained model
lr_trained = results_lr %>% 
  extract_fit_parsnip() 

print(lr_trained)

# Summary
lr_trained %>% 
  pluck('fit') %>% 
  summary()

# Install - load performance                                                       
if(require(performance) == FALSE){                                                
  install.packages('performance')                                                 
  library(performance)                                                            
}else{                                                                          
  library(performance)                                                            
}

check_model(lr_trained)

# GLM-Net -----------------------------------------------------------------
glm_net = linear_reg() %>% 
  set_engine('glmnet') %>% 
  set_mode('regression') %>% 
  set_args(
    penalty = tune(),
    mixture = tune()
  )

# Create a workflow
glmnet_workflow = workflow() %>% 
  # Add your recipe
  add_recipe(recipe) %>% 
  # Add your model
  add_model(glm_net)


# Create a tune grid
glmnet_tuning = tune_grid(
  object = glmnet_workflow,
  resamples = smoke_folds,
  #  param_info = glmnet_parameters,
  metrics = metric_set(yardstick::rmse),
  control = control_grid(verbose = TRUE),
  grid = 100
)

# Collect metrics
tunning_metrics = glmnet_tuning %>%
  collect_metrics() %>% 
  # Arrange results from higher to lower
  arrange(desc(mean))

# Select the best model
best_glmnet = glmnet_tuning %>%
  select_best("rmse")

print(best_glmnet)

# Finalize the workflow
final_glmnet = glmnet_workflow %>% 
  finalize_workflow(best_glmnet)

# Make a final fit
results_glmnet = final_glmnet %>% 
  last_fit(
    split = smoke_split,
    metrics = metric_set(yardstick::rmse)
  )

# Asses testing metrics
metrics_glmnet = results_glmnet %>% 
  collect_metrics()

print(metrics_glmnet)


# Working with the trained model ------------------------------------------
# Extract the trained model
glmnet_trained = results_glmnet %>% 
  extract_fit_parsnip() 

print(glmnet_trained)

# Summary
glmnet_trained %>% 
  pluck('fit') %>% 
  summary()

preproc = log_recipe %>% 
  prep() %>% 
  bake(new_data = cigarettes)

# Make predictions
predictions_tb =  augment(glmnet_trained, preproc)

predictions_tb %>% 
  ggplot(aes(x = packs, y = .pred)) +
  geom_point() +
  geom_abline(slope = 1, col = 'red') +
  coord_obs_pred()  




