# Libraries ---------------------------------------------------------------
# Install - load tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}
# Install - load patchwork                                                       
if(require(patchwork) == FALSE){                                                
  install.packages('patchwork')                                                 
  library(patchwork)                                                            
}else{                                                                          
  library(patchwork)                                                            
}
# Install - load performance                                                       
if(require(performance) == FALSE){                                                
  install.packages('performance')                                                 
  library(performance)                                                            
}else{                                                                          
  library(performance)                                                            
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
    # Keep year and packs
    year = factor(year), 
    packs,
    # Deflact real values
    real_price = price / cpi,
    real_local_tax =  tax / cpi,
    real_sales_tax =  taxs / cpi,
    real_income_percapita = (income / population) / cpi
  )

# Result
cigarettes %>% 
  GGally::ggpairs()

price_plot = ggplot(
  data = cigarettes, 
  mapping = aes(x = log(real_price), y = log(packs), col = year)) +
  # Add scatter plot
  geom_point() +
  # Add regression line
  stat_smooth(method = 'lm', se = FALSE) +
  # Mofify labels
  labs(y = 'Consumed Packs (log)', x = 'Average Price (log)', title = 'Prices') +
  theme_bw() +
  # Remove legend
  theme(legend.position = 'none')

print(price_plot)

income_plot = ggplot(
  data = cigarettes, 
  mapping = aes(x = log(real_income_percapita), y = log(packs), col = year)) +
  # Add scatter plot
  geom_point() +
  # Add regression line
  stat_smooth(method = 'lm', se = FALSE) +
  # Mofify labels
  labs(y = 'Consumed Packs (log)', x = 'Average Income (log)', title = 'Income percapita') +
  theme_bw()

print(income_plot)

tax_plot = ggplot(
  data = cigarettes, 
  mapping = aes(x = log(real_local_tax), y = log(packs), col = year)) +
  # Add scatter plot
  geom_point() +
  # Add regression line
  stat_smooth(method = 'lm', se = FALSE) +
  # Mofify labels
  labs(y = 'Consumed Packs (log)', x = 'Average Tax (log)', title = 'Local Tax') +
  theme_bw() +
  # Remove legend
  theme(legend.position = 'none')

print(tax_plot)

# Patchwork plot
(price_plot + income_plot) / tax_plot

# Multivariate lineal model
lm_model = lm(
  formula = log(packs) ~ log(real_price) + log(real_local_tax) +log(real_sales_tax) + log(real_income_percapita) + year,
  data = cigarettes
  )

# Model summary
summary(lm_model)

# Model performance
check_model(lm_model)


# The glmnet framework ----------------------------------------------------
# Install - load tidymodels                                                       
if(require(tidymodels) == FALSE){                                                
  install.packages('tidymodels')                                                 
  library(tidymodels)                                                            
}else{                                                                          
  library(tidymodels)                                                            
}

# Install - load glmnet                                                       
if(require(glmnet) == FALSE){                                                
  install.packages('glmnet')                                                 
  library(glmnet)                                                            
}else{                                                                          
  library(glmnet)                                                            
}

# Create a recipe
recipe = recipe(
  # Set a formula
  formula = packs ~ .,
  # Set input data
  data = cigarettes
) %>% 
  step_log(all_numeric()) %>% 
  step_dummy(year)



# Ridge regression --------------------------------------------------------
ridge_reg = linear_reg() %>% 
  # Set engine
  set_engine('glmnet') %>% 
  # Set mode
  set_mode('regression') %>% 
  # Ridge setting (mixture = 0)
  set_args(penalty = tune(), mixture = 0)

# Behind ridge scene
ridge_fit = fit(
  object = ridge_reg, 
  formula = log(packs) ~ .,
  data = recipe %>% prep %>% juice
  )

# Plot
ridge_plot = ridge_fit %>%
  autoplot() +
  # Agrega títulos
  ggtitle('Ridge regularization') +
  # Add hroizontal line
  geom_hline(yintercept = 0, linewidth = 0.5, alpha = 0.2) +
  #  Usa un tema predefinido
  theme_bw() 


# Lasso regression --------------------------------------------------------
lasso_reg = linear_reg() %>% 
  # Set engine
  set_engine('glmnet') %>% 
  # Set mode
  set_mode('regression') %>% 
  # Lasso setting (mixture = 1)
  set_args(penalty = tune(), mixture = 1)

# Lasso fitting
lasso_fit = fit(
  object = lasso_reg, 
  formula = log(packs) ~ .,
  data = recipe %>% prep %>% juice
)

# Plot
lasso_plot = lasso_fit %>%
  autoplot() +
  # Add titles
  ggtitle('Lasso regularization') +
  # Add hroizontal line
  geom_hline(yintercept = 0, linewidth = 0.5, alpha = 0.2) +
  #  Set theme
  theme_bw() 

print(lasso_plot)

ridge_plot + lasso_plot

# Machine learning framework ----------------------------------------------
glmnet_reg = linear_reg() %>% 
  # Set engine
  set_engine('glmnet') %>% 
  # Set mode
  set_mode('regression') %>% 
  # Tunning hyperparameters
  set_args(penalty = tune(), mixture = tune())


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
train = training(smoke_split)
test = testing(smoke_split)

# Create cross-validation folds
set.seed(123)
folds = vfold_cv(train, v = 3)

# Create a workflow
lr_workflow = workflow() %>% 
  # Add your recipe
  add_recipe(recipe) %>% 
  # Add your model
  add_model(glmnet_reg)

# Get parameters to tune
lr_parameters = lr_workflow %>% 
  extract_parameter_set_dials() 

# Create a tune grid
lr_tuning = tune_grid(
  # set Workflow
  object = lr_workflow,
  # set Folds
  resamples = folds,
  # set hyperparameters
  param_info = lr_parameters,
  # Set metrics
  metrics = metric_set(yardstick::rmse, yardstick::mae),
  control = control_grid(verbose = TRUE),
  # Set grid (combinations of mixture and penalty to try)
  grid = 100
)

# Collect metrics
tunning_metrics = lr_tuning %>%
  collect_metrics() %>% 
  # Arrange results from higher to lower
  arrange(desc(mean))

# Select the best model
best_lr = lr_tuning %>%
  select_best(metric = "rmse")

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
glmnet_trained = results_lr %>% 
  extract_fit_parsnip() 

print(glmnet_trained)

# Summary
glmnet_trained %>% 
  pluck('fit') %>%
  summary()

# Get coefficients
coefficients = glmnet_trained %>% 
  extract_fit_engine() %>% 
  # Create a tibble including values of cero
  tidy(return_zeros = T)  

# Visually
ggplot(
  data = coefficients, 
  mapping = aes(x = lambda, y = estimate)
  ) +
  # Scatter plit
  geom_point() +
  # Add vertical line
  geom_vline(
    xintercept = pull(best_lr,penalty),
    col = 'red', linetype = 'dashed'
    ) + 
  # Change scale ox x-axis
  scale_x_log10() +
  # facet by term
  facet_wrap(~term, scales = 'free')

# Compare with the original model
summary(lm_model)

# Make predictions using GLMNET
glmnet_predictions =  augment(
  x = glmnet_trained, 
  new_data = recipe %>% prep %>% bake(new_data = cigarettes)
  )

# Make predictions using Linear Model
lm_predictions =  augment(
  x = lm_model, 
  new_data = cigarettes
)

# Compare predictions with observed values
ggplot() +
  # GLMnet
  geom_point(
    data = glmnet_predictions,
    aes(x = exp(packs), y = exp(.pred), col = 'GLMnet')
  ) + 
  # Linear model
  geom_point(
    data = lm_predictions,
    aes(x = exp(`log(packs)`), y = exp(.fitted), col = 'Linar Model')
  ) +
  # Add vertical line
  geom_abline(slope = 1, col = 'red') +
  # Add labels
  labs(x = 'Observed', y = 'Predicted', col = 'Model') +
  # Fix coordenates
  coord_obs_pred()  

# Compare residuals
ggplot() +
  # GLMnet
  geom_density(
    data = glmnet_predictions,
    aes(x = packs - .pred, fill = 'GLMnet'),
    alpha = 0.5
  ) + 
  # Linear model
  geom_density(
    data = lm_predictions,
    aes(x = `log(packs)` - .fitted, fill = 'Linar Model'),
    alpha = 0.5
  ) +
  # Add labels
  labs(x = 'Rediduals', y = 'Density', fill = 'Model') 


