# To work with different fonts
# install.packages('extrafont')
# remotes::install_version("Rttf2pt1", version = "1.3.8")
# extrafont::font_import()
# install.packages('lubridate')
# Set working directory
setwd("~/Desktop/ITESM/Cursos/TC2001B/")
# Libraries ---------------------------------------------------------------
# Install - load tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}
# Install - load tidymodels                                                       
if(require(tidymodels) == FALSE){                                                
  install.packages('tidymodels')                                                 
  library(tidymodels)                                                            
}else{                                                                          
  library(tidymodels)                                                            
}
# Install - load lubridate                                                       
if(require(lubridate) == FALSE){                                                
  install.packages('lubridate')                                                 
  library(lubridate)                                                            
}else{                                                                          
  library(lubridate)                                                            
}
# Install - load patchwork                                                       
if(require(patchwork) == FALSE){                                                
  install.packages('patchwork')                                                 
  library(patchwork)                                                            
}else{                                                                          
  library(patchwork)                                                            
}
# Install - load poissonreg                                                       
if(require(poissonreg) == FALSE){                                                
  install.packages('poissonreg')                                                 
  library(poissonreg)                                                            
}else{                                                                          
  library(poissonreg)                                                            
}
# Install - load mgcv                                                       
if(require(mgcv) == FALSE){                                                
  install.packages('mgcv')                                                 
  library(mgcv)                                                            
}else{                                                                          
  library(mgcv)                                                            
}

# Get files 
files = list.files('data/servicios_integrales_LOCALTEL', full.names = TRUE)

# Load data
data = map_df(
  .x = files,
  .f = read_csv,
  col_types = cols(cp_usuaria = col_character(), cp_hechos = col_character())
  ) %>% 
  # Feature engineering
  transmute(
    # Day of the week
    day = wday(fecha_alta, label = TRUE),
    # Month
    month = month(fecha_alta, label = TRUE),
    # Year
    year = year(fecha_alta),
    # Hour
    hour = hour(hora_alta),
    # Time
    time = ymd_h(paste0(fecha_alta, ' ', hour))
  ) %>% 
  # Aggregate
  count(time, year, month, day, hour, name = 'calls')

glimpse(data)

# Separate by year
calls_past = data %>% 
  subset(year %in% c(2021, 2022))

calls_future = data %>% 
  subset(year == 2023)

# Exploratory data analysis
# Time Series
ggplot(calls_past, aes(x = time, y = calls)) +
  # Add line
  geom_line() +
  # Add title
  labs(title = 'LOCATEL hourly calls') +
  # Edit theme
  theme_bw() +
  theme(
    # Remove axis title
    axis.title = element_blank(),
    # Modify font
    text = element_text(family = 'Bebas Neue')
  )

# Histogram over time
ggplot(calls_past, aes(x = calls)) +
  # Add histogram
  geom_histogram() +
  # Facet by month
  facet_grid(year ~ month) +
  # Add labels
  labs(
    title = 'LOCATEL hourly calls',
    x = 'Hourly Calls'
    ) +
  # Edith theme
  theme_bw() +
  theme(
    # Remove axis title
    axis.title = element_blank(),
    # Modify font
    text = element_text(family = 'Bebas Neue')
  )

# Heatmap
ggplot(calls_past, aes(x = day, y = hour, fill = sqrt(calls))) + 
  # Add tile map
  geom_tile(show.legend = FALSE) +
  # Add title
  labs(
    title = 'LOCATE hourly calls for 2022',
    y = 'Hour') +
  # Edit y axis
  scale_y_continuous(expand = c(0,0), breaks = seq(0,23,1)) + 
  # Edit fill pallete
  scale_fill_viridis_c(option = "B") +
  # Facet by month
  facet_grid(year ~ month) +
  # Edit theme
  theme_bw() + 
  theme(
    # Edit axis
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90),
    # Modify font
    text = element_text(family = 'Bebas Neue')
  )

# Explaining non-linear relationships
plot = ggplot(calls_past, aes(x = hour, y = calls)) +
  # Add points
  geom_jitter(alpha = 0.25, col ='gray50') + 
  # Add labels
  labs(
    x = 'Hour',
    col = ''
    ) +
  # Modify theem
  theme_bw() + 
  theme(
    # Remove axis title
    axis.title.y = element_blank(),
    # Move legend
    legend.position = 'top',
    # Modify font
    text = element_text(family = 'Bebas Neue')
  ) 

print(plot)

# Linear model
linear = plot  + 
  # Add linear trend
  stat_smooth(
    # Set color
    aes(col = 'Linear'), 
    # Set GLM and canonical link
    method = 'glm', method.args = list(family = poisson(link = 'log')),
    # Set formula
    formula = y ~ x, show.legend = FALSE
    ) + 
  # Add Labels
  labs(
    title = 'Linear Model',
    subtitle = '(degree = 1)'   
    ) +
  # Set color
  scale_color_manual(values = 'red') 

# Cuadratic Model
cuadratic = plot  + 
  # Add linear trend
  stat_smooth(
    # Set color
    aes(col = 'Cuadratic'), 
    # Set GLM and canonical link
    method = 'glm', method.args = list(family = poisson(link = 'log')),
    # Set formula
    formula = y ~ poly(x, 2), show.legend = FALSE
  ) + 
  # Add Labels
  labs(
    title = 'Cuadratic Model',
    subtitle = '(degree = 2)'   
  ) +
  # Set color
  scale_color_manual(values = 'blue') 

# Cubic model
cubic = plot  + 
  # Add linear trend
  stat_smooth(
    # Set color
    aes(col = 'Cubic'), 
    # Set GLM and canonical link
    method = 'glm', method.args = list(family = poisson(link = 'log')),
    # Set formula
    formula = y ~ poly(x, 3), show.legend = FALSE
  ) + 
  # Add Labels
  labs(
    title = 'Cubic Model',
    subtitle = '(degree = 3)'   
  ) +
  # Set color
  scale_color_manual(values = 'navy') 

linear + cuadratic + cubic 


# Fitting a polynomial model ----------------------------------------------
# Cosidering the time structure
cubic + facet_wrap(~ month)

# Set seed for pedagogic purposes
set.seed(35)
# Make a time split
data_split = calls_past %>% 
  initial_time_split(
    strata = calls,
    prop = 0.75
  )

# Extract training and testing sets
train = training(data_split) 
test = testing(data_split)

# The key difference
summary(train)
summary(test)


# Rolling origin cross validation (temporal cross validation)
rolling_origin = rolling_origin(
  train,
  initial = 240 * 24, # First 240 days hourly
  assess = 60 * 24, # Assess the next 30 days hourly
  cumulative = TRUE, # Do not make cumulative learning
  skip = 60 * 24 # Skip 30 days hourly
)

# This function unnests and summarises the rolling origin folds
unfolding_rolling_origin = function(fold_num){
  # Summarise the analysis set
  analysis_set = rolling_origin %>% 
    pluck('splits') %>% 
    pluck(fold_num) %>% 
    analysis() %>% 
    summarize(
      fold = fold_num,
      set = 'Analysis',
      min = min(time),
      max = max(time)
    )
  # Summarise the assessment set
  assessment_set = rolling_origin %>% 
    pluck('splits') %>% 
    pluck(fold_num) %>% 
    assessment()%>% 
    summarize(
      fold = fold_num,
      set = 'Assessment',
      min = min(time),
      max = max(time)
    )
  # Bind the summaries
  fold = bind_rows(analysis_set, assessment_set)
  # Return the fold summary
  return(fold)
}

# Apply the unfolding function
map_df(1:nrow(rolling_origin),unfolding_rolling_origin) %>% 
  # Create a ggplot setting aes to be inherited 
  ggplot(aes(y = fold, col = set)) +
  # Add a segment
  geom_segment(aes(x = min, xend = max, yend = fold)) + 
  # Add points
  geom_point(aes(x = max))+ 
  geom_point(aes(x = min)) +
  # Modify colors
  scale_color_manual(values = c('darkred','orange')) +
  # Reverse y axis
  scale_y_reverse() +
  # Edit x axis
  scale_x_datetime(date_breaks = '2 months', date_labels = "%b-%y") +
  # Add proper labels
  labs(
    title = 'Rolling Origin Explanation',
    subtitle = 'Fold Subset',
    col = 'Set',
    caption = '@renorosgon'
  ) +
  # Modify themes
  theme_bw(base_size = 16) +
  theme(
    axis.title = element_blank(),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(fill = "transparent"),
    legend.position = c(0.9, 0.95),
    legend.key.height = unit(0.5,'cm'),
    text = element_text(family = 'Bebas Neue')
  )

# Create a recipe
polynomial_recipe = recipe(
  # Set formula
  formula = calls ~ .,
  # Set data
  data = train
) %>% 
  # Mutate the dataset
  step_mutate( 
    time = as.numeric(time)
    ) %>% 
  # Create the polynomial variable
  step_poly(hour, degree = 3)

# Explaining what will happen
# Create a poisson model
poisson_model = poisson_reg() %>% 
  set_engine('glm') %>% 
  set_mode('regression')

# Fit the recipe
poisson_fit = poisson_model %>% 
  fit(
    calls ~ ., 
    data = polynomial_recipe %>% prep() %>% juice()
  )

# Explore results 
poisson_fit %>% 
  pluck('fit') %>% 
  summary() 

# The dummy variables
recipe_dummies = polynomial_recipe %>% 
  # Day dummies
  step_dummy(day, one_hot = TRUE) %>% 
  # Month dummies
  step_dummy(month, one_hot = TRUE) %>% 
  # Avoid dummy traps
  step_mutate(
    day_1 = NULL,
    month_01 = NULL
  )

# Behind the scene
recipe_dummies %>% 
  prep() %>% 
  juice()

# Explaining the differences
poisson_fit_dummies = poisson_model %>% 
  fit(
    calls ~ ., 
    data = recipe_dummies %>% prep() %>% juice()
  ) 

poisson_fit_dummies %>% 
  pluck('fit') %>% 
  summary() 

# Assessing the results
poisson_fit %>% 
  # Augment Predictions
  augment(
    polynomial_recipe %>% prep() %>% juice()
  ) %>% 
  # Add hour
  mutate(
    time = as.POSIXct(time, origin = "1970-01-01", tz = "UTC"),
    hour = hour(time)
    ) %>% 
  # Plot predictions
  ggplot(aes(x = hour)) +
  # Add points
  geom_jitter(aes( y = calls, col = 'Observed'), alpha = 0.25) + 
  stat_smooth(aes( y = .pred, col = 'Polynomial Predicction'), formula = y ~ poly(x,3)) +
  # Modify colors
  scale_color_manual(values = c('gray50','blue')) +
  # Modify labs
  labs(x = 'Hour', col = 'Calls') +
  # Facet by mont
  facet_wrap(~ month, nrow = 2) +
  # Modify theme
  theme_bw() +
  theme(
    legend.position = 'top',
    axis.title.y = element_blank()
  )


# Review of GLMnet --------------------------------------------------------
# Set model
poisson_glmnet = poisson_reg() %>% 
  # Set engine
  set_engine('glmnet') %>% 
  # Set mode
  set_mode('regression') %>%  
  # Set arguments
  set_args(penalty = tune(), mixture = tune())

# Create workflow
glmnet_workflow = workflow() %>%
  # Add recipe
  add_recipe(recipe_dummies) %>% 
  # Add model
  add_model(poisson_glmnet) 

# Get parameters to tune
glmnet_parametrs = glmnet_workflow %>% 
  extract_parameter_set_dials() 

start = Sys.time()
# Create grid
set.seed(123)
glmnet_tuning = tune_grid(
  # Set workflow
  object = glmnet_workflow,
  # Set resamples
  resamples = rolling_origin,
  # Set parameters
  param_info = glmnet_parametrs,
  # Set metrics
  metrics = metric_set(yardstick::rmse),
  # Set control grid
  control = control_grid(verbose = TRUE),
  # Number of combinations to try
  grid = 100
)
end = Sys.time()

end - start

# Collect metrics
glmnet_metrics = glmnet_tuning %>%
  collect_metrics() %>% 
  # Order by mean
  arrange(mean)

# Select best model
best_glmnet = glmnet_tuning %>%
  select_best("rmse")

print(best_glmnet)

# Finalize workflow
final_glmnet = glmnet_workflow %>% 
  finalize_workflow(best_glmnet)

# Last fit
results_glmnet = final_glmnet %>% 
  last_fit(
    split = data_split,
    metrics = metric_set(yardstick::rmse)
  )

# Collect test metrics
metrics_glmnet = results_glmnet %>% 
  collect_metrics() 

print(metrics_glmnet)

# Working with the trained model
glmnet_trained = results_glmnet %>% 
  extract_fit_parsnip() 

print(glmnet_trained)

# Understanding the coefficients
coefficients = tidy(glmnet_trained, return_zeros = F)
print(coefficients)

# Visually
coefficients %>% 
  # Mutate variables
  mutate(
    sign = ifelse(estimate < 0 ,'Negative','Positive'),
    estimate = abs(estimate)
  ) %>% 
  # Create plot
  ggplot(aes(x = estimate, y = reorder(term, estimate), fill = sign)) + 
  # Add columns
  geom_col() +
  # Modify colors
  scale_fill_manual(values = c('red','blue')) +
  # Modify labels
  labs(
    x = 'Estimate', 
    col = 'Sign', 
    title = 'GLMnet coefficients'
    ) +
  # Modify theme
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    text = element_text(family = 'Bebas Neue')
  )


# Extract the trained model
new_data = recipe_dummies %>% 
  prep() %>% 
  bake(new_data = calls_future) 

# Make predictions
predictions_glmnet =  augment(glmnet_trained, new_data) %>% 
  # Crreate base variables
  mutate(
    time = as.POSIXct(time, origin = "1970-01-01", tz = "UTC"),
    hour = hour(time),
    month = month(time, label = T),
    day = wday(time, label = T)
  )

# Plot predictions
predictions_glmnet %>% 
  # Observed vs Prediction
  ggplot(aes(x = calls, y = .pred)) +
  # Add points
  geom_point(alpha = 0.5, col = 'gray50') +
  # Add 45 degree line
  geom_abline(slope = 1, col = 'red') +
  # Add labels
  labs(x = 'Observed', y = 'Predicted', title = 'GLMnet model predictions') +
  # Modify axis range
  coord_obs_pred() +
  # Modify theme
  theme_bw() 

# Prediction by hour
predictions_glmnet %>% 
  ggplot(aes(x = hour)) +
  # Add observed data
  geom_jitter(aes( y = calls, col = 'Observed'), alpha = 0.5) + 
  # Add predicted data
  stat_smooth(aes( y = .pred, col = 'Predicted')) +
  # Modify colors
  scale_color_manual(values = c('gray50', 'navy')) +
  # Modify labels
  labs(x = 'Hour', col = 'Calls', title = 'LOCATEL Hourly Calls') +
  facet_grid(month ~ day) +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    legend.position = 'top',
    text = element_text(family = 'Bebas Neue')
  )

new_glmnet_metrics = predictions_glmnet %>% 
  rmse(truth = calls, estimate = .pred)

print(paste('Testing Set RMSE:', pull(metrics_glmnet, .estimate)))
print(paste('Testing New Data RMSE:', pull(new_glmnet_metrics, .estimate)))

# Generalized Additive Models ---------------------------------------------
gam_model = gen_additive_mod() %>% 
  # Set mode
  set_mode('regression') %>% 
  # Set engine (and additional parameters)
  set_engine('mgcv', family = poisson(link="log"), method = 'REML') %>% 
  # Set arguments
  set_args(select_features = TRUE, adjust_deg_free = tune())
  
# Create a recipe
recipe = recipe(
  # Set formula
  formula = calls ~ .,
  # Set data
  data = train
) 

# Create a workflow
gam_workflow = workflow() %>% 
  # Add recipe
  add_recipe(recipe) %>% 
  # Add model
  add_model(
    gam_model,
    # Set formula
    formula = calls ~ s(hour) + day + month + year
    )

# Get parameters to tune
gam_parametrs = gam_workflow %>% 
  extract_parameter_set_dials() 

start = Sys.time()
# Create a tuning grid
set.seed(123)
gam_tuning = tune_grid(
  # Set workflow
  object = gam_workflow,
  # Set resamples
  resamples = rolling_origin,
  # Set parameters
  param_info = gam_parametrs,
  # Set metrics
  metrics = metric_set(yardstick::rmse),
  # Set control grid
  control = control_grid(verbose = TRUE, save_pred = FALSE),
  # Set number of combinations
  grid = grid_regular(adjust_deg_free(), levels = 8)
)
end = Sys.time()

end - start

# Collect metrics
gam_metrics = gam_tuning %>%
  collect_metrics() %>% 
  arrange(mean)

# Select best model
best_gam = gam_tuning %>%
  select_best("rmse")

print(best_gam)

# Finalize workflow
final_gam = gam_workflow %>% 
  finalize_workflow(best_gam)

# Last fit
results_gam = final_gam %>% 
  last_fit(
    split = data_split,
    metrics = metric_set(yardstick::rmse)
  )

# Collect test metrics
metrics_gam = results_gam %>% 
  collect_metrics() 

print(metrics_gam)

# Extract trained model
gam_trained = results_gam %>% 
  extract_fit_parsnip() 

print(gam_trained)

# Collect coefficients
coefficients = tidy(gam_trained, parametric = TRUE)
print(coefficients)

coefficients = tidy(gam_trained)
print(coefficients)

# Prep recipe
new_data = recipe %>% 
  prep() %>% 
  bake(new_data = calls_future) 

# Make predictions
predictions_tb =  augment(gam_trained, new_data)

predictions_tb %>% 
  # Observed vs Prediction
  ggplot(aes(x = calls, y = .pred)) +
  # Add points
  geom_point(alpha = 0.5, col = 'gray50') +
  # Add 45 degree line
  geom_abline(slope = 1, col = 'red') +
  # Add labels
  labs(x = 'Observed', y = 'Predicted', title = 'GLMnet model predictions') +
  # Modify axis range
  coord_obs_pred() +
  # Modify theme
  theme_bw() 

# Prediction by hour
predictions_tb %>% 
  ggplot(aes(x = hour)) +
  # Add observed data
  geom_jitter(aes( y = calls, col = 'Observed'), alpha = 0.5) + 
  # Add predicted data
  stat_smooth(aes( y = .pred, col = 'GAM')) +
  # Add glmnet
  stat_smooth(data = predictions_glmnet, aes( y = .pred, col = 'GLMnet')) +
  # Modify colors
  scale_color_manual(values = c('navy', 'red', 'gray50')) +
  # Modify labels
  labs(x = 'Hour', col = 'Calls', title = 'Comparing GLMnet vs GAM') +
  facet_grid(month ~ day) +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
    legend.position = 'top',
    text = element_text(family = 'Bebas Neue')
  )

new_gam_metrics = predictions_tb %>% 
  rmse(truth = calls, estimate = .pred)

print(paste('GLMnet Testing RMSE:', round(pull(metrics_glmnet, .estimate), 2)))
print(paste('GLMnet New Data RMSE:', round(pull(new_glmnet_metrics, .estimate), 2)))

print(paste('GAM Testing RMSE:', round(pull(metrics_gam, .estimate), 2)))
print(paste('GAM New Data RMSE:', round(pull(new_gam_metrics, .estimate), 2)))

