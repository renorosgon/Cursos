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
# Install - kknn                                        
if(require(kknn) == FALSE){                                                
  install.packages('kknn')                                                 
}

# Data --------------------------------------------------------------------
# Load iris data
data(iris)

# Exploratory Data Analysis -----------------------------------------------
# See first n observations
head(iris, n = 5)
# See last n observations
tail(iris, n = 5)

# Explore data structure
## R base
str(iris)
## Tidyway
glimpse(iris)

# Summary
summary(iris)

# Clean names
iris = iris %>% 
  janitor::clean_names()

# It looks tidier
glimpse(iris)

# Graphs with ggplot2
# Set a canvas
ggplot(iris) +
  # Add geom
  geom_point(
    # Set aesthetics
    aes(x = sepal_length, y = sepal_width, color = species)
  ) + 
  # Modify colors
  scale_color_manual(values = c('darkorange','darkred','violet')) +
  # Add proper labels
  labs(
    x = 'sepal Length',
    y = 'sepal Width',
    title = 'Iris sepals grow proportionaly in width and length',
    subtitle = 'Relationtship between sepal length and width',
    caption = 'Example for TC2002B'
  ) +
  # Use a default theme
  theme_bw() +
  # Modify legend attributes using legend.attribute
  theme(
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(fill = "transparent"),
    legend.position = c(0.05, 0.95),
    legend.key.height = unit(0.5,'cm')
  ) 

# Explaining K-Nearest Neighbors ------------------------------------------
# Take the first observation as example
point = filter(iris, row_number() == 1)

# Fixing the point coordinates
point_sepal_length = pull(point, sepal_length)
point_sepal_width = pull(point, sepal_width)

# Take the rest of the observations as potential neighbors
neighbors = filter(iris, row_number() != 1)

# Set the number of neighbors (k)
num_neighbors = 3

# Basic knn step by step using euclidian distance
knn_prediction = neighbors %>% 
  mutate(
    # Get euclidian distance from each neighbor
    distance = abs(sepal_length - point_sepal_length) + abs(sepal_width - point_sepal_width)) %>% 
  # Keep the n closer neighbors based on the euclidian distance
  top_n(n = num_neighbors, wt = - distance) %>% 
  # Solve ties by random picking
  sample_n(size = num_neighbors) %>% 
  # Summarise the neighbours' width
  summarise(sepal_width = mean(sepal_width)) %>% 
  # Pull predicted width
  pull(sepal_width)

# The result
print(knn_prediction)

# Lets make this steps (algorithm) into a function
basic_knn = function(tibble, row_index, num_neighbors){
  # Take the target observation
  point = filter(tibble, row_number() == row_index)
  
  # Fixing the point coordinates
  point_sepal_length = pull(point, sepal_length)
  point_sepal_width = pull(point, sepal_width)
  
  # Take the rest of the observations as potential neighbors
  neighbors = filter(tibble, row_number() != row_index)
  
  knn_prediction = neighbors %>% 
    mutate(
      # Get euclidian distance from each neighbor
      distance = abs(sepal_length - point_sepal_length) + abs(sepal_width - point_sepal_width)) %>% 
    # Keep the n closer neighbors based on the euclidian distance
    top_n(n = num_neighbors, wt = - distance) %>% 
    # Solve ties by random picking
    sample_n(size = num_neighbors) %>% 
    # Summarise the neighbours' width
    summarise(sepal_width = mean(sepal_width)) %>% 
    # Pull predicted width
    pull(sepal_width)
  
  return(knn_prediction)
}

# Try the function for different observations and number of neighbors
basic_knn(tibble = iris, row_index = 1, num_neighbors = 3)

# Making a prediction for each observation
# Create an empty vector
predicctions = c()
# Iterate over each observation
for(row_index in 1:nrow(iris)){
  # Make a prediction
  predicction = basic_knn(tibble = iris, row_index = row_index, num_neighbors = 3) 
  # Append de result
  predicctions = c(predicctions, predicction)
}

# The result
print(predicctions)

# Model performance -------------------------------------------------------
# Calculate the Root Mean Square Error
rmse = iris %>% 
  # Add predictions to the tibble
  mutate(.pred = predicctions) %>% 
  # Select observed and predicted
  select(sepal_width, .pred) %>% 
  # Summarise the root mean square error
  summarise(rmse = sqrt(mean((sepal_width - .pred)^2)))

print(rmse)

# Knn with tidymodels -----------------------------------------------------
# Creating a model specification object
knn_model = nearest_neighbor(neighbors = 1) %>% 
  # Set engine
  set_engine("kknn") %>% 
  # Set mode to regression
  set_mode('regression') 

# Fitting a model
knn_fit = knn_model %>% 
  fit(
    # Set a formula
    formula = sepal_width ~ sepal_length, 
    # Set input data
    data = iris
  ) 

# Predicting a numeric value
predictions_ = knn_fit %>% 
  predict(iris)

# Augmenting predictions to a tibble
predictions_tb = knn_fit %>% 
  augment(iris)

# RMSE with yardstick
rmse = predictions_tb %>% 
  rmse(truth = sepal_width, estimate = .pred)

print(rmse)


# The regression plot -----------------------------------------------------
ggplot(predictions_tb, aes(x = sepal_width, y = .pred)) + 
  geom_point(
    aes(col = species),
    alpha = 0.5
    ) + 
  geom_abline() + 
  coord_obs_pred() +
  # Modify colors
  scale_color_manual(values = c('darkorange','darkred','violet')) +
  # Add proper labels
  labs(
    x = 'Observed',
    y = 'Predicted',
    title = 'KNN Results',
    subtitle = 'Sepal width observed vs predicted values',
    caption = '@renorosgon'
  ) +
  # Use a default theme
  theme_bw() +
  # Modify legend attributes using legend.attribute
  theme(
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(fill = "transparent"),
    legend.position = c(0.05, 0.95),
    legend.key.height = unit(0.5,'cm')
  ) 

# The machine learning framework ------------------------------------------
# Data splitting for samples
set.seed(123)
iris_split = initial_split(
  # Data to split
  data = iris,
  # Proportions
  prop = 0.80,
  # Variable of interest
  strata = sepal_width
)

# Get training and testing samples
iris_training = training(iris_split)
iris_testing = testing(iris_split)

# Create cross-validation folds
set.seed(123)
iris_folds = vfold_cv(iris_training, v = 3)

# Create a recipe
basic_recipe = recipe(
  # Set a formula
  formula = sepal_width ~ .,
  # Set input data
  data = iris_testing
) 

# The idea behind a recipe
basic_recipe %>% 
  # Prep the recipe
  prep() %>% 
  # Get the juice (or bake it!)
  juice() 

# Creating a model specification object
knn_model = nearest_neighbor() %>% 
  # Set engine
  set_engine("kknn") %>% 
  # Set mode to regression
  set_mode('regression') %>% 
  # Set hyperparameters to tune
  set_args(
    neighbors = tune()
  )

# Create a workflow
knn_workflow = workflow() %>% 
  # Add your recipe
  add_recipe(basic_recipe) %>% 
  # Add your model
  add_model(knn_model)

# Get parameters to tune
knn_parameters = knn_workflow %>% 
  extract_parameter_set_dials() 

# Create a tune grid
knn_tuning = tune_grid(
  object = knn_workflow,
  resamples = iris_folds,
  param_info = knn_parameters,
  metrics = metric_set(yardstick::rmse),
  control = control_grid(verbose = TRUE),
  grid = 100
)

# Collect metrics
tunning_metrics = knn_tuning %>%
  collect_metrics() %>% 
  # Arrange results from lower to higher
  arrange(mean)

# Ploting the training
tunning_metrics %>% 
  # Number of neighbors vs the mean accuracy
  ggplot(aes(x = neighbors, y = mean)) +
  # Add standar error
  geom_errorbar(aes(
    ymin = mean - std_err,
    ymax = mean + std_err
  ),
  alpha = 0.25
  ) +
  # Add the geom line
  geom_line(size = 0.5)

# Select the best model
best_knn = knn_tuning %>%
  select_best("rmse")

print(best_knn)

# Finalize the workflow
final_knn = knn_workflow %>% 
  finalize_workflow(best_knn)

# Make a final fit
results_knn = final_knn %>% 
  last_fit(
    split = iris_split,
    metrics = metric_set(yardstick::rmse)
    )

# Asses testing metrics
metrics_knn = results_knn %>% 
  collect_metrics()

print(metrics_knn)

# Multiple hyperparameters ------------------------------------------------
# Creating a model specification object
knn_model = nearest_neighbor() %>% 
  # Set engine
  set_engine("kknn") %>% 
  # Set mode to regression
  set_mode('regression') %>% 
  # Set hyperparameters to tune
  set_args(
    neighbors = tune(),
    dist_power = tune(),
    weight_func = tune()
  )

# Create a workflow
knn_workflow = workflow() %>% 
  # Add your recipe
  add_recipe(basic_recipe) %>% 
  # Add your model
  add_model(knn_model)

# Get parameters to tune
knn_parameters = knn_workflow %>% 
  extract_parameter_set_dials() 

# Create a tune grid
set.seed(123)
knn_tuning = tune_grid(
  object = knn_workflow,
  resamples = iris_folds,
  param_info = knn_parameters,
  metrics = metric_set(yardstick::rmse),
  control = control_grid(verbose = TRUE),
  grid = 100
)

# Collegt metrics
tunning_metrics = knn_tuning %>%
  # Recolectamos las metricas
  collect_metrics() %>% 
  arrange(mean)

# Ploting the training
tunning_metrics %>% 
  # Number of neighbors vs the mean accuracy
  ggplot(aes(x = neighbors, y = mean, col = weight_func)) +
  # Add standar error
  geom_errorbar(aes(
    ymin = mean - std_err,
    ymax = mean + std_err
  ),
  alpha = 0.25
  ) +
  # Add the geom line
  geom_line(size = 0.5) 


# Select the best model
best_knn = knn_tuning %>%
  select_best("rmse")

print(best_knn)

# Finalize the workflow
final_knn = knn_workflow %>% 
  finalize_workflow(best_knn)

# Make a final fit
results_knn = final_knn %>% 
  last_fit(
    split = iris_split,
    metrics = metric_set(yardstick::rmse)
  )

# Asses testing metrics
metrics_knn = results_knn %>% 
  collect_metrics()

print(metrics_knn)

# Working with the trained model ------------------------------------------
# Extract the trained model
knn_trained = results_knn %>% 
  extract_fit_parsnip() 

# Make predictions
predictions_tb =  augment(knn_trained, iris)

# Plot decision boundatires
ggplot(predictions_tb, aes(x = sepal_width, y = .pred)) + 
  geom_point(
    aes(col = species),
    alpha = 0.5
  ) + 
  geom_abline() + 
  coord_obs_pred() +
  # Modify colors
  scale_color_manual(values = c('darkorange','darkred','violet')) +
  # Add proper labels
  labs(
    x = 'Observed',
    y = 'Predicted',
    title = 'KNN Results',
    subtitle = 'Sepal width observed vs predicted values',
    caption = '@renorosgon'
  ) +
  # Use a default theme
  theme_bw() +
  # Modify legend attributes using legend.attribute
  theme(
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(fill = "transparent"),
    legend.position = c(0.05, 0.95),
    legend.key.height = unit(0.5,'cm')
  ) 
