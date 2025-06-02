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
# Install - janitor                                        
if(require(janitor) == FALSE){                                                
  install.packages('janitor')                                                 
}

# Data --------------------------------------------------------------------
# Read data
edificios = read_csv('data/edificios_irregulares.csv')

# Exploratory Data Analysis -----------------------------------------------
# See first n observations
head(edificios, n = 5)
# See last n observations
tail(edificios, n = 5)

# Explore data structure
## R base
str(edificios)
## Tidyway
glimpse(edificios)

# Summary
summary(edificios)

# Data wrangling
edificios = edificios %>% 
  # Clean names
  janitor::clean_names() %>% 
  # Select columns
  select(longitud, latitud, niveles_excedentes, oic_manifestacion) %>% 
  # Transdorm variables
  mutate(
    oic_manifestacion = factor(x = str_trim(oic_manifestacion), levels = c('CON DOCUMENTO', 'SIN DOCUMENTO'))
  )

# It looks tidier
glimpse(edificios)
summary(edificios)

# Graphs with ggplot2
# Set a canvas
ggplot(edificios) +
  # Add geom
  geom_point(
    # Set aesthetics
    aes(x = longitud, y = latitud, color = niveles_excedentes),
    # Set geom qualities
    size = 5, shape = 15
  ) + 
  # Modify colors
  scale_color_gradient(low = 'violet', high = 'darkred') +
  # Add proper labe ls
  labs(
    x = 'Longitud',
    y = 'Latitud',
    title = 'Edificaciones irregulares en la alcaldía Benito Juárez',
    col = 'Niveles Excedentes',
    caption = 'Autor: René\nFuente: Elaboración propia con datos de CDMX'
  ) +
  # Use a default theme
  theme_bw() + 
  facet_wrap(~oic_manifestacion) +
  # Modify legend attributes using legend.attribute
  theme(
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(fill = "transparent"),
    legend.position = c(0.075, 0.9),
    legend.key.height = unit(0.5,'cm')
  ) 

# Explaining K-Nearest Neighbors ------------------------------------------
# Take the first observation as example
point = filter(edificios, row_number() == 1)

# Fixing the point coordinates
point_longitud = pull(point, longitud)
point_latitud = pull(point, latitud)

# Take the rest of the observations as potential neighbors
neighbors = filter(edificios, row_number() != 1)

# Set the number of neighbors (k)
num_neighbors = 3

# Basic knn step by step using euclidian distance
knn_prediction = neighbors %>% 
  mutate(
    # Get euclidian distance from each neighbor
    euclidian_distance = sqrt((longitud - point_longitud)^2 + (latitud - point_latitud)^2)
  ) %>% 
  # Keep the n closer neighbors based on the euclidian distance
  top_n(n = num_neighbors, wt = -euclidian_distance) %>% 
  # Summarise the mean
  summarise(
    niveles_excedentes = mean(niveles_excedentes)
  ) %>% 
  # Pull predicted gender
  pull(niveles_excedentes)

# The result
print(knn_prediction)

# Lets make this steps (algorithm) into a function
basic_knn = function(tibble, row_index, num_neighbors){
  # Take the target observation
  point = filter(tibble, row_number() == row_index)
  
  # Fixing the point coordinates
  point_longitud = pull(point, longitud)
  point_latitud = pull(point, latitud)
  
  # Take the rest of the observations as potential neighbors
  neighbors = filter(tibble, row_number() != row_index)
  
  knn_prediction = neighbors %>% 
    mutate(
      # Get euclidian distance from each neighbor
      euclidian_distance = sqrt((longitud - point_longitud)^2 + (latitud - point_latitud)^2)
    ) %>% 
    # Keep the n closer neighbors based on the euclidian distance
    top_n(n = num_neighbors, wt = -euclidian_distance) %>% 
    # Solve ties by random picking
    sample_n(size = num_neighbors) %>% 
    # Summarise the mean
    summarise(
      niveles_excedentes = mean(niveles_excedentes)
    ) %>% 
    # Pull predicted gender
    pull(niveles_excedentes)
  
  return(knn_prediction)
}

# Try the function for different observations and number of neighbors
basic_knn(tibble = edificios, row_index = 10, num_neighbors = 3)

# Making a prediction for each observation
# Create an empty vector
predictions = c()
# Iterate over each observation
for(row_index in 1:nrow(edificios)){
  # Make a prediction
  prediction = basic_knn(tibble = edificios, row_index = row_index, num_neighbors = 3) 
  # Append de result
  predictions = c(predictions, prediction)
}

# The result
print(predictions)

# Model performance -------------------------------------------------------
# Create a confusion matrix
rmse = edificios %>% 
  # Add predictions to the tibble
  mutate(.pred = predictions) 

ggplot(rmse, aes(x = niveles_excedentes, y = .pred)) +
  geom_abline(color = 'red') +   
  geom_point() +
  scale_x_continuous(limits = c(0,7), breaks = 0:7) +
  scale_y_continuous(limits = c(0,7), breaks = 0:7) +
  # Add proper labels
  labs(
    x = 'Niveles Excedentes',
    y = 'Predicciones',
    title = 'Resultados del modelo knn',
    caption = 'Autor: René\nFuente: Elaboración propia con datos de CDMX'
  ) +
  # Use a default theme
  theme_bw() +
  # Modify legend attributes using legend.attribute
  theme(
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(fill = "transparent"),
    legend.position = c(0.075, 0.9),
    legend.key.height = unit(0.5,'cm')
  ) 

# Calculate the root mean square error
rmse %>% 
  summarise(
    rmse = sqrt(mean((niveles_excedentes - .pred)^2))
  )
  
# Knn with tidymodels -----------------------------------------------------
# Creating a model specification object
knn_model = nearest_neighbor() %>% 
  # Set engine
  set_engine("kknn") %>% 
  # Set classification
  set_mode('regression') %>% 
  # Set hyperparameters to tune
  set_args(
    neighbors = tune(),
    dist_power = tune(),
    weight_func = tune()
  )

# Data splitting for samples
set.seed(123)
edificios_split = initial_split(
  # Data to split
  data = edificios,
  # Proportions
  prop = 0.80,
  # Variable of interest
  strata = niveles_excedentes
)

# Get training and testing samples
edificios_training = training(edificios_split)
edificios_testing = testing(edificios_split)

summary(edificios_training)
summary(edificios_testing)

# Create cross-validation folds
set.seed(123)
edificios_folds = vfold_cv(edificios_training, v = 5, strata = niveles_excedentes)

# Create a recipe
basic_recipe = recipe(
  # Set a formula
  formula = niveles_excedentes ~ latitud + longitud + oic_manifestacion,
  # Set input data
  data = edificios_training
) %>% 
  step_normalize(all_numeric_predictors())

# The idea behind a recipe
basic_recipe %>% 
  # Prep the recipe
  prep() %>% 
  # Get the juice (or bake it!)
  juice() %>% 
  # Look at the summary
  summary()

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
  resamples = edificios_folds,
  param_info = knn_parameters,
  metrics = metric_set(yardstick::rmse),
  control = control_grid(verbose = TRUE),
  grid = 100
)

# Collegt metrics
tunning_metrics = knn_tuning %>%
  # Recolectamos las metricas
  collect_metrics() %>% 
  arrange()

# Ploting the training
tunning_metrics %>% 
  # Number of neighbors vs the mean accuracy
  ggplot(aes(x = neighbors, y = mean, col = dist_power)) +
  # Add standar error
  geom_errorbar(aes(
    ymin = mean - std_err,
    ymax = mean + std_err
  ),
  alpha = 0.25
  ) +
  # Add the geom line
  geom_line(linewidth = 0.5)  +
  scale_x_continuous(breaks = 1:15) +
  # Split by metric
  facet_wrap(~weight_func)  


# Select the best model
best_knn = knn_tuning %>%
  select_best(metric = "rmse")

print(best_knn)

# Finalize the workflow
final_knn = knn_workflow %>% 
  finalize_workflow(best_knn)

# Make a final fit
results_knn = final_knn %>% 
  last_fit(
    split = edificios_split,
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

# Make predictions with raw data
predictions_tb =  augment(knn_trained, edificios)

# Make predictions with a recipe
prep_data = basic_recipe %>% 
  prep() %>% 
  bake(new_data = edificios) %>% 
  select(longitud, latitud, oic_manifestacion)

# Add the predictions
predictions_tb = predictions_tb %>% 
  mutate(.pred_bake = predict(knn_trained, prep_data, type = 'raw'))

summary(predictions_tb)

# Graph the observed vs the predictions
ggplot(predictions_tb, aes(x = niveles_excedentes, y = .pred_bake)) +
  # Add a 45 degree line
  geom_abline(col = 'red') +
  # Add points
  geom_point() +
  # Scale axis limits 
  coord_obs_pred() +
  # Add proper labels
  labs(
    x = 'Niveles Excedentes',
    y = 'Predicciones',
    title = 'Modelo KNN',
    caption = 'Autor: René\nFuente: Elaboración propia con datos de CDMX'
  ) +
  # Use a default theme
  theme_bw() +
  # Modify legend attributes using legend.attribute
  theme(
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(fill = "transparent"),
    legend.position = c(0.075, 0.9),
    legend.key.height = unit(0.5,'cm')
  ) 


# Graphical analysis ------------------------------------------------------
# Looking for systemic errors
ggplot(predictions_tb, aes(x = longitud, y = latitud)) +
  # Add points
  geom_point(aes(col = (niveles_excedentes - .pred_bake))) + 
  # Modify colors
  scale_color_gradient2(
    low = 'blue', 
    mid = 'yellow3', 
    high = 'blue', 
    midpoint = 0
    ) +
  # Add proper labels
  labs(
    x = 'Longitud',
    y = 'Latitud',
    title = 'Niveles Excedentes',
    col = 'Manifestación del OIC',
    caption = 'Autor: René\nFuente: Elaboración propia con datos de CDMX'
  ) +
  # Use a default theme
  theme_bw() +
  # Modify legend attributes using legend.attribute
  theme(
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(fill = "transparent"),
    legend.position = c(0.075, 0.9),
    legend.key.height = unit(0.5,'cm')
  ) 

# Graphical interpretation
ggplot(predictions_tb, aes(x = longitud, y = .pred_bake)) +
  # Add point
  geom_point() + 
  # Add proper labe ls
  labs(
    x = 'Longitud',
    y = 'Predicción',
    title = 'Niveles Excedentes',
    col = 'Manifestación del OIC',
    caption = 'Autor: René\nFuente: Elaboración propia con datos de CDMX'
  ) +
  # Use a default theme
  theme_bw() +
  # Modify legend attributes using legend.attribute
  theme(
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(fill = "transparent"),
    legend.position = c(0.075, 0.9),
    legend.key.height = unit(0.5,'cm')
  ) 


# Human Intelligence ------------------------------------------------------
knn_tuning = tune_grid(
  object = knn_workflow,
  resamples = edificios_folds,
  param_info = knn_parameters,
  metrics = metric_set(yardstick::rmse),
  control = control_grid(verbose = TRUE),
  grid = grid_regular(
    neighbors(range = c(1, 5)), 
    dist_power(range = c(1,3)), 
    weight_func(c('gaussian','optimal','rectangular')),
    levels = 50
  ) 
)

# Collegt metrics
tunning_metrics = knn_tuning %>%
  # Recolectamos las metricas
  collect_metrics() %>% 
  arrange()

# Ploting the training
tunning_metrics %>% 
  # Number of neighbors vs the mean accuracy
  ggplot(aes(x = neighbors, y = mean, col = dist_power)) +
  # Add standar error
  geom_errorbar(aes(
    ymin = mean - std_err,
    ymax = mean + std_err
  ),
  alpha = 0.25
  ) +
  # Add the geom line
  geom_line(linewidth = 0.5)  +
  scale_x_continuous(breaks = 1:15) +
  # Split by metric
  facet_wrap(~weight_func)  


# Select the best model
best_knn = knn_tuning %>%
  select_best(metric = "rmse")

print(best_knn)

# Finalize the workflow
final_knn = knn_workflow %>% 
  finalize_workflow(best_knn)

# Make a final fit
results_knn = final_knn %>% 
  last_fit(
    split = edificios_split,
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

# Make predictions with raw data
predictions_tb =  augment(knn_trained, edificios)

# Make predictions with a recipe
prep_data = basic_recipe %>% 
  prep() %>% 
  bake(new_data = edificios) %>% 
  select(longitud, latitud, oic_manifestacion)

# Add the predictions
predictions_tb = predictions_tb %>% 
  mutate(.pred_bake = predict(knn_trained, prep_data, type = 'raw'))

summary(predictions_tb)

# Graph the observed vs the predictions
ggplot(predictions_tb, aes(x = niveles_excedentes, y = .pred_bake)) +
  # Add a 45 degree line
  geom_abline(col = 'red') +
  # Add points
  geom_point() +
  # Scale axis limits 
  coord_obs_pred() +
  # Add proper labels
  labs(
    x = 'Niveles Excedentes',
    y = 'Predicciones',
    title = 'Modelo KNN',
    caption = 'Autor: René\nFuente: Elaboración propia con datos de CDMX'
  ) +
  # Use a default theme
  theme_bw() +
  # Modify legend attributes using legend.attribute
  theme(
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(fill = "transparent"),
    legend.position = c(0.075, 0.9),
    legend.key.height = unit(0.5,'cm')
  ) 


# Graphical analysis ------------------------------------------------------
# Looking for systemic errors
ggplot(predictions_tb, aes(x = longitud, y = latitud)) +
  # Add points
  geom_point(aes(col = (niveles_excedentes - .pred_bake))) + 
  # Modify colors
  scale_color_gradient2(
    low = 'blue', 
    mid = 'yellow3', 
    high = 'blue', 
    midpoint = 0
  ) +
  # Add proper labels
  labs(
    x = 'Longitud',
    y = 'Latitud',
    title = 'Niveles Excedentes',
    col = 'Manifestación del OIC',
    caption = 'Autor: René\nFuente: Elaboración propia con datos de CDMX'
  ) +
  # Use a default theme
  theme_bw() +
  # Modify legend attributes using legend.attribute
  theme(
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(fill = "transparent"),
    legend.position = c(0.075, 0.9),
    legend.key.height = unit(0.5,'cm')
  ) 

# Graphical interpretation
ggplot(predictions_tb, aes(x = oic_manifestacion, y = niveles_excedentes)) +
  # Add point
  geom_boxplot() + 
  geom_jitter(aes(y = .pred_bake, col = 'Prediccón'),
              width = 0.05) + 
  geom_jitter(aes(y = niveles_excedentes, col = 'Observado'),
              width = 0.05) + 
  # Add proper labe ls
  labs(
    x = 'Manifestación OIC',
    title = 'Niveles Excedentes',
    caption = 'Autor: René\nFuente: Elaboración propia con datos de CDMX'
  ) +
  # Use a default theme
  theme_bw() +
  # Modify legend attributes using legend.attribute
  theme(
    legend.background = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(fill = "transparent"),
    legend.position = c(0.075, 0.95),
    legend.key.height = unit(0.5,'cm')
  ) 



