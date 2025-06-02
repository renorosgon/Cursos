# Set working directory
setwd(___)

# Libraries ---------------------------------------------------------------
# Install - load tidyverse                                                       
if(___){                                                
  install.packages(___)                                                 
  library(___)                                                            
}else{                                                                          
  library(___)                                                            
}
# Install - load tidymodels                                                       
if(___){                                                
  install.packages(___)                                                 
  library(___)                                                            
}else{                                                                          
  library(___)                                                            
}
# Install - kknn                                        
if(___){                                                
  install.packages(___)                                                 
}
# Install - janitor                                        
if(___){                                                
  install.packages(___)                                                 
}

# Data --------------------------------------------------------------------
# Read data
edificios = read_csv(___)

# Exploratory Data Analysis -----------------------------------------------
# See first n observations
head(___)
# See last n observations
tail(___)

# Explore data structure
## R base
str(___)
## Tidyway
glimpse(___)

# Summary
summary(___)

# Data wrangling
edificios = edificios %>% 
  # Clean names
  janitor::clean_names() %>% 
  # Select columns
  select(___) %>% 
  # Transdorm variables
  mutate(
    oic_manifestacion = factor(___)
  )

# It looks tidier
glimpse(___)
summary(___)

# Graphs with ggplot2
# Set a canvas
ggplot(___) +
  # Add geom
  geom_point(
    # Set aesthetics
    aes(___),
    # Set geom qualities
    size = 3, shape = 15
  ) + 
  # Modify colors
  scale_color_manual(___) +
  # Add proper labe ls
  labs(
    x = 'Longitud',
    y = 'Latitud',
    title = 'Edificaciones Irregulares',
    col = 'Manifestación del OIC',
    caption = 'Autor: René\nFuente: Elaboración propia con datos de CDMX'
  ) +
  # Use a default theme
  theme_bw() +
  # Modify legend attributes using legend.attribute
  theme(
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(___),
    legend.position = c(___),
    legend.key.height = unit(___)
  ) 

# Explaining K-Nearest Neighbors ------------------------------------------
# Take the first observation as example
point = filter(___)

# Fixing the point coordinates
point_longitud = pull(___)
point_latitud = pull(___)

# Take the rest of the observations as potential neighbors
neighbors = filter(___)

# Set the number of neighbors (___)
num_neighbors = 3

# Basic knn step by step using euclidian distance
knn_prediction = neighbors %>% 
  mutate(
    # Get euclidian distance from each neighbor
    euclidian_distance = sqrt(___)
  ) %>% 
  # Keep the n closer neighbors based on the euclidian distance
  top_n(___) %>% 
  # Solve ties by random picking
  sample_n(___) %>% 
  # Count neighbors categories
  count(___) %>% 
  # Keep the most frequent (___) category
  top_n(___) %>% 
  # Pull predicted gender
  pull(___)

# The result
print(___)

# Lets make this steps (___) into a function
basic_knn = function(___){
  # Take the target observation
  point = filter(___)
  
  # Fixing the point coordinates
  point_longitud = pull(___)
  point_latitud = pull(___)
  
  # Take the rest of the observations as potential neighbors
  neighbors = filter(___)
  
  knn_prediction = neighbors %>% 
    mutate(
      # Get euclidian distance from each neighbor
      euclidian_distance = sqrt(___)
    ) %>% 
    # Keep the n closer neighbors based on the euclidian distance
    top_n(___) %>% 
    # Solve ties by random picking
    sample_n(___) %>% 
    # Count neighbors categories
    count(___) %>% 
    # Keep the most frequent (___) category
    top_n(___) %>% 
    # Pull predicted gender
    pull(___)
  
  return(___)
}

# Try the function for different observations and number of neighbors
basic_knn(___)

# Making a prediction for each observation
# Create an empty vector
predictions = c()
# Iterate over each observation
for(___){
  # Make a prediction
  prediction = basic_knn(___) 
  # Append de result
  predictions = c(___)
}

# The result
print(___)

# Model performance -------------------------------------------------------
# Create a confusion matrix
confusion_matrix = edificios %>% 
  # Add predictions to the tibble
  mutate(___) %>% 
  # Select observed and predicted
  select(___) %>% 
  # Create confusion matrix
  table() 

print(___)

# Knn with tidymodels -----------------------------------------------------
# Creating a model specification object
knn_model = nearest_neighbor(___) %>% 
  # Set engine
  set_engine(___) %>% 
  # Set mode to classification
  set_mode(___) 

# Fitting a model
knn_fit = knn_model %>% 
  fit(
    # Set a formula
    formula = oic_manifestacion ~ latitud + longitud, 
    # Set input data
    data = edificios
  ) 

# Predicting classes
predictions_clase = knn_fit %>% 
  predict(___)

# Predictiong probabilities
predictions_prob = knn_fit %>% 
  predict(___)

# Augmenting predictions to a tibble
predictions_tb = knn_fit %>% 
  augment(___)

# Confusion matrix with yardstick
confusion_matrix = predictions_tb %>% 
  conf_mat(___)


# The decision boundry ----------------------------------------------------
# Set observed points
longitud = pull(___)
latitud = pull(___)

# Create the ranges of observations
x_range = seq(___)
y_range = seq(___)

# Create a grid of cases
space_grid = expand.grid(___)
# Modify names
names(___)
# Add predicted category
grid_prediction = augment(___)

# Plot the decision boundary 
ggplot(___) +
  geom_contour(
    aes(___), 
    breaks=c(___)
  ) +
  geom_contour(
    aes(___), 
    breaks=c(___)
  ) +
  geom_point(
    data = predictions_tb, 
    aes(x = longitud, y = latitud, col = oic_manifestacion, 
        shape = (___),
        alpha = 0.75, size = 3
    ) + 
      # Modify legend guides
      guides(___) + 
      # Modify colors
      scale_color_manual(___) +
      # Modify shape
      scale_shape_manual(___) +
      # Modify scales
      scale_x_continuous(___) +
      scale_y_continuous(___) +
      # Add proper labels
      labs(
        x = 'Longitud',
        y = 'Latitud',
        title = 'KNN Decision Boundaries',
        subtitle = 'Manifestación del OIC',
        caption = 'Autor: René\nFuente: Elaboración propia con datos de CDMX'
      ) +
      # Use a default theme
      theme_bw() +
      # Modify legend attributes using legend.attribute
      theme(
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_rect(___),
        legend.position = c(___),
        legend.key.height = unit(___)
      ) 
    
# The machine learning framework ------------------------------------------
# Data splitting for samples
set.seed(___)
edificios_split = initial_split(
# Data to split
data = edificios,
# Proportions
prop = 0.80,
# Variable of interest
strata = oic_manifestacion
)

# Get training and testing samples
edificios_training = training(___)
edificios_testing = testing(___)

summary(___)
summary(___)

# Create cross-validation folds
set.seed(___)
edificios_folds = vfold_cv(___)

# Create a recipe
basic_recipe = recipe(
# Set a formula
formula = oic_manifestacion ~ latitud + longitud,
# Set input data
data = edificios_training
) 

# The idea behind a recipe
basic_recipe %>% 
# Prep the recipe
prep() %>% 
# Get the juice (___)
juice() %>% 
summary()

# Creating a model specification object
knn_model = nearest_neighbor() %>% 
# Set engine
set_engine(___) %>% 
# Set classification
set_mode(___) %>% 
# Set hyperparameters to tune
set_args(
neighbors = tune()
)

# Create a workflow
knn_workflow = workflow() %>% 
# Add your recipe
add_recipe(___) %>% 
# Add your model
add_model(___)

# Get parameters to tune
knn_parameters = knn_workflow %>% 
extract_parameter_set_dials() 

# Create a tune grid
knn_tuning = tune_grid(
object = knn_workflow,
resamples = edificios_folds,
param_info = knn_parameters,
metrics = metric_set(___),
control = control_grid(___),
grid = 100
)

# Collect metrics
tunning_metrics = knn_tuning %>%
collect_metrics() %>% 
# Arrange results from higher to lower
arrange(___)

# Ploting the training
tunning_metrics %>% 
# Number of neighbors vs the mean accuracy
ggplot(___) +
# Add standar error
geom_errorbar(aes(
ymin = mean - std_err,
ymax = mean + std_err
),
alpha = 0.25
) +
# Add the geom line
geom_line(___) +
scale_x_continuous(___)

# Select the best model
best_knn = knn_tuning %>%
select_best(___)

print(___)

# Finalize the workflow
final_knn = knn_workflow %>% 
finalize_workflow(___)

# Make a final fit
results_knn = final_knn %>% 
last_fit(
split = edificios_split,
metrics = metric_set(___)
)

# Asses testing metrics
metrics_knn = results_knn %>% 
collect_metrics()

print(___)

# Multiple hyperparameters ------------------------------------------------
# Creating a model specification object
knn_model = nearest_neighbor() %>% 
# Set engine
set_engine(___) %>% 
# Set classification
set_mode(___) %>% 
# Set hyperparameters to tune
set_args(
neighbors = tune(),
dist_power = tune(),
weight_func = tune()
)

# Create a workflow
knn_workflow = workflow() %>% 
# Add your recipe
add_recipe(___) %>% 
# Add your model
add_model(___)

# Get parameters to tune
knn_parameters = knn_workflow %>% 
extract_parameter_set_dials() 

# Create a tune grid
set.seed(___)
knn_tuning = tune_grid(
object = knn_workflow,
resamples = edificios_folds,
param_info = knn_parameters,
metrics = metric_set(___),
control = control_grid(___),
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
ggplot(___) +
# Add standar error
geom_errorbar(aes(
ymin = mean - std_err,
ymax = mean + std_err
),
alpha = 0.25
) +
# Add the geom line
geom_line(___)  +
scale_x_continuous(___) +
# Split by metric
facet_wrap(___)  


# Select the best model
best_knn = knn_tuning %>%
select_best(___)

print(___)

# Finalize the workflow
final_knn = knn_workflow %>% 
finalize_workflow(___)

# Make a final fit
results_knn = final_knn %>% 
last_fit(
split = edificios_split,
metrics = metric_set(___)
)

# Asses testing metrics
metrics_knn = results_knn %>% 
collect_metrics()

print(___)


# Working with the trained model ------------------------------------------
# Extract the trained model
knn_trained = results_knn %>% 
extract_fit_parsnip() 

# Make predictions
grid_prediction = augment(___) 
predictions_tb =  augment(___)

# Plot decision boundaries
ggplot(___) +
geom_contour(
aes(___), 
breaks=c(___)
) +
geom_contour(
aes(___), 
breaks=c(___)
) +
geom_point(
data = predictions_tb, 
aes(x = longitud, y = latitud, col = oic_manifestacion, 
    shape = (___),
    alpha = 0.75, size = 3
) + 
  # Modify legend guides
  guides(___) + 
  # Modify colors
  scale_color_manual(___) +
  # Modify shape
  scale_shape_manual(___) +
  # Modify scales
  scale_x_continuous(___) +
  scale_y_continuous(___) +
  # Add proper labels
  labs(
    x = 'Longitud',
    y = 'Latitud',
    title = 'KNN Decision Boundaries',
    subtitle = 'Manifestación del OIC',
    caption = 'Autor: René\nFuente: Elaboración propia con datos de CDMX'
  ) +
  # Use a default theme
  theme_bw() +
  # Modify legend attributes using legend.attribute
  theme(
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(___),
    legend.position = c(___),
    legend.key.height = unit(___)
  )


# Print the confusion matrix
predictions_tb %>% 
  select(___) %>% 
  table()


# The data science version ------------------------------------------------
# Create a tune grid
set.seed(___)
knn_tuning = tune_grid(
  object = knn_workflow,
  resamples = edificios_folds,
  param_info = knn_parameters,
  metrics = metric_set(___),
  control = control_grid(___),
  grid = grid_regular(
    neighbors(___), 
    dist_power(___), 
    weight_func(___),
    levels = 25
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
  ggplot(___) +
  # Add standar error
  geom_errorbar(aes(
    ymin = mean - std_err,
    ymax = mean + std_err
  ),
  alpha = 0.25
  ) +
  # Add the geom line
  geom_line(___)  +
  scale_x_continuous(___)


# Select the best model
best_knn = knn_tuning %>%
  select_best(___)

print(___)

# Finalize the workflow
final_knn = knn_workflow %>% 
  finalize_workflow(___)

# Make a final fit
results_knn = final_knn %>% 
  last_fit(
    split = edificios_split,
    metrics = metric_set(___)
  )

# Asses testing metrics
metrics_knn = results_knn %>% 
  collect_metrics()

print(___)


# Working with the trained model ------------------------------------------
# Extract the trained model
knn_trained = results_knn %>% 
  extract_fit_parsnip() 

# Make predictions
grid_prediction = augment(___) 
predictions_tb =  augment(___)

# Plot decision boundatires
ggplot(___) +
  geom_contour(
    aes(___), 
    breaks=c(___)
  ) +
  geom_contour(
    aes(___), 
    breaks=c(___)
  ) +
  geom_point(
    data = predictions_tb, 
    aes(x = longitud, y = latitud, col = oic_manifestacion, 
        shape = (___),
        alpha = 0.75, size = 3
    ) + 
      # Modify legend guides
      guides(___) + 
      # Modify colors
      scale_color_manual(___) +
      # Modify shape
      scale_shape_manual(___) +
      # Modify scales
      scale_x_continuous(___) +
      scale_y_continuous(___) +
      # Add proper labels
      labs(
        x = 'Longitud',
        y = 'Latitud',
        title = 'KNN Decision Boundaries',
        subtitle = 'Manifestación del OIC',
        caption = 'Autor: René\nFuente: Elaboración propia con datos de CDMX'
      ) +
      # Use a default theme
      theme_bw() +
      # Modify legend attributes using legend.attribute
      theme(
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_rect(___),
        legend.position = c(___),
        legend.key.height = unit(___)
      )
    
    