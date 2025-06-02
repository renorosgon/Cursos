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
consumo_agua = read_csv('data/consumo_agua_historico_2019.csv') %>% 
  mutate(
    indice_des = factor(indice_des, levels = c('POPULAR', 'BAJO', 'MEDIO','ALTO')),
    colonia = factor(colonia),
    alcaldia = factor(alcaldia)
  ) %>% 
  filter(
    !is.na(alcaldia),
    !is.na(colonia),
    bimestre == 1
    )

select(consumo_agua, indice_des, contains('total'))  %>% 
  mutate_if(is.numeric, sqrt) %>% 
  GGally::ggpairs(label = T)

library(ggdist)

ggplot(consumo_agua, aes(x = indice_des, y = consumo_total^0.2)) +
  stat_halfeye(
    # adjust bandwidth
    adjust = 0.5,
    # remove the slub interval
    .width = 0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.12,
    # removing outliers
    outlier.color = NA,
    alpha = 0.5
  ) 


ggplot(consumo_agua, aes(x = longitud, y = latitud, col = log(consumo_total+1))) +
  geom_point(alpha = 0.2) +
  facet_wrap(~indice_des)

# Knn with tidymodels -----------------------------------------------------
# Data splitting for samples
set.seed(123)
data = initial_split(
  # Data to split
  data = consumo_agua,
  # Proportions
  prop = 0.80,
  # Variable of interest
  strata = indice_des
)

# Get training and testing samples
train = training(data)
test = testing(data)

# Create cross-validation folds
set.seed(123)
folds = vfold_cv(train, v = 5, strata = indice_des)



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


# Create a recipe
basic_recipe = recipe(
  # Set a formula
  formula = consumo_total ~ latitud + longitud + indice_des,
  # Set input data
  data = train
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
  resamples = folds,
  param_info = knn_parameters,
  metrics = metric_set(yardstick::rmse),
  control = control_grid(verbose = TRUE, save_pred = FALSE),
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
  select_best("rmse")

print(best_knn)

# Finalize the workflow
final_knn = knn_workflow %>% 
  finalize_workflow(best_knn)

# Make a final fit
results_knn = final_knn %>% 
  last_fit(
    split = data,
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
predictions_tb =  augment(knn_trained, consumo_agua)


summary(predictions_tb)

# Graph the observed vs the predictions
ggplot(predictions_tb, aes(x = consumo_total, y = .pred)) +
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
  geom_point(aes(col = (consumo_total - .pred)), alpha = 0.5) + 
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
ggplot(predictions_tb, aes(x = indice_des, y = .pred)) +
  # Add point
  geom_jitter() + 
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





