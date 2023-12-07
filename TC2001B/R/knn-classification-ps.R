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
# read_csv - Load csv datasets
india = read_csv('data/india.csv')

# Exploratory Data Analysis -----------------------------------------------
# See first n observations
  head(india, n = 5)
# See last n observations
  tail(india, n = 5)

# Explore data structure
## R base
  str(india)
## Tidyway
  glimpse(india)
  
# Summary
summary(india)

# Graphs with ggplot2
# Set a canvas
ggplot(india) +
  # Add geom
  geom_point(
    # Set aesthetics
    aes(x = water, y = irrigation, color = female)
    ) 

# Data wrangling ----------------------------------------------------------
# Adding a new column with mutate
india = india %>% 
  mutate(
    # Create a factor variable
    politician_gender = factor(
                              female, 
                              levels = c(0,1), 
                              labels = c('Male', 'Female')
                              )
    )

# Take a look
glimpse(india)

# Making a better visualization
# Create a canvas
ggplot(india, aes(x = water, y = irrigation)) +
  # Add a geom
  geom_point(aes(color = politician_gender)) +
  # Modify axes scales
  scale_y_sqrt() +
  scale_x_sqrt() +
  # Modify colors
  scale_color_manual(values = c('darkorange','darkred')) +
  # Add proper labels
  labs(
    x = 'New or repaired water facilities',
    y = 'New or repaired irrigation facilities',
    title = 'Do Women Promote Different Policies than Men?',
    subtitle = 'Hydric infrastructure policies by head council gender',
    caption = 'Own elaboration with data from Chattopadhya & Dufflo (2004)'
    ) +
  # Use a default theme
  theme_bw() +
  # Modify legend attributes using legend.attribute
  theme(
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(fill = "transparent"),
    legend.position = c(0.1, 0.95),
    legend.key.height = unit(0.5,'cm')
  ) 

# Can we predict the head council gender by using the hydric policy?

# Knn with tidymodels -----------------------------------------------------
knn_model = nearest_neighbor() %>% 
  # Set engine
  set_engine("kknn") %>% 
  # Set classification
  set_mode('classification') %>% 
  # Set hyperparameters to tune
  set_args(
    neighbors = tune(),
    dist_power = tune(),
    weight_func = tune()
    )

# Data splitting for samples
set.seed(123)
india_split = initial_split(
  # Data to split
  data = india,
  # Proportions
  prop = 0.75,
  # Variable of interest
  strata = politician_gender
)

# Get training and testing samples
india_training = training(india_split)
india_testing = testing(india_split)

# Create cross-validation folds
set.seed(123)
india_folds = vfold_cv(india_training, v = 2)

# Create a recipe
basic_recipe = recipe(
  # Set a formula
  formula = politician_gender ~ water + irrigation, 
  # Set input data
  data = india_testing
) %>% 
  step_sqrt(all_numeric_predictors())

# The idea behind a recipe
basic_recipe %>% 
  # Prep the recipe
  prep() %>% 
  # Get the juice (or bake it!)
  juice() 


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
  resamples = india_folds,
  param_info = knn_parameters,
  metrics = metric_set(yardstick::accuracy, yardstick::roc_auc),
  control = control_grid(verbose = TRUE),
  grid = 100
  )

# Collegt metrics
tunning_metrics = knn_tuning %>%
  # Recolectamos las metricas
  collect_metrics() %>% 
  arrange()

tunning_metrics%>% 
  # Hacemos un grafico
  ggplot(aes(x = neighbors, y = mean, col = weight_func)) +
  # Agregamos barras de error
  geom_errorbar(aes(
    ymin = mean - std_err,
    ymax = mean + std_err
  ),
  alpha = 0.5
  ) +
  # Agregamos una linea
  geom_line(size = 0.5) +
  # Separamos por metrica
  facet_wrap(~.metric, scales = "free") 


# Seleccionamos nuestro mejor modelo
best_knn = knn_tuning %>%
  select_best("roc_auc")

print(best_knn)

# Terminamos nuestro flujo de trabajo
final_knn = knn_workflow %>% 
  finalize_workflow(best_knn)

# Entrenamos nuestro flujo final
resultados_knn = final_knn %>% 
  last_fit(split = india_split,
           metrics = metric_set(yardstick::accuracy, yardstick::roc_auc))

# Estas son nuestras metricas
metricas_knn = resultados_knn %>% 
  collect_metrics()




