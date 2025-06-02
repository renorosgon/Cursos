# Set working directory
setwd("~/Desktop/ITESM/Cursos/TC2001B")

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
# Install - GGally                                        
if(require(GGally) == FALSE){                                                
  install.packages('GGally')                                                 
}

# Data --------------------------------------------------------------------
# Read data
world_governance_index = readxl::read_excel(
  path = 'data/wgidataset.xlsx', 
  # NA values
  na = ".."
  )

# Exploratory Data Analysis -----------------------------------------------
# See first n observations
head(world_governance_index, n = 5)
# See last n observations
tail(world_governance_index, n = 5)

# Explore data structure
## R base
str(world_governance_index)
## Tidyway
glimpse(world_governance_index)

# Summary
summary(world_governance_index)

# Data wrangling
world_governance_index = world_governance_index %>% 
  # Clean names
  janitor::clean_names() %>% 
  # Filter data
  filter(
    year == 2022,
    # Filter NA
    !is.na(estimate)
    ) %>% 
  # Select columns
  select(code, countryname, indicator, estimate) %>% 
  # Wide format
  pivot_wider(names_from = indicator, values_from = estimate) %>% 
  # Rename
  rename(
    voice_accountability = va,
    political_stability = pv,
    government_effectiveness = ge,
    regulatory_quality = rq,
    rule_of_law = rl,
    control_of_corruption = cc
  ) %>% 
  column_to_rownames(var = 'code')

# It looks tidier
glimpse(world_governance_index)
summary(world_governance_index)

# Scatter pair plot
world_governance_index %>% 
  select_if(is.numeric) %>% 
  pairs()

# Correlation plot
world_governance_index %>% 
  select_if(is.numeric) %>% 
  GGally::ggcorr(label = T, method = 'complete.obs')

# Create a quality_class
world_governance_index = world_governance_index %>%
  mutate(
    quality_score = rowMeans(
      select(., control_of_corruption:voice_accountability), 
      na.rm = TRUE
      ),
    quality_class = ifelse(
        test = quality_score >= quantile(quality_score, 0.5), 
        yes = "High", 
        no = "Low"
    ),
    quality_class = factor(x = quality_class, levels = c('Low','High'))
    ) %>%
  # Remove quality_score
  select(-quality_score) 

# Table our index
world_governance_index %>% 
  pull(quality_class) %>% 
  table()

# Scatter analysis
world_governance_index %>% 
  select(quality_class, control_of_corruption:voice_accountability) %>% 
  GGally::ggpairs()

# Knn with tidymodels -----------------------------------------------------
# Creating a model specification object
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
data_split = initial_split(
  # Data to split
  data = world_governance_index,
  # Proportions
  prop = 0.80,
  # Variable of interest
  strata = quality_class
)

# Get training and testing samples
training = training(data_split)
testing = testing(data_split)

summary(training)
summary(testing)

# Create a recipe
recipe = recipe(
  # Set a formula
  formula = quality_class ~ .,
  # Set input data
  data = training
) %>% 
  # Select only numeric predictors
  step_select(all_numeric_predictors(), all_outcomes()) %>% 
  # Impute missing values
  step_impute_knn(all_numeric_predictors()) 

# The idea behind a recipe
recipe %>% 
  # Prep the recipe
  prep() %>% 
  # Get the juice (or bake it!)
  juice() %>% 
  summary()
  

# Create cross-validation folds
set.seed(123)
folds = vfold_cv(data = training, v = 5, strata = quality_class)

# Create a workflow
knn_workflow = workflow() %>% 
  # Add your recipe
  add_recipe(recipe) %>% 
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
  metrics = metric_set(yardstick::accuracy),
  control = control_grid(verbose = TRUE),
  grid = 100
)

# Collegt metrics
tunning_metrics = knn_tuning %>%
  # Recolectamos las metricas
  collect_metrics() %>% 
  arrange(desc(mean))

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
  geom_line(size = 0.5)  +
  geom_point(aes(col = dist_power)) +
  scale_x_continuous(breaks = 1:15) +
  # Split by metric
  facet_wrap(~weight_func)  


# Select the best model
best_knn = knn_tuning %>%
  select_best(metric = "accuracy")

print(best_knn)

# Finalize the workflow
final_knn = knn_workflow %>% 
  finalize_workflow(best_knn)

# Make a final fit
results_knn = final_knn %>% 
  last_fit(
    split = data_split,
    metrics = metric_set(yardstick::accuracy)
  )

# Asses testing metrics
metrics_knn = results_knn %>% 
  collect_metrics()

print(metrics_knn)

# Asses confusion matrix
confusion_matrix = results_knn %>% 
  collect_predictions() %>% 
  conf_mat(truth = quality_class, estimate = .pred_class)

print(confusion_matrix)
summary(confusion_matrix)

# Extract the trained model
knn_trained = results_knn %>% 
  extract_fit_parsnip() 

# Add predictions 
# (This won't work)
predictions_tb =  augment(knn_trained, world_governance_index)

# This will
preprocessing = recipe %>% 
  prep() %>% 
  bake(new_data = world_governance_index)

# This works because we have the data preprocessed
predictions_tb =  augment(knn_trained, preprocessing)

# The decision boundary ----------------------------------------------------
ranges = function(tibble, column){
  values = pull(tibble, column)
  range = seq(
    from = min(values, na.rm = T) - 0.1, 
    to = max(values, na.rm = T) + 0.1, 
    length.out = 50
    )
  return(range)
}

# Set 2 variables to study the boundary
va_ranges = ranges(world_governance_index, 'voice_accountability')
ps_ranges = ranges(world_governance_index, 'political_stability')

space_grid = expand.grid(
  # X variable
  voice_accountability = va_ranges, 
  # Y variable
  political_stability = ps_ranges
  ) %>% 
  # Add the central values of other variables (ceterisparibus)
  mutate(
    control_of_corruption = median(world_governance_index$control_of_corruption, na.rm = T),
    rule_of_law = median(world_governance_index$rule_of_law, na.rm = T),
    regulatory_quality = median(world_governance_index$regulatory_quality, na.rm = T),
    government_effectiveness = median(world_governance_index$government_effectiveness, na.rm = T),
  )

# Add predicted category
grid_prediction = augment(knn_trained, space_grid)

# Plot the decision boundary 
grid_prediction %>% 
  # Create a canvas
  ggplot(aes(x = voice_accountability, y = political_stability)) +
  # Add countours
  geom_contour(
    aes(z = as.numeric(.pred_class == 'Low'), col = 'Low'), 
    breaks=c(0,.5)
  ) +
  geom_contour(
    aes(z = as.numeric(.pred_class == 'High'), col = 'High'), 
    breaks=c(0,.4)
  ) +
  # Add points
  geom_point(
    # New data source
    data = predictions_tb, 
    aes(
      # Same axis aestetics
      x = voice_accountability, 
      y = political_stability, 
      col = quality_class, 
      shape = (quality_class == .pred_class)
      ),
    # None data related aesthetics
    alpha = 0.75, size = 3
  ) + 
  # Modify legend guides
  guides(shape = 'none') + 
  # Modify colors
  scale_color_manual(values = c('darkred','green4')) +
  # Modify shape
  scale_shape_manual(values = c(4,20)) +
  # Modify scales
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  # Add proper labels
  labs(
    x = 'Voice Accountability',
    y = 'Political Stability',
    title = 'KNN Decision Boundaries',
    subtitle = 'Government Quality',
    caption = 'Autor: @renorosgon\nSource: Own elaboration with World Bank Group'
  ) +
  # Use a default theme
  theme_bw() +
  # Modify legend attributes using legend.attribute
  theme(
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(fill = "transparent"),
    legend.position = c(0.075, 0.95),
    legend.key.height = unit(0.5,'cm')
  ) 

# Correlation analysis
predictions_tb %>% 
  GGally::ggcorr(label = T, method = 'complete.obs')
# Dispersion analysis
predictions_tb %>% 
  GGally::ggpairs(label = T, method = 'complete.obs')

