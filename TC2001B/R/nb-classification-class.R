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

# Pairplot
GGally::ggpairs(iris)

# Data wrangling ----------------------------------------------------------
# Discretizing values
iris_discrete = iris %>% 
  # Mutate all numeric variables intro 3 bins factors
  mutate_if(is.numeric, cut, 3, labels = c('low','mid','high'))

# Pairplot
GGally::ggpairs(iris_discrete)

# Explaining Naive Bayes --------------------------------------------------
# Total count by all categories 
nb_iris = iris_discrete %>% 
  # Count observation by groups
  count(species, petal_length) %>% 
  # Calculat percentages
  mutate(pct = n/sum(n))

# Ploting frequencies
ggplot(nb_iris, aes(x = petal_length, y = species)) +
  # Mapping a bubble plot for factors
  geom_point(aes(size = pct), show.legend = FALSE, col = 'gray') +
  # Custom size scales 
  scale_size(range = c(15,30)) +
  # Add text to a plot
  geom_text(aes(label = round(pct, 2))) +
  # Add proper labels
  labs(
    x = 'Petal Length', y = "Species", 
    title = 'Iris species by petal length',
    subtitle = 'Observed frequencies'
    ) +
  theme_bw()

# Creating a function to repeat plots
plot_probabilities = function(variable, class){
  # Create a boolean vector for color
  color = pull(nb_iris, variable) == class
  
  # Create the plot object
  plot = ggplot(nb_iris, aes(x = petal_length, y = species)) +
    # Mapping a bubble plot for factors
    geom_point(aes(size = pct, col = color), show.legend = FALSE) +
    # Custom size scales
    scale_size(range = c(15,30)) +
    # Custom colors
    scale_color_manual(values = c('darkblue','deepskyblue')) +
    # Add text
    geom_text(aes(label = round(pct, 2))) +
    # Add labels
    labs(
      x = 'Petal Length', y = "Species", 
      title = paste0('P(', variable,'=',class,')'),
      subtitle = 'Observed frequencies'
      ) +
    theme_bw()
  # Return the plot object
  return(plot)
}


# Probability for having species = versicolor
plot_probabilities('species','versicolor')

# Wrangling the data
prob_versicolor = nb_iris %>% 
  # Summarise by species
  with_groups(
    .groups = species,
    summarise,
    # Add totals
    n = sum(n)
  ) %>% 
  # Calculate percentages (probabilities)
  mutate(prob = n / sum(n)) %>%
  # Filter only versicolor
  filter(species == 'versicolor') %>% 
  # Pull the probability
  pull(prob)

print(prob_versicolor)

# Probability for having petal_length = mid
plot_probabilities('petal_length','mid')

# Wrangling the data
prob_length_mid = nb_iris %>% 
  # Summarise by petal_length
  with_groups(
    .groups = petal_length,
    summarise,
    # Add totals
    n = sum(n)
  ) %>% 
  # Calculate percentages (probabilities)
  mutate(prob = n / sum(n)) %>%
  # Filter only mid
  filter(petal_length == 'mid') %>% 
  # Pull the probability
  pull(prob)

print(prob_length_mid)

# Probability for having petal_length = mid and species = versicolor
# Create a boolean vector for color
color = pull(nb_iris, 'species') == 'versicolor' & pull(nb_iris, 'petal_length') == 'mid'

# Create the plot object
ggplot(nb_iris, aes(x = petal_length, y = species)) +
  # Mapping a bubble plot for factors
  geom_point(aes(size = pct, col = color), show.legend = FALSE) +
  # Custom size scales
  scale_size(range = c(15,30)) +
  # Custom colors
  scale_color_manual(values = c('darkblue','deepskyblue')) +
  # Add text
  geom_text(aes(label = round(pct, 2))) +
  # Add labels
  labs(
    x = 'Petal Length', y = "Species", 
    title = paste0('P(Versicolor and Mid)'),
    subtitle = 'Observed frequencies'
  ) +
  theme_bw()

# Wrangling the data
prob_versicolor_and_length_mid = nb_iris %>% 
  # Summarise by petal_length
  with_groups(
    .groups = c(species, petal_length),
    summarise,
    # Add totals
    n = sum(n)
  ) %>% 
  # Calculate percentages (probabilities)
  mutate(prob = n / sum(n)) %>%
  # Filter only mid
  filter(petal_length == 'mid', species == 'versicolor') %>% 
  # Pull the probability
  pull(prob)

print(prob_versicolor_and_length_mid)

# Probability of versicolor given mid
prob_versicolor_given_length_mid = prob_versicolor_and_length_mid / prob_length_mid

print(prob_versicolor_given_length_mid)

# Naive Bayes with tidymodels -----------------------------------------------------
# install.packages('klaR','discrim')
library(discrim)
# Creating a model specification object
nb_model = naive_Bayes() %>% 
  # Set engine
  set_engine("klaR") %>% 
  # Set mode to classification
  set_mode('classification') 

# Fitting a model
nb_fit = nb_model %>% 
  fit(
    # Set a formula
    formula = species ~ petal_length, 
    # Set input data
    data = iris_discrete
  ) 

print(nb_fit)

# Augmenting predictions to a tibble
predictions_tb = nb_fit %>% 
  augment(iris_discrete) 

predictions_tb %>% 
  dplyr::select(species, petal_length, .pred_class:.pred_virginica) %>% 
  unique()

# Confusion matrix with yardstick
confusion_matrix = predictions_tb %>% 
  conf_mat(truth = species, estimate = .pred_class)

print(confusion_matrix)

# The machine learning framework ------------------------------------------
# Data splitting for samples
set.seed(123)
iris_split = initial_split(
  # Data to split
  data = iris_discrete,
  # Proportions
  prop = 0.80,
  # Variable of interest
  strata = species
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
  formula = species ~ petal_width + petal_length,
  # Set input data
  data = iris_testing
) 

set.seed(123)
nb_model = naive_Bayes() %>% 
  set_mode("classification") %>% 
  set_engine("naivebayes")  %>% 
  set_args(
    Laplace = tune()
  )

# Create a workflow
nb_workflow = workflow() %>% 
  # Add your recipe
  add_recipe(basic_recipe) %>% 
  # Add your model
  add_model(nb_model)

# Get parameters to tune
nb_parameters = nb_workflow %>% 
  extract_parameter_set_dials() 

# Create a tune grid
nb_tuning = tune_grid(
  object = nb_workflow,
  resamples = iris_folds,
  param_info = nb_parameters,
  metrics = metric_set(yardstick::accuracy),
  control = control_grid(verbose = TRUE),
  grid = 100
)

# Collect metrics
tunning_metrics = nb_tuning %>%
  collect_metrics() %>% 
  # Arrange results from higher to lower
  arrange(desc(mean))

# Select the best model
best_nb = nb_tuning %>%
  select_best("accuracy")

print(best_nb)

# Finalize the workflow
final_nb = nb_workflow %>% 
  finalize_workflow(best_nb)

# Make a final fit
results_nb = final_nb %>% 
  last_fit(
    split = iris_split,
    metrics = metric_set(yardstick::accuracy)
    )

# Asses testing metrics
metrics_nb = results_nb %>% 
  collect_metrics()

print(metrics_nb)

# Working with the trained model ------------------------------------------
# Extract the trained model
nb_trained = results_nb %>% 
  extract_fit_parsnip() 

print(nb_trained)

# Make predictions
predictions_tb =  augment(nb_trained, iris_discrete)

predictions_tb %>% 
  conf_mat(truth = species, estimate = .pred_class) %>% 
  autoplot(type = 'heatmap')
