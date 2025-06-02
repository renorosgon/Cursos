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
# Read Brexit Survey
bes = read_csv('data/BES.csv') 

# Exploratory Data Analysis -----------------------------------------------
# See first n observations
head(bes, n = 5)
# See last n observations
tail(bes, n = 5)

# Explore data structure
## R base
str(bes)
## Tidyway
glimpse(bes)

# Summary
summary(bes)

# Count
bes %>% 
  count(vote, leave)

# Clean names
bes = bes %>% 
  # Filter the NA in the leave column
  filter(!is.na(leave)) %>% 
  # Transform the vote column into a factor with 'stay' and 'leave' as levels
  mutate(
    vote = factor(vote, levels = c('stay', 'leave')),
    education = factor(education, levels = c(1:5,NA), exclude = NULL)
    )

# Look at a summary
summary(bes)

# It looks tidier
glimpse(bes)

# install.packages('GGally')
# Pairplot
GGally::ggpairs(bes)

bes %>% 
  # Count observations
  count(vote, education) %>% 
  # Perform operations by group
  with_groups(
    # Use education as grouping variable
    .groups = education,
    # Set the mutate operation
    mutate,
    # calculate the percentage 
    pct = 100 * n/sum(n)
  )  %>% 
# Create a ggplot
ggplot(aes(x = education, y = pct, group = vote)) +
  # Color by vote
  geom_col(aes(fill = vote)) + 
  # Add text
  geom_text(aes(label = round(pct, 2)), position = position_stack(vjust = 0.5)) +
  # Add proper labels
  labs(
    # Modify the x and y axis labels to make them clearer
    x = 'Education Level',
    title = 'Less educated people were more likely to vote "leave"',
    subtitle = 'Share by educational level',
    # Add the following caption
    caption = 'Own elaboration with data from Llaudet & Imai (2022)',
    col = 'Voted'
  ) +
  # Use a default theme
  theme_bw() +
  # Modify legend attributes using legend.attribute
  theme(
    axis.title.y = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(fill = "transparent"),
    legend.key.height = unit(0.5,'cm'),
    legend.position = 'top'
  ) 
# Data wrangling ----------------------------------------------------------
# Explaining Naive Bayes --------------------------------------------------
# Total count by all categories 
nb_bes = bes %>% 
  # Count observations
  count(vote, education) %>% 
  # Calculat percentages
  mutate(pct = n/sum(n))



# Creating a function to repeat plots
plot_probabilities = function(variable, class){
  # Create a boolean vector for color
  color = pull(nb_bes, variable) == class
  
  # Create the plot object
  plot = ggplot(nb_bes, aes(x = education, y = vote)) +
    # Mapping a bubble plot for factors
    geom_point(aes(size = pct, col = color), show.legend = FALSE) +
    # Custom size scales
    scale_size(range = c(15,30)) +
    # Custom colors
    scale_color_manual(values = c('orange','deepskyblue')) +
    # Add text
    geom_text(aes(label = round(pct, 2))) +
    # Add labels
    labs(
      x = 'Education', y = "Vote", 
      title = paste0('P(', variable,'=',class,')'),
      subtitle = 'Observed frequencies'
    ) +
    theme_bw()
  # Return the plot object
  return(plot)
}

# Probability of Leaving
prob_leave = nb_bes %>% 
  # Summarise by species
  with_groups(
    .groups = vote,
    summarise,
    # Add totals
    n = sum(n)
  ) %>% 
  # Calculate percentages (probabilities)
  mutate(prob = n / sum(n)) %>%
  # Filter only versicolor
  filter(vote == 'leave') %>% 
  # Pull the probability
  pull(prob)

print(prob_leave)

# Probability for having vote == leave
plot_probabilities('vote','leave')

# Probability of education == 1
prob_ed_1 = nb_bes %>% 
  # Summarise by petal_length
  with_groups(
    .groups = education,
    summarise,
    # Add totals
    n = sum(n)
  ) %>% 
  # Calculate percentages (probabilities)
  mutate(prob = n / sum(n)) %>%
  # Filter only education == 1
  filter(education == '1') %>% 
  # Pull the probability
  pull(prob)

print(prob_ed_1)

plot_probabilities('education', 1)

# Probability for having petal_length = mid and species = versicolor
# Create a boolean vector for color
color = pull(nb_bes, 'vote') == 'leave' & pull(nb_bes, 'education') == '1'

# Create the plot object
ggplot(nb_bes, aes(x = education, y = vote)) +
  # Mapping a bubble plot for factors
  geom_point(aes(size = pct, col = color), show.legend = FALSE) +
  # Custom size scales
  scale_size(range = c(15,30)) +
  # Custom colors
  scale_color_manual(values = c('orange','deepskyblue')) +
  # Add text
  geom_text(aes(label = round(pct, 2))) +
  # Add labels
  labs(
    x = 'Petal Length', y = "Species", 
    title = paste0('P(Leave and Ed == 1)'),
    subtitle = 'Observed frequencies'
  ) +
  theme_bw()

# Wrangling the data
prob_leave_education_1 = nb_bes %>% 
  # Summarise by petal_length
  with_groups(
    .groups = c(vote, education),
    summarise,
    # Add totals
    n = sum(n)
  ) %>% 
  # Calculate percentages (probabilities)
  mutate(prob = n / sum(n)) %>%
  # Filter only mid
  filter(education == 1, vote == 'leave') %>% 
  # Pull the probability
  pull(prob)

print(prob_leave_education_1)

# Probability of leave given ed 1
prob_leave_given_ed_1 = prob_leave_education_1 / prob_ed_1

print(prob_leave_given_ed_1)

# Naive Bayes with tidymodels -----------------------------------------------------
install.packages(c('klaR','discrim'))
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
    formula = vote ~ education, 
    # Set input data
    data = bes
  ) 

str(nb_fit)

nb_fit %>% 
  pluck('fit') %>% 
  pluck('tables')

# Augmenting predictions to a tibble
predictions_tb = nb_fit %>% 
  augment(bes) 

predictions_tb %>% 
  dplyr::select(vote, leave, education, .pred_class, .pred_stay, .pred_leave) %>% 
  unique()

# Confusion matrix with yardstick
confusion_matrix = predictions_tb %>% 
  conf_mat(truth = vote, estimate = .pred_class)

print(confusion_matrix)

bes %>% 
  # Count observations
  count(vote, education) %>% 
  spread(vote, n)


# The machine learning framework ------------------------------------------
bes = read_csv('data/BES.csv') %>% 
  # Filter the NA in the leave column
  filter(!is.na(leave)) %>% 
  # Transform the vote column into a factor with 'stay' and 'leave' as levels
  mutate(
    vote = factor(vote, levels = c('stay', 'leave'))
  )

# Data splitting for samples
set.seed(123)
bes_split = initial_split(
  # Data to split
  data = bes,
  # Proportions
  prop = 0.80,
  # Variable of interest
  strata = vote
)

# Get training and testing samples
bes_training = training(bes_split)
bes_testing = testing(bes_split)

# Create cross-validation folds
set.seed(123)
bes_folds = vfold_cv(bes_training, v = 5)

# Create a recipe
basic_recipe = recipe(
  # Set a formula
  formula = vote ~ education + age,
  # Set input data
  data = bes_training
) %>% 
  # Impute missing values in the education column using knn
  step_impute_knn(education) %>% 
  # Transform number to factor in the education column
  step_num2factor(education, levels = as.character(c(1:5))) 

set.seed(123)
nb_model = naive_Bayes() %>% 
  set_mode("classification") %>% 
  set_engine("naivebayes")  %>% 
  set_args(
    Laplace = tune(),
    smoothness = tune()
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
set.seed(123)

# Create a tune grid
nb_tuning = tune_grid(
  object = nb_workflow,
  resamples = bes_folds,
  param_info = nb_parameters,
  metrics = metric_set(yardstick::accuracy, yardstick::roc_auc),
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
  select_best("roc_auc")

print(best_nb)

# Finalize the workflow
final_nb = nb_workflow %>% 
  finalize_workflow(best_nb)

# Make a final fit
results_nb = final_nb %>% 
  last_fit(
    split = bes_split,
    metrics = metric_set(yardstick::accuracy, yardstick::roc_auc)
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

# Preproc
preproc = basic_recipe %>% prep %>% bake(new_data = bes)
# Make predictions
predictions_tb =  augment(nb_trained, preproc)

predictions_tb %>% 
  conf_mat(truth = vote, estimate = .pred_class) %>% 
  summary()


# Plot decision boundaries
ggplot(predictions_tb, aes(x = age)) +
  # Add the observations
  geom_point(aes(y = .pred_leave, col = education)) + 
  # Modificar el eje y
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  # Add proper labels
  labs(
    col = 'Education level',
    # Modify the x and y axis labels to make them clearer
    x = 'Age',
    title = 'Older people were more likely to vote "leave"',
    subtitle = 'Probability of voting "leave"',
    # Add the following caption
    caption = 'Own elaboration with data from Llaudet & Imai (2022)'
  ) +
  # Use a default theme
  theme_bw(base_size = 12) +
  # Modify legend attributes using legend.attribute
  theme(
    axis.title.y = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(fill = "transparent"),
    legend.position = c(0.1, 0.85),
    legend.key.height = unit(0.5,'cm')
  ) 




