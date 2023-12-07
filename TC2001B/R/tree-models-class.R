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
# Instalar - Cargar rpart                                                       
if(require(rpart) == FALSE){                                                
  install.packages('rpart')                                                 
  library(rpart)                                                            
}else{                                                                          
  library(rpart)                                                            
}
# Instalar - Cargar rpart.plot                                                       
if(require(rpart.plot) == FALSE){                                                
  install.packages('rpart.plot')                                                 
  library(rpart.plot)                                                            
}else{                                                                          
  library(rpart.plot)                                                            
}
# Instalar - Cargar ranger                                                       
if(require(ranger) == FALSE){                                                
  install.packages('ranger')                                                 
  library(ranger)                                                            
}else{                                                                          
  library(ranger)                                                            
}

# Data --------------------------------------------------------------------
creditcard = read_csv('data/creditcard.csv') %>% 
  janitor::clean_names() 

# Exploratory analysis
glimpse(creditcard)
summary(creditcard)

# Overall correlation
creditcard %>% 
  cor() %>% 
  GGally::ggcorr(label = TRUE)

# Correlation of the last 100 obs
creditcard %>% 
  tail(100) %>% 
  cor() %>% 
  GGally::ggcorr(label = TRUE)

# Random sampling correlation
creditcard %>% 
  sample_n(100) %>% 
  cor() %>% 
  GGally::ggcorr(label = TRUE)

# Understanding an unbalanced dataset
ggplot(creditcard, aes(x = factor(class))) + 
  # Create a bar plot
  geom_bar() +
  # Modify scale labels
  scale_y_continuous(
    labels = label_number(scale = 1e-3)
  ) +
  # Add proper labels
  labs(
    title = 'This is an unbalanced dataset',
    subtitle = 'Number of transactions (thousands)') +
  # Modify theme
  theme_bw() +
  theme(
    axis.title = element_blank()
  )

# Understanding data over time
creditcard %>% 
  # Count class of transaction by time
  count(time, class) %>% 
  # Create a line plot
  ggplot(aes(x = time, y = n)) +
  geom_line() +
  # Facet by class
  facet_wrap(~class, ncol = 1, scales = 'free_y')

# Mutate the data to work with
creditcard = creditcard %>% 
  mutate(
    # Create a datetime variable
    time = as.POSIXct(time,  origin = "2020-01-01"),
    # Change class to factor
    class = factor(class, levels = 0:1, labels = c('No Fraud', 'Fraud'))
  ) %>% 
  # Keep the last n observations
  tail(20000)



# Rolling origin framework ------------------------------------------------
# Split the date considering the time frame
data_split = initial_time_split(
  data = creditcard,
  prop = 0.80
)

# Get train and test dataset
train = training(data_split)
test = testing(data_split)

# Understand how they are different
train %>% select(time, class) %>%  summary()
test %>% select(time, class) %>%  summary()

# Initialize rolling origin cross validation
rolling_origin = rolling_origin(
  train,
  initial = 5000, # Size of learning sample
  assess = 2000, # Size of assessing sample
  cumulative = FALSE, # Avoif cumulative learning chunk
  skip = 2000 # Skipp 2000 obs for the next chunk
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
  ggplot(aes(y = factor(fold), col = set)) +
  # Add a segment
  geom_segment(aes(x = min, xend = max, yend = factor(fold))) + 
  # Add points
  geom_point(aes(x = max))+ 
  geom_point(aes(x = min)) +
  # Modify colors
  scale_color_manual(values = c('darkred','orange')) +
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
    legend.position = c(0.1, 0.95),
    legend.key.height = unit(0.5,'cm'),
    text = element_text(family = 'Bebas Neue')
  )

# Create a simple recipe
recipe = recipe(
  formula = class ~ .,
  data = train
)


# Explaining random trees -------------------------------------------------
# Create a simple classification tree
tree = decision_tree(cost_complexity = 0, min_n = 1) %>% 
  set_engine("rpart") %>% 
  set_mode("classification") %>% 
  set_args(model = TRUE)


# Create a simple workflow
tree_wf = workflow() %>% 
  add_recipe(recipe) %>% 
  add_model(tree) 

# Fit the simple tree
big_tree = tree_wf %>% 
  fit(train) %>% 
  extract_fit_parsnip()

# Visual explanaition
big_tree %>% 
  pluck('fit') %>% 
  prp(type=2, extra=4)


# Pruning a shorter tree
big_tree %>% 
  pluck('fit') %>% 
  prune(cp = 0.1) %>% 
  prp(type=2, extra=4)


# Hyperparameter tuning for trees -----------------------------------------
# Create a tunable tree
tree = decision_tree() %>% 
  set_engine("rpart") %>% 
  set_mode("classification") %>% 
  set_args(
    cost_complexity = tune(),
    tree_depth = tune(),
    min_n = tune()
  )

# Update the workflow
tree_wf = workflow() %>% 
  add_recipe(recipe) %>% 
  add_model(tree) 

# Extract hyperparameters
tree_parametrs = tree_wf %>% 
  extract_parameter_set_dials() 

# Tuning grid
set.seed(123)
tree_tuning = tune_grid(
  # Object to work with
  object = tree_wf,
  # Resampling set
  resamples = rolling_origin,
  # Hyperparameters
  param_info = tree_parametrs,
  # Metrics to asses
  metrics = metric_set(roc_auc, accuracy, sens, yardstick::spec),
  # Control grid
  control = control_grid(verbose = TRUE),
  # Grid size
  grid = 10
)

# Collect metrics
tree_metrics = tree_tuning %>%
  collect_metrics() %>% 
  # Order by mean
  arrange(mean)

# Select best model
best_tree = tree_tuning %>%
  select_best("spec")

print(best_tree)

# Finalize workflow
final_tree = tree_wf %>% 
  finalize_workflow(best_tree)

# Make last fit
results_tree = final_tree %>% 
  last_fit(
    split = data_split,
    metrics = metric_set(
      yardstick::roc_auc, 
      yardstick::accuracy,
      yardstick::sens,
      yardstick::spec)
  )

# Colect metrics
metrics_tree = results_tree %>% 
  collect_metrics() 

print(metrics_tree)

# Confusion matrix
results_tree %>% 
  collect_predictions() %>% 
  conf_mat(truth = class, estimate = .pred_class)


# Bagging Trees -----------------------------------------------------------
# Bootstrap 1
set.seed(2)
sample_1 = sample_frac(train, 1 , replace = TRUE)
tree_1  = rpart(class ~ ., data =  sample_1, method = "class") %>% 
  prune(cp=0.03)

prp(tree_1, type = 4, extra = 4)

# Bootstrap 2
set.seed(510)
sample_2 = sample_frac(train, 1 , replace = TRUE)
tree_2  = rpart(class ~ ., data =  sample_2, method = "class") %>% 
  prune(cp=0.03)

prp(tree_2, type = 4, extra = 4)

# Create bootstrap samples
bag_samples = bootstraps(train, times = 30)

# This function explains bootstrapping
explain_bootstrap = function(fold_num){
  # Unfold the sample
  fold = bag_samples %>% 
    pluck('splits') %>% 
    pluck(fold_num) %>% 
    analysis() %>% 
    # Summarise by class
    count(class) %>% 
    mutate(fold = paste('Fold', fold_num))
  # Return fold
  return(fold)
}

# Extract folds
map_df(1:nrow(bag_samples), explain_bootstrap) %>% 
  # Create a column plot
  ggplot(aes(x = class, y = n)) +
  geom_col() +
  # Add labels
  geom_label(aes(label = n)) +
  labs(title = 'Bootstrap Samples') +
  # Facet by fold
  facet_wrap(~fold) +
  # Modify theme
  theme_bw() +
  theme(
    axis.title = element_blank()
  )

# Create a simple tree
tree_model = decision_tree(cost_complexity = 0, min_n = 5) %>% 
  set_engine("rpart") %>% 
  set_mode("classification") %>% 
  set_args(model = TRUE)



# Create bagging function
bagging <- function(fold_num, bag_samples, model){
  # Extract foldd
  fold = bag_samples %>% 
    pluck('splits') %>% 
    pluck(fold_num) %>% 
    analysis()
  # Fit tree
  tree = model %>% fit(class ~ ., data = fold) %>% 
    pluck('fit')
  # Return tree
  return(tree)
}

# Apply bagging function to each sample
bagged_trees = map(
  .x = 1:nrow(bag_samples),
  .f = bagging,  
  bag_samples = bag_samples, 
  model = tree_model
  )

# How each tree looks like
prp(prune(bagged_trees[[1]], cp = 0.01))
prp(prune(bagged_trees[[2]], cp = 0.01))
prp(prune(bagged_trees[[3]], cp = 0.01))

# Collect predictions
prob_bagging = map(
  # For each bagged tree
  .x = bagged_trees, 
  # Predict probability of fraud
  .f = function(tree){ preds = predict(tree, test, type = "prob")[, 2]}
  ) %>% 
  # Create tibble
  as_tibble(.name_repair = "unique") %>% 
  # Add id
  mutate(id = row_number()) %>%
  # Pivot longer
  pivot_longer(cols = -id, names_to = "tree", values_to = "prob") %>% 
  # Group by id
  group_by(id) %>% 
  # Summarise
  summarise(prob = mean(prob)) %>% 
  # Add true values
  bind_cols(
    test %>% select(class)
    )

# Get ROC AUC
roc_auc(prob_bagging, truth = class, estimate = prob, event_level = "second")

# Confussion Matrix
table(
  pull(prob_bagging, prob) > 0.5,
  pull(prob_bagging, class)
  ) %>% 
  prop.table(2) %>% 
  round(2)


# Random forest -----------------------------------------------------------
randomforest = rand_forest() %>% 
  set_engine("ranger") %>% 
  set_mode("classification") %>% 
  set_args(
    mtry = tune(),
    trees= tune(),
    min_n = tune()
  )

# Create a workflow
randomforest_wf = workflow() %>% 
  add_recipe(recipe) %>% 
  add_model(randomforest) 

# Extract hyperparameters
randomforest_parametrs = randomforest_wf %>% 
  extract_parameter_set_dials() 

# Tuning grid
set.seed(123)
randomforest_tuning = tune_grid(
  # Object to work with
  object = randomforest_wf,
  # Resampling set
  resamples = rolling_origin,
  # Hyperparameters
  param_info = randomforest_parametrs,
  # Metrics to asses
  metrics = metric_set(yardstick::roc_auc, yardstick::accuracy, yardstick::sens, yardstick::spec),
  # Control grid
  control = control_grid(verbose = TRUE),
  # Grid size
  grid = 10
)

# Collect metrics
randomforest_metrics = randomforest_tuning %>%
  collect_metrics() %>% 
  arrange(mean)

# Select best
best_randomforest = randomforest_tuning %>%
  select_best("roc_auc")

print(best_randomforest)

# Finalize workflow
final_randomforest = randomforest_wf %>% 
  finalize_workflow(best_randomforest)

# Make last fit
results_randomforest = final_randomforest %>% 
  last_fit(
    split = data_split,
    metrics = metric_set(yardstick::roc_auc, yardstick::accuracy, yardstick::sens, yardstick::spec)
  )

# Collect metrics
metrics_randomforest = results_randomforest %>% 
  collect_metrics() 

print(metrics_randomforest)

# Asses results
randomforest_trained = results_randomforest %>% 
  extract_fit_parsnip() 

print(randomforest_trained)

# Make predictions
predictions_tb =  augment(randomforest_trained, creditcard)


# Get correlations
corr_prediction = predictions_tb %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
  as_tibble()

names = names(corr_prediction)

# Create a ggplot
corr_prediction %>% 
  select(.pred_Fraud, `.pred_No Fraud`) %>% 
  mutate(variable = names) %>% 
  gather(key, value, -variable) %>% 
  mutate(key = str_remove_all(key, '\\.pred_')) %>% 
  rename(
    class = key,
    correlation = value
  ) %>% 
  filter(!str_detect(variable,'Fraud')) %>% 
  ggplot(aes(x = correlation, y = variable, fill = correlation > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~class) +
  labs(title = 'Variable correlation with probabilities') + 
  theme_bw() +
  theme(
    axis.title = element_blank()
  )



