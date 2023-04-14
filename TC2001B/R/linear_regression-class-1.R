# Libraries ---------------------------------------------------------------
# Install - load tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}
# Install - load ggside                                                       
if(require(ggside) == FALSE){                                                
  install.packages('ggside')                                                 
  library(ggside)                                                            
}else{                                                                          
  library(ggside)                                                            
}
# Install - AER                                        
if(require(AER) == FALSE){                                                
  install.packages('AER')                                                 
}

# Data --------------------------------------------------------------------
data("CigarettesSW", package = "AER")

# Description
?AER::CigarettesSW 
# EDA
CigarettesSW %>% 
  select_if(is.numeric) %>% 
  GGally::ggpairs()

# Data wrangling
cigarettes = CigarettesSW %>% 
  # Transmute date
  transmute(
    # Keep state, year and packs
    state, year, packs,
    # Deflact real values
    real_price = price / cpi,
    real_local_tax =  tax / cpi,
    real_sales_tax =  taxs / cpi,
    tax_diff = real_local_tax - real_sales_tax,
    real_income_percapita = income / population / cpi
  )

# Retult
cigarettes %>% 
  select_if(is.numeric) %>% 
  GGally::ggpairs()


# Simple Regression -------------------------------------------------------
# Select 2 variables
simple_reg = cigarettes %>% 
  select(real_price, packs)

# EDA
summary(simple_reg)

# Histogram
ggplot(simple_reg, aes(x = packs)) +
  geom_histogram() +
  geom_vline(
    aes(xintercept = mean(packs)), 
    col = 'red', linetype = 'dashed'
    ) 

ggplot(simple_reg, aes(x = real_price)) +
  geom_histogram() +
  geom_vline(
    aes(xintercept = mean(real_price)), 
    col = 'red',
    linetype = 'dashed'
  )


# Regression model visualy
g = ggplot(simple_reg, aes(x = packs, y = real_price)) +
  geom_point() +
  geom_xsidehistogram() +
  geom_ysidehistogram() +
  stat_smooth(method = 'lm', se = FALSE) +
  labs(x = 'Consumed Pack', 'Average Price')

print(g)

# Elasticidades
g = ggplot(simple_reg, aes(x = log(packs), y = log(real_price))) +
  geom_point() +
  geom_xsidehistogram() +
  geom_ysidehistogram() +
  stat_smooth(method = 'lm', se = FALSE) +
  labs(x = 'Consumed Pack (log)', 'Average Price (log)')

print(g)

# Some intuition behind
x = pull(simple_reg, real_price) %>%  log()
y = pull(simple_reg, packs) %>%  log()

# Calcula el promedio de x
sum(x) / (length(x))

# Utiliza el comando mean(x)
mean(x)

# Calcula la varianza de x 
sum((x - mean(x))^2) / (length(x) -1)

# Utiliza el comando var(x)
var(x)

# Calcula la desviacion estandar de x 
sqrt(var(x))

# Utiliza el comando sd(x)
sd(x)

# Calcula la covarianza entre x y y
sum((x - mean(x)) * (y - mean(y))) / (length(x) -1)

# Utiliza el comando cov(x,y)
cov(x,y)

# La estimacion de la pendiente
b_hat = cov(x,y)/var(x)
print(b_hat)

# Calcula el ratio de las desviaciones estandar de y y x
ratio = sd(y)/sd(x)

# Multiplica el ratio creado por la correlacion entre x y y
print(cor(x,y) * ratio)

# Calcula la media de y, posteriormente restale el producto de beta_hat por x.
# Asignalo a un objeto llamado a_hat
a_hat = mean(y) - b_hat * mean(x)
print(a_hat)
exp(a_hat)

# Adding the prediction
simple_reg = simple_reg %>% 
  mutate(
    # Prediccion
    .pred = a_hat + b_hat * log(real_price),
    # Residuals
    resid = log(packs) - .pred
  ) 

# Residual histogram
ggplot(simple_reg, aes(x = resid)) +
  geom_histogram() +
  geom_vline(aes(xintercept = mean(resid)), col = 'red', linetype = 'dashed')

# Godness of the fit
simple_reg %>% 
  ggplot(aes(x = log(packs), y = .pred)) + 
  geom_point() +
  geom_abline(slope = 1, col = 'red') +
  scale_x_continuous(limits = c(4,6)) +
  scale_y_continuous(limits = c(4,6)) 

simple_reg %>% 
  ggplot(aes(x = sort(log(packs)), y = sort(.pred))) + 
  geom_point() +
  geom_abline(slope = 1, col = 'red') +
  scale_x_continuous(limits = c(3.5,6)) +
  scale_y_continuous(limits = c(3.5,6)) 

# Satisticaly
simple_reg %>% 
  summarise(
    varianza_total = sum((log(packs)-mean(y))^2),
    devianza = sum((log(packs)-.pred)^2),
    r_cuadrada = 1 - (devianza/varianza_total),
    rmse = sqrt(mean((log(packs)-.pred)^2))
  )


# The machine learning framework ------------------------------------------
# Install - load tidymodels                                                       
if(require(tidymodels) == FALSE){                                                
  install.packages('tidymodels')                                                 
  library(tidymodels)                                                            
}else{                                                                          
  library(tidymodels)                                                            
}
# Data splitting for samples
set.seed(123)
smoke_split = initial_split(
  # Data to split
  data = cigarettes,
  # Proportions
  prop = 0.80,
  # Variable of interest
  strata = packs
)

# Get training and testing samples
smoke_training = training(smoke_split)
smoke_testing = testing(smoke_split)

# Create cross-validation folds
set.seed(123)
smoke_folds = vfold_cv(smoke_training, v = 3)

# Create a recipe
basic_recipe = recipe(
  # Set a formula
  formula = packs ~ real_price,
  # Set input data
  data = smoke_testing
) %>% 
  step_log(all_numeric())

set.seed(123)
linear_regression = linear_reg() %>% 
  set_engine('lm') %>% 
  set_mode('regression')

# Create a workflow
lr_workflow = workflow() %>% 
  # Add your recipe
  add_recipe(basic_recipe) %>% 
  # Add your model
  add_model(linear_regression)

# Get parameters to tune
lr_parameters = lr_workflow %>% 
  extract_parameter_set_dials() 

# Create a tune grid
lr_tuning = tune_grid(
  object = lr_workflow,
  resamples = smoke_folds,
#  param_info = lr_parameters,
  metrics = metric_set(yardstick::rmse),
  control = control_grid(verbose = TRUE),
  grid = 1
)

# Collect metrics
tunning_metrics = lr_tuning %>%
  collect_metrics() %>% 
  # Arrange results from higher to lower
  arrange(desc(mean))

# Select the best model
best_lr = lr_tuning %>%
  select_best("rmse")

print(best_lr)

# Finalize the workflow
final_lr = lr_workflow %>% 
  finalize_workflow(best_lr)

# Make a final fit
results_lr = final_lr %>% 
  last_fit(
    split = smoke_split,
    metrics = metric_set(yardstick::rmse)
  )

# Asses testing metrics
metrics_lr = results_lr %>% 
  collect_metrics()

print(metrics_lr)


# Working with the trained model ------------------------------------------
# Extract the trained model
lr_trained = results_lr %>% 
  extract_fit_parsnip() 

print(lr_trained)

# Summary
lr_trained %>% 
  pluck('fit') %>% 
  summary() 

# Tidy coefficients
tidy(lr_trained)

# Preproc
preproc = basic_recipe %>% prep() %>% bake(new_data = cigarettes)
# Make predictions
predictions_tb =  augment(lr_trained, preproc)

predictions_tb %>% 
  ggplot(aes(x = packs, y = .pred)) +
  geom_point() +
  geom_abline(slope = 1, col = 'red') +
  coord_obs_pred()  

# Install - load performance                                                       
if(require(performance) == FALSE){                                                
  install.packages('performance')                                                 
  library(performance)                                                            
}else{                                                                          
  library(performance)                                                            
}
check_model(lr_trained)
