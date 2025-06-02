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

# Look at the data
glimpse(CigarettesSW)
summary(CigarettesSW)

# Pairs plot
CigarettesSW %>% 
  select(-state) %>% 
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
  select(-state) %>% 
  GGally::ggpairs()


# Simple Regression -------------------------------------------------------
# Select 2 variables
simple_reg = cigarettes %>% 
  select(real_price, packs)

# EDA
summary(simple_reg)

# Packs distribution 
ggplot(simple_reg, aes(x = packs)) +
  geom_histogram() +
  # Add vertical line
  geom_vline(
    # We can perform operations from data
    mapping = aes(xintercept = mean(packs)), 
    # Add non-data related aesthetics
    col = 'darkblue', linetype = 'dashed', linewidth = 2
    ) +
  # Change theme
  theme_bw()

# Real price distribution
ggplot(simple_reg, aes(x = real_price)) +
  geom_histogram() +
  geom_vline(
    # We can perform operations from data
    mapping = aes(xintercept = mean(real_price)), 
    # Add non-data related aesthetics
    col = 'darkblue', linetype = 'dashed', linewidth = 2
  ) +
  # Change theme
  theme_bw()


# Regression model visualy
g = ggplot(
  data = simple_reg, 
  mapping = aes(x = real_price, y = packs)) +
  # Add scatter plot
  geom_point() +
  # Add x-histrogram
  geom_xsidehistogram() +
  # Add y-histogram
  geom_ysidehistogram() +
  # Add regression line
  stat_smooth(method = 'lm', se = FALSE) +
  # Mofify labels
  labs(y = 'Consumed Packs', x = 'Average Price') +
  theme_bw()

print(g)

# Some intuition behind
x = pull(simple_reg, real_price)
y = pull(simple_reg, packs)

# Manual average
sum(x) / (length(x))

# mean(x)
mean(x)

# Manual variance
sum((x - mean(x))^2) / (length(x) -1)

# var(x)
var(x)

# Manual standard deviation
sqrt(var(x))

# sd(x)
sd(x)

# Manual covariance
sum((x - mean(x)) * (y - mean(y))) / (length(x) -1)

# cov(x,y)
cov(x,y)

# Slope estimation
b_hat = cov(x,y)/var(x)
print(b_hat)

# Standard deviation ratios
ratio = sd(y)/sd(x)

# Weighted correlation
print(cor(x,y) * ratio)

# Intercept estimation
a_hat = mean(y) - b_hat * mean(x)
print(a_hat)

# Adding the prediction
simple_reg = simple_reg %>% 
  mutate(
    # Prediction
    .pred = a_hat + b_hat * real_price,
    # Residuals
    resid = packs - .pred
  ) 

# Residual histogram
ggplot(simple_reg, aes(x = resid)) +
  geom_histogram() +
  geom_vline(
    # We can perform operations from data
    mapping = aes(xintercept = mean(resid)), 
    # Add non-data related aesthetics
    col = 'darkblue', linetype = 'dashed', linewidth = 2
  ) +
  # Change theme
  theme_bw()

# Godness of the fit
simple_reg %>% 
  # Compare observed vs predicted values
  ggplot(aes(x = packs, y = .pred)) + 
  geom_point() +
  # Add 45 degree line
  geom_abline(slope = 1, col = 'red') +
  # Modify axis
  scale_x_continuous(limits = c(50,200)) +
  scale_y_continuous(limits = c(50,200)) 

# Satistical fitnes
simple_reg %>% 
  summarise(
    total_variance = sum((packs - mean(y))^2),
    deviance = sum((packs-.pred)^2),
    r_squared = 1 - (deviance/total_variance),
    rmse = sqrt(mean((packs-.pred)^2))
  )


# R base framework --------------------------------------------------------
model = lm(
  formula = packs ~ real_price,
  data = simple_reg
)

# Result
print(model)

# Coefficients
coefficients(model)

# Summary
summary(model)

# Install - load performance                                                       
if(require(performance) == FALSE){                                                
  install.packages('performance')                                                 
  library(performance)                                                            
}else{                                                                          
  library(performance)                                                            
}
check_model(model)

# Structural equation -----------------------------------------------------
ggplot(
  data = simple_reg, 
  mapping = aes(x = real_price, y = packs)) +
  # Add scatter plot
  geom_point() +
  # Add x-histrogram
  geom_xsidehistogram() +
  # Add y-histogram
  geom_ysidehistogram() +
  # Add regression line
  stat_smooth(method = 'lm', se = FALSE, formula = y ~ poly(x,2)) +
  # Mofify labels
  labs(y = 'Consumed Packs', x = 'Average Price') +
  theme_bw()

# Model definition
model_2 = lm(
  formula = packs ~ poly(real_price, 2),
  data = simple_reg
)

# Summary
summary(model_2)

# Performance
check_model(model_2)

# Linear - log model ------------------------------------------------------
ggplot(
  data = simple_reg, 
  mapping = aes(x = log(real_price), y = packs)) +
  # Add scatter plot
  geom_point() +
  # Add x-histrogram
  geom_xsidehistogram() +
  # Add y-histogram
  geom_ysidehistogram() +
  # Add regression line
  stat_smooth(method = 'lm', se = FALSE) +
  # Mofify labels
  labs(y = 'Consumed Packs', x = 'Average Price') +
  theme_bw()

# Model definition
model_3 = lm(
  formula = packs ~ log(real_price),
  data = simple_reg
)

# Summary
summary(model_3)

# Performance
check_model(model_3)

# Log-Log model (elasticities) --------------------------------------------
ggplot(
  data = simple_reg, 
  mapping = aes(x = log(real_price), y = log(packs))) +
  # Add scatter plot
  geom_point() +
  # Add x-histrogram
  geom_xsidehistogram() +
  # Add y-histogram
  geom_ysidehistogram() +
  # Add regression line
  stat_smooth(method = 'lm', se = FALSE) +
  # Mofify labels
  labs(y = 'Consumed Packs', x = 'Average Price') +
  theme_bw()

# Model definition
model_4 = lm(
  formula = log(packs) ~ log(real_price),
  data = simple_reg
)

# How do you interpret this?
coefficients(model_4)[1] %>% 
  exp()

# Summary
summary(model_4)

# Performance
check_model(model_4)


