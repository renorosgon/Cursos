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
# Install - load poissonreg                                                       
if(require(poissonreg) == FALSE){                                                
  install.packages('poissonreg')                                                 
  library(poissonreg)                                                            
}else{                                                                          
  library(poissonreg)                                                            
}
# Install - load patchwork                                                       
if(require(patchwork) == FALSE){                                                
  install.packages('patchwork')                                                 
  library(patchwork)                                                            
}else{                                                                          
  library(patchwork)                                                            
}

# Data --------------------------------------------------------------------
# Load de data
crime = read_csv('https://raw.githubusercontent.com/proback/BeyondMLR/master/data/c_data2.csv') %>%
  # Clean data
  janitor::clean_names() %>% 
  # Change characters to factor
  mutate_if(is.character, factor)

# Look like this
summary(crime)

# Absolute vs relative values
# Histograms
abs_hist = ggplot(crime, aes(x = num_viol, fill = type)) +
  geom_histogram() +
  labs(
    x = 'Violent Crimes', 
    y = 'Count',
    title = 'Absolute Values',
    fill = 'Type'
    ) + 
  theme_bw()+
  theme(
    axis.title.x = element_blank()
  )

rel_hist = ggplot(crime, aes(x = viol_rate_10000, fill = type)) +
  geom_histogram()  +
  labs(
    title = 'Relative Values',
    fill = 'Type'
    ) + 
  theme_bw() +
  theme(
    axis.title = element_blank()
  )

# Boxplots
abs_box = ggplot(crime, aes(x = num_viol, y = region, fill = type)) +
  geom_boxplot(show.legend = FALSE) +
  labs(
    x = 'Violent Crimes', 
    y = 'Region'
  ) + 
  theme_bw() 

rel_box = ggplot(crime, aes(x = viol_rate_10000, y = region, fill = type)) +
  geom_boxplot(show.legend = FALSE) +
  labs(
    x = 'Violent Crimes per 10000 students', 
    y = 'Region'
  ) + 
  theme_bw() +
  theme(
    axis.title.y = element_blank()
  )

# Patchwork
(abs_hist + rel_hist) / (abs_box + rel_box) + plot_layout(guides = "collect")

# The linear model
ggplot(crime, aes(y = num_viol, x = enrollment)) +
  geom_point() +
  stat_smooth(method = 'lm', se = FALSE) + 
  labs(
    x = 'Students',
    y = 'Violent Crimes',
    title = 'Linear Regression'
  ) + 
  theme_bw() 


# Gaussian Fit ------------------------------------------------------------
linear_model = linear_reg() %>% 
  set_engine('glm') %>% 
  set_mode('regression')

# Hagamos un modelo de todas las variables
gaussian_fit = linear_model  %>% 
  fit(num_viol ~ type + enrollment + region, data = crime)  %>% 
  extract_fit_engine()

# Check performance
performance::check_model(gaussian_fit)

# Extract results
ols_results = gaussian_fit %>% 
  augment(crime, se_fit = T) %>% 
  dplyr::select(enrollment, num_viol, .fitted, .se.fit, type)

# Remember the efect of categorical predictors
ggplot(ols_results, aes(x = enrollment)) +
 geom_point(aes(y = .fitted, col = type), show.legend = F) +
  stat_smooth(aes(y = .fitted, col = type), 
              method = 'lm', se = F) +
  labs(
    title = 'Model Fit', 
    x = 'Enrollment', 
    col = 'Type'
    ) +
  theme_bw()
  
# Assesing Gaussian fit. Do you notice something weird?
lineal = ggplot(ols_results, aes(x = enrollment)) +
  # Add Labels
  labs(title = 'Gaussian Model', x = 'Enrollment', col = 'Violent Crimes') +
  # Add fitted values
  geom_point(aes(y = .fitted, col = 'Predicted')) +
  stat_smooth(aes(y = .fitted, col = 'Predicted'), 
              method = 'lm', se = F, show.legend = F) +
  # Add a horizontal line
  geom_hline(yintercept = 0) +
  # Change colors
  scale_color_manual(values = c('gray80','blue')) + 
  # Add real values
  geom_point(aes(y = num_viol, col = 'Observed')) +
  # Modify y axis scale
  scale_y_continuous(limits = c(-6,35)) + 
  # Modify theme
  theme_bw() +
  theme(axis.title.y = element_blank())

print(lineal)

# Check residuals
ggplot(ols_results, aes(x = num_viol - .fitted)) +
  geom_histogram() + 
  labs(title = 'Fit Residuals', x = 'Residuals') +
  theme_bw() +
  theme(axis.title.y = element_blank())

# Log-Log Fit -------------------------------------------------------------
# Please do not add a +1 this is just for pedagogic purposes
log_log_fit = linear_model %>% 
  fit(log(num_viol + 1) ~ type + log(enrollment + 1) + region, data = crime)  %>% 
  extract_fit_engine()

# Extract the results
log_log_results = log_log_fit %>% 
  augment(crime, se_fit = T) %>% 
  transmute(enrollment, num_viol, .fitted = exp(.fitted), .se.fit = exp(.se.fit))

# Check model
performance::check_model(log_log_fit)

# Asses results
log_log = ggplot(log_log_results, aes(x = enrollment)) +
  labs(title = 'Log-Log Model', x = 'Enrollment', col = 'Violent Crimes') +
  # Add fitted values
  geom_point(aes(y = .fitted, col = 'Predicted')) +
  stat_smooth(aes(y = .fitted, col = 'Predicted'), 
              method = 'lm', se = F, show.legend = F) +
  # Add a horizontal line
  geom_hline(yintercept = 0) +
  # Change colors
  scale_color_manual(values = c('gray80','blue')) + 
  # Add real values
  geom_point(aes(y = num_viol, col = 'Observed')) +
  # Modify y axis scale
  scale_y_continuous(limits = c(-6,35)) + 
  # Modify theme
  theme_bw() +
  theme(axis.title.y = element_blank())

print(log_log)

# Check Residuals
ggplot(log_log_results, aes(x = num_viol - .fitted)) +
  geom_histogram() + 
  labs(title = 'Fit Residuals', x = 'Residuals') +
  theme_bw() +
  theme(axis.title.y = element_blank())


# Poisson Regression ------------------------------------------------------
poisson_model = poisson_reg() %>% 
  set_engine('glm') %>% 
  set_mode('regression')

# Fit poisson model
poisson_fit = poisson_model %>% 
  fit(num_viol ~ type + log(enrollment) + region, data = crime)  %>% 
  extract_fit_engine()

performance::check_model(poisson_fit)

# Extract results
poisson_results = poisson_fit %>% 
  augment(crime, se_fit = T) %>%
  transmute(enrollment, num_viol, .fitted = exp(.fitted), .se.fit = exp(.se.fit))

# Asses Fit  
poisson = ggplot(poisson_results, aes(x = enrollment)) +
  labs(title = 'Poisson Model', x = 'Enrollment', col = 'Violent Crimes') +
  # Add fitted values
  geom_point(aes(y = .fitted, col = 'Predicted')) +
  stat_smooth(aes(y = .fitted, col = 'Predicted'), 
              method = 'lm', se = F, show.legend = F) +
  # Add a horizontal line
  geom_hline(yintercept = 0) +
  # Change colors
  scale_color_manual(values = c('gray80','blue')) + 
  # Add real values
  geom_point(aes(y = num_viol, col = 'Observed')) +
  # Modify y axis scale
  scale_y_continuous(limits = c(-6,35)) + 
  # Modify theme
  theme_bw() +
  theme(axis.title.y = element_blank())

print(poisson)

# Check Residuals
ggplot(poisson_results, aes(x = num_viol - .fitted)) +
  geom_histogram() + 
  labs(title = 'Fit Residuals', x = 'Residuals') +
  theme_bw() +
  theme(axis.title.y = element_blank())


# Comparisson -------------------------------------------------------------
plots = lineal + log_log + poisson + plot_layout(guides = "collect")
print(plots)






