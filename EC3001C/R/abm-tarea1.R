setwd("~/Desktop/ITESM/Cursos/EC3001C")

# Libraries ---------------------------------------------------------------
# Install - load tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}
# Install - load progress                                                       
if(require(progress) == FALSE){                                                
  install.packages('progress')                                                 
  library(progress)                                                            
}else{                                                                          
  library(progress)                                                            
}
# Install - load patchwork                                                       
if(require(patchwork) == FALSE){                                                
  install.packages('patchwork')                                                 
  library(patchwork)                                                            
}else{                                                                          
  library(patchwork)                                                            
}
# Install - load geomtextpath                                                       
if(require(geomtextpath) == FALSE){                                                
  install.packages('geomtextpath')                                                 
  library(geomtextpath)                                                            
}else{                                                                          
  library(geomtextpath)                                                            
}

# ABM Simulations ---------------------------------------------------------
# We will be working on the models proposed in
# Wealth inequality and social mobility: A simulation based modelling approach
# By Xiaoliang Yang and Peng Zhou
# https://www.econstor.eu/bitstream/10419/261231/1/E2022-03.pdf

# QUIZ MODEL
# Imagine there is an island with 𝑁 = 1,000 residents, and each of them initially owns
# exactly the same wealth, 𝑤0 = 3 units of coconuts. At the beginning of every period,
# each resident must hand in 1 unit of coconut to the chief, who then randomly distributes
# each collected coconut to all residents with absolutely equal opportunity. After, say,
# 𝑇 = 1,000 periods, what does the final wealth distribution look like?

# Set number of agents  
num_agents = ___

# Create agents with initial conditions
agents = tibble(
  # Define a unique id
  id = ___,
  # Set initial wealth
  wealth_0 = ___,
  # Set the iterative wealth
  wealth_T = ___
)
# Set number of periods
num_periods = ___

# Set coconut_handout
coconut_handout = ___
# Economy's production (Y)
production = num_agents * coconut_handout
# Autonomous consumption (c)
subsistence_level = ___
# Marginal propensity to consume pmc
marginal_propensity = ___

# Initialize a progress bar
bar = progress_bar$new(total = ___)
# Iterate over each period
for(___ in ___){
  # Create a beneficiaries tibble
  gains  = tibble(
    # Pick a random sample with replacement of size = collected_coconuts
    id = sample(x = ___, size = ___, replace = ___)
  )  %>%  
    # Count the number of times each id received a coconut
    count(id, name = 'income') 
  
  agents = agents %>% 
    # Join the gains
    left_join(gains, by = 'id') %>% 
    mutate(
      # Fill NA values with 0
      income = ___,
      # Keynesian consumption
      consumption = ___,
      # Update wealth
      wealth_T = ___,
      # Store the wealth history for this period
      "wealth_{t}" := wealth_T,
      # Delete columns
      income = NULL,
      consumption = NULL
    )
  
  # Bar tick
  bar$tick()
}

# Get final wealth
final_wealth = agents %>% 
  select(id, wealth_T)

# Get min and max limits of wealth
limits = range( pull(final_wealth, wealth_T) )
min = round(limits[1]) - 5
max = round(limits[2]) + 5
limits = c(min,max)

probability_plot = final_wealth %>% 
  # Createa a ggplot
  ggplot(aes(y = ___)) + 
  # Add a density histogram
  geom_histogram(
    aes(x = after_stat(density)), 
    fill = "white", 
    col = 'gray30'
  ) +
  # Add density plot
  geom_density() +
  # Add horizontal line at the initial wealth
  geom_hline(
    yintercept = 3,
    # Modify de line to be red, dashed and thinesr
    col = '___', 
    linetype = '___', 
    linewidth = ___
  ) +
  # Modify y-axis position and limits
  scale_y_continuous(position = "right", limits = limits) + 
  # reverse x-axis
  scale_x_reverse() +
  # Add labels
  labs(x = 'Probability') +
  # Modify theme
  theme(
    axis.title.y = element_blank(),
    plot.margin = margin(0,0.1,0,0, "cm")
  )


individuals_plot = final_wealth %>% 
  # Create a ggplot of individuals sorted by wealth
  ggplot(aes(x = ___, y = sort(___))) +
  # Add points
  geom_point() +
  # Modify y-axis limits
  scale_y_continuous(limits = ___) + 
  # Add labels
  labs(x = 'Individuals (sorted)') +
  # Add horizontal line at the initial wealth
  geom_hline(
    yintercept = ___,
    # Modify de line to be red, dashed and thinesr
    col = '___', 
    linetype = '___', 
    linewidth = ___
  ) +
  # Add label
  geom_text(label = 'Initial wealth', x = 500, y =  25, col = 'red') + 
  # Add arrow
  annotate(
    "segment", x = 500, xend = 500, y = 20, yend = 3,  colour = "red", linewidth = 0.5, 
    arrow = arrow(length = unit(.3,"cm"))
  ) +
  # Modify theme
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  )

# Patchworking two plots
quiz_plot = probability_plot + individuals_plot + 
  plot_annotation(
    title = "Quiz Model - Wealth Distribution with Stochastic Income",
    caption = 'Based on Yang & Zhou (2022)'
  )

print(quiz_plot)

# Lorenz curve
final_wealth = final_wealth %>% 
  # Order by wealth
  arrange(wealth_T) %>% 
  mutate(
    # Normalize wealth
    wealth_i = wealth_T + abs(min(wealth_T)) +  1/num_agents,
    # Calculate share of wealt
    share_of_wealth = ___/sum(___),
    # Calculate cumulative share of wealth (Lorenz curve)
    wealth_distribution = cumsum(___)/ sum(___),
    # Calculate perfect equality share
    perfect_equality_share = ___/___,
    # Calculate perfect equality line
    perfect_equality = cumsum(___),
    # Get de difference between the ideal scenario and actual wealth distribution
    difference = abs(___ - ___)
  ) 

# Calculate gini index
quiz_gini_index = final_wealth %>% 
  reframe(gini =  ___)) %>% 
  pull(gini)

print(quiz_gini_index)

# Lorenz curve
quiz_lorenz_plot = ggplot(
  data = final_wealth, 
  # Set perfect equality in the x axis
  aes(x = ___)) + 
  # Add line with text
  geom_textline(
    label = "Lorenz Curve", 
    # Set wealth_distribution in the y axis
    aes(y = ___), 
    vjust = 1.5, linewidth = 1, color = "red",
  ) +
  geom_textline(
    label = "Perfect equality", 
    # Set perfect_distribution in the y axis
    aes(y = ___), 
    vjust = -0.75, linewidth = 1,
  ) +
  # Add percents to ticks labels
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  # Add labs
  labs(
    x = 'Cumulative Population',
    y = 'Cumulative Wealth',
    title = "Quiz Model - Wealth Distribution with Stochastic Income",
    subtitle = paste('Gini Index:', round(quiz_gini_index, 2)),
    caption = 'Based on Yang & Zhou (2022)'
  )

print(quiz_lorenz_plot)

# Transform the time series to long format
wealth_series = agents %>% 
  # Select all except wealt_T
  select(-wealth_T) %>% 
  # Change to long format keeping the id column intact
  gather(time, wealth, - id) %>% 
  # Remove the text from the time column
  mutate(time = as.numeric(str_remove(time, 'wealth_')))

# Plot wealth distribution
quiz_model_time_plot = wealth_series %>% 
  # Filter periods 300 and 1000
  filter(time %in% c(___, ___)) %>% 
  ggplot(aes(y = wealth)) +
  # Add histogram
  geom_histogram(
    # Set density in the x axis
    aes(x = ___(___)), colour = "___", fill = "___") +
  # Modify y-axis position and limits
  scale_y_continuous(position = "right", limits = limits) + 
  # reverse x-axis
  scale_x_reverse() +
  # Add labels
  labs(
    x = 'Probability',
    title = "Wealth Distribution with Stochastic Income",
    caption = 'Based on Yang & Zhou (2022)'
  )  +
  # Facet by period
  facet_wrap(~time, nrow = 1, scales = 'free_x') +
  # Modify theme
  theme(
    axis.title.y = element_blank(),
    plot.margin = margin(0,0.1,0,0, "cm")
  )

print(quiz_model_time_plot)

# Mobility Matrix
quiz_model_mobility = wealth_series %>% 
  # Keep period 300 (age = 30's) and 1000
  filter(time %in% c(___, ___)) %>% 
  # Spread time
  spread(time, wealth, sep = '_') %>% 
  mutate(
    # Calculate initial wealth quintiles
    quintile_start = cut(
      x = time_300, 
      breaks = quantile(time_300, seq(0,1,.2)), 
      labels = c('I','II','III','IV','V'),
      include.lowest = TRUE
    ),
    # Calculate final  wealth quintiles
    quintile_end = cut(
      x = time_1000, 
      breaks = quantile(time_1000, seq(0,1,.2)), 
      labels = c('I','II','III','IV','V'),
      include.lowest = TRUE
    )
  ) %>% 
  # Count cases
  count(quintile_start, quintile_end) %>% 
  # Calculate percentages
  with_groups(
    .groups = quintile_start,
    mutate,
    n = n/sum (n)
  ) %>% 
  # Create matrix
  pivot_wider(names_from = quintile_end, values_from = n)

print(quiz_model_mobility)


# Base Model --------------------------------------------------------------
# We extend the quiz model by including production and consumption to form
# the baseline experiment. Assume that there are 𝑌 = 1000 units of outputs 
# produced in every period and each resident (endowed with the same initial 
# wealth 𝑤0 = 3) has a Keynesian consumption function:
#  𝑐_𝑡 = 𝑐̅+ 𝛼 × 𝑦_𝑡
# where 𝑐̅= 0.4 is the subsistence level of consumption, 𝛼 = 0.6 is the
# marginal propensity to consume, and 𝑦𝑡 is the total earnings distributed 
# to the agent. Those whose wealth falls below 𝑐̅in the beginning of each
# period will get 𝑐̅from the government, but they still have an equal
# chance to be distributed with new earnings.

# Set number of agents  
num_agents = ___

# Create agents with initial conditions
agents = tibble(
  # Define a unique id
  id = ___,
  # Set initial wealth
  wealth_0 = ___,
  # Set the iterative wealth
  wealth_T = ___
)
# Set number of periods
num_periods = ___

# Set coconut_handout
allowance = ___
# Economy's production (Y)
production = num_agents * allowance
# Autonomous consumption (c)
subsistence_level = ___
# Marginal propensity to consume pmc
marginal_propensity = ___

# Initialize a progress bar
bar = progress_bar$new(total = ___)
# Iterate over each period
for(___ in ___){
  # Number of agents who need subsidy
  subsidies = sum(___ < ___) * ___
  
  # Create a gains tibble
  gains  = tibble(
    # Pick a random sample with replacement of size = num_agents
    id = sample(x = ___, size = ___, replace = ___)
  )  %>%  
    # Count the number of times each id received a coconut
    count(id, name = 'income') %>% 
    mutate(income = ___ * (___ - ___) / ___)
  
  
  agents = agents %>% 
    # Join the gains
    left_join(gains, by = 'id') %>% 
    mutate(
      # Round 1: Subsidy the poor
      subsidy = ifelse(___ < ___, ___, ___),
      # Round 2: Distribute the rest of production
      income = coalesce(income, 0),
      # Total income is the sum of subdidy and income
      total_income = ___ + ___
      # Keynesian consumption
      consumption = ifelse(
        # If current wealth is less than the consumption capabilities
        wealth_T + ___ < ___ + ___ * ___,
        # Exhaust wealth (wealth + total_income)
        ___ + ___,
        # Else consume its keynesian demand
        ___ + ___ * ___
      ),
      # Update wealth
      wealth_T = ___ + ___ - ___,
      # Store the wealth history for this period
      "wealth_{t}" := ___,
      # Delete columns
      subsidy = ___,
      income = ___,
      consumption = ___,
      total_income = ___
    )
  
  # Bar tick
  bar$tick()
}

# Get final wealth
final_wealth = agents %>% 
  select(id, wealth_T)

# Get min and max limits of wealth
limits = ___
min = ___
max = ___
limits = ___

probability_plot = final_wealth %>% 
  # Create a ggplot
  ggplot(aes(y = ___)) + 
  # Add a density histogram
  ___(aes(x = ___(___)), fill = "___", col = '___') +
  # Add density plot
  ___() +
  # Add horizontal line at the initial wealth
  ___(yintercept = ___, col = '___', ___ = '___', ___ = ___) +
  # Modify y-axis position and limits
  scale_y_continuous(position = "right", limits = ___) + 
  # reverse x-axis
  scale_x_reverse() +
  # Add labels
  labs(x = 'Probability') +
  # Modify theme
  theme(
    axis.title.y = element_blank()
  )


individuals_plot = final_wealth %>% 
  # Create a ggplot of individuals sorted by wealth
  ggplot(aes(x = ___, y = ___(___))) +
  # Add points
  geom_point() +
  # Modify y-axis limits
  scale_y_continuous(limits = ___) + 
  # Add labels
  labs(x = 'Individuals (sorted)') +
  # Add horizontal line at the initial wealth
  geom_hline(yintercept = ___, col = '___', linetype = '___', linewidth = ___) +
  # Add label
  geom_text(label = 'Initial wealth', x = 500, y =  25, col = 'red') + 
  # Add arrow
  annotate(
    "segment", x = 500, xend = 500, y = 20, yend = 3,  colour = "red", linewidth = 0.5, 
    arrow = arrow(length = unit(.3,"cm"))
  ) +
  # Modify theme
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank()
  )

# Patchworking two plots
base_model_plot = probability_plot + individuals_plot + 
  plot_annotation(
    title = "Base Model - Wealth Distribution with Stochastic Income",
    caption = 'Based on Yang & Zhou (2022)'
  )

print(base_model_plot)

# Lorenz curve
final_wealth = final_wealth %>% 
  # Order by wealth
  ___(___) %>% 
  mutate(
    # Normalize wealth
    wealth_i = ___,
    # Calculate share of wealth
    share_of_wealth = ___,
    # Calculate cumulative share of wealth (Lorenz curve)
    wealth_distribution = ___,
    # Calculate perfect equality share
    perfect_equality_share = ___,
    # Calculate perfect equality line
    perfect_equality = ___(___),
    # Get de difference between the ideal scenario and actual wealth distribution
    difference = ___(___ - ___)
  ) 

# Save base wealth distribution
base_model = ___

# Calculate gini index
base_gini_index = final_wealth %>% 
  reframe(gini =  ___) %>% 
  pull(gini)

print(base_gini_index)

# Lorenz curve
base_lorenz_plot = ggplot(final_wealth, aes(x = ___)) + 
  # Add line with text
  ___(
    label = "Lorenz Curve", aes(y = ___), 
    vjust = ___, linewidth = ___, color = "___",
  ) +
  ___(
    label = "Perfect equality", aes(y = ___), 
    vjust = -___, linewidth = ___,
  ) +
  # Add percents to ticks labels
  scale_x_continuous(labels = ___) +
  scale_y_continuous(labels = ___) +
  # Add labs
  labs(
    x = 'Cumulative Population',
    y = 'Cumulative Wealth',
    title = "Base Model - Wealth Distribution with Stochastic Income",
    subtitle = paste('Gini Index:', round(base_gini_index, 2)),
    caption = 'Based on Yang & Zhou (2022)'
  )

print(base_lorenz_plot)

# Transform the time series to long format
wealth_series = agents %>% 
  # Select all except wealth_T
  select(-wealth_T) %>% 
  # Change to long format keeping the id column intact
  gather(time, wealth, - id) %>% 
  # Remove the text from the time column
  mutate(time = as.numeric(str_remove(time, 'wealth_')))

# Plot wealth distribution
base_model_time_plot = wealth_series %>% 
  # Filter periods 300 and 1000
  filter(___) %>% 
  ggplot(aes(y = ___)) +
  # Add histogram
  ___(aes(x = ___(___)), colour = "___", fill = "___") +
  # Modify y-axis position and limits
  scale_y_continuous(position = "right", limits = limits) + 
  # reverse x-axis
  scale_x_reverse() +
  # Add labels
  labs(
    x = 'Probability',
    title = "Base Model - Wealth Distribution with Stochastic Income",
    caption = 'Based on Yang & Zhou (2022)'
  )  +
  # Facet by period
  facet_wrap(~___, nrow = 1, scales = '___') +
  # Modify theme
  theme(
    axis.title.y = element_blank()
  )

print(base_model_time_plot)

# Mobility Matrix
base_model_mobility = wealth_series %>% 
  # Keep period 300 (age = 30's) and 1000
  filter(___) %>% 
  # Spread time
  spread(time, wealth, sep = '_') %>% 
  mutate(
    # Calculate initial wealth quintiles
    quintile_start = cut(
      x = time_300, 
      breaks = quantile(time_300, ___), 
      labels = ___,
      include.lowest = ___
    ),
    # Calculate final  wealth quintiles
    quintile_end = cut(
      x = time_1000, 
      breaks = quantile(time_300, ___), 
      labels = ___,
      include.lowest = ___
    )
  ) %>% 
  # Count cases
  count(___, ___) %>% 
  # Calculate percentages
  with_groups(
    .groups = ___,
    mutate,
    n = ___
  ) %>% 
  # Create matrix
  pivot_wider(names_from = ___, values_from = ___) %>% 
  # fill nas
  mutate_if(is.numeric, coalesce, 0) 

print(base_model_mobility)
