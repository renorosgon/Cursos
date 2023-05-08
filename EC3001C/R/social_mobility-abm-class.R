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
num_agents = 1000

# Create agents with initial conditions
agents = tibble(
  # Define a unique id
  id = 1:num_agents,
  # Set initial wealth
  wealth_0 = 3,
  # Set the iterative wealth
  wealth_T = 3
)
# Set number of periods
num_periods = 1000

# Set coconut_handout
coconut_handout = 1
# Economy's production (Y)
production = num_agents * coconut_handout
# Autonomous consumption (c)
subsistence_level = 0.4
# Marginal propensity to consume pmc
marginal_propensity = 0.6

# Initialize a progress bar
bar = progress_bar$new(total = num_periods)
# Iterate over each period
for(t in 1:num_periods){
  # Create a beneficiaries tibble
  gains  = tibble(
    # Pick a random sample with replacement of size = collected_coconuts
    id = sample(x = pull(agents, id), size = num_agents, replace = TRUE)
  )  %>%  
    # Count the number of times each id received a coconut
    count(id, name = 'income') 
  
  agents = agents %>% 
    # Join the gains
    left_join(gains, by = 'id') %>% 
    mutate(
      # Fill NA values with 0
      income = coalesce(income, 0),
      # Keynesian consumption
      consumption = subsistence_level + marginal_propensity * income,
      # Update wealth
      wealth_T = wealth_T + income - consumption,
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
  ggplot(aes(y = wealth_T)) + 
  # Add a density histogram
  geom_histogram(aes(x = after_stat(density)), fill = "white", col = 'gray30') +
  # Add density plot
  geom_density() +
  # Add horizontal line at the initial wealth
  geom_hline(yintercept = 3, col = 'red', linetype = 'dashed', linewidth = 1) +
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
  ggplot(aes(x = id, y = sort(wealth_T))) +
  # Add points
  geom_point() +
  # Modify y-axis limits
  scale_y_continuous(limits = limits) + 
  # Add labels
  labs(x = 'Individuals (sorted)') +
  # Add horizontal line at the initial wealth
  geom_hline(yintercept = 3, col = 'red', linetype = 'dashed', linewidth = 1) +
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
    share_of_wealth = wealth_i/sum(wealth_i),
    # Calculate cumulative share of wealth (Lorenz curve)
    wealth_distribution = cumsum(wealth_i)/ sum(wealth_i),
    # Calculate perfect equality share
    perfect_equality_share = 1/num_agents,
    # Calculate perfect equality line
    perfect_equality = cumsum(perfect_equality_share),
    # Get de difference between the ideal scenario and actual wealth distribution
    difference = abs(perfect_equality - wealth_distribution)
  ) 

# Calculate gini index
quiz_gini_index = final_wealth %>% 
  reframe(gini =  2 * mean(difference)) %>% 
  pull(gini)

print(quiz_gini_index)

# Lorenz curve
quiz_lorenz_plot = ggplot(final_wealth, aes(x = perfect_equality)) + 
  # Add line with text
  geom_textline(
    label = "Lorenz Curve", aes(y = wealth_distribution), 
    vjust = 1.5, linewidth = 1, color = "red",
  ) +
  geom_textline(
    label = "Perfect equality", aes(y = perfect_equality), 
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
  # Filter periods
  filter(time %in% c(300, 1000)) %>% 
  ggplot(aes(y = wealth)) +
  # Add histogram
  geom_histogram(aes(x = after_stat(density)), colour = "gray20", fill = "white") +
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
  filter(time %in% c(300, 1000)) %>% 
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
num_agents = 1000

# Create agents with initial conditions
agents = tibble(
  # Define a unique id
  id = 1:num_agents,
  # Set initial wealth
  wealth_0 = 3,
  # Set the iterative wealth
  wealth_T = 3
)
# Set number of periods
num_periods = 1000

# Set coconut_handout
allowance = 1
# Economy's production (Y)
production = num_agents * allowance
# Autonomous consumption (c)
subsistence_level = 0.4
# Marginal propensity to consume pmc
marginal_propensity = 0.6

# Initialize a progress bar
bar = progress_bar$new(total = num_periods)
# Iterate over each period
for(t in 1:num_periods){
  # Number of agents who need subsidy
  subsidies = sum(pull(agents, wealth_T) < subsistence_level) * subsistence_level
  
  # Create a gains tibble
  gains  = tibble(
    # Pick a random sample with replacement of size = num_agents
    id = sample(x = pull(agents, id), size = num_agents, replace = TRUE)
  )  %>%  
    # Count the number of times each id received a coconut
    count(id, name = 'income') %>% 
    mutate(income = income * (production - subsidies) / num_agents)
  
  
  agents = agents %>% 
    # Join the gains
    left_join(gains, by = 'id') %>% 
    mutate(
      # Round 1: Subsidy the poor
      subsidy = ifelse(wealth_T < subsistence_level, subsistence_level, 0),
      # Round 2: Distribute the rest of production
      income = coalesce(income, 0),
      # Total income
      total_income = subsidy + income,
      # Keynesian consumption
      consumption = ifelse(
        # If current wealth is less than the consumption capabilities
        wealth_T + total_income < subsistence_level + marginal_propensity * income,
        # Exhaust wealth
        wealth_T + total_income,
        # Else consume
        subsistence_level + marginal_propensity * income
      ),
      # Update wealth
      wealth_T = wealth_T + total_income - consumption,
      # Store the wealth history for this period
      "wealth_{t}" := wealth_T,
      # Delete columns
      subsidy = NULL,
      income = NULL,
      consumption = NULL,
      total_income = NULL
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
  # Create a ggplot
  ggplot(aes(y = wealth_T)) + 
  # Add a density histogram
  geom_histogram(aes(x = after_stat(density)), fill = "white", col = 'gray30') +
  # Add density plot
  geom_density() +
  # Add horizontal line at the initial wealth
  geom_hline(yintercept = 3, col = 'red', linetype = 'dashed', linewidth = 1) +
  # Modify y-axis position and limits
  scale_y_continuous(position = "right", limits = limits) + 
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
  ggplot(aes(x = id, y = sort(wealth_T))) +
  # Add points
  geom_point() +
  # Modify y-axis limits
  scale_y_continuous(limits = limits) + 
  # Add labels
  labs(x = 'Individuals (sorted)') +
  # Add horizontal line at the initial wealth
  geom_hline(yintercept = 3, col = 'red', linetype = 'dashed', linewidth = 1) +
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
  arrange(wealth_T) %>% 
  mutate(
    # Normalize wealth
    wealth_i = wealth_T,
    # Calculate share of wealth
    share_of_wealth = wealth_i/sum(wealth_i),
    # Calculate cumulative share of wealth (Lorenz curve)
    wealth_distribution = cumsum(wealth_i)/ sum(wealth_i),
    # Calculate perfect equality share
    perfect_equality_share = 1/num_agents,
    # Calculate perfect equality line
    perfect_equality = cumsum(perfect_equality_share),
    # Get de difference between the ideal scenario and actual wealth distribution
    difference = abs(perfect_equality - wealth_distribution)
  ) 

# Save base wealth distribution
base_model = pull(final_wealth, wealth_distribution)

# Calculate gini index
base_gini_index = final_wealth %>% 
  reframe(gini =  2 * mean(difference)) %>% 
  pull(gini)

print(base_gini_index)

# Lorenz curve
base_lorenz_plot = ggplot(final_wealth, aes(x = perfect_equality)) + 
  # Add line with text
  geom_textline(
    label = "Lorenz Curve", aes(y = wealth_distribution), 
    vjust = 1.5, linewidth = 1, color = "red",
  ) +
  geom_textline(
    label = "Perfect equality", aes(y = perfect_equality), 
    vjust = -0.75, linewidth = 1,
  ) +
  # Add percents to ticks labels
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
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
  # Filter periods
  filter(time %in% c(300, 1000)) %>% 
  ggplot(aes(y = wealth)) +
  # Add histogram
  geom_histogram(aes(x = after_stat(density)), colour = "gray20", fill = "white") +
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
  facet_wrap(~time, nrow = 1, scales = 'free_x') +
  # Modify theme
  theme(
    axis.title.y = element_blank()
  )

print(base_model_time_plot)

# Mobility Matrix
base_model_mobility = wealth_series %>% 
  # Keep period 300 (age = 30's) and 1000
  filter(time %in% c(300, 1000)) %>% 
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
  pivot_wider(names_from = quintile_end, values_from = n) %>% 
  # fill nas
  mutate_if(is.numeric, coalesce, 0) 

print(base_model_mobility)


# Progressive Tax ---------------------------------------------------------
# Building on the baseline experiment, we add a tax system with a simplified 
# progressive income tax (𝜏) and the transfer payment (𝑇). Each resident 
# beyond a certain tax allowance depending on the national income level 
# is taxed at a rate of 20%. The tax revenue is then equally redistributed 
# to everyone to simulate the use of taxes as transfer payments and public 
# goods/services. 

# Set number of agents  
num_agents = 1000

# Create agents with initial conditions
agents = tibble(
  # Define a unique id
  id = 1:num_agents,
  # Set initial wealth
  wealth_0 = 3,
  # Set the iterative wealth
  wealth_T = 3
)
# Set number of periods
num_periods = 1000

# Set coconut_handout
allowance = 1
# Economy's production (Y)
production = num_agents * 1
# Production percapita 
production_percapita = production / num_agents
# Autonomous consumption (c)
subsistence_level = 0.4
# Marginal propensity to consume pmc
marginal_propensity = 0.6
# Income tax
tax = 0.2

# Initialize a progress bar
bar = progress_bar$new(total = num_periods)
for(t in 1:num_periods){
  
  # Number of agents who need subsidy
  subsidies = sum(pull(agents, wealth_T) < subsistence_level) * subsistence_level
  
  # Create a gains tibble
  gains  = tibble(
    # Pick a random sample with replacement of size = num_agents
    id = sample(x = pull(agents, id), size = num_agents, replace = TRUE)
  )  %>%  
    # Count the number of times each id received a coconut
    count(id, name = 'income') %>% 
    mutate(income = income * (production - subsidies) / num_agents)
  
  
  agents = agents %>% 
    # Join the gains
    left_join(gains, by = 'id') %>% 
    mutate(
      # Round 1: Subsidy the poor
      subsidy = ifelse(wealth_T < subsistence_level, subsistence_level, 0),
      # Round 2: Distribute the rest of production
      income = coalesce(income, 0),
      # Round 3: Charge Tax
      taxpay = ifelse(
        # If did not received income, then no tax is paid
        income == 0, 0,
        # Else
        ifelse(
          subsidy + income > allowance * production_percapita, 
          (subsidy + income - allowance * production_percapita) *  tax, 
          0
        )
      ),
      # Total income
      total_income = subsidy + income - taxpay,
      # Round 4
      # Keynesian consumption
      consumption = ifelse(
        # If current wealth is less than the consumption capabilities
        wealth_T + total_income < subsistence_level + marginal_propensity * income,
        # Exhaust wealth
        wealth_T + total_income,
        # Else consume
        subsistence_level + marginal_propensity * (income - taxpay)
      ),
      # Update wealth
      wealth_T = wealth_T + total_income - consumption + (sum(taxpay)/num_agents),
      # Store the wealth history for this period
      "wealth_{t}" := wealth_T,
      # Delete columns
      subsidy = NULL,
      income = NULL,
      consumption = NULL,
      total_income = NULL
    )
  
  # Bar tick
  bar$tick()
}

# Get final wealth
final_wealth = agents %>% 
  select(id, wealth_T)

# Get min and max limits of wealth
limits = range( pull(final_wealth, wealth_T) )
min = min(round(limits[1]) - 5, 0)
max = round(limits[2]) + 5
limits = c(min,max)

probability_plot = final_wealth %>% 
  # Create a ggplot
  ggplot(aes(y = wealth_T)) + 
  # Add a density histogram
  geom_histogram(aes(x = after_stat(density)), fill = "white", col = 'gray30') +
  # Add density plot
  geom_density() +
  # Add horizontal line at the initial wealth
  geom_hline(yintercept = 3, col = 'red', linetype = 'dashed', linewidth = 1) +
  # Modify y-axis position and limits
  scale_y_continuous(position = "right", limits = limits) + 
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
  ggplot(aes(x = id, y = sort(wealth_T))) +
  # Add points
  geom_point() +
  # Modify y-axis limits
  scale_y_continuous(limits = limits) + 
  # Add labels
  labs(x = 'Individuals (sorted)') +
  # Add horizontal line at the initial wealth
  geom_hline(yintercept = 3, col = 'red', linetype = 'dashed', linewidth = 1) +
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
tax_plot = probability_plot + individuals_plot + 
  plot_annotation(
    title = "Wealth Distribution with Consumption, Income Tax and Stochastic Income",
    caption = 'Based on Yang & Zhou (2022)'
  )

print(tax_plot)

# Lorenz curve
final_wealth = final_wealth %>% 
  # Order by wealth
  arrange(wealth_T) %>% 
  mutate(
    # Normalize wealth
    wealth_i = wealth_T,
    # Calculate share of wealth
    share_of_wealth = wealth_i/sum(wealth_i),
    # Calculate cumulative share of wealth (Lorez curve)
    wealth_distribution = cumsum(wealth_i)/ sum(wealth_i),
    # Calculate perfect equality shate
    perfect_equality_share = 1/num_agents,
    # Calculate perfect equality line
    perfect_equality = cumsum(perfect_equality_share),
    # Get de difference between the ideal scenario and actual wealth distribution
    difference = abs(perfect_equality - wealth_distribution),
    # Add base model data
    base_model = base_model
  ) 

# Calculate gini index
tax_gini_index = final_wealth %>% 
  reframe(gini =  2 * mean(difference)) %>% 
  pull(gini)

print(tax_gini_index)

# Lorenz curve
tax_lorenz_plot = ggplot(final_wealth, aes(x = perfect_equality)) + 
  # Add line with text
  geom_textline(
    label = "Base Model", aes(y = base_model), 
    vjust = 1.5, linewidth = 1, color = "red", linetype = 'dashed'
  ) +
  # Add line with text
  geom_textline(
    label = "Tax Model", aes(y = wealth_distribution), 
    vjust = 1.5, linewidth = 1, color = "blue",
  ) +
  geom_textline(
    label = "Perfect equality", aes(y = perfect_equality), 
    vjust = -0.75, linewidth = 1,
  ) +
  # Add percents to ticks labels
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  # Add labs
  labs(
    x = 'Cumulative Population',
    y = 'Cumulative Wealth',
    title = "Tax Model - Wealth Distribution with Stochastic Income",
    subtitle = paste('Gini Index:', round(tax_gini_index, 2)),
    caption = 'Based on Yang & Zhou (2022)'
  )

print(tax_lorenz_plot)

# Transform the time series to long format
wealth_series = agents %>% 
  # Select all except wealth_T
  select(-wealth_T) %>% 
  # Change to long format keeping the id column intact
  gather(time, wealth, - id) %>% 
  # Remove the text from the time column
  mutate(time = as.numeric(str_remove(time, 'wealth_')))

# Plot wealth distribution
tax_model_time_plot = wealth_series %>% 
  # Filter periods
  filter(time %in% c(300, 1000)) %>% 
  ggplot(aes(y = wealth)) +
  # Add histogram
  geom_histogram(aes(x = after_stat(density)), colour = "gray20", fill = "white") +
  # Modify y-axis position and limits
  scale_y_continuous(position = "right", limits = limits) + 
  # reverse x-axis
  scale_x_reverse() +
  # Add labels
  labs(
    x = 'Probability',
    title = "Progressive Tax Model",
    caption = 'Based on Yang & Zhou (2022)'
  )  +
  # Facet by period
  facet_wrap(~time, nrow = 1, scales = 'free_x') +
  # Modify theme
  theme(
    axis.title.y = element_blank()
  )

print(tax_model_time_plot)

# Mobility Matrix
tax_model_mobility = wealth_series %>% 
  # Keep period 300 (age = 30's) and 1000
  filter(time %in% c(300, 1000)) %>% 
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
  pivot_wider(names_from = quintile_end, values_from = n) %>% 
  # fill nas
  mutate_if(is.numeric, coalesce, 0) 

print(tax_model_mobility)


# Economic Growth ---------------------------------------------------------
# Still building on the baseline experiment, we now allow for sustained 
# economic growth in output (𝑌) with an annual rate of r. This experiment is 
# to see the effect of the aggregate-level factor (exogenous economic growth).

# Set number of agents  
num_agents = 1000

# Create agents with initial conditions
agents = tibble(
  # Define a unique id
  id = 1:num_agents,
  # Set initial wealth
  wealth_0 = 3,
  # Set the iterative wealth
  wealth_T = 3
)
# Set number of periods
num_periods = 1000

# Set coconut_handout
allowance = 1
# Economy's production (Y)
production = num_agents * 1
# Autonomous consumption (c)
subsistence_level = 0.4
# Marginal propensity to consume pmc
marginal_propensity = 0.6
# Production growth rate
growth_rate = 0.08

# Initialize a progress bar
bar = progress_bar$new(total = num_periods)
for(t in 1:num_periods){
  cumulative_growth = (1+growth_rate)^(t-1)
  subsistence_level_t = subsistence_level * cumulative_growth
  production_t = production * cumulative_growth
  # Number of agents who need subsidy
  subsidies = sum(pull(agents, wealth_T) < subsistence_level_t) * subsistence_level_t
  
  # Create a gains tibble
  gains  = tibble(
    # Pick a random sample with replacement of size = num_agents
    id = sample(x = pull(agents, id), size = num_agents, replace = TRUE)
  )  %>%  
    # Count the number of times each id received a coconut
    count(id, name = 'income') %>% 
    mutate(income = income * (production_t - subsidies) / num_agents)
  
  
  agents = agents %>% 
    # Join the gains
    left_join(gains, by = 'id') %>% 
    mutate(
      # Round 1: Subsidy the poor
      subsidy = ifelse(wealth_T < subsistence_level_t, subsistence_level_t, 0),
      # Round 2: Distribute the rest of production
      income = coalesce(income, 0),
      # Total income
      total_income = subsidy + income,
      # Round 4
      # Keynesian consumption
      consumption = ifelse(
        # If current wealth is less than the consumption capabilities
        wealth_T + total_income < subsistence_level_t + marginal_propensity * income,
        # Exhaust wealth
        wealth_T + total_income,
        # Else consume
        subsistence_level_t + marginal_propensity * income 
      ),
      # Update wealth
      wealth_T = wealth_T + total_income - consumption,
      # Store the wealth history for this period
      "wealth_{t}" := wealth_T,
      # Delete columns
      subsidy = NULL,
      income = NULL,
      consumption = NULL,
      total_income = NULL
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
  # Create a ggplot
  ggplot(aes(y = wealth_T)) + 
  # Add a density histogram
  geom_histogram(aes(x = after_stat(density)), fill = "white", col = 'gray30') +
  # Add density plot
  geom_density() +
  # Add horizontal line at the initial wealth
  geom_hline(yintercept = 3, col = 'red', linetype = 'dashed', linewidth = 1) +
  # Modify y-axis position and limits
  scale_y_continuous(position = "right", limits = limits) + 
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
  ggplot(aes(x = id, y = sort(wealth_T))) +
  # Add points
  geom_point() +
  # Modify y-axis limits
  scale_y_continuous(limits = limits) + 
  # Add labels
  labs(x = 'Individuals (sorted)') +
  # Add horizontal line at the initial wealth
  geom_hline(yintercept = 3, col = 'red', linetype = 'dashed', linewidth = 1) +
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
growth_plot = probability_plot + individuals_plot + 
  plot_annotation(
    title = "Exonomic Growth Model - Wealth Distribution with Stochastic Income",
    caption = 'Based on Yang & Zhou (2022)'
  )

print(growth_plot)

# Lorenz curve
final_wealth = final_wealth %>% 
  # Order by wealth
  arrange(wealth_T) %>% 
  mutate(
    # Normalize wealth
    wealth_i = wealth_T,
    # Calculate share of wealth
    share_of_wealth = wealth_i/sum(wealth_i),
    # Calculate cumulative share of wealth (Lorenz curve)
    wealth_distribution = cumsum(wealth_i)/ sum(wealth_i),
    # Calculate perfect equality share
    perfect_equality_share = 1/num_agents,
    # Calculate perfect equality line
    perfect_equality = cumsum(perfect_equality_share),
    # Get de difference between the ideal scenario and actual wealth distribution
    difference = abs(perfect_equality - share_of_wealth)
  ) 

# Calculate gini index
growth_gini_index = final_wealth %>% 
  reframe(gini =  mean(difference)) %>% 
  pull(gini)

print(growth_gini_index)

# Lorenz curve
growth_lorenz_plot = ggplot(final_wealth, aes(x = perfect_equality)) + 
  # Add line with text
  geom_textline(
    label = "Base Model", aes(y = base_model), 
    vjust = 1.5, linewidth = 1, color = "red", linetype = 'dashed'
  ) +
  # Add line with text
  geom_textline(
    label = "Growth Model", aes(y = wealth_distribution), 
    vjust = 1.5, linewidth = 1, color = "blue",
  ) +
  geom_textline(
    label = "Perfect equality", aes(y = perfect_equality), 
    vjust = -0.75, linewidth = 1,
  ) +
  # Add percents to ticks labels
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  # Add labs
  labs(
    x = 'Cumulative Population',
    y = 'Cumulative Wealth',
    title = "Base Model - Wealth Distribution with Stochastic Income",
    subtitle = paste('Gini Index:', round(growth_gini_index, 2)),
    caption = 'Based on Yang & Zhou (2022)'
  )

print(growth_lorenz_plot)

# Transform the time series to long format
wealth_series = agents %>% 
  # Select all except wealth_T
  select(-wealth_T) %>% 
  # Change to long format keeping the id column intact
  gather(time, wealth, - id) %>% 
  # Remove the text from the time column
  mutate(time = as.numeric(str_remove(time, 'wealth_')))

# Plot wealth distribution
growth_model_time_plot = wealth_series %>% 
  # Filter periods
  filter(time %in% c(300, 1000)) %>% 
  ggplot(aes(y = wealth)) +
  # Add histogram
  geom_histogram(aes(x = after_stat(density)), colour = "gray20", fill = "white") +
  # Modify y-axis position and limits
  scale_y_continuous(position = "right", limits = limits) + 
  # reverse x-axis
  scale_x_reverse() +
  # Add labels
  labs(
    x = 'Probability',
    title = "Economic Growth Model - Wealth Distribution with Stochastic Income",
    caption = 'Based on Yang & Zhou (2022)'
  )  +
  # Facet by period
  facet_wrap(~time, nrow = 1, scales = 'free_x') +
  # Modify theme
  theme(
    axis.title.y = element_blank()
  )

print(growth_model_time_plot)

# Mobility Matrix
growth_model_mobility = wealth_series %>% 
  # Keep period 300 (age = 30's) and 1000
  filter(time %in% c(300, 1000)) %>% 
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
  pivot_wider(names_from = quintile_end, values_from = n) %>% 
  # fill nas
  mutate_if(is.numeric, coalesce, 0) 

print(growth_model_mobility)


# Human Capital -----------------------------------------------------------
# Now add labor-related endowment or human capital to the baseline experiment. 
# Residents are still endowed with the same initial wealth, but with different 
# diligence and intelligence. 
# Human capital endowment h follows a normal distribution, which determines the 
# probability of obtaining the earnings in the form of a logit function
#
# Pr(εit =1)= exp(β0 + β1*h)/ 1+exp(β0 + β1*h)  ,where h∼N(0,σ) 
#
# In the logit function, β0 is equal to ln(1/(N-1)) such that it becomes the 
# baseline case when everyone has zero human capital (h = 0), and β1 is the 
# return on human capital. The dispersion of the wealth distribution depends 
# on the dispersion of human capital σh (set as 10%).

# Set number of agents  
num_agents = 1000

# Set Human Capital parameters
# Dispersion of human capital
sigma_h = 0.1
# Baseline
beta_0 = log(1/(num_agents-1))
# Return on human capital
beta_1  = 1

# Create agents with initial conditions
agents = tibble(
  # Define a unique id
  id = 1:num_agents,
  # Creates probabilities determined by Human Capital
  effort = rnorm(n = num_agents, mean = 0, sd = sigma_h),
  raw_probability = 1 - 1/(1 + exp(beta_0 + beta_1 * effort)),
  human_capital_probability = raw_probability/sum(raw_probability),
  # Set initial wealth
  wealth_0 = 3,
  # Set the iterative wealth
  wealth_T = 3
)
# Set number of periods
num_periods = 1000

# Set coconut_handout
allowance = 1
# Economy's production (Y)
production = num_agents * allowance
# Autonomous consumption (c)
subsistence_level = 0.4
# Marginal propensity to consume pmc
marginal_propensity = 0.6

# Initialize a progress bar
bar = progress_bar$new(total = num_periods)
# Iterate over each period
for(t in 1:num_periods){
  # Number of agents who need subsidy
  subsidies = sum(pull(agents, wealth_T) < subsistence_level) * subsistence_level
  
  # Create a gains tibble
  gains  = tibble(
    # Pick a random sample with replacement of size = num_agents
    id = sample(x = pull(agents, id), size = num_agents, replace = TRUE, 
                prob = pull(agents, human_capital_probability))
  )  %>%  
    # Count the number of times each id received a coconut
    count(id, name = 'income') %>% 
    mutate(income = income * (production - subsidies) / num_agents)
  
  
  agents = agents %>% 
    # Join the gains
    left_join(gains, by = 'id') %>% 
    mutate(
      # Round 1: Subsidy the poor
      subsidy = ifelse(wealth_T < subsistence_level, subsistence_level, 0),
      # Round 2: Distribute the rest of production
      income = coalesce(income, 0),
      # Total income
      total_income = subsidy + income,
      # Keynesian consumption
      consumption = ifelse(
        # If current wealth is less than the consumption capabilities
        wealth_T + total_income < subsistence_level + marginal_propensity * income,
        # Exhaust wealth
        wealth_T + total_income,
        # Else consume
        subsistence_level + marginal_propensity * income
      ),
      # Update wealth
      wealth_T = wealth_T + total_income - consumption,
      # Store the wealth history for this period
      "wealth_{t}" := wealth_T,
      # Delete columns
      subsidy = NULL,
      income = NULL,
      consumption = NULL,
      total_income = NULL
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
  # Create a ggplot
  ggplot(aes(y = wealth_T)) + 
  # Add a density histogram
  geom_histogram(aes(x = after_stat(density)), fill = "white", col = 'gray30') +
  # Add density plot
  geom_density() +
  # Add horizontal line at the initial wealth
  geom_hline(yintercept = 3, col = 'red', linetype = 'dashed', linewidth = 1) +
  # Modify y-axis position and limits
  scale_y_continuous(position = "right", limits = limits) + 
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
  ggplot(aes(x = id, y = sort(wealth_T))) +
  # Add points
  geom_point() +
  # Modify y-axis limits
  scale_y_continuous(limits = limits) + 
  # Add labels
  labs(x = 'Individuals (sorted)') +
  # Add horizontal line at the initial wealth
  geom_hline(yintercept = 3, col = 'red', linetype = 'dashed', linewidth = 1) +
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
human_capital_plot = probability_plot + individuals_plot + 
  plot_annotation(
    title = "Human Capital Model - Wealth Distribution with Stochastic Income",
    caption = 'Based on Yang & Zhou (2022)'
  )

print(human_capital_plot)

# Lorenz curve
final_wealth = final_wealth %>% 
  # Order by wealth
  arrange(wealth_T) %>% 
  mutate(
    # Normalize wealth
    wealth_i = wealth_T,
    # Calculate share of wealth
    share_of_wealth = wealth_i/sum(wealth_i),
    # Calculate cumulative share of wealth (Lorenz curve)
    wealth_distribution = cumsum(wealth_i)/ sum(wealth_i),
    # Calculate perfect equality share
    perfect_equality_share = 1/num_agents,
    # Calculate perfect equality line
    perfect_equality = cumsum(perfect_equality_share),
    # Get de difference between the ideal scenario and actual wealth distribution
    difference = abs(perfect_equality - wealth_distribution)
  ) 

# Calculate gini index
human_capital_gini_index = final_wealth %>% 
  reframe(gini =  2 * mean(difference)) %>% 
  pull(gini)

print(human_capital_gini_index)

# Lorenz curve
human_capital_lorenz_plot = ggplot(final_wealth, aes(x = perfect_equality)) + 
  # Add line with text
  geom_textline(
    label = "Base Model", aes(y = base_model), 
    vjust = 1.5, linewidth = 1, color = "red", linetype = 'dashed'
  ) +
  # Add line with text
  geom_textline(
    label = "Human Capital Model", aes(y = wealth_distribution), 
    vjust = 1.5, linewidth = 1, color = "blue",
  ) +
  geom_textline(
    label = "Perfect equality", aes(y = perfect_equality), 
    vjust = -0.75, linewidth = 1,
  ) +
  # Add percents to ticks labels
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  # Add labs
  labs(
    x = 'Cumulative Population',
    y = 'Cumulative Wealth',
    title = "Human Capital Model - Wealth Distribution with Stochastic Income",
    subtitle = paste('Gini Index:', round(human_capital_gini_index, 2)),
    caption = 'Based on Yang & Zhou (2022)'
  )

print(human_capital_lorenz_plot)

# Transform the time series to long format
wealth_series = agents %>% 
  # Select all except wealth_T
  select(-wealth_T) %>% 
  # Change to long format keeping the id column intact
  gather(time, wealth, - id) %>% 
  # Remove the text from the time column
  mutate(time = as.numeric(str_remove(time, 'wealth_')))

# Plot wealth distribution
human_capital_model_time_plot = wealth_series %>% 
  # Filter periods
  filter(time %in% c(300, 1000)) %>% 
  ggplot(aes(y = wealth)) +
  # Add histogram
  geom_histogram(aes(x = after_stat(density)), colour = "gray20", fill = "white") +
  # Modify y-axis position and limits
  scale_y_continuous(position = "right") + 
  # reverse x-axis
  scale_x_reverse() +
  # Add labels
  labs(
    x = 'Probability',
    title = "Base Model - Wealth Distribution with Stochastic Income",
    caption = 'Based on Yang & Zhou (2022)'
  )  +
  # Facet by period
  facet_wrap(~time, nrow = 1, scales = 'free_x') +
  # Modify theme
  theme(
    axis.title.y = element_blank()
  )

print(human_capital_model_time_plot)

# Mobility Matrix
human_capital_model_mobility = wealth_series %>% 
  # Keep period 300 (age = 30's) and 1000
  filter(time %in% c(300, 1000)) %>% 
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
  pivot_wider(names_from = quintile_end, values_from = n) %>% 
  # fill nas
  mutate_if(is.numeric, coalesce, 0) 

print(human_capital_model_mobility)

# Physical Capital --------------------------------------------------------
# After consumption, an agent can invest the rest of her wealth in the capital 
# market or return in the next period. To emulate reality, the proportion of 
# her capital/wealth in the capital  market determines the probability of
# obtaining new income.

# Set number of agents  
num_agents = 1000

# Create agents with initial conditions
agents = tibble(
  # Define a unique id
  id = 1:num_agents,
  # Set initial wealth
  wealth_0 = 3,
  # Set the iterative wealth
  wealth_T = 3
)
# Set number of periods
num_periods = 1000

# Set coconut_handout
allowance = 1
# Economy's production (Y)
production = num_agents * 1
# Autonomous consumption (c)
subsistence_level = 0.4
# Marginal propensity to consume pmc
marginal_propensity = 0.6

# Initialize a progress bar
bar = progress_bar$new(total = num_periods)
# Iterate over each period
for(t in 1:num_periods){
  # Creates probabilities determined by Physical Capital Investment
  invesment_share = pull(agents, wealth_T)/ sum(pull(agents, wealth_T))
  
  # Number of agents who need subsidy
  subsidies = sum(pull(agents, wealth_T) < subsistence_level) * subsistence_level
  
  # Create a gains tibble
  gains  = tibble(
    # Pick a random sample with replacement of size = num_agents
    id = sample(x = pull(agents, id), size = num_agents, replace = TRUE, 
                prob = invesment_share)
  )  %>%  
    # Count the number of times each id received a coconut
    count(id, name = 'income') %>% 
    mutate(income = income * (production - subsidies) / num_agents)
  
  
  agents = agents %>% 
    # Join the gains
    left_join(gains, by = 'id') %>% 
    mutate(
      # Round 1: Subsidy the poor
      subsidy = ifelse(wealth_T < subsistence_level, subsistence_level, 0),
      # Round 2: Distribute the rest of production
      income = coalesce(income, 0),
      # Total income
      total_income = subsidy + income,
      # Keynesian consumption
      consumption = ifelse(
        # If current wealth is less than the consumption capabilities
        wealth_T + total_income < subsistence_level + marginal_propensity * income,
        # Exhaust wealth
        wealth_T + total_income,
        # Else consume
        subsistence_level + marginal_propensity * income
      ),
      # Update wealth
      wealth_T = wealth_T + total_income - consumption,
      # Store the wealth history for this period
      "wealth_{t}" := wealth_T,
      # Delete columns
      subsidy = NULL,
      income = NULL,
      consumption = NULL,
      total_income = NULL
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
  # Create a ggplot
  ggplot(aes(y = wealth_T)) + 
  # Add a density histogram
  geom_histogram(aes(x = after_stat(density)), fill = "white", col = 'gray30') +
  # Add density plot
  geom_density() +
  # Add horizontal line at the initial wealth
  geom_hline(yintercept = 3, col = 'red', linetype = 'dashed', linewidth = 1) +
  # Modify y-axis position and limits
  scale_y_continuous(position = "right", limits = limits) + 
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
  ggplot(aes(x = id, y = sort(wealth_T))) +
  # Add points
  geom_point() +
  # Modify y-axis limits
  scale_y_continuous(limits = limits) + 
  # Add labels
  labs(x = 'Individuals (sorted)') +
  # Add horizontal line at the initial wealth
  geom_hline(yintercept = 3, col = 'red', linetype = 'dashed', linewidth = 1) +
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
physical_capital_plot = probability_plot + individuals_plot + 
  plot_annotation(
    title = "physical Capital Model - Wealth Distribution with Stochastic Income",
    caption = 'Based on Yang & Zhou (2022)'
  )

print(physical_capital_plot)

# Lorenz curve
final_wealth = final_wealth %>% 
  # Order by wealth
  arrange(wealth_T) %>% 
  mutate(
    # Normalize wealth
    wealth_i = wealth_T,
    # Calculate share of wealth
    share_of_wealth = wealth_i/sum(wealth_i),
    # Calculate cumulative share of wealth (Lorenz curve)
    wealth_distribution = cumsum(wealth_i)/ sum(wealth_i),
    # Calculate perfect equality share
    perfect_equality_share = 1/num_agents,
    # Calculate perfect equality line
    perfect_equality = cumsum(perfect_equality_share),
    # Get de difference between the ideal scenario and actual wealth distribution
    difference = abs(perfect_equality - wealth_distribution)
  ) 

# Calculate gini index
physical_capital_gini_index = final_wealth %>% 
  reframe(gini =  2 * mean(difference)) %>% 
  pull(gini)

print(physical_capital_gini_index)

# Lorenz curve
physical_capital_lorenz_plot = ggplot(final_wealth, aes(x = perfect_equality)) + 
  # Add line with text
  geom_textline(
    label = "Base Model", aes(y = base_model), 
    vjust = 1.5, linewidth = 1, color = "red", linetype = 'dashed'
  ) +
  # Add line with text
  geom_textline(
    label = "Physical Capital Model", aes(y = wealth_distribution), 
    vjust = 1.5, linewidth = 1, color = "blue",
  ) +
  geom_textline(
    label = "Perfect equality", aes(y = perfect_equality), 
    vjust = -0.75, linewidth = 1,
  ) +
  # Add percents to ticks labels
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  # Add labs
  labs(
    x = 'Cumulative Population',
    y = 'Cumulative Wealth',
    title = "Physical Capital Model - Wealth Distribution with Stochastic Income",
    subtitle = paste('Gini Index:', round(physical_capital_gini_index, 2)),
    caption = 'Based on Yang & Zhou (2022)'
  )

print(physical_capital_lorenz_plot)

# Transform the time series to long format
wealth_series = agents %>% 
  # Select all except wealth_T
  select(-wealth_T) %>% 
  # Change to long format keeping the id column intact
  gather(time, wealth, - id) %>% 
  # Remove the text from the time column
  mutate(time = as.numeric(str_remove(time, 'wealth_')))

# Plot wealth distribution
physical_capital_model_time_plot = wealth_series %>% 
  # Filter periods
  filter(time %in% c(300, 1000)) %>% 
  ggplot(aes(y = wealth)) +
  # Add histogram
  geom_histogram(aes(x = after_stat(density)), colour = "gray20", fill = "white") +
  # Modify y-axis position and limits
  scale_y_continuous(position = "right") + 
  # reverse x-axis
  scale_x_reverse() +
  # Add labels
  labs(
    x = 'Probability',
    title = "Base Model - Wealth Distribution with Stochastic Income",
    caption = 'Based on Yang & Zhou (2022)'
  )  +
  # Facet by period
  facet_wrap(~time, nrow = 1, scales = 'free_x') +
  # Modify theme
  theme(
    axis.title.y = element_blank()
  )

print(physical_capital_model_time_plot)

# Mobility Matrix
physical_capital_model_mobility = wealth_series %>% 
  # Keep period 300 (age = 30's) and 1000
  filter(time %in% c(300, 1000)) %>% 
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
  pivot_wider(names_from = quintile_end, values_from = n) %>% 
  # fill nas
  mutate_if(is.numeric, coalesce, 0) 

print(physical_capital_model_mobility)


(tax_lorenz_plot + growth_lorenz_plot) / (human_capital_lorenz_plot + physical_capital_lorenz_plot) 


