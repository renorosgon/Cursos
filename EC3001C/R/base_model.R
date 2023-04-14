# Description -------------------------------------------------------------

# Assume that there are 𝑌 = 1000 units of outputs 
# produced in every period and each resident (endowed with the same initial 
# wealth 𝑤0 = 3) has a Keynesian consumption function:
#  𝑐_𝑡 = 𝑐̅+ 𝛼 × 𝑦_𝑡
# where 𝑐̅= 0.4 is the subsistence level of consumption, 𝛼 = 0.6 is the
# marginal propensity to consume, and 𝑦𝑡 is the total earnings distributed 
# to the agent. Those whose wealth falls below 𝑐̅in the beginning of each
# period will get 𝑐̅from the government, but they still have an equal
# chance to be distributed with new earnings.

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

base_model <- function(
    # Set number of agents 
    num_agents = 1000, 
    # Set initial wealth
    initial_wealth = 3,
    # Autonomous consumption (c)
    subsistence_level = 0.4, 
    # Marginal propensity to consume pmc
    marginal_propensity = 0.6, 
    # Set number of periods
    num_periods = 1000,
    # Economy's production (Y)
    production = 1000
    ) {
# Model -------------------------------------------------------------------
  # Create agents with initial conditions
  agents = tibble(
    # Define a unique id
    id = 1:num_agents,
    # Set initial wealth
    wealth_0 = initial_wealth,
    # Set the iterative wealth
    wealth_T = wealth_0
  )
 
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
  return(agents)
}
