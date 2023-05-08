# This is a function that runs the agent based model simulation from
# Yang, X., & Zhou, P. (2022). Wealth inequality and social mobility: 
# A simulation-based modelling approach. Journal of Economic Behavior 
# & Organization, 196, 307-329.
# The original code and data can be downlodaed from:
# https://www.sciencedirect.com/science/article/pii/S0167268122000580
# This adaptation was don by René Rosado González

social_mobility_model <- function(
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
  production = 1000,
  # Set tax allowance
  allowance = 1,
  # Set progressive tax
  tax = 0.2,
  # Set growth rate
  growth_rate = 0.08,
  # Include human capital?
  human_capital = TRUE,
  # Set human capital dispersion
  sigma_h = 0.1,
  # Set return on human capital
  beta_1 = 1,
  # Include physical capital?
  physical_capital = TRUE,
  # Income share of labor 
  gamma = 0.7
) {
  # Create agents with initial conditions
  agents = tibble(
    # Define a unique id
    id = 1:num_agents,
    # Set initial wealth
    wealth_0 = initial_wealth,
    # Set the iterative wealth
    wealth_T = wealth_0
  )
  
  # Creates probabilities determined by Human Capital
  effort = rnorm(n = num_agents, mean = 0, sd = sigma_h) * human_capital
  raw_probability = 1 - 1/(1 + exp(log(1/(num_agents-1)) + beta_1 * effort))
  human_capital_probability = raw_probability/sum(raw_probability)

  
  # Initialize a progress bar
  bar = progress_bar$new(total = num_periods)
  # Iterate over each period
  for(t in 1:num_periods){
    # Update cumulative growth
    cumulative_growth = (1 + growth_rate)^(t - 1)
    # Update subsistence level
    subsistence_level_t = subsistence_level * cumulative_growth
    # Update production
    production_t = production * cumulative_growth
    # Set production percapita 
    production_percapita = production_t / num_agents
    # Creates probabilities determined by Physical Capital Investment
    investment_share = pull(agents, wealth_T)/ sum(pull(agents, wealth_T))
    # Set probabilities 
    human_capital_probabilities = human_capital * human_capital_probability * gamma ^ (human_capital * physical_capital)
    physical_capital_probabilityes = physical_capital * investment_share * (1-gamma) ^ (human_capital * physical_capital)
    prob = ifelse(human_capital_probability + physical_capital_probabilityes > 0, 
                  human_capital_probability + physical_capital_probabilityes, 1/num_agents)
    # Number of agents who need subsidy
    subsidies = sum(pull(agents, wealth_T) < subsistence_level_t) * subsistence_level_t

    # Create a gains tibble
    gains  = tibble(
      # Pick a random sample with replacement of size = num_agents
      id = sample(x = 1:num_agents, size = num_agents, replace = TRUE, 
                   prob = prob)
    )  %>%  
      # Count the number of times each id received a coconut
      count(id, name = 'income') %>% 
      mutate(income = income * (production_t - subsidies) / num_agents)
    
    # 
    agents = agents %>% 
      # Join the gains
      left_join(gains, by = 'id') %>% 
      mutate(
        # Round 1: Subsidy the poor
        subsidy = ifelse(wealth_T < subsistence_level_t, subsistence_level_t, 0),
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
        # Round 4: Keynesian consumption
        consumption = ifelse(
          # If current wealth is less than the consumption capabilities
          wealth_T + total_income < subsistence_level_t + marginal_propensity * income,
          # Exhaust wealth
          wealth_T + total_income,
          # Else consume
          subsistence_level_t + marginal_propensity * (income - taxpay)
        ),
        # Update wealth
        wealth_T = wealth_T + total_income - consumption + (sum(taxpay)/num_agents),
        # Store the wealth history for this period
        "wealth_{t}" := wealth_T,
        # Delete columns
        subsidy = NULL,
        income = NULL,
        consumption = NULL,
        total_income = NULL,
        taxpay = NULL
      )
    
    # Bar tick
    bar$tick()
  }
  
  wealth_series = agents %>% 
    # Select all except wealth_T
    select(-wealth_T) %>% 
    # Change to long format keeping the id column intact
    gather(time, wealth, - id) %>% 
    # Remove the text from the time column
    mutate(time = as.numeric(str_remove(time, 'wealth_')))
  
  return(wealth_series)
}






