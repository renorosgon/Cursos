# Librerías
library(tidyverse)
library(progress)

social_mobility_model = function(
    # Las condiciones iniciales
  # Número de periodos
  num_periods = 1000,
  # Numero de agentes
  num_agents = 1000,
  # Riqueza inicial
  inicial_wealth = 3,
  # Consumo autónomo
  subsistence_level = 0.4,
  # Propención marginal a consumir
  marginal_propensity = 0.6,
  # Impuestos
  tax = 0.2,
  # Crecimiento económico
  growth_rate = 0.08,
  # Tributo
  allowance = 1,
  # Producción
  production = num_agents * allowance,
  # Capital humano 
  human_capital = TRUE,
  ### Retornos a la educación
  beta_1 = 1,
  ### Varianza en la inteligencia
  sigma_h = 0.1,
  # Capital físico 
  physical_capital = TRUE,
  # Contribución del trabajo a la producción
  gamma = 0.7
){
  # Crear una tabla de agentes
  agents = tibble(
    # Crear id único
    id = 1:num_agents,
    # Riqueza inicial
    wealth_0 = inicial_wealth,
    # Riqueza en el tiempo T
    wealth_T = wealth_0
  )
  
  # Probabilidad de conseguir empleo dado tu capital humano
  effort = rnorm(n = num_agents, mean = 0, sd = sigma_h) * human_capital
  raw_probability = 1 - 1/(1 + exp(log(1/(num_agents-1)) + beta_1 * effort))
  human_capital_probability = raw_probability/sum(raw_probability)
  
  bar = progress_bar$new(total = num_periods)
  ### Reglas del juego para cada periodo
  for(t in 1:num_periods){
    # Calcular la tasa de crecimiento acumulada
    cumulative_growth = (1+growth_rate)^(t-1)
    # Actualizar el nivel de subsistencia
    subsistence_level_t = subsistence_level * cumulative_growth
    # Actualizar la producción
    production_t = production * cumulative_growth
    # Producción per capita
    production_percapita = production_t/num_agents
    
    # Crear la cuota de inversion
    investment_share = pull(agents, wealth_T)/sum(pull(agents, wealth_T))
    # Fijar la probabilidad de gener riqueza
    human_capital_probabilities = human_capital * human_capital_probability * gamma ^ (human_capital * physical_capital)
    physical_capital_probabilities = physical_capital * investment_share * (1-gamma)^ (human_capital * physical_capital)
    prob = ifelse(
      human_capital_probabilities + physical_capital_probabilities > 0,
      human_capital_probabilities + physical_capital_probabilities,
      1/num_agents
    )
    
    # Subsidios
    subsidies = sum(pull(agents, wealth_T) < subsistence_level_t) * subsistence_level_t
    
    # Creamos nuestra reparticion de riqueza
    gains = tibble(
      id = sample(x = 1:num_agents, size = num_agents, replace = TRUE,  prob = prob)
    ) %>% 
      count(id, name = 'income') %>% 
      mutate(income = income * (production_t - subsidies)/num_agents)
    
    # Actualizar agentes
    agents = agents %>% 
      # Concatenando riqueza
      left_join(gains, by = 'id') %>% 
      # La actualización de la riqueza
      mutate(
        # Round 1: subsidiar a los pobres
        subsidy = ifelse(wealth_T < subsistence_level_t, subsistence_level_t, 0),
        # Round 2: Distribuir la riqueza
        income = coalesce(income, 0),
        # Round 3: Cobrar impuestos
        taxpay = ifelse(
          # Condición
          subsidy + income > allowance * production_percapita,
          # Resultado si TRUE
          (subsidy + income - allowance * production_percapita) * tax,
          # Resulta si FALSE
          0
        ),
        # Ingreso total
        total_income = subsidy + income - taxpay,
        # Round 4: Consumo keynesiano
        consumption = ifelse(
          # Si no le alcanza
          wealth_T + total_income < subsistence_level_t + marginal_propensity * income,
          # Agotar el ingreso
          wealth_T + total_income,
          # Consume con su ingreso disponible
          subsistence_level_t + marginal_propensity * (income - taxpay)
        ),
        # Actualiar riqueza
        wealth_T = wealth_T + total_income - consumption + (sum(taxpay)/num_agents),
        # Guardar el periodo
        "wealth_{t}" := wealth_T,
        # Borrar columnas
        subsidy = NULL,
        income = NULL,
        consumption = NULL,
        total_income = NULL,
        taxpay = NULL
      )
    bar$tick()
  }
  
  wealth_series = agents %>% 
    select(id, wealth_300, wealth_1000) %>% 
    gather(time, wealth, -id) %>% 
    mutate(time = str_remove(time, 'wealth_'))
  
  return(wealth_series)
}