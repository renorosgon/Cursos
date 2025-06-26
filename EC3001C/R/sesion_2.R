# Directorio de trabajo
setwd("~/Desktop/ITESM/Cursos/EC3001C")

# Instalar paqueterias (esto solo se hace la primera vez)
install.packages('tidyverse')

# En cada sesión que yo quiera usar estas funciones
library(tidyverse)

# Leer archivos en R
gapminder = read_csv("data/gapminder.csv")

head(gapminder)

# R base
names(gapminder) = c('country','continent','year','life_expectancy',
                     'population','gdp_percapita')
# Tidyway
gapminder = read_csv("data/gapminder.csv")

gapminder = rename(
  gapminder, 
  life_expectancy = lifeExp,
  population = pop,
  gdp_percapita = gdpPercap
  )

head(gapminder)

# Introdución a pipes
# Windows: [Ctrl + Shift + m] 
# Mac: [Cmnd + Shift + m]

# Leer el archivo de gapminder y después
gapminder = read_csv("data/gapminder.csv") %>% 
  # Renombrar las columnas
  rename(
    life_expectancy = lifeExp,
    population = pop,
    gdp_percapita = gdpPercap
  ) %>% # Y después...
  # Mutar mi data frame
  mutate(
    country = factor(country),
    continent = factor(continent)
  )

# Sin encadenar funciones
# Leer
gapminder = read_csv("data/gapminder.csv") 
# Renombrar las columnas
gapminder = rename(
  .data = gapminder,
    life_expectancy = lifeExp,
    population = pop,
    gdp_percapita = gdpPercap
  )
# Mutar mi data frame
gapminder = mutate(
    gapminder,
    country = factor(country),
    continent = factor(continent)
  )
# R base
gapminder$country = factor(gapminder$country)
gapminder$continent = factor(gapminder$continent)

head(gapminder)
summary(gapminder)

unique(gapminder$country)

# Filtrado con R base
mexico = gapminder[gapminder$country == 'Mexico', ]
mexico = subset(gapminder, country == 'Mexico')
# Filtrado con Tidyverse
mexico = filter(gapminder, country == 'Mexico')

# Eliminar columnas
mexico$country = NULL
mexico = mutate(mexico, country = NULL)
# R base
mexico = mexico[ ,-1]
# Tidyvverse
mexico = select(mexico, -continent)

select(gapminder, country, year, population, gdp_percapita)

gapminder %>% 
  filter(
    year == 2002,
    #continent == 'Asia' | continent == 'Europe',
    continent %in% c('Asia','Europe'),
    life_expectancy < 60
  ) %>% 
  glimpse()

resumen = gapminder %>% 
  group_by(year, continent) %>% 
  summarise(
    gdp = sum(population * gdp_percapita),
    world_population = sum(population),
    average_life_expectancy = mean(life_expectancy),
    average_gdp_percapita = mean(gdp_percapita),
    gdp_percapita = gdp/world_population
    )

summary(resumen)


gapminder %>% 
  select(life_expectancy, gdp_percapita) %>% 
  cor()

modelo = lm(
  formula = life_expectancy ~ gdp_percapita,
  data = gapminder 
)

summary(modelo)

ggplot(
  gapminder,  
  aes(x = gdp_percapita, y = life_expectancy)
  ) +
  geom_point(aes(col = year, shape = continent)) +
  # stat_smooth(method = 'lm', formula = y ~ poly(x,3))
  stat_smooth(method = 'lm', formula = y ~ log(x))

modelo = lm(
  formula = life_expectancy ~ log(gdp_percapita),
  data = gapminder 
)

summary(modelo)

ggplot(
  gapminder,  
  aes(x = log(gdp_percapita), y = life_expectancy,
      col = continent)
) +
  geom_point(alpha = 0.25) +
  stat_smooth(method = 'lm') +
  scale_color_manual(values = c('darkred','darkblue',
                                'darkgreen','pink',
                                'orange')) +
  facet_wrap(~continent, nrow = 1)
  
# Efectos fijos (distintos interceptos)
modelo1 = lm(
  formula = life_expectancy ~ log(gdp_percapita) + continent, 
  data = gapminder
  )

summary(modelo1)


# Interacciones (distintas pendientes)
modelo2 = lm(
  formula = life_expectancy ~ log(gdp_percapita) : continent, 
  data = gapminder
)

summary(modelo2)

# Todos contra todos
modelo3 = lm(
  formula = life_expectancy ~ log(gdp_percapita) * continent, 
  data = gapminder
)

summary(modelo3)

gini = read_csv('data/inequality_index_gini.csv') %>% 
  gather(key = year, value = gini_index, -country, na.rm = T) %>% 
  mutate(
    year = as.numeric(year),
    country = str_replace(country, pattern = 'USA', replacement = 'United States'),
    country = case_when(
      country == 'UK' ~ 'United Kingdom',
      country == 'Yemen' ~ "Yemen, Rep.",
      country == 'South Korea' ~ 'Korea, Rep.',
      TRUE ~ country
    )
    )

glimpse(gini)
glimpse(gapminder)
#gini = gather(gini, key = year, value = gini, -country)

gapminder %>% 
  left_join(gini, by = c('country','year')) %>% 
  filter(is.na(gini_index)) %>% 
  count(country) %>% 
  filter(n == 12) %>% 
  pull(country)

gapminder$country
pull(gapminder, country)

data = gapminder %>% 
  inner_join(gini, by = c('country','year')) 
  

# SELECT gdp_percapita, gini_index FROM data;
data %>% 
  select(gdp_percapita, gini_index, life_expectancy) %>% 
  cor()

data %>% 
  select(gdp_percapita, gini_index, life_expectancy) %>% 
  pairs()

install.packages('GGally')

data %>% 
  select(gdp_percapita, gini_index, life_expectancy) %>% 
  GGally::ggpairs()


ggplot(data, aes(x = gdp_percapita, y = gini_index)) +
  geom_point() +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2)) +
  facet_wrap(~continent, scales = 'free')

modelo = lm(
  formula = gini_index ~ poly(gdp_percapita, 2),
  data = data
)

summary(modelo)

data = data %>% 
  mutate(prediccion = predict(modelo, data))

ggplot(data, aes(x = gdp_percapita, y = prediccion)) +
  geom_point(aes(col = 'Estimación')) +
  geom_point(aes(y = gini_index, col = 'Observado')) +
  labs(
    x = 'PIB Percápita', 
    y = 'Coeficiente de Gini', 
    col = '',
    title = 'Estimación de la curva de Kuznets',
    caption = 'Fuente: estimación propia con datos de Gapminder\nAutoría:@renorosgon'
    ) +
  theme_bw(base_size = 16, base_family = 'Arial') +
  theme(
    legend.position = 'top'
  )


ggplot(data, aes(x = gdp_percapita, y = prediccion)) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), se = F)+
  geom_point(aes(y = gini_index, col = 'Observado')) +
  labs(
    x = 'PIB Percápita', 
    y = 'Coeficiente de Gini', 
    col = '',
    title = 'Estimación de la curva de Kuznets',
    caption = 'Fuente: estimación propia con datos de Gapminder\nAutoría:@renorosgon'
  ) +
  theme_bw(base_size = 16, base_family = 'Arial') +
  theme(
    legend.position = 'top'
  )




