# Cargamos nuestras librerias
library(tidyverse)
library(tidymodels)
library(GGally)

# Cargamos nuestros datos
advertising = read_csv('https://raw.githubusercontent.com/nguyen-toan/ISLR/master/dataset/Advertising.csv')

# Conocer nuestros datos
advertising %>% 
  glimpse()

advertising %>% 
  summary()

ggpairs(advertising)

# Extraemos nuestra columna Sales como variable dependiente
y = advertising %>% 
  pull(Sales) 

# Extraemos y creamos nuestros regresores
x = advertising %>% 
  # Creamos la variable llamada intercepto
  mutate(Intercepto = rep(1,200)) %>% 
  # Seleccionamos intercepto y TV
  select(Intercepto, TV) %>% 
  # Transformamos a una matriz
  as.matrix() 

# Asi luce
print(x)

# Operaciones con matrices

# Transpuesta
print(t(x))

# Multiplicacion
print(t(x) %*% x)
print(t(x) %*% y)

# Inversa
print(solve(t(x) %*% x))

# Solucion OLS
beta = solve(t(x) %*% x) %*% (t(x) %*% y)
print(beta)

# Estimacion OLS
y_hat = x %*% beta

# Operaciones con vectores
variacion_total = sum((y - mean(y))^2)
variacion_explicada = sum((y_hat - mean(y))^2)
residuales = y - y_hat
variacion_no_explicada = sum(residuales^2)
r2 = variacion_explicada/variacion_total
print(r2)
print(cor(y,y_hat) ^ 2)

# Usando lm
modelo = lm(
  formula = Sales ~ TV, 
  data = advertising
  )

# Analizando nuestro modelo
print(modelo)
summary(modelo)
glance(modelo)

# La version corta
library(performance)
check_model(modelo)

# Un modelo mas robusto
modelo = lm(
  formula = Sales ~ TV + Radio , 
  data = advertising
  )

# Revisando nuestro modelo
print(modelo)
summary(modelo)
glance(modelo)
check_model(modelo)


# Un ejemplo de valicion cruzada con gapminder
library(gapminder)

# Cargamos los datos
gapminder = force(gapminder)

# Revisamos nuestros datos
gapminder %>% 
  ggpairs(cardinality_threshold = 142)

# La version base
gapminder %>% 
  pairs()

# Un correlograma
gapminder %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
  ggcorrplot::ggcorrplot(lab = T)

# Analisis Exploratorio de datos
grafica = df %>% 
  ggplot(
    aes(
      x = x,
      y = y
    )
  ) 

# Puntos
grafica +
  geom_point()

# Puntos con ruido
grafica +
  geom_jitter()

# Cajas y bigotes
grafica +
  geom_boxplot()

# Regresiones
gapminder %>% 
  ggplot(
    aes(
      x = gdpPercap,
      y = lifeExp
    )
  ) +
  geom_point() +
  stat_smooth(aes(col = 'Lineal'), method = 'lm') +
  stat_smooth(aes(col = 'Polynomial'), method = 'lm', formula = y ~poly(x,2)) +
  stat_smooth(aes(col = 'Logaritmo'), method = 'lm', formula = y ~log(x))

# Cambios en los ejes
gapminder %>% 
  ggplot(
    aes(
      x = gdpPercap,
      y = lifeExp,
      col = country
    )
  ) +
  geom_point(alpha = 0.25) +
  stat_smooth(method = 'lm', formula = y ~x, se = F)+
  scale_x_log10() +
  theme(legend.position = 'none')


# Cajas y bigotes ordenado
gapminder %>% 
  ggplot(
    aes(
      x = lifeExp,
      y = reorder(country,lifeExp)
    )
  ) +
  geom_boxplot()

# Crear un modelo
modelo = lm(
  formula = lifeExp ~ year + log(gdpPercap), 
  data = gapminder
  )
summary(modelo)
glance(modelo)
check_model(modelo)

# Predicciones simples
datos_nuevos = tibble(
  year = 2020,
  gdpPercap = 8346.70,
  country = 'Mexico')

actual = 75.23

predict(modelo, newdata = datos_nuevos)

# Fijar semillas aleatorias
set.seed(130)

# Separar en prueba y entrenamiento
gapminder_split = initial_split(
  data = gapminder,
  prop = 0.75,
  strata = lifeExp,
)

# Extraemos nuestro conjunto de entrenamiento
entrenamiento = gapminder_split %>% 
  training()

# Extraemos nuestro conjunto de prueba
prueba = gapminder_split %>% 
  testing()

# Podemos compararlos y ver que no son iguales
entrenamiento %>% 
  summary()

prueba %>% 
  summary()

# The tidy way for tidymodels
# Creamos un objeto de regresion lineal
regresion_lineal = linear_reg() %>% 
  # Usaremos lm como motor
  set_engine('lm') %>% 
  # Queremos hacer una regression
  set_mode('regression')

# Ajustamos nuestra regresion
ajuste = regresion_lineal %>%  
  fit(
    formula = lifeExp ~ year + log(gdpPercap) , 
    data = entrenamiento)

ajuste %>% 
  extract_fit_engine() %>% 
  summary()

ajuste %>% 
  extract_fit_engine() %>% 
  glance()

check_model(ajuste$fit)

ajuste %>% 
  augment(new_data = entrenamiento) %>% 
  yardstick::rmse(truth = lifeExp, estimate = .pred)

predicciones = ajuste %>% 
  predict(new_data = prueba)

resultados = ajuste %>% 
  augment(new_data = prueba) 

resultados %>% 
  yardstick::rmse(truth = lifeExp, estimate = .pred)

maxl = 0
# Create an R squared plot of model performance
ggplot(resultados, aes(x = lifeExp, y = .pred)) +
  geom_point(alpha = 0.5) + 
  geom_abline(color = 'blue', linetype = 2) +
  coord_obs_pred() +
  labs(x = 'Expectativa de Vida', y = 'Prediccion')

ajuste %>% 
  predict(new_data = datos_nuevos)

