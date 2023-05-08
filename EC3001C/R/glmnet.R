# Fija el directorio de trabajo
setwd("~/Desktop/ITESM/Cursos/Eco Aplicada y Ciencia de Datos/")


# Librerías ---------------------------------------------------------------
# Instalar - Cargar tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}

# Instalar - Cargar tidymodels                                                       
if(require(tidymodels) == FALSE){                                                
  install.packages('tidymodels')                                                 
  library(tidymodels)                                                            
}else{                                                                          
  library(tidymodels)                                                            
}

# Instalar - Cargar lubridate                                                       
if(require(lubridate) == FALSE){                                                
  install.packages('lubridate')                                                 
  library(lubridate)                                                            
}else{                                                                          
  library(lubridate)                                                            
}

# Instalar - Cargar poissonreg                                                       
if(require(poissonreg) == FALSE){                                                
  install.packages('poissonreg')                                                 
  library(poissonreg)                                                            
}else{                                                                          
  library(poissonreg)                                                            
}

# Datos -------------------------------------------------------------------
# Para este ejercicio utilizaremos los datos de
# https://www.kaggle.com/datasets/fedesoriano/traffic-prediction-dataset
# Gracias @fedesoriano por los datos :)

# Carga los datos disponibles en 
traffic = read_csv('datasets/traffic.csv') %>% 
  # Agrega nuevas variables
  mutate(
    year  = year(DateTime),                  # Año de la observación
    month = month(DateTime),                 # Mes de la observación
    week_day = wday(DateTime, label = TRUE), # Día de la semana
    hour  = hour(DateTime)                   # Hora de la observación
  ) %>% 
  # Limpias los nombres
  janitor::clean_names() %>% 
  filter(junction == 1)

# Imprime la estructura del dataset
str(traffic) 

# Imprime un resumen de las variables
summary(traffic)

# Imprime un gráfico de las variables
traffic %>% 
  ggplot( 
    aes(
      # En el eje x coloca la fecha
      x = date_time, 
      # En el eje y el total de vehículos
      y = vehicles)
    ) +
  # Agrega una línea
  geom_line() +
  # Agrega títulos
  ggtitle('Tránsito vehicular por hora en las intersecciones viales',
          subtitle = 'Número de vehículos') +
  #  Usa un tema predefinido
  theme_bw() +
  # Haz modificaciones
  theme(
    # Cambia los textos
    text = element_text(family = 'Bebas Neue'),
    # Cambia los títulos
    axis.title = element_blank(),
  )

# Las semillas aleatorias nos ayudan a fijar la aleatoriedad
set.seed(35)
# Separa los datos en dos conjuntos
datos_split = traffic %>% 
  initial_time_split(
    strata = vehicles,
    prop = 0.75
  )

# Extrae el conjunto de entrenamiento
entrenamiento = training(datos_split) 

# Extrae el conjunto de prueba
prueba = testing(datos_split)

# Observa cómo son diferentes
summary(entrenamiento)
summary(prueba)

# Construye las muestras de validación cruzada (temporal) con los datos de entrenamiento
validacion_cruzada = rolling_origin(
  entrenamiento,
  initial = 150 * 24, # La primer muestra es de 6 meses por 24 horas
  assess = 30 * 24, # Evaluaremos cada 24 horas,
  cumulative = FALSE, # No queremos elementos acumulativos,
  skip = 30 * 24
)

# Crea una una receta
receta = recipe(
  # Define la ecuación
  formula = vehicles ~ year + month + hour + week_day,
  # Define los datos a utilizar
  data = entrenamiento
) %>% 
  # Crear dummies
  step_dummy(week_day, one_hot = TRUE) %>% 
  # Quitar una dummy para evitar la trampa
  step_mutate(week_day_1 = NULL)

# Regresión Gaussiana -----------------------------------------------------
# Define el modelo de ajuste
regresion_gaussiana = linear_reg() %>% 
  # Selecciona el motor a utilizar
  set_engine('glmnet') %>% 
  # Fija el problema a resolver
  set_mode('regression') %>%  
  # Fija los hiperparametros
  set_args(penalty = tune(), mixture = tune())

receta_gaussiana = receta %>% 
  # Agrega un paso para transformar a logaritmos
  step_log(vehicles)

# Crea un flujo de trabajo
gaussiana_workflow = workflow() %>%
  # Añade una receta
  add_recipe(receta_gaussiana) %>% 
  # Agrega un modelo
  add_model(regresion_gaussiana) 

# Ajusta con flujo de trabajo 
# Get parameters to tune
glmnet_parametros = gaussiana_workflow %>% 
  extract_parameter_set_dials() 

# Crear una maya de calibración
set.seed(123)
gaussiana_tuning = tune_grid(
  # Define el objeto a utilizar
  object = gaussiana_workflow,
  # Método de remuestreo
  resamples = validacion_cruzada,
  # Parametros a calibrar
  param_info = glmnet_parametros,
  # Métricas de evaluación
  metrics = metric_set(yardstick::rmse),
  # Parámetros de control
  control = control_grid(verbose = TRUE),
  # Número de combinaciones a probar
  grid = 100
)

# Recolectar métricas
gaussiana_metricas = gaussiana_tuning %>%
  collect_metrics() %>% 
  # Ordenar por promedios
  arrange(mean)

# Gráfica de calibración
gaussiana_metricas %>% 
  # Penalización vs error
  ggplot(aes(x = mixture, y = mean)) +
  # Geometría del error estandar
  geom_errorbar(
    aes(
      ymin = mean - std_err,
      ymax = mean + std_err
      ),
    alpha = 0.25
  ) +
  # Geometría de línea
  geom_line(size = 0.5) 

# Seleccionar el mejor modelo
best_gaussiana = gaussiana_tuning %>%
  select_best("rmse")

# Imprime el mejor modelo
print(best_gaussiana)

# Finalizar el flujo de trabajo
final_gaussiana = gaussiana_workflow %>% 
  finalize_workflow(best_gaussiana)

# Ejecutar el último ajuste
resultados_gaussiana = final_gaussiana %>% 
  last_fit(
    split = datos_split,
    metrics = metric_set(yardstick::rmse)
  )

# Recolectar las métricas de prueba
metricas_gaussiana = resultados_gaussiana %>% 
  collect_predictions() %>% 
  summarise(
    rmse = sqrt(mean((exp(.pred) - exp(vehicles))^2))
    )

print(metricas_gaussiana)


# Regresión Poisson -------------------------------------------------------
# Define el modelo de ajuste
regresion_poisson = poisson_reg() %>% 
  # Selecciona el motor a utilizar
  set_engine('glmnet') %>% 
  # Fija el problema a resolver
  set_mode('regression') %>%  
  # Fija los hiperparametros
  set_args(penalty = tune(), mixture = tune())

# Crea un flujo de trabajo
poisson_workflow = workflow() %>%
  # Añade una receta
  add_recipe(receta) %>% 
  # Agrega un modelo
  add_model(regresion_poisson) 

# Ajusta con flujo de trabajo 
# Get parameters to tune
glmnet_parametros = poisson_workflow %>% 
  extract_parameter_set_dials() 

# Crear una maya de calibración
set.seed(123)
poisson_tuning = tune_grid(
  # Define el objeto a utilizar
  object = poisson_workflow,
  # Método de remuestreo
  resamples = validacion_cruzada,
  # Parametros a calibrar
  param_info = glmnet_parametros,
  # Métricas de evaluación
  metrics = metric_set(yardstick::rmse),
  # Parámetros de control
  control = control_grid(verbose = TRUE),
  # Número de combinaciones a probar
  grid = 100
)

# Recolectar métricas
poisson_metricas = poisson_tuning %>%
  collect_metrics() %>% 
  # Ordenar por promedios
  arrange(mean)

# Gráfica de calibración
poisson_metricas %>% 
  # Penalización vs error
  ggplot(aes(x = penalty, y = mean)) +
  # Geometría del error estandar
  geom_errorbar(
    aes(
      ymin = mean - std_err,
      ymax = mean + std_err
    ),
    alpha = 0.25
  ) +
  # Geometría de línea
  geom_line(size = 0.5) 

# Seleccionar el mejor modelo
best_poisson = poisson_tuning %>%
  select_best("rmse")

# Imprime el mejor modelo
print(best_poisson)

# Finalizar el flujo de trabajo
final_poisson = poisson_workflow %>% 
  finalize_workflow(best_poisson)

# Ejecutar el último ajuste
resultados_poisson = final_poisson %>% 
  last_fit(
    split = datos_split,
    metrics = metric_set(yardstick::rmse)
  )

# Recolectar las métricas de prueba
metricas_poisson = resultados_poisson %>% 
  collect_metrics() 

print(metricas_poisson)


# Working with the trained model ------------------------------------------
# Extract the trained model
glmnet_trained = resultados_poisson %>% 
  extract_fit_parsnip() 

print(glmnet_trained)

coefficients = tidy(glmnet_trained)

preproc = receta %>% 
  prep() %>% 
  bake(new_data = traffic)

# Make predictions
predictions_tb =  augment(glmnet_trained, preproc)

predictions_tb %>% 
  ggplot(aes(x = vehicles, y = .pred)) +
  geom_point() +
  geom_abline(slope = 1, col = 'red') +
  coord_obs_pred()  


save(glmnet_trained, file="models/mymodel.RData")

# Reload model
load("models/mymodel.RData")
