# Fija el directorio de trabajo
setwd("~/Desktop/ITESM/Cursos/EC3001C")


# Librerías ---------------------------------------------------------------
# Instalación y carga de paqueterías
# Instalar - Cargar tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}

# Instalación y carga de paqueterías
# Instalar - Cargar tidymodels                                                       
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
# Para este ejercicio utilizaremos los datos de
# https://www.kaggle.com/datasets/fedesoriano/traffic-prediction-dataset
# Gracias @fedesoriano por los datos :)

# Carga los datos disponibles en 
traffic = read_csv('data/traffic.csv') %>% 
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


# Regression Ridge --------------------------------------------------------
# Regresión de ridge
reg_ridge = poisson_reg() %>% 
  # Define el motor
  set_engine('glmnet') %>% 
  # Define el modo
  set_mode('regression') %>% 
  # Define los hyperparametros
  set_args(penalty = tune(), mixture = 0)

# Qué hace la regularización de ridge
ridge_fit = fit(
  # Modelo  
  reg_ridge,
  # Formula
  vehicles ~ year + month + hour + week_day, 
  # Data
  traffic
  )
# Visualmente
ridge_plot = ridge_fit %>%
  autoplot() +
  # Agrega títulos
  ggtitle('Ridge regularization',
          subtitle = 'Coeffient penalization') +
  #  Usa un tema predefinido
  theme_bw() +
  # Haz modificaciones
  theme(
    # Cambia los textos
    text = element_text(family = 'Avenir Next'),
    # Posición de la leyenda
    legend.position = 'bottom',
    # Quita el título de la leyenda
    legend.title = element_blank()
  )


# Regresión LASSO ---------------------------------------------------------
# Regresión de lasso
reg_lasso = poisson_reg() %>% 
  # Define el motor
  set_engine('glmnet') %>% 
  # Define el modo
  set_mode('regression') %>% 
  # Define los hyperparametros
  set_args(penalty = tune(), mixture = 1)

# Qué hace la regularización de lasso
lasso_fit =fit(
  # Modelo  
  reg_lasso,
  # Formula
  vehicles ~ year + month + hour + week_day, 
  # Data
  traffic
)
# Visualmente
lasso_plot = lasso_fit %>%
  autoplot() +
  # Agrega títulos
  ggtitle('Lasso regularization',
          subtitle = 'Coeffient penalization') +
  #  Usa un tema predefinido
  theme_bw() +
  # Haz modificaciones
  theme(
    # Cambia los textos
    text = element_text(family = 'Avenir Next'),
    # Posición de la leyenda
    legend.position = 'bottom',
    # Quita el título de la leyenda
    legend.title = element_blank(),
    axis.title.y = element_blank()
  ) 

ridge_plot / lasso_plot


# Aprendizaje automático --------------------------------------------------
# Las semillas aleatorias nos ayudan a fijar la aleatoriedad
set.seed(35)
# Separa los datos en dos conjuntos considerando el tiempo
data_split = initial_time_split(traffic, prop = 0.9)

# Extraer el conjunto de prueba y entrenamiento
train = training(data_split) 
test = testing(data_split)

# La diferencia 
summary(train)
summary(test)

# Validacion cruzada intertemporal
rolling_origin = rolling_origin(
  train,
  initial = 180 * 24, # Primeros 180 días por 24 horas
  assess = 60 * 24, # Evaluar el siguiente mes las 24 horas
  cumulative = F, # Acumulado
  skip = 60 * 24 # Saltos de 150 días
)

# Esta función resume cada pliegue del objeto rolling_origin
unfolding_rolling_origin = function(fold_num){
  # Resumen de cada pliegue de entrenamiento
  analysis_set = rolling_origin %>% 
    pluck('splits') %>% 
    pluck(fold_num) %>% 
    analysis() %>% 
    summarize(
      fold = fold_num,
      set = 'Analysis',
      min = min(date_time),
      max = max(date_time)
    )
  # Resumen de cada pliegue de prueba
  assessment_set = rolling_origin %>% 
    pluck('splits') %>% 
    pluck(fold_num) %>% 
    assessment()%>% 
    summarize(
      fold = fold_num,
      set = 'Assessment',
      min = min(date_time),
      max = max(date_time)
    )
  # Concatenar pliegues
  fold = bind_rows(analysis_set, assessment_set)
  # Regresar el priegue
  return(fold)
}

# Aplica la función a cada pliegue
map_df(1:nrow(rolling_origin),unfolding_rolling_origin) %>% 
  # Crea un lienzo
  ggplot(aes(y = factor(fold), col = set)) +
  # Agrega un segemento
  geom_segment(
    aes(x = min, xend = max, yend = factor(fold)),
    linewidth = 5
    ) + 
  # Agrega puntos
  geom_point(aes(x = max))+ 
  geom_point(aes(x = min)) +
  # Modifica los colores
  scale_color_manual(values = c('darkred','orange')) +
  # Edita el eje x
  scale_x_datetime(date_breaks = '2 months', date_labels = "%b-%y") +
  # Agrega etiquetas
  labs(
    title = 'Rolling Origin Explanation',
    subtitle = 'Fold Subset',
    col = 'Set',
    caption = '@renorosgon'
  ) +
  # Modifica el tema
  theme_bw(base_size = 16, base_family = 'Avenir Next') +
  theme(
    axis.title = element_blank(),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(fill = "transparent"),
    legend.position = c(0.9, 0.95),
    legend.key.height = unit(0.5,'cm')
  )

# Crea una una receta
receta = recipe(
  # Define la ecuación
  formula = vehicles ~ year + month + hour + week_day,
  # Define los datos a utilizar
  data = train
) %>% 
  # Crear dummies
  step_dummy(week_day, one_hot = TRUE) %>% 
  # Quitar una dummy para evitar la trampa
  step_mutate(week_day_1 = NULL)


receta %>% 
  prep %>% juice %>% glimpse()

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
  resamples = rolling_origin,
  # Parametros a calibrar
  param_info = glmnet_parametros,
  # Métricas de evaluación
  metrics = metric_set(yardstick::rmse, yardstick::mae),
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
  ggplot(aes(x = penalty, y = mean, col = .metric)) +
  # Geometría del error estandar
  geom_errorbar(
    aes(
      ymin = mean - std_err,
      ymax = mean + std_err
    ),
    alpha = 0.25
  ) +
  scale_x_log10() +
  # Geometría de línea
  geom_line(size = 0.5) 

# Seleccionar el mejor modelo
best_poisson = poisson_tuning %>%
  select_best(metric = "rmse")

# Imprime el mejor modelo
print(best_poisson)

# Finalizar el flujo de trabajo
final_poisson = poisson_workflow %>% 
  finalize_workflow(best_poisson)

# Ejecutar el último ajuste
resultados_poisson = final_poisson %>% 
  last_fit(
    split = data_split,
    metrics = metric_set(yardstick::rmse)
  )

# Recolectar las métricas de prueba
metricas_poisson = resultados_poisson %>% 
  collect_metrics() 

print(metricas_poisson)


# Full machine learning ---------------------------------------------------
receta = recipe(
  # Define la ecuación
  formula = vehicles ~ year + month + hour + week_day,
  # Define los datos a utilizar
  data = train
) %>% 
  # Crear dummies
  step_ordinalscore(week_day)  %>% 
  # Create the polynomial variable
  step_poly(week_day, degree = 6) %>% 
  step_poly(month, degree = 6) %>% 
  step_poly(hour, degree = 6)

receta %>% 
  prep %>% 
  juice() %>% 
  glimpse()

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
  resamples = rolling_origin,
  # Parametros a calibrar
  param_info = glmnet_parametros,
  # Métricas de evaluación
  metrics = metric_set(yardstick::rmse, yardstick::mae),
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
  ggplot(aes(x = penalty, y = mean, col = .metric)) +
  # Geometría del error estandar
  geom_errorbar(
    aes(
      ymin = mean - std_err,
      ymax = mean + std_err
    ),
    alpha = 0.25
  ) +
  scale_x_log10() +
  # Geometría de línea
  geom_line(size = 0.5) 

# Seleccionar el mejor modelo
best_poisson = poisson_tuning %>%
  select_best(metric = "rmse")

# Imprime el mejor modelo
print(best_poisson)

# Finalizar el flujo de trabajo
final_poisson = poisson_workflow %>% 
  finalize_workflow(best_poisson)

# Ejecutar el último ajuste
resultados_poisson = final_poisson %>% 
  last_fit(
    split = data_split,
    metrics = metric_set(yardstick::rmse)
  )

# Recolectar las métricas de prueba
metricas_poisson = resultados_poisson %>% 
  collect_metrics() 

print(metricas_poisson)

# Modelos entrenados ------------------------------------------------------
# Extraer el modelo
glmnet_trained = resultados_poisson %>% 
  extract_fit_parsnip() 
print(glmnet_trained)

# Coeficientes
coefficients = tidy(glmnet_trained)
print(coefficients)

# Incluir
preproc = receta %>% 
  prep() %>% 
  bake(new_data = traffic)

# Make predictions
predictions_tb =  augment(glmnet_trained, preproc) %>% 
  bind_cols(select(traffic, date_time))

glimpse(predictions_tb)


# Revisa como de desempeña el conjunto completo
rmse_poisson = predictions_tb %>%  
  # Calcula el rmse
  yardstick::rmse(vehicles,.pred) %>% 
  pull(.estimate)

rmse_poisson

# Realiza una gráfica del ajuste
predictions_tb %>% 
  ggplot(
    # Esta estética se hereda a todas las geometrías
    aes( x = date_time )
  ) +
  # Agrega una línea con los valores observados
  geom_line(aes(y = vehicles, col = 'Observados'),
            # Modifica el ancho de línea
            linewidth = 0.25
  ) +
  # Agrega una línea con los valores estimados
  geom_line(aes(y = .pred , col = 'Predicción'),
            # Modifica el ancho de línea
            linewidth = 0.25
  ) +
  # Modifica los colores base
  scale_color_manual(values = c('gray50','#003399')) +
  # Agrega títulos
  ggtitle('Resultados del Modelo Poisson-Glmnet',
          subtitle = 'Número de vehículos por hora en la intersección 1') +
  #  Usa un tema predefinido
  theme_bw() +
  # Haz modificaciones
  theme(
    # Cambia los textos
    text = element_text(family = 'Avenir Next'),
    # Cambia los títulos
    axis.title = element_blank(),
    # Posición de la leyenda
    legend.position = 'bottom',
    # Quita el título de la leyenda
    legend.title = element_blank()
  )


# Realiza una gráfica del ajuste
predictions_tb %>% 
  ggplot(
    # Esta estética se hereda a todas las geometrías
    aes( x = vehicles - .pred )
  ) +
  # Agrega un histograma con los valores estimados
  geom_histogram(
    col = 'gray50',
    fill = '#003399'
  ) +
  scale_color_manual(values = c('gray50','#003399')) +
  # Agrega títulos
  ggtitle('Residuales del ModeloPoisson-Glmnet') +
  #  Usa un tema predefinido
  theme_bw() +
  # Haz modificaciones
  theme(
    # Cambia los textos
    text = element_text(family = 'Avenir Next'),
    # Cambia los títulos
    axis.title = element_blank(),
    # Posición de la leyenda
    legend.position = 'bottom',
    # Quita el título de la leyenda
    legend.title = element_blank()
  )



