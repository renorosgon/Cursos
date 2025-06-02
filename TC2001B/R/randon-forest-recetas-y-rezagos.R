# Fija el directorio de trabajo
setwd("~/Desktop/ITESM/Cursos/TC2001B")

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

# Instalar - Cargar tidyquant                                                       
if(require(tidyquant) == FALSE){                                                
  install.packages('tidyquant')                                                 
  library(tidyquant)                                                            
}else{                                                                          
  library(tidyquant)                                                            
}

# Instalar - Cargar quantmod                                                       
if(require(quantmod) == FALSE){                                                
  install.packages('quantmod')                                                 
  library(quantmod)                                                            
}else{                                                                          
  library(quantmod)                                                            
}
# Instalar - Cargar ranger                                                       
if(require(ranger) == FALSE){                                                
  install.packages('ranger')                                                 
  library(ranger)                                                            
}else{                                                                          
  library(ranger)                                                            
}

# Data --------------------------------------------------------------------
# Extraer los datos
getSymbols("NFLX",src="yahoo",from="2015-01-01",to = "2023-03-13")

# Tidy data
netflix_stock = NFLX %>% 
  # zoo a tible
  fortify.zoo() %>% 
  as_tibble() %>% 
  # Limpiar los nombre
  janitor::clean_names()

#Un ggplot estandar
ggplot(netflix_stock, aes(x = index, y = log(nflx_close/lag(nflx_close)))) +
  # grafico de linea
  geom_line() +
  # Agregar etiquetas
  labs(title = "NFLX Line Chart", subtitle = "Closing Price") + 
  # Modificar un tema
  theme_tq() +
  theme(
    axis.title = element_blank()
  )

# Grafico de velas
netflix_stock %>% 
  tail(100) %>% 
ggplot(aes(x = index)) +
  # grafico de linea
  geom_candlestick(
    aes(
      open = nflx_open, 
      high = nflx_high, 
      low = nflx_low, 
      close = nflx_close
      )
    ) +
  # Agregar etiquetas
  labs(title = "NFLX Candlestick", subtitle = "Closing Price") + 
  # Modificar un tema
  theme_tq() +
  theme(
    axis.title = element_blank()
  )

nombres = names(netflix_stock)[-1] 

# preprocesamiento
nflx_fe = recipe(
  # Define la ecuación
  formula = nflx_adjusted ~ .,
  # Define los datos a utilizar
  data = netflix_stock
) %>%   
  # Calcula promedios moviles
  step_window(
    starts_with('nflx_'),
    size = 15, statistic = 'median',
    role = 'predictor', names = paste0('ma_mediana_15_', nombres)
  ) %>% 
  # Calcula promedios moviles
  step_window(
    starts_with('nflx_'),
    size = 15, statistic = 'var',
    role = 'predictor', names = paste0('ma_var_15_', nombres)
  ) %>% 
  # Calcula rezagos
  step_lag(starts_with('nflx_'), lag = 1:7) %>% 
  step_mutate(
    nflx_high = NULL,
    nflx_low = NULL,
    nflx_volume = NULL, 
    nflx_close = NULL
  ) %>% 
  prep() %>% 
  bake(new_data = NULL) %>% 
  filter(!is.na(lag_7_nflx_adjusted))

# Las semillas aleatorias nos ayudan a fijar la aleatoriedad
set.seed(35)
# Separa los datos en dos conjuntos
datos_split = nflx_fe %>% 
  initial_time_split(
    prop = 0.80
  )

# Extrae el conjunto de entrenamiento
entrenamiento = training(datos_split) 

# Extrae el conjunto de prueba
prueba = testing(datos_split)

# Observa cómo son diferentes
summary(entrenamiento)
summary(prueba)

# Construye las muestras de validación cruzada (temporal) con los datos de entrenamiento
rolling_origin = rolling_origin(
  entrenamiento,
  initial = 1000, 
  assess = 60,
  cumulative = FALSE, 
  skip = 60,
  lag = 30
)


# This function unnests and summarises the rolling origin folds
unfolding_rolling_origin = function(fold_num){
  # Summarise the analysis set
  analysis_set = rolling_origin %>% 
    pluck('splits') %>% 
    pluck(fold_num) %>% 
    analysis() %>% 
    summarize(
      fold = fold_num,
      set = 'Analysis',
      min = min(index),
      max = max(index)
    )
  # Summarise the assessment set
  assessment_set = rolling_origin %>% 
    pluck('splits') %>% 
    pluck(fold_num) %>% 
    assessment()%>% 
    summarize(
      fold = fold_num,
      set = 'Assessment',
      min = min(index),
      max = max(index)
    )
  # Bind the summaries
  fold = bind_rows(analysis_set, assessment_set)
  # Return the fold summary
  return(fold)
}

# Apply the unfolding function
map_df(1:nrow(rolling_origin), unfolding_rolling_origin) %>% 
  # Create a ggplot setting aes to be inherited 
  ggplot(aes(y = factor(fold), col = set)) +
  # Add a segment
  geom_segment(aes(x = min, xend = max, yend = factor(fold))) + 
  # Add points
  geom_point(aes(x = max))+ 
  geom_point(aes(x = min)) +
  # Modify colors
  scale_color_manual(values = c('darkred','orange')) +
  # Add proper labels
  labs(
    title = 'Rolling Origin Explanation',
    subtitle = 'Fold Subset',
    col = 'Set',
    caption = '@renorosgon'
  ) +
  # Modify themes
  theme_bw(base_size = 16) +
  theme(
    axis.title = element_blank(),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(fill = "transparent"),
    legend.position = c(0.1, 0.95),
    legend.key.height = unit(0.5,'cm'),
    text = element_text(family = 'Bebas Neue')
  )


# Bosque Aleatorio ---------------------------------------------------------
# Creamos nuestro modelo
bosque = rand_forest() %>% 
  set_engine("ranger") %>% 
  set_mode("regression") %>% 
  set_args(
    mtry = tune(),
    trees= tune(),
    min_n = tune()
    )

# receta
receta = recipe(
  # Define la ecuación
  formula = nflx_adjusted ~ .,
  # Define los datos a utilizar
  data = entrenamiento
)

flujo_bosque = workflow() %>% 
  add_recipe(receta) %>% 
  add_model(bosque) 

# Ajusta con flujo de trabajo 
# Get parameters to tune
bosque_parametros = flujo_bosque %>% 
  extract_parameter_set_dials() 

# Crear una maya de calibración
set.seed(123)
install.packages('doParallel')
# Arranca un cluster
doParallel::registerDoParallel()

# Entrena en paralelo 100 modelos
bosque_tuning = tune_grid(
  # Define el objeto a utilizar
  object = flujo_bosque,
  # Método de remuestreo
  resamples = rolling_origin,
  # Parametros a calibrar
  param_info = bosque_parametros,
  # Métricas de evaluación
  metrics = metric_set(yardstick::rmse),
  # Parámetros de control
  control = control_grid(parallel_over = "everything"),
  # Número de combinaciones a probar
  grid = 10
)
# Detiene el cluster
doParallel::stopImplicitCluster()

# Recolectar métricas
bosque_metricas = bosque_tuning %>%
  collect_metrics() %>% 
  # Ordenar por promedios
  arrange(mean)

# Seleccionar el mejor modelo
best_bosque = bosque_tuning %>%
  select_best(metric = "rmse")

# Imprime el mejor modelo
print(best_bosque)

# Finalizar el flujo de trabajo
final_bosque = flujo_bosque %>% 
  finalize_workflow(best_bosque)

# Ejecutar el último ajuste
resultados_bosque = final_bosque %>% 
  last_fit(
    split = datos_split,
    metrics = metric_set(yardstick::rmse)
  )

# Recolectar las métricas de prueba
metricas_bosque = resultados_bosque %>% 
  collect_metrics() 

print(metricas_bosque)

bosque_trained = resultados_bosque %>% 
  extract_fit_parsnip() 

print(bosque_trained)


# Make predictions
predictions_tb =  augment(bosque_trained, prueba)


#Un ggplot estandar
ggplot(predictions_tb, aes(x = index)) +
  # grafico de linea
  geom_line(aes(y = nflx_adjusted, col = 'Observed'), lwd = 2) +
  # grafico de linea
  geom_line(aes(y = .pred, col = 'Predicted'), lwd = 1) +
  # Agregar etiquetas
  labs(title = "NFLX Line Chart", subtitle = "Closing Price", col = '') + 
  # Modificar un tema
  theme_tq() +
  scale_color_manual(values = c('darkred','green'))+
  theme(
    axis.title = element_blank()
  )


# Explicando ML -----------------------------------------------------------
# Instalar - Cargar vip                                                       
if(require(vip) == FALSE){                                                
  install.packages('vip')                                                 
  library(vip)                                                            
}else{                                                                          
  library(vip)                                                            
}
# Instalar - Cargar ingredients                                                       
if(require(ingredients) == FALSE){                                                
  install.packages('ingredients')                                                 
  library(ingredients)                                                            
}else{                                                                          
  library(ingredients)                                                            
}
# Instalar - Cargar iBreakDown                                                       
if(require(iBreakDown) == FALSE){                                                
  install.packages('iBreakDown')                                                 
  library(iBreakDown)                                                            
}else{                                                                          
  library(iBreakDown)                                                            
}
# Instalar - Cargar DALEX                                                       
if(require(DALEX) == FALSE){                                                
  install.packages('DALEX')                                                 
  library(DALEX)                                                            
}else{                                                                          
  library(DALEX)                                                            
}
# Instalar - Cargar DALEXtra                                                       
if(require(DALEXtra) == FALSE){                                                
  install.packages('DALEXtra')                                                 
  library(DALEXtra)                                                            
}else{                                                                          
  library(DALEXtra)                                                            
}

# Crear un Explainer
explainer_modelo = explain_tidymodels(
  # Definir el modelo
  bosque_trained,
  # Definir los datos
  data = prueba ,
  # Definir la variable objetivo
  y = pull(prueba, nflx_adjusted),
  # Agregar una etiqueta
  label = 'Bosque'
)

# Evaluar el desemeño
desempeno = explainer_modelo %>% 
  model_performance(explainer_modelo)

plot(desempeno)

# Revisar importancia de variables
importancia_variables = feature_importance(explainer_modelo)
importancia_variables %>% 
  plot()

# Desgloce 
obs = last(prueba)
desgloce = break_down(explainer_modelo, obs)
plot(desgloce)

# Perfil de la variable
perfil = predict_profile(explainer_modelo, obs)
plot(perfil, variables = c('nflx_open'))
