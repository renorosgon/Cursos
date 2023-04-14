# Fija el directorio de trabajo
setwd("~/Desktop/ITESM/Cursos/EC3001C")

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

# Instalación y carga de paqueterías
# Instalar - Cargar lubridate                                                       
if(require(lubridate) == FALSE){                                                
  install.packages('lubridate')                                                 
  library(lubridate)                                                            
}else{                                                                          
  library(lubridate)                                                            
}

# Instalar - performance                                                       
if(require(performance) == FALSE){                                                
  install.packages('performance')  
  detach("package:performance", unload = TRUE)
} 

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
  janitor::clean_names()

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
      y = vehicles, 
      # Usa cada intersección como una geometría
      group = junction) ) +
  # Agrega una línea
  geom_line() +
  # Separa por cada intersección
  facet_wrap(~junction) +
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

# Trabajaremos solo con la primera interseción
junction_1 = traffic %>% 
  subset(junction == 1) 

# Podemos replicar la gráfica anterior
junction_1 %>% 
  # Usa las mismas variables
  ggplot( aes( x = date_time, y = vehicles) ) +
  # Agrega una linea
  geom_line() +
  # Agrega títulos
  ggtitle('Tránsito vehicular por hora en la intersección 1',
          subtitle = 'Número de vehículos') +
  #Reduce los margenes a 0
  scale_x_continuous(expand = c(0,0)) +
  #  Usa un tema predefinido
  theme_bw() +
  # Haz modificaciones
  theme(
    # Cambia los textos
    text = element_text(family = 'Bebas Neue'),
    # Cambia los títulos
    axis.title = element_blank(),
  )


### Ajuste de un modelo OLS
modelo_ols = lm(
  # Define la ecuación a estrimar
  formula = log(vehicles) ~ year + month + hour + week_day, 
  # Indica los datos a utilizar
  data = junction_1
  )

# Imprime el modelo
print(modelo_ols)

# Imprime el resumen de resultados
summary(modelo_ols) 

# Imprime el 
glance(modelo_ols)

# Utiliza la función check_model para revisar los supuestos
performance::check_model(modelo_ols)

# Agrega las predicciones del modelo 
resultados_modelo_ols = modelo_ols %>% 
  augment(junction_1) %>% 
  # Dada la transformación logarítmica, hay que exponenciar el valor agustado
  mutate(
    .fitted = exp(.fitted), # Predicción
    .resid = exp(.resid)    # Residual
    )

# Luce así
glimpse(resultados_modelo_ols)

# Realiza una gráfica del ajuste
resultados_modelo_ols %>% 
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
  geom_line(aes(y = .fitted , col = 'Predicción'),
            # Modifica el ancho de línea
            linewidth = 0.25
            ) +
  # Modifica los colores base
  scale_color_manual(values = c('black','darkblue')) +
  # Agrega títulos
  ggtitle('Resultados del Modelo OLS',
          subtitle = 'Número de vehículos por hora en la intersección 1') +
  #  Usa un tema predefinido
  theme_bw() +
  # Haz modificaciones
  theme(
    # Cambia los textos
    text = element_text(family = 'Bebas Neue'),
    # Cambia los títulos
    axis.title = element_blank(),
    # Posición de la leyenda
    legend.position = 'bottom',
    # Quita el título de la leyenda
    legend.title = element_blank()
  )

# Compara los valores estimados con los ajustados
resultados_modelo_ols %>% 
  ggplot(aes(x = vehicles, y = .fitted)) + 
  # Agrega un diagrama de dispersión con transparencia
  geom_point(alpha = 0.05) +
  # Agrega una línea punteada de 45 grados
  geom_abline(col = 'red', linetype = 'dashed') +
  # Modifica las etiquetas en los ejes
  scale_x_continuous(
    # Puntos de corte
    breaks = seq(0,150,25), 
    # Límites del eje
    limits = c(0,150)
    ) +
  scale_y_continuous(
    # Puntos de corte
    breaks = seq(0,150,25), 
    # Límites del eje
    limits = c(0,150)
  ) +
  # Modifica los títulos de los ejes
  labs(x = 'Observados', y = 'Estimados') +
  # Agrega títulos
  ggtitle('Resultados del Modelo OLS',
          subtitle = 'Número de vehículos por hora en la intersección 1') +
  #  Usa un tema predefinido
  theme_bw() +
  # Haz modificaciones
  theme(
    # Cambia los textos
    text = element_text(family = 'Bebas Neue'),
    # Quita el título de la leyenda
    legend.title = element_blank()
  )

# Extraemos el error mínimo-cuadrático
rmse_ols_completo = resultados_modelo_ols %>%
  # Declara el valor obsercvado y el estimado
  yardstick::rmse(truth = vehicles, estimate =.fitted) %>% 
  pull(.estimate)

print(rmse_ols_completo)

# ¿es un buen resultado? ¿cómo funcionará ante nuevos datos?
# Para simular el futuro haremos un corte en las observaciones
# El pasado tendrá el 90% de los datos
pasado = junction_1[1:(round(nrow(junction_1))*.9),]
# Intentaremos predecir el 10% del futuro
futuro = junction_1[(nrow(pasado)+1):nrow(junction_1),]

# Repetimos la misma receta
modelo_ols_pasado = lm(
  # Define la ecuación a estrimar
  formula = log(vehicles) ~ year + month + hour + week_day, 
  # Indica los datos a utilizar
  data = pasado
  )

# Imprime el modelo
print(modelo_ols_pasado)

# Imprime el resumen de resultados
summary(modelo_ols_pasado)

# Imprime el 
glance(modelo_ols_pasado)

# Agrega las predicciones del modelo 
resultados_pasado_ols = modelo_ols_pasado %>% 
  augment(pasado) %>% 
  # Dada la transformación logarítmica, hay que exponenciar el valor agustado
  mutate(
    .fitted = exp(.fitted), # Predicción
    .resid = exp(.resid)    # Residual
  )

# Extraemos el error mínimo-cuadrático
rmse_ols_pasado = resultados_pasado_ols %>%
  # Declara el valor obsercvado y el estimado
  yardstick::rmse(truth = vehicles, estimate =.fitted) %>% 
  pull(.estimate)

# ¡El rmse es menor! 
print(rmse_ols_pasado)

# ¿Podrá predecir el futuro?
resultados_futuro_ols = futuro %>% 
  # Agrega los valores de predicción al tibble del futuro
  mutate(
    # Usa la función predict para ajustar el modelo ols del pasado
    .fitted = exp(predict(modelo_ols_pasado, futuro)),
    # Calcula los residuales
    .resid = vehicles - .fitted
    )

# Extraemos el error mínimo-cuadrático
rmse_ols_futuro = resultados_futuro_ols %>%
  # Declara el valor obsercvado y el estimado
  yardstick::rmse(truth = vehicles, estimate =.fitted) %>% 
  pull(.estimate)

# ¡El rmse incrementó considerablemente! 
print(rmse_ols_futuro)

# Comparemos OLS "omnipresente" con el que desconoce el futuro
resultados_modelo_ols = resultados_modelo_ols %>% 
  # Agrega la predicción y residuales del modelo anterior
  mutate(
    .fitted_pasado = exp(predict( modelo_ols_pasado, junction_1)),
    .resid_pasado = vehicles - .fitted_pasado
  )

# Extraemos el error mínimo-cuadrático
rmse_ols_miope = resultados_modelo_ols %>%
  # Declara el valor obsercvado y el estimado
  yardstick::rmse(truth = vehicles, estimate =.fitted_pasado) %>% 
  pull(.estimate)

# ¡El rmse es menor! 
print(rmse_ols_miope)

# Compara las estimaciones 
resultados_modelo_ols %>% 
  # Esta estética se hereda a todas las geometrías
  ggplot(aes(x = date_time)) +
  # Agrega una linea para los valores observados y cada modelo
  geom_line(aes(y = .fitted , col = 'OLS'), alpha = 0.2, lwd = 0.75)+
  geom_line(aes(y = .fitted_pasado , col = 'OLS Miope'), alpha = 0.2, lwd = 0.75) +
  geom_line(aes(y = vehicles, col = 'Observados'), alpha = 0.2, lwd = 0.75) +
  # Agrega una linea de para los valores observados
  stat_smooth(aes(y = vehicles, col = 'Observados'), lwd = 0.75,
              # Utiliza glm en una formula simple
              method = "glm", formula = y~x,
              # Agrega el argumento de liga log ya que fue la que usamos
              method.args = list(family = gaussian(link = 'log'))
              ) +
  # Agrega una linea de para el ajuste OLS
  stat_smooth(aes(y = .fitted , col = 'OLS'), lwd = 0.75,
              # Utiliza glm en una formula simple
              method = "glm", formula = y~x,
              # Agrega el argumento de liga log ya que fue la que usamos
              method.args = list(family = gaussian(link = 'log'))
              ) +
  # Agrega una linea de para el ajuste OLS
  stat_smooth(aes(y = .fitted_pasado , col = 'OLS Miope'), linewidth = 0.75,
              # Utiliza glm en una formula simple
              method = "glm", formula = y~x,
              # Agrega el argumento de liga log ya que fue la que usamos
              method.args = list(family = gaussian(link = 'log'))
              ) +
  # Cambia la escala de colores
  scale_color_manual(values = c('black','blue','red')) +
  # Agrega títulos
  ggtitle('Comparación de Modelos OLS',
          subtitle = 'Número de vehículos por hora en la intersección 1') +
  #  Usa un tema predefinido
  theme_bw() +
  # Haz modificaciones
  theme(
    # Cambia los textos
    text = element_text(family = 'Bebas Neue'),
    # Cambia los títulos
    axis.title = element_blank(),
    # Posición de la leyenda
    legend.position = 'bottom',
    # Quita el título de la leyenda
    legend.title = element_blank()
  )

# Ahora utilizaremos el enfoque de aprendizaje de máquina

# Define el modelo de ajuste
regresion_lineal = linear_reg() %>% 
  # Selecciona el motor a utilizar
  set_engine('lm') %>% 
  # Fija el problema a resolver
  set_mode('regression')

# Las semillas aleatorias nos ayudan a fijar la aleatoriedad
set.seed(35)
# Separa los datos en dos conjuntos
datos_split = pasado %>% 
  initial_split(
    strata = vehicles
  )

# Extrae el conjunto de entrenamiento
entrenamiento = training(datos_split) 

# Extrae el conjunto de prueba
prueba = testing(datos_split)

# Observa cómo son diferentes
summary(entrenamiento)
summary(prueba)

# Construye las muestras de validación cruzada con los datos de entrenamiento
validacion_cruzada = vfold_cv(entrenamiento, v = 10)

# Crea una una receta
receta = recipe(
  # Define la ecuación
  formula = vehicles ~ year + month + hour + week_day,
  # Define los datos a utilizar
  data = entrenamiento
  ) %>% 
  # Agrega un paso para transformar a logaritmos
  step_log(vehicles)

# Crea un flujo de trabajo
modelo_ols_workflow = workflow() %>%
  # Añade una receta
  add_recipe(receta) %>% 
  # Agrega un modelo
  add_model(regresion_lineal) 

# Ajusta con flujo de trabajo 
ajuste_con_validacion  = modelo_ols_workflow %>% 
  fit_resamples(
    # Utiliza los conjuntos de validación cruzada
    resamples = validacion_cruzada,
    # Define la(s) métricas a evaluar
    metrics = metric_set(yardstick::rmse)
    )

# Revisa las métricas de validación
ajuste_con_validacion %>% 
  collect_metrics(summarize = FALSE)

# Selecciona el modelo final
ajuste_ml_final = modelo_ols_workflow %>% 
  last_fit(datos_split) 

modelo_ml_final = ajuste_ml_final %>% 
  extract_fit_engine() 

# Revisa como de desempeña el conjunto de entrenamiento
rmse_entrenamiento = entrenamiento %>% 
  # Calcula las predicciones y residuales
  mutate(
    .fitted = exp(predict(modelo_ml_final, entrenamiento)),
    .resid_pasado = vehicles - .fitted
  ) %>%  
  # Calcula el rmse
  yardstick::rmse(vehicles,.fitted)

# Revisa como de desempeña el conjunto de prueba
rmse_prueba = prueba %>% 
  # Calcula las predicciones y residuales
  mutate(
    .fitted = exp(predict(modelo_ml_final, prueba)),
    .resid_pasado = vehicles - .fitted
  ) %>%  
  # Calcula el rmse
  yardstick::rmse(vehicles,.fitted)

# Revisa como de desempeña el conjunto del pasado
rmse_ml_pasado = pasado %>% 
  # Calcula las predicciones y residuales
  mutate(
    .fitted = exp(predict(modelo_ml_final, pasado)),
    .resid_pasado = vehicles - .fitted
  ) %>%  
  # Calcula el rmse
  yardstick::rmse(vehicles,.fitted) %>% 
  pull(.estimate)

print(rmse_ml_pasado)

# Revisa como de desempeña el conjunto del futuro
rmse_ml_futuro = futuro %>% 
  # Calcula las predicciones y residuales
  mutate(
    .fitted = exp(predict(modelo_ml_final, futuro)),
    .resid_pasado = vehicles - .fitted
  ) %>%  
  # Calcula el rmse
  yardstick::rmse(vehicles,.fitted) %>% 
  pull(.estimate)

print(rmse_ml_futuro)

# Revisa como de desempeña el conjunto completo
rmse_ml = junction_1 %>% 
  # Calcula las predicciones y residuales
  mutate(
    .fitted = exp(predict(modelo_ml_final, junction_1)),
    .resid = vehicles - .fitted
  ) %>%  
  # Calcula el rmse
  yardstick::rmse(vehicles,.fitted) %>% 
  pull(.estimate)

print(rmse_ml)

# Extrae las predicciones
ajuste_ml_final %>% 
  collect_predictions() %>% 
  # Selecciona las variables de interes
  select(.pred, vehicles) %>% 
  # Transforma exponenciando
  mutate_all(exp) %>% 
  # Grafica
  ggplot(aes(x = vehicles, y = .pred)) + 
  # Agrega un diagrama de dispersión con transparencia
  geom_point(alpha = 0.1) +
  # Agrega una línea punteada de 45 grados
  geom_abline(col = 'red', linetype = 'dashed') +
  # Modifica las etiquetas en los ejes
  coord_obs_pred() +
  # Modifica los títulos de los ejes
  labs(x = 'Observados', y = 'Estimados') +
  # Agrega títulos
  ggtitle('Resultados del Modelo OLS con Validación Cruzada',
          subtitle = 'Número de vehículos por hora en la intersección 1') +
  #  Usa un tema predefinido
  theme_bw() +
  # Haz modificaciones
  theme(
    # Cambia los textos
    text = element_text(family = 'Bebas Neue'),
    # Posición de la leyenda
    legend.position = 'bottom',
    # Quita el título de la leyenda
    legend.title = element_blank()
  )

# Obtener los resultados del modelo de ml
resultados_ml = junction_1 %>% 
  # Calcula las predicciones y residuales
  mutate(
    .fitted = exp(predict(modelo_ml_final, junction_1)),
    .resid = vehicles - .fitted
  )  %>% 
  # Selecciona las variables de interés
  select(date_time, .fitted_ml = .fitted, .resid_ml = .resid) 

# Concatenamos los resultados
resultados = resultados_modelo_ols %>% 
  left_join(resultados_ml, by = 'date_time') 
  

# Almacenamos los rmses
rmses = tibble(
  modelo = factor(c('OLS','OLS Miope','OLS ML')),
  rmse = c(rmse_ols_completo, rmse_ols_futuro, rmse_ml_futuro)
)

# Visualiza los resultados
resultados %>% 
  # Selecciona y renombra las variables de interés
  transmute(date_time, 
            `OLS` = .fitted,
            `OLS Miope` = .fitted_pasado,
            `OLS ML` = .fitted_ml,
            Observados = vehicles
            ) %>% 
  # Reestructura lso datos (long)
  gather(modelo, prediccion, -date_time, - Observados) %>% 
  # Crea el lienzo con las herencias
  ggplot(aes( x = date_time, y = prediccion, col = modelo)) +
  # Agrega una linea para los observados
  geom_line(aes(y = Observados), col ='black', alpha = 0.1) +
  # Agrega una linea para las predicciones
  geom_line(alpha = 0.1) +
  # Agrega una linea de tendencia para los valores observados
  stat_smooth(aes(y = Observados), col ='black', lwd = 0.75,
              # Utiliza glm en una formula simple
              method = "glm", formula = y~x,
              # Agrega el argumento de liga log ya que fue la que usamos
              method.args = list(family = gaussian(link = 'log'))) +
  # Agrega una linea de tendencia para los valores estimados
  stat_smooth(lwd = 0.75,
              # Utiliza glm en una formula simple
              method = "glm", formula = y~x,
              # Agrega el argumento de liga log ya que fue la que usamos
              method.args = list(family = gaussian(link = 'log'))) +
  # Separa por modelo
  facet_wrap(~modelo) +
  # Modifica los colores base
  scale_color_manual(values = c('blue','red','orange')) +
  # Agrega los valores del RMSE
  geom_text(
    # Selecciona tus datos
    data = rmses, 
    # Agrega la etiqueta
    aes(label = paste0('RMSE: ', round(rmse,2))),
    # Define las coordenadas
    x =as.POSIXct('2016-02-01'), y = 160,
    # Modifica la letra
    family = 'Bebas Neue', size = 3) +
  # Agrega títulos
  ggtitle('Resultados de los Modelo OLS',
          subtitle = 'Número de vehículos por hora en la intersección 1') +
  #  Usa un tema predefinido
  theme_bw() +
  # Haz modificaciones
  theme(
    # Cambia los textos
    text = element_text(family = 'Bebas Neue'),
    # Cambia los títulos
    axis.title = element_blank(),
    # Posición de la leyenda
    legend.position = 'none',
    # Quita el título de la leyenda
    legend.title = element_blank()
  ) 


### Modelo Poisson
# Instalar - Cargar poissonreg                                                     
if(require(poissonreg) == FALSE){                                                
  install.packages('poissonreg')                                                 
  library(poissonreg)                                                            
}else{                                                                          
  library(poissonreg)                                                            
}

# Iniciamos nuestro modelo
regresion_poisson = poisson_reg() %>% 
  set_engine('glm') %>% 
  set_mode('regression')

# Crea una una receta
receta = recipe(
  # Define la ecuación
  formula = vehicles ~ year + month + hour + week_day,
  # Define los datos a utilizar
  data = entrenamiento
) %>% 
  step_naomit()

# Crea un flujo de trabajo
modelo_poisson_workflow = workflow() %>%
  # Agrega una receta
  add_recipe(receta) %>% 
  # Agrega un modelo
  add_model(regresion_poisson) 

# Ajusta con flujo de trabajo 
ajuste_con_validacion  = modelo_poisson_workflow %>% 
  fit_resamples(
    # Utiliza los conjuntos de validación cruzada
    resamples = validacion_cruzada,
    # Define la(s) métricas a evaluar
    metrics = metric_set(yardstick::rmse)
  )

# Revisa las métricas de validación
ajuste_con_validacion %>% 
  collect_metrics(summarize = F)  

# Selecciona el modelo final
ajuste_poisson_final = modelo_poisson_workflow %>% 
  last_fit(datos_split) 

modelo_poisson_final = ajuste_poisson_final %>% 
  extract_fit_engine() 

# Revisa como de desempeña el conjunto de entrenamiento
rmse_entrenamiento_poisson = modelo_poisson_final %>% 
  augment(entrenamiento) %>% 
  # Calcula las predicciones y residuales
  mutate(
    .fitted = exp(.fitted),
    .resid = vehicles - .fitted
  ) %>%  
  # Calcula el rmse
  yardstick::rmse(vehicles,.fitted)

print(rmse_entrenamiento_poisson)

# Revisa como de desempeña el conjunto de prueba
rmse_prueba_poisson =prueba %>% 
  # Calcula las predicciones y residuales
  mutate(
    .fitted = exp(predict(modelo_poisson_final, prueba)),
    .resid = vehicles - .fitted
  ) %>%  
  # Calcula el rmse
  yardstick::rmse(vehicles,.fitted)

# Revisa como de desempeña el conjunto de pasado
rmse_pasado_poisson = pasado %>% 
  # Calcula las predicciones y residuales
  mutate(
    .fitted = exp(predict(modelo_poisson_final, pasado)),
    .resid = vehicles - .fitted
  ) %>%  
  # Calcula el rmse
  yardstick::rmse(vehicles,.fitted) %>% 
  pull(.estimate)

print(rmse_pasado_poisson)

# Revisa como de desempeña el conjunto de futuro
rmse_futuro_poisson = futuro %>% 
  # Calcula las predicciones y residuales
  mutate(
    .fitted = exp(predict(modelo_poisson_final, futuro)),
    .resid = vehicles - .fitted
  ) %>%  
  # Calcula el rmse
  yardstick::rmse(vehicles,.fitted) %>% 
  pull(.estimate)

print(rmse_futuro_poisson)

# Cuardamos los resultados
resultados_poisson = junction_1 %>% 
  # Calcula las predicciones y residuales
  mutate(
    .fitted = exp(predict(modelo_poisson_final, junction_1)),
    .resid = vehicles - .fitted
  ) 

# Revisa como de desempeña el conjunto completo
rmse_poisson = resultados_poisson %>%  
  # Calcula el rmse
  yardstick::rmse(vehicles,.fitted) %>% 
  pull(.estimate)

print(rmse_poisson)

# Extrae las predicciones
ajuste_poisson_final %>% 
  collect_predictions() %>% 
  # Selecciona las variables de interes
  select(.pred, vehicles) %>% 
  # Grafica
  ggplot(aes(x = vehicles, y = .pred)) + 
  # Agrega un diagrama de dispersión con transparencia
  geom_point(alpha = 0.1) +
  # Agrega una línea punteada de 45 grados
  geom_abline(col = 'red', linetype = 'dashed') +
  # Modifica las etiquetas en los ejes
  coord_obs_pred() +
  # Modifica los títulos de los ejes
  labs(x = 'Observados', y = 'Estimados') +
  # Agrega títulos
  ggtitle('Resultados del Modelo Poisson con Validación Cruzada',
          subtitle = 'Número de vehículos por hora en la intersección 1') +
  #  Usa un tema predefinido
  theme_bw() +
  # Haz modificaciones
  theme(
    # Cambia los textos
    text = element_text(family = 'Bebas Neue'),
    # Posición de la leyenda
    legend.position = 'bottom',
    # Quita el título de la leyenda
    legend.title = element_blank()
  )

# Almacenamos los rmses
rmses = tibble(
  modelo = factor(c('OLS Miope','OLS ML','Poisson ML')),
  rmse = c(rmse_ols_futuro, rmse_ml_futuro, rmse_futuro_poisson)
)

resultados_predicciones = resultados_poisson %>% 
  select(date_time, .fitted_poisson = .fitted, .resid_poisson = .resid) %>% 
  right_join(resultados, by = 'date_time') %>% 
  select(-c(.fitted:.std.resid))

# Visualiza los resultados
resultados_predicciones %>% 
  # Selecciona y renombra las variables de interés
  transmute(date_time, 
            `OLS Miope` = .fitted_pasado,
            `OLS ML` = .fitted_ml,
            `Poisson ML` = .fitted_poisson,
            Observados = vehicles
  ) %>% 
  # Reestructura lso datos (long)
  gather(modelo, prediccion, -date_time, - Observados) %>% 
  # Crea el lienzo con las herencias
  ggplot(aes( x = date_time, y = prediccion, col = modelo)) +
  # Agrega una linea para los observados
  geom_line(aes(y = Observados), col ='black', alpha = 0.1) +
  # Agrega una linea para las predicciones
  geom_line(alpha = 0.1) +
  # Agrega una linea de tendencia para los valores observados
  stat_smooth(aes(y = Observados), col ='black', lwd = 0.75,
              # Utiliza glm en una formula simple
              method = "glm", formula = y~x,
              # Agrega el argumento de liga log ya que fue la que usamos
              method.args = list(family = gaussian(link = 'log'))) +
  # Agrega una linea de tendencia para los valores estimados
  stat_smooth(lwd = 0.75,
              # Utiliza glm en una formula simple
              method = "glm", formula = y~x,
              # Agrega el argumento de liga log ya que fue la que usamos
              method.args = list(family = gaussian(link = 'log'))) +
  # Separa por modelo
  facet_wrap(~modelo, nrow = 1) +
  # Modifica los colores base
  scale_color_manual(values = c('red','orange','forestgreen')) +
  # Agrega los valores del RMSE
  geom_text(
    # Selecciona tus datos
    data = rmses, 
    # Agrega la etiqueta
    aes(label = paste0('RMSE: ', round(rmse,2))),
    # Define las coordenadas
    x =as.POSIXct('2016-01-01'), y = 160,
    # Modifica la letra
    family = 'Bebas Neue', size = 5) +
  # Agrega títulos
  ggtitle('Resultados de los Modelos de Regresión',
          subtitle = 'Número de vehículos por hora en la intersección 1') +
  #  Usa un tema predefinido
  theme_bw(base_size = 16) +
  # Haz modificaciones
  theme(
    # Cambia los textos
    text = element_text(family = 'Bebas Neue'),
    # Cambia los títulos
    axis.title = element_blank(),
    # Posición de la leyenda
    legend.position = 'none',
    # Quita el título de la leyenda
    legend.title = element_blank()
  ) 
