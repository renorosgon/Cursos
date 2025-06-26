# Fijar directorio de trabajo
setwd("~/Desktop/ITESM/Cursos/EC3001C/")


# Librerías ---------------------------------------------------------------
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}
if(require(tidymodels) == FALSE){                                                
  install.packages('tidymodels')                                                 
  library(tidymodels)                                                            
}else{                                                                          
  library(tidymodels)                                                            
}
if(require(poissonreg) == FALSE){                                                
  install.packages('poissonreg')                                                 
  library(poissonreg)                                                            
}else{                                                                          
  library(poissonreg)                                                            
}

# Leer archivos -----------------------------------------------------------
##### Vía loop ##### 
lista_archivos = list.files(
  path = './data/servicios_integrales_LOCALTEL',
  full.names = TRUE
  )

# Crear un tibble vacio
df = c()
# Iterar para cada archivo en la lista
for(archivo in lista_archivos){
  # Leer
  df = read_csv(archivo) %>% 
    # Filtrar
    filter(sexo == "FEMENINO", servicio == "JURÍDICO") %>% 
    # Mutar
    mutate(
      cp_usuaria = as.character(cp_usuaria),
      cp_hechos = as.character(cp_hechos)
      ) %>% 
    # Concatenar
    bind_rows(df)
}

##### Forma artesanal/manual ##### 
df_1 = read_csv(...)
df_2 = read_csv(...)
df_3 = read_csv(...)

##### Tidyverse ##### 
# Leer
df = read_csv("./data/servicios_integrales_LOCALTEL/servicios_integrales_2016-2018.csv")%>% 
  # Filtrar
  filter(sexo == "FEMENINO", servicio == "JURÍDICO") %>% 
  # Mutar
  mutate(
    cp_usuaria = as.character(cp_usuaria),
    cp_hechos = as.character(cp_hechos)
  ) %>%  
  # Concatenar
  bind_rows(
    # Leer
    read_csv("./data/servicios_integrales_LOCALTEL/servicios_integrales_2019-2021.csv")%>% 
      # Filtrar
      filter(sexo == "FEMENINO", servicio == "JURÍDICO") %>% 
      # Mutar
      mutate(
        cp_usuaria = as.character(cp_usuaria),
        cp_hechos = as.character(cp_hechos)
      ) 
  ) %>%  
  # Concatenar
  bind_rows(
    # Leer
    read_csv("./data/servicios_integrales_LOCALTEL/servicios_integrales_2022-2023.csv")%>% 
      # Filtrar
      filter(sexo == "FEMENINO", servicio == "JURÍDICO") %>% 
      # Mutar
      mutate(
        cp_usuaria = as.character(cp_usuaria),
        cp_hechos = as.character(cp_hechos)
      ) 
  )

##### Tidyverse PRO ##### 
# Aplicar una función a una lista con map
df = map_df(
  .x = lista_archivos,
  .f = read_csv,
  # Definir tipo de columnas
  col_types = cols(cp_usuaria = col_character(), cp_hechos = col_character())
) %>% 
  # Filtrar
  filter(sexo == "FEMENINO", servicio == "JURÍDICO") 


# EDA ---------------------------------------------------------------------
summary(df)
glimpse(df)

# Guardamos los datos del "futuro"
futuro = df %>% 
  # Filtrar donde la fecha es mayor al 1 de mayo de 2024
  filter(fecha_alta >= '2023-05-01')  %>% 
  # Mutamos una fecha con hora
  mutate(fecha_hora = ymd_h(paste(fecha_alta, hour(hora_alta)))) %>% 
  # Contamos las observaciones por fecha y hora
  count(fecha_hora)

# Guardamos el resto de los datos 
datos = df  %>% 
  # Filtrar donde la fecha es menor al 1 de mayo de 2024
  filter(fecha_alta < '2023-05-01') %>% 
  # Mutamos una fecha con hora
  mutate(fecha_hora = ymd_h(paste(fecha_alta, hour(hora_alta)))) %>% 
  # Contamos las observaciones por fecha y hora
  count(fecha_hora)

# Resumen de los datos
summary(datos)

# Un gráfico de línea
datos %>% 
  ggplot(aes(x = fecha_hora, y = n)) +
  geom_line()


# Gráfico de columnas
datos %>% 
  ggplot(aes(x = factor(hour(fecha_hora)), y = n)) +
  geom_col() +
  # Separando los lienzos por año y mes
  facet_grid(year(fecha_hora)~month(fecha_hora, label = T))

# Mapa de galor
datos %>% 
  ggplot(
    # Definir ejes y y x
    aes(y = factor(hour(fecha_hora)), x = factor(wday(fecha_hora, label = T)), 
        # Definir relleno
        fill = log(n))) +
  # Agregar tejas
  geom_tile() +
  # Cambiar escala de relleno
  scale_fill_viridis_c() +
  # Separando los lienzos por año y mes
  facet_grid(year(fecha_hora)~month(fecha_hora, label = T))


# Mapa de rezagos
datos %>% 
  mutate(diff = log(n/lag(n))) %>% 
  ggplot(aes(x = n)) +
  geom_histogram()

# Medidas de centralidad y dispersión
pull(datos, n) %>%  var()
pull(datos, n) %>%  mean()

# Agregando por distintos momentos
datos %>% 
  group_by(
    year(fecha_hora),
    month(fecha_hora),
    day(fecha_hora)
    ) %>% 
  summarise(
    var = var(n),
    mean = mean(n)
  ) %>% 
  glimpse()

# Las semillas aleatorias nos ayudan a fijar la aleatoriedad
set.seed(35)
# Separa los datos en dos conjuntos considerando el tiempo
data_split = initial_time_split(datos, prop = .85)
data_split = initial_split(
  data = datos,
  strata = n
)
# Extraer el conjunto de prueba y entrenamiento
train = training(data_split) 
test = testing(data_split)

# La diferencia 
summary(train)
summary(test)

# Validacion cruzada intertemporal
rolling_origin = rolling_origin(
  train,
  initial = 90 * 24, # Primeros 120 días por 24 horas
  assess = 61 * 24, # Evaluar el siguiente mes las 24 horas
  cumulative = FALSE, # Acumulado
  skip = 61 * 24 # Saltos de 150 días
)

# Validacion cruzada tradicinoal
# folds = vfold_cv(train, v = 10, strata = n)

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
      min = min(fecha_hora),
      max = max(fecha_hora)
    )
  # Resumen de cada pliegue de prueba
  assessment_set = rolling_origin %>% 
    pluck('splits') %>% 
    pluck(fold_num) %>% 
    assessment()%>% 
    summarize(
      fold = fold_num,
      set = 'Assessment',
      min = min(fecha_hora),
      max = max(fecha_hora)
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
    legend.position = c(0.1, 0.95),
    legend.key.height = unit(0.5,'cm')
  )

# Define el modelo de ajuste
regresion_poisson = poisson_reg() %>% 
  # Selecciona el motor a utilizar
  set_engine('glmnet') %>% 
  # Fija el problema a resolver
  set_mode('regression') %>%  
  # Fija los hiperparametros
  set_args(penalty = tune(), mixture = tune())

receta = recipe(
  # Define la ecuación
  formula = n ~ .,
  # Define los datos a utilizar
  data = train
) %>% 
  step_mutate(
    hora = hour(fecha_hora),
    dia = wday(fecha_hora),
    mes = month(fecha_hora),
    año = year(fecha_hora)
  ) %>% 
  update_role(fecha_hora, new_role = 'ID') %>% 
  # Create the polynomial variable
  # step_poly(año, degree = 2) %>% 
  step_poly(mes, degree = 2) %>% 
  step_poly(dia, degree = 6) %>% 
  step_poly(hora, degree = 6)

# Esto es lo que hace nuestra receta
receta %>%
  prep() %>% 
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
  metrics = metric_set(yardstick::mae),
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
  select_best(metric = "mae")

# Imprime el mejor modelo
print(best_poisson)

# Finalizar el flujo de trabajo
final_poisson = poisson_workflow %>% 
  finalize_workflow(best_poisson)

# Ejecutar el último ajuste
resultados_poisson = final_poisson %>% 
  last_fit(
    split = data_split,
    metrics = metric_set(yardstick::mae)
  )

# Recolectar las métricas de prueba
metricas_poisson = resultados_poisson %>% 
  collect_metrics() 

print(metricas_poisson)

glmnet_trained = resultados_poisson %>% 
  extract_fit_parsnip() 
print(glmnet_trained)

# Coeficientes
coefficients = tidy(glmnet_trained)
print(coefficients)

# Incluir
preproc = receta %>% 
  prep() %>% 
  bake(new_data = futuro)

# Make predictions
predictions_tb =  augment(glmnet_trained, preproc) 

glimpse(predictions_tb)


# Revisa como de desempeña el conjunto completo
mae_poisson = predictions_tb %>%  
  # Calcula el rmse
  yardstick::mae(n,.pred) %>% 
  pull(.estimate)

mae_poisson

# Realiza una gráfica del ajuste
predictions_tb %>% 
  ggplot(
    # Esta estética se hereda a todas las geometrías
    aes( x = fecha_hora )
  ) +
  # Agrega una línea con los valores observados
  geom_line(aes(y = n, col = 'Llamadas'),
            # Modifica el ancho de línea
            linewidth = 1
  ) +
  # Agrega una línea con los valores estimados
  geom_line(aes(y = .pred , col = 'Predicciones'),
            # Modifica el ancho de línea
            linewidth = 1
  ) +
  # Modifica los colores base
  scale_color_manual(values = c('gray50','#003399')) +
  # Agrega títulos
  ggtitle('Resultados del Modelo Poisson-Glmnet',
          subtitle = 'Número de llamadas por hora de mujeres buscando asesoría jurídica') +
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





