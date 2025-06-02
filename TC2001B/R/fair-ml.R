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

# Instalar - Cargar fairmodels                                                       
if(require(fairmodels) == FALSE){                                                
  install.packages('fairmodels')                                                 
  library(fairmodels)                                                            
}else{                                                                          
  library(fairmodels)                                                            
}

# Instalar - Cargar DT                                                       
if(require(DT) == FALSE){                                                
  install.packages('DT')                                                 
}

# Instalar - Cargar patchwork                                                       
if(require(patchwork) == FALSE){                                                
  install.packages('patchwork')                                                 
  library(patchwork)                                                            
}else{                                                                          
  library(patchwork)                                                            
}

# Data --------------------------------------------------------------------
data("compas")

# Revisar dartos
glimpse(compas)

# Limpiar nombres
compas = compas %>% 
  janitor::clean_names() 

# Checar datos
summary(compas)


# Modelo ML ---------------------------------------------------------------
# Separar las muestras de entrenamiento y prueba
data_split = initial_split(
  data = compas,
  strata = two_yr_recidivism
  ) 

# Entrenamiento
train = training(data_split)
# Prueba
test = testing(data_split)

# Pliegues de validación cruzada
folds = vfold_cv(
  data = train,
  v = 3,
  strata = two_yr_recidivism
)

# Receta
recipe_all = recipe(
  formula = two_yr_recidivism ~ ., 
  data = train
) 

# Bosque aleatorio
random_forest = rand_forest() %>% 
  set_engine("ranger", num.threads = parallel::detectCores()) %>% 
  set_mode("classification") %>% 
  set_args( 
    mtry = tune(),
    trees = tune(),
    min_n = tune()
  )

# Iniciamos un cluster para correr en paralelo
doParallel::registerDoParallel(cl = parallel::detectCores() - 1)

# Crear un conuunto de trabajo
ml_workflow = workflow_set(
  # Agregar conjunto de recetas
  preproc = list(
    recipe_all = recipe_all #, ... 
    ),
  # Agregar un conjunto de modelos
  models = list(
    random_forest = random_forest #, ... 
  )
) %>% 
  # Ajuste de las muestras
  workflow_map(
    seed = 123,
    resamples = folds,
    grid = 10,
    verbose = TRUE,
    control = control_grid( parallel_over = "everything")
  )

# Detener el cluster
doParallel::stopImplicitCluster()

# Graficar el desempeño de los modelos
autoplot(ml_workflow, metric = "accuracy")

# Seleccionar el mejor resultado de cada combinación
best_algorithms = rank_results(ml_workflow, rank_metric = "accuracy") %>% 
  group_by(wflow_id) %>% 
  filter(rank == min(rank), .metric == "accuracy")

print(best_algorithms)

# Extraer el mejor resutlado por algoritmo
best_rf = rank_results(ml_workflow, rank_metric = "accuracy") %>% 
  filter(model == 'rand_forest') %>% 
  filter(rank == min(rank), .metric == "accuracy")

# Seleccionar mejor modelo
best_rf_model = ml_workflow %>% 
  extract_workflow_set_result(id = pull(best_rf, wflow_id)) %>% 
  select_best(metric = 'accuracy') 

# Finalize workflow
compas_rf = ml_workflow %>% 
  extract_workflow(id = pull(best_rf, wflow_id))  %>%  
  finalize_workflow(parameters = best_rf_model) %>% 
  last_fit(data_split)

# Collect test mestrics
collect_metrics(compas_rf)

# Revisar la matriz de cofusión
cunfusion_matrix_rf = compas_rf %>% 
  extract_workflow() %>% 
  augment(compas) %>% 
  conf_mat(truth = two_yr_recidivism, estimate = .pred_class)

autoplot(cunfusion_matrix_rf, type ='heatmap')

# Resumen de métricas
summary(cunfusion_matrix_rf)

# Para entender merjor las métricas
# https://neptune.ai/blog/evaluation-metrics-binary-classification#:~:text=Simply%20put%20a%20classification%20metric,to%20classes%3A%20positive%20and%20negative.

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

# Extraer el modelo
rf_model = compas_rf %>% 
  extract_fit_parsnip() 

# Prerocesar la data
recipe_data = recipe_all %>% 
  prep() %>% 
  bake(new_data = compas)

# Crear un Explainer
explainer_modelo = explain_tidymodels(
  # Definir el modelo
  rf_model,
  # Definir los datos (predictores)
  data = select(recipe_data, -two_yr_recidivism),
  # Definir la variable objetivo
  y = pull(recipe_data, two_yr_recidivism) %>%  
    # La paqueteria nos pide un valor numérico
    as.character() %>% 
    as.numeric(),
  # Agregar una etiqueta
  label = 'Bosque Aleatorio'
)

# Importancia de variables
vip = model_parts(explainer_modelo) 
plot(vip)

# Desgloce por observación
obs = recipe_data %>% 
  filter(row_number() == 6000)

desgloce = break_down(explainer_modelo, obs)
plot(desgloce)

# Perfil de la variable (funciona solo para numéricos)
perfil = predict_profile(explainer_modelo, obs)
plot(perfil, variables = c('number_of_priors'))


# Justicia y paridad ------------------------------------------------------
data = rf_model %>% 
  augment(test)
  

data %>% 
  # Conteo de casos
  count(ethnicity, two_yr_recidivism, .pred_class) %>% 
  # COnstruir una matriz de confusión
  ggplot(aes(x = two_yr_recidivism, y = .pred_class, fill = n)) +
  # Mapa de calor
  geom_tile(show.legend = FALSE, na.rm = T) +
  # Agregar texto
  geom_text(aes(label = n), col = 'white') +
  # Separar por etnicidad
  facet_wrap(~ethnicity) +
  # Modificar etiquetas
  labs(x = 'Observed Recivism', y = 'Predicted Recidivism') +
  theme_minimal()


data %>% 
  # Contar casos
  count(ethnicity, two_yr_recidivism, .pred_class) %>%
  # Calcular porcentaje por grupo
  with_groups(
    .groups = ethnicity, 
    mutate,
    porcentaje = 100 * n/sum(n)
  ) %>% 
  # Construir matriz de confusión
  ggplot(aes(x = two_yr_recidivism, y = .pred_class, fill = porcentaje)) +
  # Mapa de calor
  geom_tile(show.legend = FALSE) +
  # Agregar texto
  geom_text(aes(label = round(porcentaje,2)), col = 'white') +
  # Separar por etnicidad
  facet_wrap(~ethnicity) +
  # Modificar etiquetas
  labs(x = 'Observed Recivism', y = 'Predicted Recidivism') +
  theme_minimal()


# Por esto es importante hacer el EDA
data %>% 
  # Contar casos
  count(ethnicity, two_yr_recidivism) %>% 
  # Crear un gráfico de barras
  ggplot(aes(x = n, y = reorder(ethnicity, n))) +
  geom_col() +
  # Separar por casos
  facet_wrap(~two_yr_recidivism) +
  # Agregar etiquetas
  labs(x = 'Observed Cases', title = 'Two Year Recidivism') +
  # Modificar el tema
  theme_minimal() +
  theme(axis.title.y = element_blank())


# AEQUITAS ----------------------------------------------------------------
# https://arxiv.org/abs/2405.05809
# https://www.datascienceforsocialgood.org/

data %>% 
  # Calcular la precisión por grupo
  with_groups(
    .groups = ethnicity,
    summarise,
    # Verdaderos positivos
    tp = sum(.pred_class == '1' & two_yr_recidivism == '1'),
    # Verdaderos negativos
    tn = sum(.pred_class == '0' & two_yr_recidivism == '0'),
    # Total por grupo
    group_size = n(),
    # Precision
    accuracy = (tp + tn) / group_size
  ) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  DT::datatable()

# Calcular las métricas de predicción por grupo
data %>% 
  with_groups(
    .groups = ethnicity,
    summarise,
    # Positivos predichis
    pp = sum(.pred_class == '1'),
    # Negativos predichos
    pn = sum(.pred_class == '0'),
    # Falsos positivos
    fp = sum(.pred_class == '1' & two_yr_recidivism == '0'),
    # Falsos negativos
    fn = sum(.pred_class == '0' & two_yr_recidivism == '1'),
    # Verdaderos positivos
    tp = sum(.pred_class == '1' & two_yr_recidivism == '1'),
    # Verdaderos negativos
    tn = sum(.pred_class == '0' & two_yr_recidivism == '0'),
    # Positivos totales
    group_label_pos = sum(two_yr_recidivism == '1'),
    # Negativos totales
    group_label_neg = sum(two_yr_recidivism == '0'),
    # Total de observaciones por grupo
    group_size = n(),
    # Precisión
    accuracy = (tp + tn) / group_size,
    # Tasa de verdaderos positivos
    tpr =  tp / (tp + fn),
    # Tasa de verdaderos negativos
    tnr = tn / (tn + fp),
    `for` = fn / (tn + fn),
    # Tasa de descubrimiento de falsos
    fdr = tp / (tp + fp),
    # Tasa de descubrimiento de falsos positivos
    fpr = fp / (tn + fp),
    # Tasa de descubrimiento de falsos negativos
    fnr = fn / (tp + fn),
    # Valor de positivios predichos
    ppv = 1 - fdr
  ) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  t() %>% 
  DT::datatable()

# Esta es la fucnión generalizada
metrics = function(df, group){
  df = df %>% 
    with_groups(
      .groups = group,
      summarise,
      # Positivos predichis
      pp = sum(.pred_class == '1'),
      # Negativos predichos
      pn = sum(.pred_class == '0'),
      # Falsos positivos
      fp = sum(.pred_class == '1' & two_yr_recidivism == '0'),
      # Falsos negativos
      fn = sum(.pred_class == '0' & two_yr_recidivism == '1'),
      # Verdaderos positivos
      tp = sum(.pred_class == '1' & two_yr_recidivism == '1'),
      # Verdaderos negativos
      tn = sum(.pred_class == '0' & two_yr_recidivism == '0'),
      # Positivos totales
      group_label_pos = sum(two_yr_recidivism == '1'),
      # Negativos totales
      group_label_neg = sum(two_yr_recidivism == '0'),
      # Total de observaciones por grupo
      group_size = n(),
      # Precisión
      accuracy = (tp + tn) / group_size,
      # Tasa de verdaderos positivos
      tpr =  tp / (tp + fn),
      # Tasa de verdaderos negativos
      tnr = tn / (tn + fp),
      `for` = fn / (tn + fn),
      # Tasa de descubrimiento de falsos
      fdr = tp / (tp + fp),
      # Tasa de descubrimiento de falsos positivos
      fpr = fp / (tn + fp),
      # Tasa de descubrimiento de falsos negativos
      fnr = fn / (tp + fn),
      # Valor de positivios predichos
      ppv = 1 - fdr
    ) %>% 
    # Renombrar caso
    rename(case = group) %>% 
    mutate(
      # Caso como caracter
      case = as.character(case),
      # Categoría
      cat = group
      )
  
  return(df)
}

# Ejemplo
metrics(df = data, group = 'sex') %>% 
  t()

# Extraer resultados usando funciones map
results = map_df(
  # Definir un caso de referencia
  .x = c('ethnicity','sex','age_below_twenty_five'),
  .f = metrics,
  df = data
)  %>% 
  # Formato long
  gather(metric, value, -case, - cat) %>% 
  # Modificar caso
  mutate(
    case = case_when(
      cat == 'age_below_twenty_five' & case == '1' ~ 'Age below 25', 
      cat == 'age_below_twenty_five' & case == '0' ~ 'Age below 25',
      TRUE ~ case
      )
  )
 
# Calcular disparidad
disparity = results %>% 
  # Seleccionar caso de referencia
  filter(case %in% c('Caucasian','Age below 25','Male')) %>% 
  # Quitar columna
  select(-case) %>% 
  # Concatenar por la izquierda
  left_join(
    y = results, 
    by = c('cat', 'metric'),
    suffix = c('_base','_group')
    ) %>% 
  mutate(
    # Caluclar disparidad
    disparity = value_group /value_base,
    # Sesgos
    bias = case_when(
      disparity > 1.25 ~ 'Penalized',
      disparity < 0.75 ~ 'Privileged', 
      TRUE ~ 'Neutral')
  ) 


# Create a plot
disparity %>% 
  # Filtrar caso
  filter(cat == 'ethnicity') %>% 
  # Crear un lienzo
  ggplot(
    aes(
      x = disparity, 
      y = reorder(case, disparity), 
      col = bias)
    ) +
  # Agregar lineas de rangos
  geom_vline(
    xintercept = c(0.75,1.25), 
    linetype = 'dashed', 
    linewidth = 0.25
    ) +
  # Agregar puntos
  geom_point() +
  # Separar por metrica
  facet_wrap(~metric) +
  # Agregar etiquetas
  labs(
    title = 'Aequitas Analysis by Race',
    subtitle = 'Baseline: Caucasian',
    col = 'Bias'
    ) +
  # Modificar tema
  theme_bw() +
  theme(
    axis.title = element_blank(),
    legend.position = c(0.5,0.1)
  )

# Create a plot
disparity %>% 
  # Filtrar caso
  filter(cat == 'sex') %>% 
  # Crear un lienzo
  ggplot(
    aes(
      x = disparity, 
      y = reorder(case, disparity), 
      col = bias)
  ) +
  # Agregar lineas de rangos
  geom_vline(
    xintercept = c(0.75,1.25), 
    linetype = 'dashed', 
    linewidth = 0.25
  ) +
  # Agregar puntos
  geom_point() +
  # Separar por metrica
  facet_wrap(~metric) +
  # Agregar etiquetas
  labs(
    title = 'Aequitas Analysis by Sex',
    subtitle = 'Baseline: male',
    col = 'Bias'
  ) +
  # Modificar tema
  theme_bw() +
  theme(
    axis.title = element_blank(),
    legend.position = c(0.5,0.1)
  )

# Usando fairmodels
fobject =  fairness_check(
  # Explainer
  x = explainer_modelo,             
  # Grupo
  protected = pull(compas, ethnicity),    
  # Caso base
  privileged = "Caucasian",         
  # Punto de corte
  cutoff = 0.5                        
)  
# Mapa de columnas
plot(fobject)

# Densidad de probabilidades
plot_density(fobject)

# Metricas del objeto
plot(metric_scores(fobject))
fobject$parity_loss_metric_data
fobject$groups_data


# Mitigación de sesgos ----------------------------------------------------

#####################
# Preprocesamientos # 
#####################
# (Feldman et al. (2015). 
# Esta técnica devuelve un marco de datos fijo. Mediante el parámetro lambda, 
# podemos manipular el grado de fijación de la distribución. 
# Lambda = 1 (predeterminado) devolverá datos con distribuciones idénticas para 
# todos los niveles de la variable protegida, mientras que lambda = 0 
# prácticamente no cambiará nada. Transformaremos algunas características.
data_fixed = disparate_impact_remover(
  data = compas, 
  protected = pull(compas, ethnicity), 
  features_to_transform = c('number_of_priors')
)

# Resultado
qplot(data_fixed$number_of_priors, xlab = 'Nomber of priors (Impact Remover)') /
qplot(compas$number_of_priors, xlab = 'Nomber of priors (Original)')

# Separar las muestras de entrenamiento y prueba
data_split = initial_split(
  data = data_fixed,
  strata = two_yr_recidivism
) 

# Entrenamiento
train = training(data_split)
# Prueba
test = testing(data_split)

# Pliegues de validación cruzada
folds = vfold_cv(
  data = train,
  v = 3,
  strata = two_yr_recidivism
)

# Receta
recipe_all = recipe(
  formula = two_yr_recidivism ~ ., 
  data = train
) 

# Crear el flujo de trabajo
flujo_bosque = workflow() %>% 
  add_recipe(recipe_all) %>% 
  add_model(random_forest) 

# Ajusta con flujo de trabajo 
bosque_parametros = flujo_bosque %>% 
  extract_parameter_set_dials() 

# Crear una maya de calibración
set.seed(123)
# Arranca un cluster
doParallel::registerDoParallel()

# Entrena en paralelo 100 modelos
bosque_tuning = tune_grid(
  # Define el objeto a utilizar
  object = flujo_bosque,
  # Método de remuestreo
  resamples = folds,
  # Parametros a calibrar
  param_info = bosque_parametros,
  # Métricas de evaluación
  metrics = metric_set(yardstick::accuracy),
  # Parámetros de control
  control = control_grid(parallel_over = "everything"),
  # Número de combinaciones a probar
  grid = 10
)

# Detener el cluster =
doParallel::stopImplicitCluster()

# Seleccionar el mejor modelo
best_bosque = bosque_tuning %>%
  select_best(metric = "accuracy")

# Imprime el mejor modelo
print(best_bosque)

# Finalizar el flujo de trabajo
final_bosque = flujo_bosque %>% 
  finalize_workflow(best_bosque)

# Ejecutar el último ajuste
resultados_bosque = final_bosque %>% 
  last_fit(
    split = data_split,
    metrics = metric_set(yardstick::accuracy)
  )

# Extraer modelo
bosque_impact_remover = resultados_bosque %>% 
  extract_fit_parsnip() 

# Crear un Explainer
explainer_impact_remover = explain_tidymodels(
  # Definir el modelo
  bosque_impact_remover,
  # Definir los datos
  data = select(recipe_data, -two_yr_recidivism),
  # Definir la variable objetivo
  y = pull(recipe_data, two_yr_recidivism) %>%  
    # La paqueteria nos pide un valor numérico
    as.character() %>% 
    as.numeric(),
  # Agregar una etiqueta
  label = 'Impact Remover'
)

fobject <- fairness_check(
  # Lista de explainers
  explainer_modelo, explainer_impact_remover,
  # Variable protegida
  protected = pull(recipe_data, ethnicity), 
  # Caso de referencia
  privileged = "Caucasian"
  )
plot(fobject)

########################
### Re-ponderaciones ###
########################

# La reponderación (Kamiran y Calders (2011)) es una técnica sencilla de mitigación de sesgos. 
# Genera ponderaciones basadas en datos para pasarlas al modelo, de modo que este pueda aprender 
# a tener cuidado. Dado que puede haber múltiples subgrupos, las ponderaciones se presentarán 
# en forma de vector.
weights = reweight(
  protected = pull(compas, ethnicity), 
  y = pull(compas, two_yr_recidivism) %>% as.numeric() - 1
  )

data_weights = compas %>% 
  mutate(weights = importance_weights(weights))

# Separar las muestras de entrenamiento y prueba
data_split = initial_split(
  data = data_weights,
  strata = two_yr_recidivism
) 

# Entrenamiento
train = training(data_split)
# Prueba
test = testing(data_split)

# Pliegues de validación cruzada
folds = vfold_cv(
  data = train,
  v = 3,
  strata = two_yr_recidivism
)

# Receta
recipe_all = recipe(
  formula = two_yr_recidivism ~ ., 
  data = train
) 

# Crear el flujo de trabajo
flujo_bosque = workflow() %>% 
  add_recipe(recipe_all) %>% 
  add_model(random_forest) %>% 
  add_case_weights(weights)

# Ajusta con flujo de trabajo 
bosque_parametros = flujo_bosque %>% 
  extract_parameter_set_dials() 

# Crear una maya de calibración
set.seed(123)
# Arranca un cluster
doParallel::registerDoParallel()

# Entrena en paralelo 100 modelos
bosque_tuning = tune_grid(
  # Define el objeto a utilizar
  object = flujo_bosque,
  # Método de remuestreo
  resamples = folds,
  # Parametros a calibrar
  param_info = bosque_parametros,
  # Métricas de evaluación
  metrics = metric_set(yardstick::accuracy),
  # Parámetros de control
  control = control_grid(parallel_over = "everything"),
  # Número de combinaciones a probar
  grid = 10
)

# Detener el cluster 
doParallel::stopImplicitCluster()

# Seleccionar el mejor modelo
best_bosque = bosque_tuning %>%
  select_best(metric = "accuracy")

# Imprime el mejor modelo
print(best_bosque)

# Finalizar el flujo de trabajo
final_bosque = flujo_bosque %>% 
  finalize_workflow(best_bosque)

# Ejecutar el último ajuste
resultados_bosque = final_bosque %>% 
  last_fit(
    split = data_split,
    metrics = metric_set(yardstick::accuracy)
  )

# Extraer modelo
bosque_weights = resultados_bosque %>% 
  extract_fit_parsnip() 

# Crear un Explainer
explainer_weights = explain_tidymodels(
  # Definir el modelo
  bosque_weights,
  # Definir los datos
  data = select(recipe_data, -two_yr_recidivism),
  # Definir la variable objetivo
  y = pull(recipe_data, two_yr_recidivism) %>%  
    # La paqueteria nos pide un valor numérico
    as.character() %>% 
    as.numeric(),
  # Agregar una etiqueta
  label = 'Weights'
)

fobject <- fairness_check(
  # Lista de explainers
  explainer_modelo, explainer_impact_remover, explainer_weights,
  # Variable protegida
  protected = pull(recipe_data, ethnicity), 
  # Caso de referencia
  privileged = "Caucasian"
)
plot(fobject)

##############
# Remuestreo #
##############

# Este método se deriva de la reponderación de los datos pero en lugar de 
# ponderaciones elige observaciones de los datos, el resultado de las métricas 
# (Kamiran y Calders (2011)).

indexes = resample(
  protected = pull(compas, ethnicity), 
  y = pull(compas, two_yr_recidivism) %>% as.numeric() - 1
)

data_resample = compas[indexes, ]

# Separar las muestras de entrenamiento y prueba
data_split = initial_split(
  data = data_resample,
  strata = two_yr_recidivism
) 

# Entrenamiento
train = training(data_split)
# Prueba
test = testing(data_split)

# Pliegues de validación cruzada
folds = vfold_cv(
  data = train,
  v = 3,
  strata = two_yr_recidivism
)

# Receta
recipe_all = recipe(
  formula = two_yr_recidivism ~ ., 
  data = train
) 

# Crear el flujo de trabajo
flujo_bosque = workflow() %>% 
  add_recipe(recipe_all) %>% 
  add_model(random_forest) 

# Ajusta con flujo de trabajo 
bosque_parametros = flujo_bosque %>% 
  extract_parameter_set_dials() 

# Crear una maya de calibración
set.seed(123)
# Arranca un cluster
doParallel::registerDoParallel()

# Entrena en paralelo 100 modelos
bosque_tuning = tune_grid(
  # Define el objeto a utilizar
  object = flujo_bosque,
  # Método de remuestreo
  resamples = folds,
  # Parametros a calibrar
  param_info = bosque_parametros,
  # Métricas de evaluación
  metrics = metric_set(yardstick::accuracy),
  # Parámetros de control
  control = control_grid(parallel_over = "everything"),
  # Número de combinaciones a probar
  grid = 10
)

# Detener el cluster 
doParallel::stopImplicitCluster()

# Seleccionar el mejor modelo
best_bosque = bosque_tuning %>%
  select_best(metric = "accuracy")

# Imprime el mejor modelo
print(best_bosque)

# Finalizar el flujo de trabajo
final_bosque = flujo_bosque %>% 
  finalize_workflow(best_bosque)

# Ejecutar el último ajuste
resultados_bosque = final_bosque %>% 
  last_fit(
    split = data_split,
    metrics = metric_set(yardstick::accuracy)
  )

# Extraer modelo
bosque_resample = resultados_bosque %>% 
  extract_fit_parsnip() 

# Crear un Explainer
explainer_resample = explain_tidymodels(
  # Definir el modelo
  bosque_resample,
  # Definir los datos
  data = select(recipe_data, -two_yr_recidivism),
  # Definir la variable objetivo
  y = pull(recipe_data, two_yr_recidivism) %>%  
    # La paqueteria nos pide un valor numérico
    as.character() %>% 
    as.numeric(),
  # Agregar una etiqueta
  label = 'Resample'
)

fobject <- fairness_check(
  # Lista de explainers
  explainer_modelo, explainer_impact_remover, 
  explainer_weights, explainer_resample,
  # Variable protegida
  protected = pull(recipe_data, ethnicity), 
  # Caso de referencia
  privileged = "Caucasian"
)
plot(fobject)

##################################
# Manipuación del punto de corte #
##################################
glimpse(data)

# La manipulación de puntos de corte es una técnica sencilla que permite 
# establecer puntos de corte diferentes para distintos subgrupos. 
# Es algo controvertida, ya que aumenta la injusticia individual (personas 
# similares pueden tener puntos de corte diferentes y, por lo tanto, 
# no reciben el mismo trato).
cut_off = fairness_check(
  # Lista de explainers
  fobject, 
  # Variable protegida
  protected = pull(recipe_data, ethnicity), 
  # Caso de referencia
  privileged = "Caucasian"
)

plot(ceteris_paribus_cutoff(cut_off, subgroup = "Asian"))
plot(ceteris_paribus_cutoff(cut_off, subgroup = "African_American"))

# Definir los puntos de cortes
cut_off = fairness_check(
  # Lista de explainers
  fobject, 
  # Variable para modificar los puntos de corte
  cutoff = list(African_American = 0.61, Asian = 0.4),
)
plot(cut_off)

###################################
# Trade-off precisión vs justicia #
###################################

# El equilibrio es significativo y siempre debe tenerse en cuenta. 
# Comprobación de la imparcialidad en un conjunto de prueba.
# Al desarrollar un modelo estándar, los desarrolladores suelen dividir los datos 
# en subconjuntos de entrenamiento y prueba. No solo es posible, sino también 
# recomendable, comprobar la imparcialidad en un conjunto de prueba. Esto puede 
# hacerse de la siguiente manera:

paf = performance_and_fairness(
  fobject, 
  performance_metric = "accuracy"
  )

print(paf)

plot(paf)
