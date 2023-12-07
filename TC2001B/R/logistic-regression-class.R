setwd("~/Desktop/ITESM/Cursos/TC2001B/")

# Libraries ---------------------------------------------------------------
# Install - load tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}
# Install - load tidymodels                                                       
if(require(tidymodels) == FALSE){                                                
  install.packages('tidymodels')                                                 
  library(tidymodels)                                                            
}else{                                                                          
  library(tidymodels)                                                            
}
# Instalar - Cargar patchwork                                                       
if(require(patchwork) == FALSE){                                                
  install.packages('patchwork')                                                 
  library(patchwork)                                                            
}else{                                                                          
  library(patchwork)                                                            
}
# Instalar - Cargar vip                                                       
if(require(vip) == FALSE){                                                
  install.packages('vip')                                                 
  library(vip)                                                            
}else{                                                                          
  library(vip)                                                            
}
# Install - load poliscidata                                                       
if(require(poliscidata) == FALSE){                                                
  install.packages('poliscidata')                                                 
}
# Install - load GGally                                                       
if(require(GGally) == FALSE){                                                
  install.packages('GGally')                                                 
}

# Data --------------------------------------------------------------------
nes = force(poliscidata::nesD) %>% 
  pluck('variables') %>% 
  dplyr::select( obama_vote, ft_dem , ft_rep , black , hispanic , income5) %>% 
  filter(!is.na(obama_vote)) %>% 
  mutate(obama_vote = factor(obama_vote, levels = c(0,1), labels = c('no','yes')))

# EDA
glimpse(nes)
summary(nes)
GGally::ggpairs(nes)

# Logistic Function
logist <- function(x){
  y = exp(x) / (1 + exp(x))
}

# Graphing Logistic Regression
nes %>% 
  mutate(y = as.numeric(obama_vote) - 1) %>% 
 # mutate(y = as.numeric(Vote) - 1) %>% 
  ggplot(aes(x = ft_dem, y = y, col = income5)) +
  geom_point() + 
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial)) +
  labs(
    title = 'Lowe income voters were more likely to vote for Obama',
    x = 'Feeling Thermoteter: Republican Party',
    subtitle = 'Voted for Obama',
    col = 'Income Quintile'
  ) + facet_wrap(~income5) + 
  #  Usa un tema predefinido
  theme_bw() +
  # Haz modificaciones
  theme(
    # Cambia los textos
    text = element_text(family = 'Bebas Neue'),
    # Cambia los títulos
    axis.title.y = element_blank(),
  )
  
# Separa en conjunto de prueba y entrenamiento
data_split = nes %>% 
  initial_split(
    strata = obama_vote
  )
entrenamiento = training(data_split)
prueba = testing(data_split)

## Regresión logistica logistico
reg_logistica = logistic_reg() %>% 
  # Fija el motor
  set_engine('glm') %>% 
  # Fija el modo
  set_mode('classification')

# Crea una receta
receta = recipe(formula = obama_vote ~ ., data = entrenamiento) %>%
  step_impute_bag(all_predictors()) %>% 
  step_dummy(all_nominal_predictors()) 

# Construye las muestras de validación cruzada con los datos de entrenamiento
validacion_cruzada = vfold_cv(entrenamiento, v = 5)


# Crea un flujo de trabajo
modelo_logistico = workflow() %>%
  # Añade una receta
  add_recipe(receta) %>% 
  # Agrega un modelo
  add_model(reg_logistica) 

# Ajusta el modelo logistico
ajuste_con_validacion = modelo_logistico %>% 
  fit_resamples(
    # Usa las muestras de validación
    resamples = validacion_cruzada,
    # Define las metricas de criterio
    metrics = metric_set(accuracy, roc_auc)
  )

# Imprime las métricas
metricas_logistico = ajuste_con_validacion %>% 
  collect_metrics()

print(metricas_logistico)

# Ajusta el modelo final
ajuste_ml_final = modelo_logistico %>% 
  last_fit(data_split) 

# Revisa el conjunto de entrenamiento 
matriz_entrenamiento = ajuste_ml_final %>%  
  # Extrae el flujo de trabajo
  extract_workflow() %>% 
  # Ajusta en el entrenamiento completo
  augment(entrenamiento) %>% 
  # Revisa la matriz de confusion
  conf_mat(truth = obama_vote, estimate = .pred_class) %>% 
  # Crea un mapa de calor
  autoplot(type = 'heatmap') 


# Ajusta el modelo final
ajuste_ml_final = modelo_logistico %>% 
  last_fit(data_split) 

# Construye la matriz de confusión con los datos de prueba
matriz_prueba =  ajuste_ml_final %>% 
  collect_predictions() %>% 
  conf_mat(truth = obama_vote, estimate = .pred_class) %>% 
  # Crea un mapa de calor
  autoplot(type = 'heatmap')

# Gracias a patchwork podemos combinar nuestros gráficos
matriz_entrenamiento + matriz_prueba
matriz_entrenamiento / matriz_prueba

# Extrae el resumen de todas las metricas
ajuste_ml_final %>% 
  collect_predictions() %>% 
  conf_mat(truth = obama_vote, estimate = .pred_class) %>% 
  summary()


# Regresión de ridge
reg_logistica_ridge = logistic_reg() %>% 
  # Define el motor
  set_engine('glmnet') %>% 
  # Define el modo
  set_mode('classification') %>% 
  # Define los hyperparametros
  set_args(penalty = tune(), mixture = 0)

# Qué hace la regularización de ridge
ridge_fit = fit(reg_logistica_ridge, obama_vote ~ ., data = receta %>% prep %>% juice)
# Visualmente
ridge_plot = ridge_fit %>%
  autoplot() +
  # Agrega títulos
  ggtitle('Ridge regularization',
          subtitle = 'Coeffient penalization by class') +
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

# Regresión de lasso
reg_logistica_lasso = logistic_reg() %>% 
  # Define el motor
  set_engine('glmnet') %>% 
  # Define el modo
  set_mode('classification') %>% 
  # Define los hyperparametros
  set_args(penalty = tune(), mixture = 1)

# Qué hace la regularización de lasso
lasso_fit = fit(reg_logistica_lasso, obama_vote ~ ., data = receta %>% prep %>% juice())
# Visualmente
lasso_plot = lasso_fit %>%
  autoplot() +
  # Agrega títulos
  ggtitle('lasso regularization',
          subtitle = 'Coeffient penalization by class') +
  #  Usa un tema predefinido
  theme_bw() +
  # Haz modificaciones
  theme(
    # Cambia los textos
    text = element_text(family = 'Bebas Neue'),
    # Posición de la leyenda
    legend.position = 'bottom',
    # Quita el título de la leyenda
    legend.title = element_blank(),
    axis.title.y = element_blank()
  ) 

ridge_plot + lasso_plot


# Regresión de glmnet
reg_logistica_glmnet = logistic_reg() %>% 
  # Define el motor
  set_engine('glmnet') %>% 
  # Define el modo
  set_mode('classification') %>% 
  # Define los hyperparametros
  set_args(penalty = tune(), mixture = tune())

# Crea un flujo de trabajo
modelo_logistico_glmnet = workflow() %>%
  # Añade una receta
  add_recipe(receta) %>% 
  # Agrega un modelo
  add_model(reg_logistica_glmnet)

# Construye una malla para optimizar
glmnet_parametros = modelo_logistico_glmnet %>% 
  extract_parameter_set_dials() 

# Agrega elementos de control
ctrl = control_grid(save_pred = FALSE, verbose = TRUE)

# Ajusta el modelo de glmnet
ajuste_glmnet = modelo_logistico_glmnet %>% 
  tune_grid(
    # Usa las muestras de validación cruzada
    resamples = validacion_cruzada,
    # Parametros a calibrar
    param_info = glmnet_parametros,
    # Recorre la maya de hyperparametros
    grid = 100,
    # Agrega los elementos de control
    control = ctrl,
    # Define las métricas
    metrics = metric_set(accuracy, sens, spec, roc_auc, kap))

# Imprime las métricas del modelo glmnet       
metricas_glmnet = ajuste_glmnet %>% 
  collect_metrics() 

print(metricas_glmnet)

# Grafica las métricas
metricas_glmnet %>%
  # Agrega el lienzo con las estéticas que se heredan
  ggplot(aes(penalty, mean, color = mixture)) +
  scale_x_log10() + 
  # Agrega una línea
  geom_point(size = 1) +
  # Separa por métrica
  facet_wrap(~.metric, scales = "free", ncol = 1) +
  # Agrega títulos
  ggtitle('glmnet Regressión Tunning',
          subtitle = 'Metric specification by penalty level') +
  #  Usa un tema predefinido
  theme_bw() +
  # Haz modificaciones
  theme(
    # Cambia los textos
    text = element_text(family = 'Bebas Neue'),
    # Cambia los títulos
    axis.title.y = element_blank(),
    # Posición de la leyenda
    legend.position = 'right'
  )

# Revisamos el mejor modelo según cada métrica
ajuste_glmnet %>% select_best('kap')
ajuste_glmnet %>% select_best('accuracy')
ajuste_glmnet %>% select_best('sens')
ajuste_glmnet %>% select_best('spec')
ajuste_glmnet %>% select_best('roc_auc')

# Mejor modelo segun la exactitud  
modelo_logistico_glmnet %>% 
  # Finaliza el flujo usando el mejor modelo
  finalize_workflow(ajuste_glmnet %>% 
                      select_best('accuracy')) %>% 
  # Realiza el último ajuste
  last_fit(data_split) %>%  
  # Extrae las predicciones
  collect_predictions() %>% 
  # Contruye la matriz de confusión
  conf_mat(truth = obama_vote, estimate = .pred_class)

# Mejor modelo segun la roc_auc 
modelo_logistico_glmnet %>% 
  # Finaliza el flujo usando el mejor modelo
  finalize_workflow(ajuste_glmnet %>% 
                      select_best('roc_auc')) %>% 
  # Realiza el último ajuste
  last_fit(data_split) %>%  
  # Extrae las predicciones
  collect_predictions() %>% 
  # Contruye la matriz de confusión
  conf_mat(truth = obama_vote, estimate = .pred_class)

# Mejor modelo glmnet
mejor_glmnet = ajuste_glmnet %>% 
  select_best('roc_auc')

# Selecciona el mejor modelo
ajuste_glmnet_final =  modelo_logistico_glmnet %>% 
  # Finaliza el flujo usando el mejor modelo
  finalize_workflow(mejor_glmnet) 

# Revisa cómo se comporta el conjunto de entrenamiento
ajuste_glmnet_final %>% 
  # Ajusta sobre el conjunto
  fit(entrenamiento) %>% 
  # Extrae el ajuste
  extract_fit_parsnip()  %>% 
  # Revisa la importancia de variables
  vip::vi() %>%
  mutate(
    Importance = abs(Importance),
    # Ordena por importancia
    Variable = fct_reorder(Variable, Importance),
    # Completa las etiquetas
    Sign = ifelse(Sign == 'POS', 'Positiva','Negativa')
  ) %>%
  # Grafica de importancia
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  # Agrega columnas
  geom_col() +
  # Modifica el eje x
  scale_x_continuous(
    # Ajusta los márgenes
    expand = c(0, 0)
  ) +
  # Modifica las etiquetas
  labs(x = 'Importancia') +
  # Agrega títulos
  ggtitle('Importancia de variables en el modelo glmnet',
          subtitle = paste0('Valores para lambda = ', mejor_glmnet$penalty,' y mixture = ', mejor_glmnet$mixture)) +
  # Usa un tema predefinido
  theme_bw() +
  # Haz modificaciones
  theme(
    # Cambia los textos
    text = element_text(family = 'Bebas Neue'),
    # Cambia los títulos
    axis.title.y = element_blank(),
    # Posición de la leyenda
    legend.position = 'bottom',
    # Quita el título de la leyenda
    legend.title = element_blank()
  )


ajuste_glmnet_final = ajuste_glmnet_final%>% 
  # Realiza el último ajuste
  last_fit(
    split = data_split, 
    metrics = metric_set(yardstick::accuracy, yardstick::roc_auc)
    )

# Matriz de confusión glmnet 
matriz_glmnet= ajuste_glmnet_final %>% 
  collect_predictions() %>% 
  conf_mat(truth = obama_vote, estimate = .pred_class) 

matriz_glmnet %>% 
  autoplot(type='heatmap')

# Métricas glmnet
metricas_glmnet = ajuste_glmnet_final %>% 
  collect_metrics()

# Resultados     
print(metricas_glmnet)
print(metricas_logistico)

