# Fija el directorio de trabajo
setwd("~/Desktop/ITESM/Cursos/EC3001C/")

# Librerías ---------------------------------------------------------------
# Instalar - Cargar tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}

# Instalar - cargar tidytext
if(require(tidytext) == FALSE){                                                
  install.packages('tidytext')                                                 
  library(tidytext)                                                            
}else{                                                                          
  library(tidytext)                                                            
}  

# Instalar - cargar tidymodels
if(require(tidymodels) == FALSE){                                                
  install.packages('tidymodels')                                                 
  library(tidymodels)                                                            
}else{                                                                          
  library(tidymodels)                                                            
}  

# Instalar - cargar textrecipes
if(require(textrecipes) == FALSE){                                                
  install.packages('textrecipes')                                                 
  library(textrecipes)                                                            
}else{                                                                          
  library(textrecipes)                                                            
}  


# Datos -------------------------------------------------------------------
reviews = read_csv("data/resenas.csv")  %>% 
  mutate(
    # Eliminar NUMBER
    star_rating = factor(
      x = str_remove(star_rating, " star rating"),
      levels = c('1.0','2.0','3.0','4.0','5.0'),
      ordered = TRUE
      ),
    # Pasar a minúsculas
    full_review_text = str_to_lower(full_review_text),
    # Expresiones regulares
    full_review_text = str_remove_all(full_review_text, '[^a-z ]+')
  )


# El proceso de entrenamiento ---------------------------------------------
split = initial_split(reviews, strata = star_rating)

# Conjunto de validación y entrenamiento
train = training(split)
test = testing(split)

# Crear una receta (usando el texto para predecir)
receta = recipe(star_rating ~ full_review_text, data = train) %>% 
  step_tokenize(full_review_text) %>% 
  step_stopwords(full_review_text, language = 'en', keep = FALSE) %>% 
  step_tokenfilter(full_review_text, max_tokens = 1000) %>%
  step_tfidf(full_review_text)

# Validación cruzada
set.seed(234)
folds = vfold_cv(train, v = 5)

# Regresión de lasso
logit_lasso = decision_tree() %>% 
  # Define el motor
  set_engine('rpart') %>% 
  # Define el modo
  set_mode('classification') %>% 
  # Define los hyperparametros
  set_args(
    cost_complexity = tune(),
    tree_depth = tune(),
    min_n = tune()
  )

# Crea un flujo de trabajo
modelo_lasso = workflow() %>%
  # Añade una receta
  add_recipe(receta) %>% 
  # Agrega un modelo
  add_model(logit_lasso)

# Construye una malla para optimizar
lasso_grid = modelo_lasso %>% 
  extract_parameter_set_dials() 
# Agrega elementos de control
ctrl = control_grid(save_pred = FALSE, verbose = TRUE)

# Arranca un cluster
doParallel::registerDoParallel()

# Ajusta el modelo de lasso
ajuste_lasso = tune_grid(
  object =  modelo_lasso,
    # Usa las muestras de validación cruzada
    resamples = folds,
    # Recorre la maya de hyperparametros
    grid = 20,
    # Agrega los elementos de control
    control = ctrl,
    # Define las métricas
    metrics = metric_set(accuracy, sens, spec, roc_auc, kap),
    )

# Imprime las métricas del modelo lasso       
metricas_lasso = ajuste_lasso %>% 
  collect_metrics() 

print(metricas_lasso)

# Revisamos el mejor modelo según cada métrica
ajuste_lasso %>% select_best(metric = 'kap')
ajuste_lasso %>% select_best(metric = 'accuracy')
ajuste_lasso %>% select_best(metric = 'sens')
ajuste_lasso %>% select_best(metric = 'spec')
ajuste_lasso %>% select_best(metric = 'roc_auc')

# Mejor modelo segun la exactitud  
modelo_lasso %>% 
  # Finaliza el flujo usando el mejor modelo
  finalize_workflow(ajuste_lasso %>% 
                      select_best(metric = 'roc_auc')) %>% 
  # Realiza el último ajuste
  last_fit(split) %>%  
  # Extrae las predicciones
  collect_predictions()
  # Contruye la matriz de confusión
  conf_mat(truth = star_rating, estimate = .pred_class) %>% 
  summary()


# Mejor modelo lasso
mejor_lasso = ajuste_lasso %>% 
  select_best(metric = 'roc_auc')

# Selecciona el mejor modelo
ajuste_lasso_final =  modelo_lasso %>% 
  # Finaliza el flujo usando el mejor modelo
  finalize_workflow(mejor_lasso) 


modelo_lasso %>% 
  # Finaliza el flujo usando el mejor modelo
  finalize_workflow(ajuste_lasso %>% 
                      select_best(metric = 'roc_auc')) %>% 
  # Realiza el último ajuste
  last_fit(split) %>%  
  # Extrae las predicciones
  collect_predictions() %>% 
  # Contruye la matriz de confusión
  roc_auc(star_rating, .pred_class) 

ajuste_lasso_final %>% 
  collect_metrics()


# Revisa cómo se comporta el conjunto de entrenamiento
ajuste_lasso_final %>% 
  # Ajusta sobre el conjunto
  fit(train) %>% 
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
  top_n(Importance, n = 100) %>% 
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
  ggtitle('Importancia de variables en el modelo LASSO',
          subtitle = paste0('Valores para lambda = ',mejor_lasso$penalty)) +
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


# Selecciona el mejor modelo
modelo_final =  modelo_lasso %>% 
  # Finaliza el flujo usando el mejor modelo
  finalize_workflow(mejor_lasso) %>% 
  # Realiza el último ajuste
  last_fit(split)

# Matriz de confusión lasso 
matriz_lasso= modelo_final %>% 
  collect_predictions() %>% 
  conf_mat(truth = star_rating, estimate = .pred_class) 

matriz_lasso %>% 
  autoplot(type='heatmap')

# Métricas lasso
metricas_lasso = modelo_final %>% 
  collect_metrics()

print(metricas_lasso)      



# Predicciones ------------------------------------------------------------
texto = receta %>% 
  prep() %>% 
  bake(
    new_data = tibble(
      full_review_text = 'This is the worst place to drink coffee. It is dirty')
  )

modelo_final %>% 
  extract_fit_parsnip() %>% 
  predict(texto, type="prob" )




