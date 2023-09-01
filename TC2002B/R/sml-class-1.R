# Fijar directorio de trabajo
setwd("/Users/renerosado/Desktop/ITESM/Cursos/TC2002B")
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
noticias = readxl::read_excel("data/train.xlsx")  %>% 
  # Limpiar los nombres
  janitor::clean_names() %>% 
  mutate(
    # Eliminar NUMBER
    text = str_remove(text, "\\*NUMBER\\*"),
    # Pasar a minúsculas
    text = str_to_lower(text),
    # Cambiar el encoding
    text = stringi::stri_trans_general(text, "Latin-ASCII")
  ) %>% 
  # Transformar 
  mutate(category = as.factor(category))



# El proceso de entrenamiento ---------------------------------------------
noticias_split = initial_split(noticias, strata = category)

# Conjunto de validación y entrenamiento
train = training(noticias_split)
test = testing(noticias_split)

# Crear una receta (usando el texto para predecir)
noticias_receta = recipe(category ~ text, data = train) %>% 
  step_tokenize(text) %>% 
  step_stopwords(text, language = 'es', keep = FALSE) %>% 
  step_tokenfilter(text, max_tokens = 1e4) %>%
  step_tfidf(text)

noticias_receta %>% 
  prep() %>% 
  juice() %>% 
  view()

# Validación cruzada
set.seed(234)
folds <- vfold_cv(train, v = 5)


# Regresión de lasso
logit_lasso = logistic_reg() %>% 
  # Define el motor
  set_engine('glmnet') %>% 
  # Define el modo
  set_mode('classification') %>% 
  # Define los hyperparametros
  set_args(penalty = tune(), mixture = 1)


# Crea un flujo de trabajo
modelo_lasso = workflow() %>%
  # Añade una receta
  add_recipe(noticias_receta) %>% 
  # Agrega un modelo
  add_model(logit_lasso)

# Construye una malla para optimizar
lasso_grid = grid_regular(penalty(), levels = 50)
# Agrega elementos de control
ctrl = control_grid(save_pred = FALSE, verbose = TRUE)

# Ajusta el modelo de lasso
ajuste_lasso = modelo_lasso %>% 
  tune_grid(
    # Usa las muestras de validación cruzada
    resamples = folds,
    # Recorre la maya de hyperparametros
    grid = lasso_grid,
    # Agrega los elementos de control
    control = ctrl,
    # Define las métricas
    metrics = metric_set(accuracy, sens, spec, roc_auc, kap))

# Imprime las métricas del modelo lasso       
metricas_lasso = ajuste_lasso %>% 
  collect_metrics() 

print(metricas_lasso)

# Grafica las métricas
metricas_lasso %>%
  # Agrega el lienzo con las estéticas que se heredan
  ggplot(aes(penalty, mean, color = .metric)) +
  # Agrega una línea
  geom_line(size = 1) +
  # Separa por métrica
  facet_wrap(~.metric, scales = "free", ncol = 1) +
  # Agrega títulos
  ggtitle('lasso Regressión Tunning',
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
    legend.position = 'none',
    # Quita el título de la leyenda
    legend.title = element_blank()
  )

# Revisamos el mejor modelo según cada métrica
ajuste_lasso %>% select_best('kap')
ajuste_lasso %>% select_best('accuracy')
ajuste_lasso %>% select_best('sens')
ajuste_lasso %>% select_best('spec')
ajuste_lasso %>% select_best('roc_auc')

# Mejor modelo segun la exactitud  
modelo_lasso %>% 
  # Finaliza el flujo usando el mejor modelo
  finalize_workflow(ajuste_lasso %>% 
                      select_best('roc_auc')) %>% 
  # Realiza el último ajuste
  last_fit(noticias_split) %>%  
  # Extrae las predicciones
  collect_predictions() %>% 
  # Contruye la matriz de confusión
  conf_mat(truth = category, estimate = .pred_class)


# Mejor modelo lasso
mejor_lasso = ajuste_lasso %>% 
  select_best('roc_auc')

# Selecciona el mejor modelo
ajuste_lasso_final =  modelo_lasso %>% 
  # Finaliza el flujo usando el mejor modelo
  finalize_workflow(mejor_lasso) 

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
  last_fit(noticias_split)

# Matriz de confusión lasso 
matriz_lasso= modelo_final %>% 
  collect_predictions() %>% 
  conf_mat(truth = category, estimate = .pred_class) 

matriz_lasso %>% 
  autoplot(type='heatmap')

# Métricas lasso
metricas_lasso = modelo_final %>% 
  collect_metrics()

print(metricas_lasso)      



# Predicciones ------------------------------------------------------------
texto = noticias_receta %>% 
  prep() %>% 
  bake(
    new_data = tibble(
      text = 'amlito')
  )

modelo_final %>% 
  extract_fit_parsnip() %>% 
  predict(texto, type="prob" )



