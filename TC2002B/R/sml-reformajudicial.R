# Fijar directorio de trabajo
setwd("/Users/renerosado/Desktop/ITESM/Cursos/TC2002B")

# Librerias
library(tidyverse)
library(tidytext)
library(tidymodels)
library(textrecipes)

# Leer los datos
noticias = readxl::read_excel('data/Ejercicio Clasificación.xlsx')  %>% 
  # Limpiar los nombres
  janitor::clean_names() %>% 
  mutate(
    # Pasar a minúsculas
    text = str_to_lower(texto),
    # Cambiar el encoding
    text = stringi::stri_trans_general(text, "Latin-ASCII"),
    # Quitar espacios en blanco
    text = str_squish(text),
    # Convertir a factor
    postura = as.factor(postura)
    ) %>% 
  filter(
    !is.na(postura),
    !is.na(text)
    )

summary(noticias)

# El proceso de entrenamiento ---------------------------------------------
noticias_split = noticias %>% 
  initial_split(strata = postura)

# Conjunto de validación y entrenamiento
train = training(noticias_split)
test = testing(noticias_split)

# Crear una receta (usando el texto para predecir)
receta_tf = recipe(postura ~ text, data = train) %>% 
  step_mutate(text = str_remove_all(text, '[:digit:]|[:punct:]')) %>% 
  step_tokenize(text) %>% 
  step_tokenfilter(text, max_tokens = 1000) %>%
  step_tf(text)

receta_tfidf = recipe(postura ~ text, data = train) %>% 
  step_mutate(text = str_remove_all(text, '[:digit:]|[:punct:]')) %>% 
  step_tokenize(text) %>% 
  step_tokenfilter(text, max_tokens = 1000) %>%
  step_tfidf(text)

receta_lda = recipe(postura ~ text, data = train) %>% 
  step_mutate(text = str_remove_all(text, '[:digit:]|[:punct:]')) %>% 
  step_tokenize(text) %>% 
  step_lda(text, num_topics = 3)


receta_lda %>% 
  prep() %>% 
  juice() %>% 
  glimpse()

# Validación cruzada
set.seed(234)
folds <- vfold_cv(train, v = 3)


# Define regresion logistica multinomial (con regularizacion)
modelo_glmnet =  logistic_reg() %>% 
  # Define el motor
  set_engine('glmnet') %>% 
  # Define el modo
  set_mode('classification') %>% 
  # Define los hyperparametros
  set_args(
    # Penalizacion
    penalty = tune(),
    # Combinacion ridge-lasso
    mixture = tune()
  )

# Define un arbol aleatorio
modelo_arbol = decision_tree() %>%
  # Fija el motor
  set_engine('rpart')%>%
  # Fija el método de classificación
  set_mode('classification') %>% 
  # Fija los hyperparametros a optimizar
  set_args(
    # Costo de complejidad
    cost_complexity = tune(), 
    # Profundidad
    tree_depth = tune(), 
    # Minimo de observaciones
    min_n = tune()
  )

# Define un bosque aleatorio
modelo_bosque = rand_forest() %>% 
  # Fija el motor
  set_engine("ranger", importance = "permutation") %>% 
  # Fijo el modo
  set_mode("classification") %>% 
  # Fija los argumentos
  set_args( 
    # Número de predictores por muestra
    mtry = tune(),
    # Numero de árboles
    trees = tune(),
    # Minimo de observaciones
    min_n = tune()
  )



# Define un conjunto de flujos de trabajo
noticias_workflow = workflow_set(
  # Agrega una lista de pasos de preprocesamiento
  preproc = list(
    tf = receta_tf, 
    tfidf = receta_tfidf,
    lda = receta_lda
    ),
  # Agrega una lista de modelos
  models = list(
    # Agrega el modelo de glmnet
    glmnet = modelo_glmnet,
    # Agrega el modelo de arbol aleatorio
    arbol = modelo_arbol, 
    # Agrega el modelo de bosque aleatorio
    bosque = modelo_bosque
  )
) %>% 
  # Aplicaremos estos pasos a cada uno de los modelos
  workflow_map(
    # Agrega las muestras de validación
    resamples = folds,
    # Define un numero de combinaciones para probar (puedes comenzar con 100)
    grid = 10,
    # Te recomiendo dejar verbose=TRUE para que puedas monitorear el avanza
    verbose = TRUE
  )

# Ordena de mejor a peor modelo
rank_results(noticias_workflow, rank_metric = "roc_auc")

# Podemos ver gráficamente el ranking 
autoplot(noticias_workflow, metric = "roc_auc")

# Seleccionar el mejor resultado
resultado = rank_results(noticias_workflow, rank_metric = "roc_auc") %>% 
  filter(.metric == 'roc_auc') %>% 
  filter(mean == max(mean)) %>% 
  sample_n(1) %>% 
  pull(wflow_id)

# Selecciona el mejor modelo
mejor_modelo = noticias_workflow %>% 
  # Extrae el conjunto de resultados del algoritmo con mejor desempeño
  extract_workflow_set_result(resultado) %>% 
  # Selecciona el mejor modelo con base en la metrica roc_auc
  select_best(metric = 'roc_auc') 

# Finaliza el flujo de trabajo 
modelo = noticias_workflow %>% 
  # Extrae el flujo de trabjao
  extract_workflow(resultado)  %>%  
  # Finaliza el flujo con el mejor modelo
  finalize_workflow(mejor_modelo) %>% 
  # Realiza el ultimo ajuste
  last_fit(noticias_split)

modelo %>% 
  collect_metrics()


# Revisa cómo se comporta el conjunto de entrenamiento
modelo %>% 
  # Extrae el ajuste
  extract_fit_parsnip()  %>% 
  # Revisa la importancia de variables
  vip::vi() %>%
  mutate(
    Importance = abs(Importance),
    # Ordena por importancia
    Variable = fct_reorder(Variable, Importance),
    # Completa las etiquetas
    #  Sign = ifelse(Sign == 'POS', 'Positiva','Negativa')
  ) %>%
  top_n(Importance, n = 50) %>% 
  # Grafica de importancia
  ggplot(aes(x = Importance, y = Variable)) +
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
  ggtitle('Importancia de variables') +
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

# Predicciones ------------------------------------------------------------
nuevos_datos = tibble(
  text = c(
    'La reforma judicial es una gran idea')
)
texto = receta_tf %>% 
  prep() %>% 
  bake(
    new_data = nuevos_datos
  )

modelo %>% 
  extract_fit_parsnip() %>% 
  predict(texto, type="prob" )


# Análisis ----------------------------------------------------------------
predicciones = modelo %>% 
  extract_workflow() %>% 
  augment(noticias) 

ggplot(predicciones, aes(x = `.pred_A favor`)) +
  geom_histogram() +
  labs(x = 'Propensión A Favor', y = 'Frecuencia', title = 'Postura respecto a la Reforma Judicial')

predicciones %>% 
  select(postura, .pred_class:`.pred_En contra`, usuario, texto) %>% 
  filter(
    str_detect(usuario,'Noroña|Piña')
    ) %>% 
  group_by(usuario) %>% 
  summarise(postura = mean(`.pred_A favor`)) %>% 
  ggplot(aes(x = postura, y = 0)) +
  geom_point(
    aes(col = usuario), 
    size = 8, show.legend = F
    ) +
  geom_text(
    aes(label = usuario), 
    show.legend = F, vjust = 'center', hjust = 1.25,
    angle = 90
    ) +
  labs(
    x = 'Propensión A Favor', 
    title = 'Postura respecto a la Reforma Judicial',
    col = 'Persona'
    ) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

