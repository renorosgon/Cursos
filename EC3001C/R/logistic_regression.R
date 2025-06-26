# Fijar directorio de trabajo
setwd("~/Desktop/ITESM/Cursos/EC3001C")

# Cargar librearias
library(tidyverse)
library(tidymodels)

# Leer archivos con haven
hogares = haven::read_dta("data/hogares.dta") %>% 
  mutate(foliohog = as.numeric(foliohog))
concentrado_hogar = read_csv('data/concentradohogar.csv')

data = concentrado_hogar %>% 
  # Concatenar por vivienda, hogar y factor de expansion
  left_join(hogares, by = join_by('folioviv','foliohog','factor')) %>% 
  transmute(
    # Hogar con conexion a internet
    conexion_internet = if_else(conex_inte == 2, 'No','Sí'),
    conexion_internet = factor(conexion_internet),
    # Sexo del jefe o jefa de familia
    sexo_jefe = ifelse(sexo_jefe == 2, 'Mujer', 'Hombre'),
    sexo_jefe = factor(sexo_jefe),
    # Calcular ingreso percapita
    ingreso_percap = ing_cor/tot_integ, 
    # Calcular gasto en energia percapita
    energia_percap = energia/tot_integ,
    # Categorias de eduación del jefe o jefa de familia
    educa_jefe = case_when(
      educa_jefe == '01' ~ 'Sin Educación',
      educa_jefe %in% c('02','03','04','05','06') ~ 'Básica',
      educa_jefe %in% c('07','08') ~  'Media Superior',
      educa_jefe %in% c('09','10','11') ~ 'Superior'
    ),
    educa_jefe = factor(
      x= educa_jefe, 
      levels = c('Sin Educación', 'Básica', 'Media Superior', 'Superior')
      ),
    # Factor de expansion
    factor = frequency_weights(factor),
    # El hogar cuenta con tarjeta de crédito
    tarjeta = as.numeric(pago_tarje > 0)
  )

data_split = data %>% 
  initial_split(strata = conexion_internet)

train = training(data_split)
test = testing(data_split)

folds = vfold_cv(train, v = 5)

receta = recipe(
  formula = conexion_internet ~ sexo_jefe + educa_jefe + ingreso_percap + energia_percap + tarjeta,
  data = train
) %>% 
  step_log(ingreso_percap, energia_percap, offset = 0.001) %>% 
  step_dummy(sexo_jefe, educa_jefe)

reg_logistica = logistic_reg() %>% 
  set_engine('glmnet') %>% 
  set_mode('classification') %>% 
  set_args(penalty = tune(), mixture = tune())

modelo_logistico_glmnet = workflow() %>% 
  add_recipe(receta) %>% 
  add_model(reg_logistica)

parametros = modelo_logistico_glmnet %>% 
  extract_parameter_set_dials()

calibracion = modelo_logistico_glmnet %>% 
  tune_grid(
    resamples = folds,
    param_info = parametros,
    grid = 200,
    metrics = metric_set(accuracy, roc_auc),
    control = control_grid(verbose = TRUE)
  )

metricas_calibracion = calibracion %>% 
  collect_metrics()

mejor_modelo = calibracion %>% 
  select_best(metric = 'accuracy')

ajuste_final = modelo_logistico_glmnet %>% 
  finalize_workflow(mejor_modelo) %>% 
  last_fit(data_split)

ajuste_final %>% 
  collect_predictions() %>% 
  conf_mat(truth = conexion_internet, estimate = .pred_class) %>% 
  summary()


resultados = ajuste_final %>% 
  extract_workflow() %>% 
  augment(data)

resultados %>% 
  mutate(
    tarjeta  = ifelse(
      tarjeta == 0, 
      yes = 'Sin Tarjetas de Crédito',
      no = 'Con Tarjetas de Crédito')
  ) %>% 
  ggplot(aes(x = ingreso_percap, y = .pred_Sí)) +
  geom_point(
    aes(size = energia_percap),
    col = 'gray', shape = 21
  ) +
  geom_hline(
    yintercept = 0.5, linetype = 'dashed', col = 'gray50'
    ) +
  stat_smooth(
    method = 'glm', method.args = list(family = binomial),
    formula = 'y ~ x', se = FALSE,
    aes(col = educa_jefe)
  ) +
  scale_x_continuous(
    trans = log10_trans(),
    labels = scales::dollar_format(),
    breaks = c(100,1000,10000,100000,1000000)
    ) +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  facet_grid(tarjeta ~ sexo_jefe) +
  labs(
    x = 'Ingreso trimestral percápita (log)',
    col = 'Eduación del\njefe de familia',
    size = 'Consumo de\nenergia percápita',
    title = 'Determinantes de la probabilidad de acceso a internet en los hogares',
    caption = 'Fuente: Elaboración propia con datos de ENIGH2022\nAutor:@renorosgon'
  ) + 
  theme_bw(base_size = 8) +
  theme(axis.title.y = element_blank())































