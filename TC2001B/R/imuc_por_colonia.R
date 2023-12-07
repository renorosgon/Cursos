################################################################################
#                Índice de marginación urbana por colonia 2020                 #
################################################################################

# La base de datos del índice de marginación urbana por colonia 2020 se encuentra 
# disponiblev en la página de internet del CONAPO: 
# https://www.gob.mx/conapo o bien se puede descargar
# directamente del siguiente vinculo:
# http://www.conapo.gob.mx/work/models/CONAPO/Marginacion/Datos_Abiertos/Colonia/IMC_2020.zip
# Esta es una adaptación del script original disponible en:
# http://www.conapo.gob.mx/work/models/CONAPO/Marginacion/Datos_Abiertos/Script_Indice_de_marginacion_urbana_por_colonia_2020.txt

# Librerías ---------------------------------------------------------------

# Instala - carga pacman                                                       
if(require(pacman) == FALSE){                                                
  install.packages('pacman')                                                 
  library(pacman)                                                            
}else{                                                                          
  library(pacman)                                                            
}
# Carga las librerías necesarías
pacman::p_load(tidyverse, p2distance, stratification)

# Carga de Datos ----------------------------------------------------------

# Fijar directorio de trabajo
setwd("~/Desktop/ITESM/Cursos/TC2001B")

# Definir las hojas a cargar
sheets = c("IMUC_2020_AGS-MOR",'IMUC_2020_NAY-ZAC')
# Leer el conjunto de datos
imuc_2020 = map_df(
  .x = sheets,
  .f = readxl::read_xlsx,
  path = 'data/IMUC_2020.xlsx'
)


# Distancia P2 ------------------------------------------------------------

# José Bernardo Pena Trapero: `Problemas de la medición del bienestar y 
#  conceptos afines (1977)` 

# Para obtener el indicador sintético deben cumplir la condición de no 
# ambigüedad, es decir, un aumento en el valor de cada variable es un incremento 
# en las carencia socieconómicas, por lo que se multiplica cada indicador por -1. 
# De esta forma, un aumento en el índice supone una disminución en la marginación. 

# Crear una matriz de variables como input
input_matrix = imuc_2020 %>% 
  select(P6A14NAE:OVSCEL) %>% 
  as.matrix() 


# Para permitir la comparación en el tiempo de los indicadores simples se 
# determina el vector base de referencia al valor mínimo —el peor escenario 
# teórico— de la fecha censal 2020.
vector_minimo = makeReferenceVector(
  X = input_matrix * -1, 
  reference_vector_function = min) 

# Cálculo de índice
index_2020 = p2distance(
  matriz = input_matrix * -1,
  reference_vector = vector_minimo,
  iterations = 50
)

# Extraemos el índice
p2distance = pluck(index_2020, 'p2distance') %>%  as.vector()

summary(p2distance)


# Estratificación Dalenius-Hodges -----------------------------------------

# Se identifican los casos extremos en el índice de marginación y se aplica el 
# método de caja propuesto por Hubert y Vandervieren, para establecer los límites 
# con los que se debe trabajar.
outliers_2020 = boxplot.stats(p2distance)
cota_inferior =  pluck(outliers_2020, 'stats') %>% first()

# Utilizaremos este índice para la estratificación
imuc_outliers = ifelse(p2distance > cota_inferior, p2distance, cota_inferior)

# Estratificación con base en la calibración  
strata_dh_2020 = strata.cumrootf(imuc_outliers,
                                 CV = 0.05,
                                 Ls = 5,
                                 alloc = c(0.5, 0, 0.5), 
                                 nclass = 20)

# Extraenos los estratos y asignamos las etiquetas
estratos = factor(
                x = pluck(strata_dh_2020, 'stratumID'), 
                levels = 1:5, 
                c("Muy alto", "Alto", "Medio", "Bajo", "Muy bajo")
                )


# Índice normalizado de marginación urbana por colonia --------------------

# La normalización se realiza utilizando un cambio de escala conocido como 
# normalización mínima-máxima. Con este procedimiento el índice de marginación 
# se escala a valores relativos con un rango de entre cero y uno, lo cual 
# permite su comparación numérica y le da una propiedad adicional al índice de 
# marginación. De antemano, se sabe que cada indicador simple toma valores de 
# cero a 100 y, además,  el método DP2 ya proporcionó el orden de entrada 
# de las variables.

### Desviación estandar de los indicadores simples 

# Es necesario calcular el estimador insesgado de la varianza poblacional de 
# cada  indiciador simple, ya que la función `p2distance()` no proporciona el 
# cálculo.  

# Esta función calcula la inversa de la desviación estandar
inversa_sd = function(x){
  inversa_sd = 1 / (sd(x) * sqrt((length(x) -1)/length(x)))
  return(inversa_sd)
}

# Extraer los factores de correción en un tibble
factores_de_correccion = pluck(index_2020, 'correction_factors') %>% 
  as_tibble(rownames = 'variable') %>% 
  rename(factor_de_correccion = value) 

# Cálculo del rango del ínidce
rango_indice = imuc_2020 %>% 
  # Caluclar la inversa de la desviación estandar
  summarise_at(.vars = vars("P6A14NAE":"OVSCEL"), .funs = inversa_sd) %>% 
  t() %>% 
  as_tibble(rownames = 'variable') %>% 
  rename(inv_sd = V1) %>% 
  # Concatenar factores de correción
  left_join(factores_de_correccion, by = 'variable') %>% 
  # Calcular el máximo
  mutate(
    maximo = abs(0 - as.vector(vector_minimo)) * inv_sd * factor_de_correccion
  ) %>% 
  # Resumen del rango
  reframe(
    minimo = 0,
    maximo = sum(maximo)
  )

minimo = pull(rango_indice, minimo)
maximo = pull(rango_indice, maximo)
# Resultado ---------------------------------------------------------------
imuc_2020 = imuc_2020 %>% 
  # Agregamos el índice
  mutate(
    IM_2020 = p2distance,
    GM_2020 = estratos,
    IMN_2020 = (p2distance - minimo) / (maximo - minimo)
  ) 

# Guardamos los datos
# write_excel_csv(imuc_2020, 'data/imuc_2020')
