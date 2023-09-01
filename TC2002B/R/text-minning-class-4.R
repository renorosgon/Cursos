# Fijar directorio de trabajo
setwd("~/Desktop/ITESM/Cursos/TC2002B/")

# Librerías ---------------------------------------------------------------
# Install - load tidyverse
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}    
# Install - load tidyverse
if(require(tidytext) == FALSE){                                                
  install.packages('tidytext')                                                 
  library(tidytext)                                                            
}else{                                                                          
  library(tidytext)                                                            
}  

# El conjunto de datos de Cónoceles ---------------------------------------
# El enlace directo al .xls
#url <- "https://candidaturas2021.ine.mx/documentos/descargas/baseDatosCandidatos.xls" 

#Descargar en la carpeta "datos"
#download.file(url, destfile = "data/baseDatosCandidatos.xls", mode = "wb")

# Leer el conjunto de datos
candidaturas = readxl::read_xls('data/baseDatosCandidatos.xls') %>% 
  # Homogeneizar los nombres
  janitor::clean_names() %>% 
  # Rellenar NAs con vacios
  mutate_at(
    .vars = vars(propuesta_1, propuesta_2),
    str_replace_na,''
    ) %>% 
  # Concatenar propuestas
  mutate(propuestas = str_c(propuesta_1, propuesta_2, sep = ' '))


tokens = candidaturas %>%
  filter(!is.na(propuestas)) %>% 
  unnest_tokens(
    input = propuestas,
    output = "palabra", 
    token = "words"
  )  %>% 
  count(partido_coalicion, palabra)  %>% 
  # Filtrar paralbras de paro
  filter(
    !str_detect(palabra, '[^a-záéíóúüñ ]'),
    !palabra %in% stopwords::stopwords('es',source = 'stopwords-iso')
  ) %>% 
  arrange(-n)


# Derivación (Stemming) ---------------------------------------------------
# Instalar - Cargar SnowballC                                                      
if(require(SnowballC) == FALSE){                                                
  install.packages('SnowballC')                                                 
  library(SnowballC)                                                            
}else{                                                                          
  library(SnowballC)                                                            
}   

# Variantes de un tallo
variantes = c('abandonadas','abandonados','abandonar','abandonarlos','abandono')
wordStem(words = variantes, language = 'spanish') 

# Como aplicarlo con las oraciones
oracion = 'Él abandonó el abandono al abandonar a los abandonados.'
wordStem(words = oracion, language = 'spanish') 

# El método tidy
oracion %>% 
  # Separar las oraciones
  str_split(pattern = ' ') %>% 
  # Aplicar wordStem a cada palabra
  sapply(wordStem, 'spanish') %>% 
  as.vector()

# Los stems de las candidaturas
stems = tokens %>% 
  # Aplicamos derivación
  mutate(stem = sapply(palabra, wordStem, 'spanish')) %>% 
  # Calculamos las precuencias
  with_groups(
    .groups = c(partido_coalicion, stem),
    summarise,
    n = sum(n)
    ) %>% 
  arrange(-n)

head(stems)

nrow(tokens)
nrow(stems)


# Lematización ------------------------------------------------------------
#  Instalar - Cargar udpipe                                
if(require(udpipe) == FALSE){                                                
  install.packages('udpipe')                                                 
  library(udpipe)                                                            
}else{                                                                          
  library(udpipe)                                                            
}   

# Opción A (Obligatorio la primera vez)
# Descargar el modelo en español
udmodel = udpipe_download_model(language = "spanish")
# Cargar el modelo
udmodel = udpipe_load_model(file = udmodel$file_model)

# Generar las anotaciones
udpipe_annotate(udmodel, x = variantes) %>% 
  # Transformar a tibble
  as_tibble() %>% 
  # Filtrar NAs
  filter(!is.na(lemma)) %>% 
  # Seleccionar extraer los lemmas
  pull(lemma)

# Opción B (Ya que descargué el corups)
lemmas_tibble = udpipe(x = oracion, object = 'spanish')
glimpse(lemmas_tibble)
view(lemmas_tibble)

# Así con las variantes
udpipe(x = variantes,  object = "spanish") %>% 
  View()

# Preparar nuestro datos para el modelo
entidades = candidaturas %>% 
  # Convertir todo a oraciones
  mutate(propuestas = str_to_sentence(propuestas)) %>% 
  # Renombrar
  select(doc_id = entidad, text = propuestas)
  
# Lematizar
lemmas_entidades = udpipe(x = entidades, object = 'spanish', parallel.cores = 4) %>% 
  # Filtrar los lemmas que nos interesan
  filter(
    !upos %in% c('SCONJ','CCONJ','SYM','NUM','INTJ'),
    !is.na(upos),
    !lemma %in% stopwords::stopwords('es'),
    nchar(lemma) > 2,
    !str_detect(lemma, '[^[:alpha:]]')
  ) %>% 
  # Agrupamos por documento
  with_groups(
    .groups = doc_id,
    summarise,
    # Concatenamos nuestro texto
    texto = str_c(lemma, collapse = ' ')
  ) %>% 
  # Generamos nuestros lemas
  unnest_tokens(
    input = texto,
    output = "lemma", 
    token = "words"
  )  %>% 
  # Generamos nuestra bolsa de palabras
  count(doc_id, lemma) %>% 
  # Agregamos tfidf
  bind_tf_idf(doc_id, lemma, n)


# Los términos más frecuentes
lemmas_entidades %>% 
  filter(
    # Filtrar los tokens que aparecen al menos 5 veces
    n > 5,
    # Filtrar Nacionales
    doc_id != 'NACIONAL'
    ) %>% 
  # Agrupar por doc_id
  group_by(doc_id) %>%
  # Las 10 palabras más frecuentes por entidad
  top_n(n = 10, wt = tf) %>% 
  ggplot(
    aes(
      # Term frequency
      x = tf, 
      # Ordenar lema por frecuencia por entidad
      y = reorder_within(lemma, tf, doc_id, sep = ''))
    ) +
  # Agregar un puntito
  geom_point() +
  # Agregar el texto
  geom_text(aes(label = lemma), hjust = - 0.2) +
  # Separar por entidad
  facet_wrap(~doc_id, scales = 'free_y', ncol = 8) +
  # Escala del eje x
  xlim(0, 2) +
  # Modificar temas
  theme_bw() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank()
  ) 


# Los términos más frecuentes
lemmas_entidades %>% 
  filter(
    # Filtrar los tokens que aparecen al menos 5 veces
    n > 5,
    # Filtrar Nacionales
    doc_id != 'NACIONAL'
  ) %>% 
  # Agrupar por doc_id
  group_by(doc_id) %>%
  # Las 10 palabras más características por entidad
  top_n(n = 10, wt = tf_idf) %>% 
  ggplot(
    aes(
      # Term frequency - Inverse Document Frequency
      x = tf_idf, 
      # Ordenar lema por frecuencia por entidad
      y = reorder_within(lemma, tf_idf, doc_id, sep = ''))
  ) +
  # Agregar un puntito
  geom_point() +
  # Agregar el texto
  geom_text(aes(label = lemma), hjust = - 0.2) +
  # Separar por entidad
  facet_wrap(~doc_id, scales = 'free_y', ncol = 8) +
  # Escala del eje x
  xlim(0, 4) +
  # Modificar temas
  theme_bw() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank()
  ) 
