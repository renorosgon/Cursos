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


# Introducción a análisis de texto ----------------------------------------

# Una matriz de documentos
documentos = tibble(
  doc_1 = 'el gato corre',
  doc_2 = 'el perro corre'
) %>% 
  gather(doc_id, texto)

print(documentos)

# Una mtriz de documentos y términos
matriz_documentos_terminos = documentos %>% 
  unnest_tokens(
    input = texto,
    output = "palabra", 
    token = "words",
  ) %>% 
  count(doc_id, palabra)

print(
  spread(
    data = matriz_documentos_terminos, 
    key = doc_id, 
    value = n, 
    fill = 0
    )
  )

# Similitud de documentos
doc_1 = matriz_documentos_terminos %>%
  filter(doc_id == 'doc_1') %>% 
  pull(palabra)

doc_2 = matriz_documentos_terminos %>%
  filter(doc_id == 'doc_2') %>% 
  pull(palabra)

# Uniones
union(doc_1,doc_2)

# Intersecciones
intersect(doc_1,doc_2)

similitud_palabras =  length(intersect(doc_1,doc_2)) / length(union(doc_1,doc_2))
print(similitud_palabras)

# Similitud de jaccard
similitud_jaccard = function(a, b){
  length(intersect(a, b)) / length(union(a, b))
}  

# Matriz de documentos y tejas (shingles)
matriz_documentos_tejas = documentos %>% 
  unnest_character_shingles(
    input = texto,
    output = "teja", 
    n = 3,
    strip_non_alphanum = FALSE
  ) %>% 
  count(doc_id, teja)

print(
  spread(
    data = matriz_documentos_tejas, 
    key = doc_id, 
    value = n, 
    fill = 0
    )
)

# Similitud de doumentos
doc_1 = matriz_documentos_tejas %>%
  filter(doc_id == 'doc_1') %>% 
  pull(teja)

doc_2 = matriz_documentos_tejas %>%
  filter(doc_id == 'doc_2') %>% 
  pull(teja)

similitud_tejas =  similitud_jaccard(doc_1, doc_2)
print(similitud_tejas)

# Una serie de documentos más largas
tejas = tibble(
  doc_1 = "el perro persigue al gato pero no lo alcanza", 
  doc_2 = "el gato persigue al perro, pero no lo alcanza",
  doc_3 = "este es el documento de ejemplo", 
  doc_4 = "otros animales pueden ser mascotas"
) %>% 
  gather(doc_id, texto) %>% 
  unnest_character_shingles(
    input = texto,
    output = "teja", 
    n = 8,
    strip_non_alphanum = FALSE
  ) %>% 
  count(doc_id, teja) %>% 
  spread(doc_id, n) %>% 
  mutate_if(is.numeric, replace_na, 0)
  

tejas = tibble(
  doc_1 = "el perro persigue al gato pero no lo alcanza", 
  doc_2 = "el gato persigue al perro, pero no lo alcanza",
  doc_3 = "este es el documento de ejemplo", 
  doc_4 = "otros animales pueden ser mascotas"
) %>% 
  gather(doc_id, texto) %>% 
  unnest_tokens(
    input = texto,
    output = "teja", 
    token = 'words'
  ) %>% 
  count(doc_id, teja) %>% 
  spread(doc_id, n) %>% 
  mutate_if(is.numeric, replace_na, 0)

# Similitud de varios documentos 
matriz_tejas = tejas %>% 
  select(-teja) %>% 
  as.matrix() %>% 
  t()
  
print(1 - dist(matriz_tejas, method = "binary"))