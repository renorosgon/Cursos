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
    .vars = vars(propuesta_1, propuesta_2, propuesta_genero),
    str_replace_na,''
  ) %>% 
  # Concatenar propuestas
  mutate(propuestas = str_c(propuesta_1, propuesta_2, propuesta_genero, sep = ' '))
# mutate(propuesta = paste(propuesta_1, propuesta_2, propuesta_genero, sep = ' '))



# N-gramas ----------------------------------------------------------------
# N = 1
monogramas = candidaturas %>% 
  filter(!is.na(propuestas)) %>% 
  unnest_tokens(
    input = propuestas,
    output = "palabra", 
    token = "words"
  ) %>% 
  count(palabra, sort = T)

nrow(monogramas)

# N = 2
bigramas = candidaturas %>% 
  filter(
    !is.na(propuestas)
    ) %>% 
  unnest_tokens(
    input = propuestas,
    output = "bigrama", 
    token = "ngrams", 
    n = 2
  ) %>% 
  count(bigrama, sort = T) %>% 
  separate(bigrama, c('palabra_1', 'palabra_2'), sep = ' ') %>% 
  filter(
    !palabra_1 %in% stopwords::stopwords('es'),
    !palabra_2 %in% stopwords::stopwords('es')
  )

nrow(bigramas)

# Canalizando bigramas
# Install - load tidygraph
if(require(tidygraph) == FALSE){                                                
  install.packages('tidygraph')                                                 
  library(tidygraph)                                                            
}else{                                                                          
  library(tidygraph)                                                            
}    
# Install - load ggraph
if(require(ggraph) == FALSE){                                                
  install.packages('ggraph')                                                 
  library(ggraph)                                                            
}else{                                                                          
  library(ggraph)                                                            
}  

# Construir una red
red = bigramas %>% 
  # Con los 75 bigramas más frecuentes
  top_n(75) %>% 
  # Es una red dirigida (nos importa el orden)
  as_tbl_graph(directed = TRUE)


# Crear un lienzo de grafo
ggraph(red, layout = 'graphopt') +
  # Agregar la densidar de frecuencia de los ejes coloreado de rosa
  geom_edge_density(aes(fill = n), fill = 'pink') +
  # Agregar los ejes en colo blanco
  geom_edge_link(edge_colour = 'white', 
                 # Con forma de flecha
                 arrow = arrow(length = unit(4, 'mm'))) +
  # Agregar los nodos
  geom_node_point(col = 'red') +
  # Agregar los textos de cada nodo
  geom_node_text(aes(label = name), 
                 # Evitar que se amontonen
                 repel = T, 
                 # Colocar en negritas
                 fontface = 'bold'
                 )


# Limpieza de texto -------------------------------------------------------
# Normalización del texto
candidaturas %>% 
  filter(row_number() == 1) %>% 
  pull(propuestas) %>% 
  # Minúsculas
  str_to_lower() %>%  #tolower
  # Remover números 
     # str_remove_all(pattern = '[0-9]+') %>% 
     # str_remove_all(pattern = '\\d+')%>% 
       str_remove_all(pattern = '[:digit:]+') %>% 
  # Remover signos de puntuación
  str_remove_all(pattern = '[:punct:]+') %>% 
  # Cualquier cosa que no sea alfabético o espacios
  # str_remove_all(pattern = '[^a-záéíóúñü ]') %>% 
    str_remove_all(pattern = '[^[:alpha:] ]') %>% 
  # Remplazar dobles espacios
  # str_replace_all(pattern = '  ', replacement = ' ') %>% 
  str_replace_all(pattern = '\\s{2}', replacement = ' ')



# Algunas otras expreciones regulares
candidaturas %>% 
  pull(correo_electronico) %>% 
  tolower() %>% 
  # 1 o más (+)
  str_extract(pattern = '@[[:alpha:]\\.]+')

candidaturas %>% 
  pull(correo_electronico) %>% 
  # Cualquier caracter (.) que aparece cero o más veces(*)
  str_extract(pattern = '.*@')

# Códigos postales
candidaturas %>% 
  pull(direccion_casa_campana) %>% 
  # Aparece cero o una vez (?) o en un rango de x a y veces {x,y}
  str_extract(pattern = 'C\\.? ?P\\.? ?\\d{4,5}')

# La diferencia entre extract y match
candidaturas %>% 
  pull(direccion_casa_campana) %>% 
  # Capturar un patrón (())
  str_match(pattern = 'C\\.? ?P\\.? ?(\\d{4,5})') %>% 
  .[,2]


# Crear un diccionario para bigramas
bigramas = candidaturas %>% 
  # Limiar textos
  mutate(propuestas = str_remove_all(propuestas, '[^[:alpha:] ]')) %>% 
  # Filtrar NAs
  filter(!is.na(propuestas)) %>% 
  # Generar bigramas
  unnest_tokens(
    input = propuestas,
    output = "bigrama", 
    token = "ngrams", 
    n = 2
  ) %>% 
  # Conteo de bigramas
  count(bigrama, sort = T) %>% 
  # Separar bigramas en palabras
  separate(bigrama, c('palabra_1', 'palabra_2'), sep = ' ') %>% 
  # Filtrar paralbras de paro
  filter(
    !palabra_1 %in% stopwords::stopwords('es',source = 'stopwords-iso'),
    !palabra_2 %in% stopwords::stopwords('es',source = 'stopwords-iso'),
    # Los 60 términos más comunes
    n > 60
  ) %>% 
  # Generar ngramas
  transmute(
    # Regresar al bigrama
    bigrama = paste(palabra_1, palabra_2, sep = ' '),
    # Crear el monograma
    monograma = paste(palabra_1, palabra_2, sep = '_')
  )

# Crear un diccionario
monogramas = pull(bigramas, monograma)
names(monogramas) = pull(bigramas, bigrama)

# Replazar bigramas por monogramas
candidaturas = candidaturas %>% 
  mutate(
    propuestas = tolower(propuestas),
    propuestas = str_replace_all(propuestas, pattern = monogramas),
    propuestas = str_remove_all(propuestas, pattern = '[^[:alpha:] _]')
    )

glimpse(candidaturas)

# Importancia para la visualización
# Install - load wordcloud
if(require(wordcloud) == FALSE){                                                
  install.packages('wordcloud')                                                 
  library(wordcloud)                                                            
}else{                                                                          
  library(wordcloud)                                                            
}  

# Tokenización de propuestas
tokens = candidaturas %>% 
  filter(!is.na(propuestas)) %>% 
  unnest_tokens(
    input = propuestas,
    output = "palabra", 
    token = "words"
  )  %>% 
  count(palabra)  %>% 
  # Filtrar paralbras de paro
  filter(
    !palabra %in% stopwords::stopwords('es',source = 'stopwords-iso'),
  ) 

# Configurar los márgenes
par(mar = c(0,2,2,0))

# La nube de palabras
wordcloud(
  words = pull(tokens, palabra),
  freq = sqrt(pull(tokens, n)),
  max.words = 200,
  scale=c(2,0.5),
  rot.per = 0,
  random.order = FALSE,
  colors = c('pink','pink3','red','darkred'),
)

