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
# Instalar - Cargar rvest                                                       
if(require(rvest) == FALSE){                                                
  install.packages('rvest')                                                 
  library(rvest)                                                            
}else{                                                                          
  library(rvest)                                                            
}                                                                               
# Instalar - Cargar httr                                                       
if(require(httr) == FALSE){                                                
  install.packages('httr')                                                 
  library(httr)                                                            
}else{                                                                          
  library(httr)                                                            
}                                                                               
# Instalar - Cargar progress                                                       
if(require(progress) == FALSE){                                                
  install.packages('progress')                                                 
  library(progress)                                                            
}else{                                                                          
  library(progress)                                                            
}          

# Introducción a raspado web ----------------------------------------------
# Definimos la url que queremos raspar
path = 'https://www.jornada.com.mx/2023/08/17/'

# Realizamos nuestra solicitud
response = GET(path)

# Revisamos el estado de la respuesta
pluck(response, 'status_code')

# Revisamos su contenido
content(response)

# Leemos nuestra estructura html
jornada_html = read_html(response)

# Luce asi
xml2::html_structure(jornada_html)

# Obtener las de las secciones
# Usando XPATH
secciones = jornada_html %>%
  # Obtenemos los nodos de la tabla que contiene laas secciones
  html_nodes(xpath = '//tr/td/a/div/text()') %>%
  # Obtenemos el texto
  html_text()

print(secciones)


# Obtener las de los titulares
# Usando XPATH
titulares = jornada_html %>%
  # Obtenemos los nodos de la tabla que contiene laas secciones
  html_nodes(xpath = '//div[@class = "page cont"]//a/strong') %>%
  # Obtenemos el texto
  html_text()


# Web Crawling ------------------------------------------------------------
# Crea nuestro dataframe vacio
titulares_df = c()

# Comienza en el url de la jornada
jornada_url = 'https://www.jornada.com.mx'

# Crea la secuencia de fechas
fechas = str_replace_all(
  # Genera una secuencia
  seq(
    as.Date('2023-01-01'), # Desde la Fecha de Inicio
    as.Date('2023-08-17'), # Hasta Fecha Final
    by = "day" # Sequencia por día
  ), 
  # Remplaza - por /
  '-', '/'
)

# En caso de que sufras de ansiedad
bar = progress_bar$new(total = length(fechas))
# Iteramos sobre las fechas
for(fecha in fechas){
  # Definimos la url que queremos raspar
  path = paste(jornada_url, fecha, sep = '/')
  # Realizamos nuestra solicitud
  response = GET(path)
  if(response$status_code < 400){
    # Obtener las de los titulares
    titulares = response %>%
      # Leer el html
      read_html() %>% 
      # Obtenemos los nodos de la tabla que contiene laas secciones
      html_nodes(xpath = '//div[@class = "page cont"]//a/strong') %>%
      # Obtenemos el texto
      html_text()
    
    titulares_df = tibble(
      fecha = fecha,
      id = 1:length(titulares),
      titulares = titulares
    ) %>% 
      bind_rows(titulares_df)
  }
  # Actualiza el avance
  bar$tick()
}


# Lematización ------------------------------------------------------------
#  Instalar - Cargar udpipe                                
if(require(udpipe) == FALSE){                                                
  install.packages('udpipe')                                                 
  library(udpipe)                                                            
}else{                                                                          
  library(udpipe)                                                            
}   
#  Instalar - Cargar tidytext                                
if(require(tidytext) == FALSE){                                                
  install.packages('tidytext')                                                 
  library(tidytext)                                                            
}else{                                                                          
  library(tidytext)                                                            
}   

# Modificar la estructura
lemmas= titulares_df %>% 
  mutate(doc_id = paste(fecha, id, sep = ' ')) %>% 
  select(doc_id, text = titulares) %>% 
  udpipe(object = 'spanish', parallel.cores = 4) %>% 
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
  ) 


# Crear un diccionario para bigramas
bigramas = lemmas %>% 
  # Generar bigramas
  unnest_tokens(
    input = texto,
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
    !palabra_1 %in% stopwords::stopwords('es', source = 'stopwords-iso'),
    !palabra_2 %in% stopwords::stopwords('es', source = 'stopwords-iso'),
    !is.na(palabra_1),
    !is.na(palabra_2),
    n > 1
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

# Tokenizar el texto
tokens = lemmas %>% 
  mutate(
    # pasar a minúsculas
    texto = tolower(texto),
    # sistuir bigramas
    texto = str_replace_all(texto, pattern = monogramas),
    # extraer fecha
    fecha = str_extract_all(doc_id, '\\d{4}/\\d{2}/\\d{2}'),
    # extraer mes
    mes = month(ymd(fecha), label = TRUE)
  ) %>% 
  # Generamos nuestros lemas
  unnest_tokens(
    input = texto,
    output = "token", 
    token = "words"
  )  %>% 
  # Generamos nuestra bolsa de palabras
  count(mes, token) %>% 
  # Filtrar palabras comunes
  filter(!token %in% c('ser','año','haber','mdp','sólo','tras','méxico',
                       'nuevo','mexicano','hacer','caso')) %>% 
  # Agregamos tfidf
  bind_tf_idf(mes, token, n)




# Nubes de palabras en el tiempo ------------------------------------------
# Instalar - Cargar ggwordcloud                                           
if(require(ggwordcloud) == FALSE){                                                
  install.packages('ggwordcloud')                                                 
  library(ggwordcloud)                                                          
}else{                                                                          
  library(ggwordcloud)                                                          
}     

# Los términos más frecuentes
tokens %>% 
  # Filtrar los términos que aparecen almenos 3 veces
  filter(n > 3)  %>% 
  # Crear un lienzo
  ggplot(aes(label = token, size = n, x = mes, col = n)) + 
  # Crear la nube de palabras
  geom_text_wordcloud() +
  scale_size(range = c(4,8)) +
  # Agregar títulos
  labs(title = 'TÉRMINOS MÁS FRECUENTES POR MES') +
  # Seleccionar los colores
  scale_color_gradient(low = 'pink', high = 'red4') +
  # Modificar el tema
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    title = element_text(family = 'Arial', face = 'bold'),
    plot.title = element_text(hjust = 0.5)
  )


# Los términos más frecuentes
tokens %>% 
  # Filtrar los términos que aparecen almenos 3 veces
  filter(n > 3,
         !token %in% c('amlo','lópez_obrador','día','tener','hoy','país','mdd')
         )  %>% 
  # Crear un lienzo
  ggplot(aes(label = token, size = tf_idf, x = mes, col = tf_idf)) + 
  # Crear la nube de palabras
  geom_text_wordcloud() +
  scale_size(range = c(4,8)) +
  # Agregar títulos
  labs(title = 'TÉRMINOS MÁS REPRESENTATIVOS POR MES')  +
  # Seleccionar los colores
  scale_color_gradient(low = 'pink', high = 'red4') +
  # Modificar el tema
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    title = element_text(family = 'Arial', face = 'bold'),
    plot.title = element_text(hjust = 0.5)
  )






