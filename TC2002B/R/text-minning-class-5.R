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


# Extraer las canciones ---------------------------------------------------
#Definir la url base
url = 'https://www.azlyrics.com'

# Esta es una lista de usuario-agente
user_agents_list = rjson::fromJSON(file = 'https://raw.githubusercontent.com/Said-Ait-Driss/user-agents/main/userAgents.json')

# Un GET normal
respuesta = GET(
  # Fijar la url
  url = url, 
  # Fijar la liga a seguir
  path = '/t/taylorswift.html',
  # Colocar un usuario agente aleatorio
  user_agent(sample(user_agents_list, size = 1))
  ) %>% 
  # Leer el html
  read_html() %>%
  # Extraer la lista de albums
  html_nodes(xpath = '//div[@id="listAlbum"]/div')

# La respuesta es una lista de nodos xml
class(respuesta)

# Explorar las clases de los elementos
html_attr(respuesta[1], 'class')
html_attr(respuesta[2], 'class')
html_attr(respuesta[3], 'class')

# Hacer un loop
for (elemento in respuesta) {
  print(html_attr(elemento, 'class'))
}

# Extaer solo los elementos que cuentan con el atributo clase
respuesta = GET(
  # Fijar la url
  url = url, 
  # Fijar la liga a seguir
  path = '/t/taylorswift.html',
  # Colocar un usuario agente aleatorio
  user_agent(sample(user_agents_list, size = 1))
) %>% 
  # Leer el html
  read_html() %>%
  # Extraer la lista de albums
  html_nodes(xpath = '//div[@id="listAlbum"]/div[@class]')

# Repetimos el for loop
for(elemento in respuesta) {
  print(html_attr(elemento, 'class'))
}

# Las funciones apply son equivalentes a un for loop
sapply(respuesta, html_attr, 'class') %>% 
  # Y podemos tabular
  table()

# Estructurar tidydata
# Genera un tibble vacio
taylor_swift_albums = c()

# Iterar por cada elemento en respeusta
for (elemento in respuesta) {
  # Si el texto es 'other songs:'
  if(html_text(elemento) == "other songs:"){
    # Termina el loop
    break
  } 
  # Si la clase es album
  else if(html_attr(elemento, 'class') == 'album'){
    # Crea la variable album
    album = html_text(elemento)
  # De otro modo
  } else {
  # Crea un tibble
    taylor_swift_albums = tibble(
      # Con el album actual
      album = album, 
      # Con el titulo
      title = html_text(elemento),
      # Con el hipervinculo
      path = elemento %>% 
        html_node(xpath = 'a') %>% 
        html_attr('href')
    ) %>% 
      # Pegalo al tibble original
      bind_rows(taylor_swift_albums)
  } 
}

# Luce así
glimpse(taylor_swift_albums)


# Extraer la lista de paths
list_of_paths = pull(taylor_swift_albums, path)

# Iniciar la barra de progreo
bar = progress_bar$new(total = length(list_of_paths))

# Crear un tibble vacio
taylor_swift_lyrics = c()

# Iterar sobre los paths
for (path in list_of_paths) {
  respuesta = GET(
    # Fijar la url
    url = url, 
    # Fijar la liga a seguir
    path = path,
    # Fijar agente usuario
    user_agent(sample(user_agents_list, size = 1))
  ) 
  
  # Si la respuesta es correcta
  if(pluck(respuesta, 'status_code') == 200){
    
    # Crear un tibble
    taylor_swift_lyrics = tibble(
      # Con el path
      path = path,
      # Con el texto
      text = respuesta %>% 
        read_html() %>% 
        html_nodes(xpath = '//div[@class="col-xs-12 col-lg-8 text-center"]/div[5]') %>% 
        html_text() %>% 
        str_squish()
    ) %>% 
      # Pegar al tibble original
      bind_rows(taylor_swift_lyrics)
    
    # Pausar entre 10 y 20 segundos
    Sys.sleep(runif(n = 1, min = 10, max = 20))
    # Actualizar el progreso
    bar$tick()
    }
  }
      
# Crear el dataset final      
taylor_swift_dataset = taylor_swift_albums %>% 
  # Concatenar por los albums con las canciones
  inner_join(taylor_swift_lyrics) %>% 
  mutate(
    # Idetificar Taylor's Version
    is_taylor_version = str_detect(album_name, "Taylor's Version"),
    # Extraer el nombre del album
    album_name = str_squish(str_extract(album, '".*"')),
    # Exteraer el año
    album_year = as.numeric(str_extract(str_extract(album, '\\(\\d{4}\\)'), '\\d{4}')),
    # Extraer el tipo
    album_type = str_extract(album,'\\w+'),
    # Eliminar columnas
    album = NULL,
    path = NULL
  ) %>% 
  # Reordenar el tibble
  select(album_name:is_taylor_version,title, text)  

# Guardar nuestros datos
write_excel_csv('data/taylor_swift_dataset.csv')
  
      

# Análisis de texto -------------------------------------------------------
# Instalar - cargar tidytext
if(require(tidytext) == FALSE){                                                
  install.packages('tidytext')                                                 
  library(tidytext)                                                            
}else{                                                                          
  library(tidytext)                                                            
}  

# Cargar el dataset
taylor = read_csv('data/taylor_swift_dataset.csv') 

# Tokenización
tokens =  taylor %>%
  unnest_tokens(
    input = text,
    output = "word", 
    token = "words"
  )  %>% 
  count(word)  %>% 
  # Filtrar paralbras de paro
  filter(
    !word %in% c(stopwords::stopwords('en',source = 'stopwords-iso'),
                 'yeah','ooh','ha','day','hey')
  ) %>%
  # Ordenar por frecuencia
  arrange(-n)


# Visualizar los términos más comunes
# Instalar - cargar extrafont
if(require(extrafont) == FALSE){                                                
  install.packages('extrafont')                                                 
  library(extrafont)                                                            
}else{                                                                          
  library(extrafont)                                                            
}  

# Cargar las fuentes en tu computadora
# extrafont::font_import()
# En caso de marcar error, posiblemente hay que actualizar esto
# remotes::install_version("Rttf2pt1", version = "1.3.8")

# Gráfico de barras
tokens %>% 
  # Filtrar los 30 términos más frecuentes
  top_n(30, wt = n) %>% 
  # Crear un lienzo
  ggplot(aes(x = n, y = reorder(word, n))) +
  # Agregar columnas
  geom_col(fill = '#cc6b96') +
  # Agregar etiquetas
  labs(
    title = 'Taylor Swift mostly sings about "time" and "love"...',
    subtitle = "Word frequency across songs",
    caption = '@renorosgo'
    ) +
  # Modificar el eje x
  scale_x_continuous(expand = c(0,0,0.1,0)) +
  # Seleccionar un tema
  theme_void() +
  # Modificar el tema
  theme(
    text = element_text(family = 'BrushScriptMT', color = '#462245'),
    axis.text.y = element_text(size = 24),
    axis.text.x = element_text(size = 16),
    title = element_text(size = 24),
  ) 


# Nube de palabras
# Instalar - Cargar ggwordcloud                                           
if(require(ggwordcloud) == FALSE){                                                
  install.packages('ggwordcloud')                                                 
  library(ggwordcloud)                                                          
}else{                                                                          
  library(ggwordcloud)                                                          
}     

# Gráfico de barras
tokens %>% 
  # Seleccionar los 300 términos más frecuentes
  top_n(300, wt = n)  %>% 
  # Crear un lienzo
  ggplot(aes(label = word, size = n, col = n)) + 
  # Crear la nube de palabras
  geom_text_wordcloud(
    # Remover sobrantes
    rm_outside = TRUE, 
    # Cambiar la tipografía
    family = 'BrushScriptMT', 
    # Forma de la nube de palabras                  
    shape = "square",
    # Excentricidad (espacio)
    eccentricity = 1
    ) +
  # Modificar el rango de tamaños
  scale_size(range = c(2,24)) +
  # Agregar etiquetas
  labs(
    title = 'Taylor Swift mostly sings about "time" and "love"...',
    subtitle = "Word frequency across songs",
    caption = '@renorosgo'
  ) +
  # Seleccionar los colores
  scale_color_gradient(low = 'pink', high = 'darkviolet') +
  # Modificar el tema
  theme_minimal() +
  theme(
    text = element_text(family = 'BrushScriptMT', color = '#462245'),
    title = element_text(size = 24),
  ) 
  

# Tokenizaciôn por album
tokens =  taylor %>%
  # Eliminamos el EP de navidad
  filter(album_type == 'album') %>% 
  # tokenizaciôn
  unnest_tokens(
    input = text,
    output = "word", 
    token = "words"
  )  %>% 
  # Contar por album y año
  count(album_name, album_year, word)  %>% 
  # Filtrar paralbras de paro
  filter(
    !word %in% c(stopwords::stopwords('en',source = 'stopwords-iso'), 'yeah','ooh','ha','day','hey'),
    str_detect(word, '[:alpha:]')
  ) %>% 
  # Ordenar por frecuencia
  arrange(-n) %>% 
  # Agregar tf_idf
  bind_tf_idf(term = word, document = album_name, n = n)



tokens %>% 
  group_by(album_name) %>% 
  top_n(50, wt = n)  %>% 
  # Crear un lienzo
  ggplot(aes(label = word, size = n, col = n)) + 
  # Crear la nube de palabras
  geom_text_wordcloud(rm_outside = TRUE, 
                      family = 'BrushScriptMT', 
                      shape = "square",
                      eccentricity = 1,
                      ) +
  # Agregar títulos
  labs(
    title = "Taylor Swift's Discography",
    caption = '@renorosgo'   
       ) +
  # Separar por album
  facet_wrap(~album_name) +
  # Modificar colores
  scale_color_gradient(low = 'pink', high = 'darkviolet') +
  # Modificar el tema
  theme_minimal() +
  theme(
    text = element_text(family = 'BrushScriptMT', color = '#462245'),
    plot.title = element_text(size = 24, hjust = 0.5),
    strip.text = element_text(size = 18)
  ) 


# Crear un vector de etiquetas
labels =  tokens %>% 
  # Ordenar por año
  arrange(album_year) %>% 
  # Seleccionar nombre y año
  select(album_name, album_year) %>% 
  # Eliminar duplicados
  unique() %>% 
  # Crear etiquetas
  reframe(label = str_c(album_name, '\n(', album_year,')')) %>% 
  # Extraer el vector
  pull(label)

# Serie de tiempo
tokens %>% 
  # Agrupar por album
  group_by(album_name) %>% 
  # Seleccionar los 50 términos más comunes
  top_n(50, wt = n)  %>% 
  # Crear un lienzo
  ggplot(aes(label = word, size = n, col = log(n), y = factor(album_year))) + 
  # Crear la nube de palabras
  geom_text_wordcloud(rm_outside = TRUE, 
                      family = 'BrushScriptMT', 
                      eccentricity = 1
                      ) +
  # Modificar rango de tamaños
  scale_size(range = c(2,10)) +
  # Modificar etiquetas
  scale_y_discrete(labels = labels) +
  # Agregar títulos
  labs(
    title = "Most common terms by album in Taylor Swift's Discography",
    caption = '@renorosgo'
    ) +
  scale_color_gradient(low = 'pink2', high = 'darkviolet') +
  # Modificar el tema
  theme_minimal(base_size = 16) +
  theme(
    text = element_text(family = 'BrushScriptMT', color = '#462245'),
    plot.title = element_text(size = 24, hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_text(size = 14, color = '#462245')
    )

tokens %>% 
  # Agrupar por album
  group_by(album_name) %>% 
  # Seleccionar los 50 términos más comunes
  top_n(50, wt = tf)  %>% 
  # Crear un lienzo
  ggplot(aes(label = word, size = tf, col = tf, y = factor(album_year))) + 
  # Crear la nube de palabras
  geom_text_wordcloud(rm_outside = TRUE, 
                      family = 'BrushScriptMT', 
                      eccentricity = 1
  ) +
  # Modificar rango de tamaños
  scale_size(range = c(2,10)) +
  # Modificar etiquetas
  scale_y_discrete(labels = labels) +
  # Agregar títulos
  labs(title = "Most term frequency by album in Taylor Swift's Discography",
       caption = '@renorosgo'
  ) +
  scale_color_gradient(low = 'pink2', high = 'darkviolet') +
  # Modificar el tema
  theme_minimal(base_size = 16) +
  theme(
    text = element_text(family = 'BrushScriptMT', color = '#462245'),
    plot.title = element_text(size = 24, hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_text(size = 14, color = '#462245')
  )

tokens %>% 
  # Agrupar por album
  group_by(album_name) %>% 
  # Seleccionar los 50 términos más comunes
  top_n(50, wt = tf_idf)  %>% 
  # Crear un lienzo
  ggplot(aes(label = word, size = tf_idf, col = tf_idf, y = factor(album_year))) + 
  # Crear la nube de palabras
  geom_text_wordcloud(rm_outside = TRUE, 
                      family = 'BrushScriptMT', 
                      eccentricity = 1
  ) +
  # Modificar rango de tamaños
  scale_size(range = c(2,10)) +
  # Modificar etiquetas
  scale_y_discrete(labels = labels) +
  # Agregar títulos
  labs(title = "Most relevant terms by album in Taylor Swift's Discography",
       caption = '@renorosgo'
  ) +
  scale_color_gradient(low = 'pink2', high = 'darkviolet') +
  # Modificar el tema
  theme_minimal(base_size = 16) +
  theme(
    text = element_text(family = 'BrushScriptMT', color = '#462245'),
    plot.title = element_text(size = 24, hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_text(size = 14, color = '#462245')
  )


# Diferencias entre menciones
# Conteos
tokens %>% 
  # Trabajaremos con "Fearles"
  filter(str_detect(album_name, 'Fearles')) %>% 
  # Seleccionar columnos de interes
  select(album_name, word, n) %>% 
  # Estructurar los datos
  spread(album_name, n) %>% 
  # Ordenar de mayor a menor
  arrange(-`"Fearless"`) %>% 
  # Crear un lienzo
  ggplot(aes(x = `"Fearless (Taylor's Version)"`, y = `"Fearless"`)) +
  # Agregar una línea de 45 grados
  geom_abline(col = 'darkviolet', linewidth = 0.5, alpha = .5) +
  # Agregar puntos
  geom_point(size = 4, alpha = .4, col = 'pink2') +
  # Agregar texto
  geom_text(
    aes(label = word), 
    family = 'BrushScriptMT', check_overlap = T, col ='#462245', size = 10
    ) +
  # Modificar los ejes
  scale_y_continuous(limits = c(0,55), breaks = seq(5,55,10)) +
  scale_x_continuous(limits = c(0,55), breaks = seq(5,55,10)) +
  # Agregar títulos
  labs(
    title = "Word count differences in Taylor Swift's 'Fearless' album",
    subtitle = 'Original Version',
    x = "Taylor's Version"
    ) +
  # Modificar el tema
  theme_void(base_size = 16) +
  theme(
    text = element_text(family = 'BrushScriptMT', color = '#462245'),
    plot.title = element_text(size = 24, hjust = 0.5),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 10, color = '#462245'),
  )

# Term Frequency
tokens %>% 
  # Trabajaremos con "Fearles"
  filter(str_detect(album_name, 'Fearles')) %>% 
  # Seleccionar columnos de interes
  select(album_name, word, tf) %>% 
  # Estructurar los datos
  spread(album_name, tf) %>% 
  # Ordenar de mayor a menor
  arrange(-`"Fearless"`) %>% 
  # Crear un lienzo
  ggplot(aes(x = `"Fearless (Taylor's Version)"`, y = `"Fearless"`)) +
  # Agregar una línea de 45 grados
  geom_abline(col = 'darkviolet', linewidth = 0.5, alpha = .5) +
  # Agregar puntos
  geom_point(size = 4, alpha = .4, col = 'pink2') +
  # Agregar texto
  geom_text(
    aes(label = word), 
    family = 'BrushScriptMT', check_overlap = T, col ='#462245', size = 10
  ) +
  # Modificar los ejes
  scale_y_continuous(limits = c(0,0.025)) +
  scale_x_continuous(limits = c(0,0.025)) +
  # Agregar títulos
  labs(
    title = "Word term frequency differences in Taylor Swift's 'Fearless' album",
    subtitle = 'Original Version',
    x = "Taylor's Version"
  ) +
  # Modificar el tema
  theme_void(base_size = 16) +
  theme(
    text = element_text(family = 'BrushScriptMT', color = '#462245'),
    plot.title = element_text(size = 24, hjust = 0.5),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 10, color = '#462245'),
  )


# Tf-IDF
tokens %>% 
  # Trabajaremos con "Fearles"
  filter(str_detect(album_name, 'Fearles')) %>% 
  # Seleccionar columnos de interes
  select(album_name, word, tf_idf) %>% 
  # Estructurar los datos
  spread(album_name, tf_idf) %>% 
  # Ordenar de mayor a menor
  arrange(-`"Fearless"`) %>% 
  # Crear un lienzo
  ggplot(aes(x = `"Fearless (Taylor's Version)"`, y = `"Fearless"`)) +
  # Agregar una línea de 45 grados
  geom_abline(col = 'darkviolet', linewidth = 0.5, alpha = .5) +
  # Agregar puntos
  geom_point(size = 4, alpha = .4, col = 'pink2') +
  # Agregar texto
  geom_text(
    aes(label = word), 
    family = 'BrushScriptMT', check_overlap = T, col ='#462245', size = 10
  ) +
  # Modificar los ejes
  scale_y_continuous(limits = c(0,0.025)) +
  scale_x_continuous(limits = c(0,0.025)) +
  # Agregar títulos
  labs(
    title = "Word TF-IDF differences in Taylor Swift's 'Fearless' album",
    subtitle = 'Original Version',
    x = "Taylor's Version"
  ) +
  # Modificar el tema
  theme_void(base_size = 16) +
  theme(
    text = element_text(family = 'BrushScriptMT', color = '#462245'),
    plot.title = element_text(size = 24, hjust = 0.5),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 10, color = '#462245'),
  )


# Sistemas de recomendación -----------------------------------------------
# Instalar - cargar widyr
if(require(widyr) == FALSE){                                                
  install.packages('widyr')                                                 
}

# Instalar - cargar igraph
if(require(igraph) == FALSE){                                                
  install.packages('igraph')                                                 
  library(igraph)                                                            
}else{                                                                          
  library(igraph)                                                            
}  
# Instalar - cargar ggraph
if(require(ggraph) == FALSE){                                                
  install.packages('ggraph')                                                 
  library(ggraph)                                                            
}else{                                                                          
  library(ggraph)                                                            
}  

# Recomendation network
taylor %>%
  filter(
    is_taylor_version == FALSE,
    !str_detect(title, 'Version|From The Vault|Remix|Bonus')
    )%>% 
  # Tokenizaciôn
  unnest_tokens(
    input = text,
    output = "word", 
    token = "words"
  ) %>%  
  # Contar por album y año
  count(album_name, title, album_year, word)  %>%  View()
  # Filtrar paralbras de paro
  filter(
    !word %in% c(stopwords::stopwords('en',source = 'stopwords-iso'), 'yeah','ooh','ha','day','hey'),
    str_detect(word, '[:alpha:]')
  ) %>% 
  # Ordenar por frecuencia
  arrange(-n) %>% 
  # Agregar tf_idf
  bind_tf_idf(term = word, document = album_name, n = n) %>% 
  # Clacular la correlación
  widyr::pairwise_cor(title, word, sort = TRUE) %>% 
  # Definir un umbral
  filter(correlation > .1) %>%
  # Crear un grafo
  graph_from_data_frame() %>%
  # Crear un lienzo
  ggraph(layout = "circle") +
  # Agregar el lienzo
  geom_edge_link(
    aes(edge_alpha = correlation), 
    edge_color = 'darkviolet',
    show.legend = FALSE
    ) +
  # Agregar puntos
  geom_node_point(color = "pink", size = 5) +
  # Agregar texto
  geom_node_text(
    aes(label = name),
    repel = TRUE, size = 8, 
    family = 'BrushScriptMT', color = '#462245'
    ) +
  # Modificar el tema
  theme_void(base_size = 16) +
  theme(
    text = element_text(family = 'BrushScriptMT', color = '#462245'),
    plot.title = element_text(size = 24, hjust = 0.5),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 10, color = '#462245'),
  )
    

  

