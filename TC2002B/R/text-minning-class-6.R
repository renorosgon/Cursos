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

# Instalar - cargar udpipe
if(require(udpipe) == FALSE){                                                
  install.packages('udpipe')                                                 
  library(udpipe)                                                            
}else{                                                                          
  library(udpipe)                                                            
}  

# Instalar - cargar textdata
if(require(textdata) == FALSE){                                                
  install.packages('textdata')                                                 
  library(textdata)                                                            
}


# Datos -------------------------------------------------------------------
# Discografía Taylor Swift
taylor = read_csv('data/taylor_swift_dataset.csv') 
# Lexicos
sentiments = get_sentiments('bing')


# Lematización
# Descargar el modelo en ingles
# udpipe_download_model(language = "english")
lemmas = udpipe(x = pull(taylor, text), object = 'english', parallel.cores = 4)

# Concatenar los datos
taylor_sentiments = lemmas %>% 
  # Unir por la izquierda
  left_join(
    # Nuestro tibble de sentimientos
    sentiments,
    # Donde el lemma coincida con la palabra
    by = c('lemma'='word'), 
    # Es una relacion muchos a muchos
    relationship = "many-to-many"
    ) %>%
  # Agrupar por documentos
  with_groups(
    .groups = doc_id,
    mutate,
    # Crear un índice de avance de la canción
    index = round((sentence_id - min(sentence_id))/(max(sentence_id) - min(sentence_id)), 2)
  ) %>% 
  # Contabilizar los sentimientos por documento e indice
  count(doc_id, index, sentiment) %>%
  # Agregar neutral
  mutate(sentiment = coalesce(sentiment, 'neutral')) %>% 
  # Expandir el tibble por sentimientop
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  # Generar el score de polaridad
  mutate(sentiment = positive - negative) %>% 
  # Concatenar metadatos de los albums
  left_join(
    # Agregar doc_id
    mutate(taylor, doc_id = as.character(1:n())),
    by = 'doc_id'
  ) %>% 
  # Ordenar por año yid
  arrange(album_year, doc_id) %>% 
  # Crear un indicador por año
  with_groups(
    .groups = album_year,
    mutate,
    year_id = (1:n())/n()
  ) %>% 
  # Volver albul name un factor ordenado
  mutate(
    album_name = factor(
      album_name, levels = c("\"Taylor Swift\"",
                             "\"Sounds Of The Season: The Taylor Swift Holiday Collection\"",
                             "\"Fearless\"", "\"Speak Now\"","\"Red\"","\"1989\"", 
                             "\"Reputation\"","\"Lover\"","\"evermore\"","\"folklore\"",
                             "\"Red (Taylor's Version)\"","\"Fearless (Taylor's Version)\"",
                             "\"Midnights\"","\"Speak Now (Taylor's Version)\"")
    )
  )


# Crear un vector de etiquetas
labels = taylor_sentiments %>% 
  # Ordenar por año
  arrange(album_year) %>% 
  # Seleccionar nombre y año
  select(album_name, album_year, year_id) %>% 
  # Resumir etiquetas
  with_groups(
    .groups = c(album_name, album_year),
    reframe,
    year_id = mean(year_id)
  ) %>% 
  # Ordenar por año
  arrange(album_year, year_id) %>% 
  # Editar texto
  mutate(
    album_name = str_replace_all(album_name, ' \\(' , '\n\\('),
    album_name = str_replace_all(album_name, ': ' , ':\n'),
    # Coordenadas del texto
    y = rep(c(-14,15,-19,20,-17,17,-17),n()/7),
    yend = rep(c(-13,13,-18,18,-15,15,-15),n()/7)
    )


# Gráfica -----------------------------------------------------------------
taylor_sentiments %>%
  # Crear un lienzo
  ggplot(aes(x = album_year + year_id, y = sentiment)) +
  # Ageregar lína horizontal
  geom_hline(yintercept = 0) +
  # Agregar plecas (Segementos)
  geom_segment(
    # Usamos nuestro tibble de etiquetas
    data = labels, 
    # Coordenadas
    aes(
      x = album_year + year_id, 
      xend = album_year + year_id,
      y = yend , 
      yend = 0
    ),
    # Estética de la línea
    linetype = 'dashed', col = 'gray50'
  ) +
  # Agregar columnas
  geom_col(aes(fill = album_name, col = album_name), show.legend = F) +
  # Modificar colores
  scale_color_manual(values = rep(c('red4','darkviolet'),7)) +
  # Agregar texto
  geom_text(
    # Usamos nuestro tibble de etiquetas
    data = labels, 
    # Agregar coordenadas
    aes(x = album_year + year_id, label = album_name, y = y),
    # Jugar con la tipografía y el color
    family = 'Bebas Neue', color = '#462245'
  ) +
  scale_y_continuous(limits = c(-40,40)) +
  # Agregar anotaciones 
  annotate('text', x = 2006, y = 40, label = 'Positive', col = 'gray40', hjust = 'left') +
  annotate('text', x = 2006, y = -40, label = 'Negative', col = 'gray40', hjust = 'left') +
  # Agregar etiquetas
  labs(title = "Polarity score of Taylor Swift's Discography by song") +
  # Modificar el tema
  theme_void(base_size = 16) +
  theme(
    text = element_text(family = 'Axiforma', color = 'gray40'),
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_text(size = 10, color ='gray40')
  )
  

