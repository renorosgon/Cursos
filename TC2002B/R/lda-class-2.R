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

# Instalar - Cargar rvest                                                       
if(require(rvest) == FALSE){                                                
  install.packages('rvest')                                                 
  library(rvest)                                                            
}else{                                                                          
  library(rvest)                                                            
}   

# Instalar - Cargar stringi                                                       
if(require(stringi) == FALSE){                                                
  install.packages('stringi')                                                 
  library(stringi)                                                            
}else{                                                                          
  library(stringi)                                                            
}   

# Instalar - Cargar tm                                                  
if(require(tm) == FALSE){                                                
  install.packages('tm')                                                 
}

# Instalar - Cargar udpipe                                                  
if(require(udpipe) == FALSE){                                                
  install.packages('udpipe')                                                 
  library(udpipe)                                                            
}else{                                                                          
  library(udpipe)                                                            
}   

# Web scraping ------------------------------------------------------------
# Función para extraer los informes de gobierno
get_informes =  function(url){
  informe = httr::GET(
    # Definir la URL
    url = url,
    # Fijar usuario agente
    httr::user_agent('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99.0.4844.84 Safari/537.36')
    ) %>% 
    # Leer el HTML
    read_html() %>% 
    # Extraer parrafos
    html_elements(xpath = '//div[@class="article-body"]//p') %>% 
    # Extraer los textos
    html_text() %>% 
    # Colver un tibble
    as_tibble_col(column_name = 'texto') %>% 
    mutate(
      # Extraer los interlocutores
      interlocutores = str_extract(texto, '^[:upper:]{2,} ?[[:upper:]{2,} ?]+'),
      # Remover los interlocutores del texto
      texto = str_remove(texto, '^[:upper:]{2,} ?[[:upper:]{2,} ?]+: ?')
      ) %>% 
    # Rellenar NAs
    fill(interlocutores) %>% 
    # Nos quedamos con las intervenciones de AMPLO
    subset(interlocutores == 'PRESIDENTE ANDRÉS MANUEL LÓPEZ OBRADOR')  %>% 
    # Generamos el tibble final
    summarise(
      doc_id = str_extract(url, '\\w+-informe'),
      texto = str_c(texto, collapse = ' ')
    )
  # Regresa el informe
  return(informe)
}


# Lista de urls
urls = c('https://www.gob.mx/presidencia/es/articulos/version-estenografica-del-primer-informe-de-gobierno?idiom=es',
         'https://www.gob.mx/presidencia/es/articulos/version-estenografica-2-informe-de-gobierno-2019-2020?idiom=es',
         'https://www.gob.mx/presidencia/articulos/version-estenografica-tercer-informe-2020-2021',
         'https://www.gob.mx/presidencia/articulos/version-estenografica-4-informe-de-gobierno?idiom=es',
         'https://www.gob.mx/presidencia/articulos/version-estenografica-5-informe-de-gobierno?idiom=es',
         'https://www.gob.mx/presidencia/articulos/version-estenografica-6-informe-de-gobierno?idiom=es'
         )

# Extraer informes
informes = map_df(urls, get_informes) %>% 
  mutate(doc_id = case_when(
    doc_id == 'primer-informe' ~ '1er informe',
    doc_id == '2-informe' ~ '2ndo informe',
    doc_id == 'tercer-informe' ~ '3er informe',
    doc_id == '4-informe' ~ '4to informe',
    doc_id == '5-informe' ~ '5to informe',
    doc_id == '6-informe' ~ '6to informe'
  ),
  doc_id = factor(doc_id)
  )


# Bigramas ----------------------------------------------------------------
palabras_vacias = c(stopwords::stopwords('es',source = 'stopwords-iso'), 
                    'mil', 'miles', 'meses', 'año' ,'años', 'uno', 'dos', 'tres', 
                    'cuatro', 'cinco', 'nueve', 'seis', 'siete', 'ocho', 
                    'nueve', 'diez', 'millón','millones', 'billón','billones',
                    'peso', 'pesos','unido', 'mes',
                    'construcción', 'paraíso') 

# Crear un diccionario para bigramas
bigramas = informes %>% 
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
    !palabra_1 %in% palabras_vacias,
    !palabra_2 %in% palabras_vacias,
    palabra_1 != palabra_2
  ) %>% 
  # Generar ngramas
  transmute(
    # Regresar al bigrama
    bigrama = paste(palabra_1, palabra_2, sep = ' '),
    # Crear el monograma
    monograma = paste(palabra_1, palabra_2, sep = '_'),
    n = n
  ) %>% 
  filter(
    n > 2,
    !str_detect(bigrama, '\\d')
  )

# Crear un diccionario de monogramas
monogramas = pull(bigramas, monograma)
names(monogramas) = pull(bigramas, bigrama)

# Remplazar bigramas
informes = informes %>% 
  mutate(texto = str_replace_all(texto, monogramas))
# Lematización ------------------------------------------------------------
lemmas = udpipe(
  x = pull(informes, texto), 
  object = 'spanish', 
  parallel.cores = 7
)


# Tokenización ------------------------------------------------------------
informes_tokens = lemmas %>% 
  filter(
    !upos %in% c('SCONJ','CCONJ','SYM','NUM','INTJ','AUX','X'),
    !is.na(upos),
    !lemma %in% palabras_vacias,
    nchar(lemma) > 2,
    !str_detect(lemma, '[:digit:]')
  ) %>% 
  with_groups(
    .groups = doc_id,
    summarise,
    texto = str_c(lemma, collapse = ' ')
  ) %>% 
  unnest_tokens(
    input = texto,
    output = palabra,
    token = 'words'
  ) %>% 
  filter(
    !palabra %in% palabras_vacias,
    !str_detect(palabra, '[:digit:]'),
    nchar(palabra) > 2
  ) %>%
  # Conteo de palabras por articulo
  count(doc_id, palabra) %>% 
  # Agergar TF-IDF
  bind_tf_idf(term = palabra, document = doc_id, n = n)

# Matriz dtm
informes_tokens %>% 
  select(doc_id, palabra, tf) %>% 
  spread(palabra,tf) %>% 
  View()

# Matriz de documentos y términos
informes_dtm = informes_tokens %>% 
  # Creamos la dtm
  cast_dtm(
    # Identificador de cada documento
    document = doc_id, 
    # Terminos a evaluar
    term = palabra,
    # Valor de las celdas
    value = n, 
    # Ponderadores
    weighting = tm::weightTf
  ) 



# Modeloado de Tópicos ----------------------------------------------------
# Install - topicmodels
if(require(topicmodels) == FALSE){                                                
  install.packages('topicmodels')                                                 
  library(topicmodels)                                                            
}else{                                                                          
  library(topicmodels)                                                            
}  

# Crear un LDA
modelo_lda = LDA(
  # Matriz de documentos y términos
  x = informes_dtm, 
  # Número de tópicos
  k = 5,
  # Método de muestreo
  method = 'Gibbs',
  # Semilla aleatoria
  control = list(seed = 123)
  )

print(modelo_lda) 
terms(modelo_lda, k = 10)

# Visualización -----------------------------------------------------------
# Relación Tópico - Documento
gamma_lda = tidy(modelo_lda, matrix = 'gamma')


grafico_gamma = gamma_lda %>% 
  # Crea un gráfico
  ggplot(
    aes(
      # En el eje x la métrica que encuentres adecuada
      x = gamma, 
      # En el eje y las palabras ordenadas por métrica y por interkocutor
      y = reorder_within(document, gamma, topic))) +
  # Agrega columnas color gris
  geom_col(fill = 'gray80') +
  # Agrega cada palabra como etiqueta
  geom_text(aes(label = document, x = 0.00001), 
            # Modifica el color del texto y alinealo a la izquierda
            col = 'gray10', hjust = 'left') +
  # Crea una faceta para cada interlocutor
  facet_wrap(~topic, 
             # Deja libre el eje y, separando en 5 columnas
             scales = 'free_y', ncol = 5) + 
  # Este comando expande los márgenes del gráfico
  scale_x_continuous(expand = c(0,0)) +
  # Modifica el tema
  theme_bw() +
  theme(
    # Elimina el título del eje y
    axis.title.y = element_blank(),
    # Elimina el texto del eje y
    axis.text.y = element_blank(),
    # Modifica la fuente del texto
    text = element_text(family = 'Arial')
  )

# Relación Tópico Palabra
beta_lda = tidy(modelo_lda, matrix = 'beta') 

grafico_beta = beta_lda %>% 
  group_by(topic) %>% 
  top_n(beta, n = 20) %>% 
  # Crea un gráfico
  ggplot(
    aes(
      # En el eje x la métrica que encuentres adecuada
      x = beta, 
      # En el eje y las palabras ordenadas por métrica y por interkocutor
      y = reorder_within(term, beta, topic))) +
  # Agrega columnas color gris
  geom_col(fill = 'gray80') +
  # Agrega cada palabra como etiqueta
  geom_text(aes(label = term, x = 0.00001), 
            # Modifica el color del texto y alinealo a la izquierda
            col = 'gray10', hjust = 'left') +
  # Crea una faceta para cada interlocutor
  facet_wrap(~topic, 
             # Deja libre el eje y, separando en 5 columnas
             scales = 'free_y', ncol = 5) + 
  # Este comando expande los márgenes del gráfico
  scale_x_continuous(expand = c(0,0)) +
  # Modifica el tema
  theme_bw(base_size = 8) +
  theme(
    # Elimina el título del eje y
    axis.title.y = element_blank(),
    # Elimina el texto del eje y
    axis.text.y = element_blank(),
    # Modifica la fuente del texto
    text = element_text(family = 'Arial')
  )



# Install - patchwork
if(require(patchwork) == FALSE){                                                
  install.packages('patchwork')                                                 
  library(patchwork)                                                            
}else{                                                                          
  library(patchwork)                                                            
}  

grafico_beta / grafico_gamma


# Install - ggtern
if(require(ggtern) == FALSE){                                                
  install.packages('ggtern')                                                 
  library(ggtern)                                                            
}else{                                                                          
  library(ggtern)                                                            
}  



gamma_lda %>% 
  # Seleccionar documento 1
#  filter(document %in% c(1,3)) %>% 
  # Formato wide
  spread(key = topic, value = gamma, sep = '_') %>% 
  # Gráficos ternarios
  ggtern(aes(x = topic_1, y = topic_2, z = topic_3)) +
  # Agregar direcciones
  theme_showarrows() + 
  # Agregar puntos
  geom_point(size = 5) +
  # Agregar etiquetas
  geom_text(aes(label = document), col = 'white') +
  geom_text(
    aes(
      # Etiquetas con las coordenadas
      label = paste0(
        '(', round(100 * topic_1,2), ', ',
             round(100 * topic_2,2), ', ', 
             round(100 * topic_3,2), 
        ')')
      ), 
    col = 'gray30', vjust = 2
    ) +
  labs(
    x = 'Tópico 1',
    y = 'Tópico 2',
    z = 'Tópico 3',
    title = 'Informes de Gobierno del Presindente AMLO',
    subtitle = 'Alocación Latente de Dirichlet'
  ) 



# Distribución de Dirchlet
distribucion_lda = gamma_lda %>% 
  # Formato wide
  spread(key = topic, value = gamma, sep = '_') %>% 
  # Gráficos ternarios
  ggtern(aes(x = topic_1, y = topic_2, z = topic_3)) +
  # Agregar direcciones
  theme_showarrows() + 
  # Agregar puntos
  geom_point(size = 5) +
  # Agregar etiquetas
  geom_text(aes(label = document), col = 'white') +
  labs(
    x = 'Tópico 1',
    y = 'Tópico 2',
    z = 'Tópico 3',
    title = 'Informes de Gobierno del Presindente AMLO',
    subtitle = 'Alocación Latente de Dirichlet'
  )

print(distribucion_lda)

# Mapa de calor
mapa_lda = gamma_lda %>% 
  # Crear un lienzo
  ggplot(aes(y = document, x = gamma, fill = factor(topic))) +
  # Agregar las tejas
  geom_col() +
  # Agregar el texto
  geom_text(aes(label = round(100 * gamma,2)), 
            position = 'fill', hjust = 1.5)  +
  # Jugar con las escalas
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  # Agregar etiquetas
  labs(
    x = 'Relación tópico documento',
    subtitle = 'Noª de Informe de Gobierno'
  ) +
  theme(
    axis.title.y = element_blank(),
    legend.position = 'top'
  )

print(mapa_lda)



# Funciones para graficar -------------------------------------------------
distribucion = function(modelo, a){
  # Distribución de Dirchlet
  distribucion_lda = tidy(modelo, matrix = 'gamma') %>% 
    # Formato wide
    spread(key = topic, value = gamma, sep = '_') %>% 
    # Gráficos ternarios
    ggtern(aes(x = topic_1, y = topic_2, z = topic_3)) +
    # Agregar direcciones
    theme_showarrows() + 
    # Agregar puntos
    geom_point(size = 5) +
    # Agregar etiquetas
    geom_text(aes(label = document), col = 'white') +
    labs(
      x = 'Tópico 1',
      y = 'Tópico 2',
      z = 'Tópico 3',
      title = paste0('Relación tópico documento (a=', a, ')'),
      subtitle = 'Alocación Latente de Dirichlet'
    )
  
  return(distribucion_lda)
}


mapa = function(modelo, a){
  # Mapa de calor
  mapa_lda = tidy(modelo, matrix = 'gamma') %>% 
    # Crear un lienzo
    ggplot(aes(y = document, x = gamma, fill = factor(topic))) +
    # Agregar las tejas
    geom_col(show.legend = FALSE) +
    # Agregar el texto
    geom_text(aes(label = round(100 * gamma, 1)), 
              position = 'fill', hjust = 1.5)  +
    # Jugar con las escalas
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0)) +
    # Agregar etiquetas
    labs(
      x = 'Relación tópico documento',
      subtitle = 'Noª de Informe de Gobierno',
      title = paste0('Alpha: ', a)
    ) +
    theme(
      axis.title.y = element_blank(),
      legend.position = 'top'
    )
  return(mapa_lda)
}

beta_plot = function(modelo, a){

  grafico_beta = tidy(modelo, matrix = 'beta')  %>% 
    group_by(topic) %>% 
    top_n(beta, n = 10) %>% 
    # Crea un gráfico
    ggplot(
      aes(
        # En el eje x la métrica que encuentres adecuada
        x = beta, 
        # En el eje y las palabras ordenadas por métrica y por interkocutor
        y = reorder_within(term, beta, topic))) +
    # Agrega columnas color gris
    geom_col(fill = 'gray80') +
    # Agrega cada palabra como etiqueta
    geom_text(aes(label = term, x = 0.00001), 
              # Modifica el color del texto y alinealo a la izquierda
              col = 'gray10', hjust = 'left') +
    labs(
      x = 'Relación tópico-palabra',
      subtitle = 'Noª de Informe de Gobierno',
      title = paste0('Alpha: ', a)
    ) +
    # Crea una faceta para cada interlocutor
    facet_wrap(~topic, 
               # Deja libre el eje y, separando en 5 columnas
               scales = 'free_y', ncol = 5) + 
    # Este comando expande los márgenes del gráfico
    scale_x_continuous(expand = c(0,0)) +
    # Modifica el tema
    theme_bw(base_size = 8) +
    theme(
      # Elimina el título del eje y
      axis.title.y = element_blank(),
      # Elimina el texto del eje y
      axis.text.y = element_blank(),
      # Modifica la fuente del texto
      text = element_text(family = 'Arial')
    )
  
  return(grafico_beta)
}

# Parámetros de LDA -------------------------------------------------------
modelo = function(alpha){
  LDA(
    # Matriz de documentos y términos
    x = informes_dtm, 
    # Número de tópicos
    k = 5,
    # Método de muestreo
    method = 'Gibbs',
    control = list(
      # Semilla aleatoria
      seed = 123,
      alpha = alpha
    )
  )
  }
  
# Graficar los modelos
alpha_0 = mapa(modelo(0), a = 0)
dist_0 = distribucion(modelo(0), a = 0)

alpha_1 = mapa(modelo(1), a = 1)
dist_1 = distribucion(modelo(1), a = 1)

alpha_10 = mapa(modelo(10), a = 10)
dist_10 = distribucion(modelo(10), a = 10)

alpha_100 = mapa(modelo(100), a = 100)
dist_100 = distribucion(modelo(100), a = 100)

# Comparando modelos
(alpha_0 + alpha_1) / (alpha_10 + alpha_100) 


modelo = function(delta){
  LDA(
    # Matriz de documentos y términos
    x = informes_dtm, 
    # Número de tópicos
    k = 5,
    # Método de muestreo
    method = 'Gibbs',
    control = list(
      # Semilla aleatoria
      seed = 123,
      delta = delta
    )
  )
}

# Graficar los modelos
beta_0 = beta_plot(modelo(0.001), a = 0)
beta_1 = beta_plot(modelo(1), a = 1)
beta_10 = beta_plot(modelo(10), a = 10)
beta_100 = beta_plot(modelo(100), a = 100)

# Comparando modelos
(beta_0 + beta_1) / (beta_10 + beta_100) 


# Visualización Interactiva -----------------------------------------------
# Install - LDAvis
if(require(LDAvis) == FALSE){                                                
  install.packages('LDAvis')                                                 
  library(LDAvis)                                                            
}else{                                                                          
  library(LDAvis)                                                            
}  
# Install - slam
if(require(slam) == FALSE){                                                
  install.packages('slam')                                                 
}

# Relación tópico-palabra
beta = modelo_lda %>% 
  posterior() %>% 
  pluck('terms') 

# Relación tópico-documento
gamma = modelo_lda %>% 
  posterior() %>% 
  pluck('topics')

# Definir el vocabulario
vocabulario = colnames(beta)
# Longitud de documentos
doc.length = slam::row_sums(informes_dtm)
# Frecuencia de términos
term.freq = slam::col_sums(informes_dtm)[match(vocabulario, colnames(informes_dtm))]

# Crear un json
json = createJSON(
  phi = beta, 
  theta = gamma,
  vocab = vocabulario,
  doc.length = doc.length, 
  term.frequency = term.freq)

# Visualizar
serVis(json)



gamma_lda %>% 
  spread(topic, gamma) %>% 
  View()



