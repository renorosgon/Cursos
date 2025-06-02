# Fijar directorio de trabajo
setwd("~/Desktop/ITESM/Cursos/TC2002B/")


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

# Instalar - Cargar tidytext                                                       
if(require(tidytext) == FALSE){                                                
  install.packages('tidytext')                                                 
  library(tidytext)                                                            
}else{                                                                          
  library(tidytext)                                                            
}   

# Instalar - Cargar pdftools                                                       
if(require(pdftools) == FALSE){                                                
  install.packages('pdftools')                                                 
  library(pdftools)                                                            
}else{                                                                          
  library(pdftools)                                                            
}   


# Datos -------------------------------------------------------------------
banxico_url = httr::GET(
  # Definir la url
  url = 'https://www.banxico.org.mx',
  # Definir el path
  path = 'publicaciones-y-prensa/anuncios-de-las-decisiones-de-politica-monetaria/anuncios-politica-monetaria-t.html'
) %>% 
  # Leer el html
  read_html() 

# Obtener un alista de paths
paths = banxico_url %>% 
  # Extraer vínculos
  html_elements(xpath = '//td/a/@href') %>% 
  # Volver textos
  html_text()

# Función para estructurar
leer_anuncios = function(path){
  # Definir la url del pdf
  pdf_url = paste0('https://www.banxico.org.mx/', path)
  
  # Extraer el texto pdf
  texto = pdf_text(pdf_url) %>% 
    # Separar por salto de línea
    str_split(pattern = '\n\n') %>% 
    # Volver vector de caracteres
    flatten_chr() %>% 
    # Concatenar los textos
    str_c(collapse = ' ') %>% 
    # Eliminar espacios
    str_squish()
  
  return(texto)
}

# Extrear todos los textos
textos = map_chr(paths, leer_anuncios)

# Estrucutrar un tibble
anuncios_politica_monetaria = banxico_url %>% 
  # Extraer la table
  html_element(xpath = '//table') %>% 
  html_table() %>% 
  # Cambiar nombres
  rename(doc_id = X1, anuncio = X2) %>% 
  # Agregar metadatos
  mutate(
    month = str_remove_all(str_extract(doc_id, '/\\d{2}/'),'/'),
    year = as.numeric(paste0('20',str_extract(doc_id, '\\d{2}$'))),
    fecha = ym(paste(year, month, sep = '-')),
    month = month(fecha, label = TRUE),
    path = paths,
    text = textos,
    decision = case_when(
      str_detect(anuncio, 'aumenta|incrementa') ~ 'aumento',
      str_detect(anuncio, 'reduce|disminuye') ~ 'reducción',
      str_detect(anuncio, 'sin cambio') ~ 'sin cambio'
    ),
    decision = factor(decision, levels = c('reducción','sin cambio', 'aumento'))
  ) %>% 
  filter(fecha > '2008-01-01')


# Lematización ------------------------------------------------------------
#  Instalar - Cargar udpipe                                
if(require(udpipe) == FALSE){                                                
  install.packages('udpipe')                                                 
  library(udpipe)                                                            
}else{                                                                          
  library(udpipe)                                                            
}   

# Construcción de lemas
lemmas = udpipe(
  x = anuncios_politica_monetaria, 
  object = 'spanish',
  parallel.cores = 7
) %>% 
  # Filtrar elementos irrelevantes
  filter(
    !upos %in% c('PUNCT','SCONJ','CCONJ','SYM','NUM','INTJ','AUX'),
    !is.na(upos),
    !lemma %in% c(stopwords::stopwords('es',source = 'stopwords-iso'),
                  'año','ene','feb','mar','abr','may','jun','jul','ago',
                  'sep','oct','nov','dic','gráfico','p.p'),
    nchar(lemma) > 2,
    !str_detect(lemma,'[:digit:]')
  ) %>% 
  # Pasar a minúsculas
  mutate(lemma = tolower(lemma)) %>% 
  # Concatenar textos
  with_groups(
    .groups = doc_id,
    reframe,
    text = str_c(lemma, collapse = ' ')
  ) 

# Crear un diccionario para bigramas
bigramas = lemmas %>% 
  # Generar bigramas
  unnest_tokens(
    input = text,
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
    !palabra_1 %in% c(stopwords::stopwords('es',source = 'stopwords-iso'),'decidir','fuente','nivel','objetivo'),
    !palabra_2 %in% c(stopwords::stopwords('es',source = 'stopwords-iso'),'mantener','unido','nivel',
                      'decidir','méxico','inflación','objetivo','meta'),
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
    n > 30,
    !str_detect(bigrama, '\\d')
  )

# Crear un diccionario de monogramas
monogramas = pull(bigramas, monograma)
names(monogramas) = pull(bigramas, bigrama)

# Remplazar bigramas
lemmas = lemmas %>% 
  mutate(text = str_replace_all(text, monogramas))


# Correlated Topic Modelling ----------------------------------------------
# Install - stm
if(require(stm) == FALSE){                                                
  install.packages('stm')                                                 
  library(stm)                                                            
}else{                                                                          
  library(stm)                                                            
}  

# Preprocesamiento de los datos
procesamiento = textProcessor(
  # Identificar documentos
  documents = pull(lemmas, text), 
  # Agregar metadatos
  metadata = anuncios_politica_monetaria, 
  # Definir lenguaje
  language = "es", 
  # Definir radicalización
  stem = FALSE,
  # Remover signos de puntuación
  removepunctuation = FALSE,
  # Minúsculas
  lowercase = TRUE,
  # Remover valabras de paro
  removestopwords = TRUE,
  # Remover números
  removenumbers = TRUE,
  # Longitud de palabras
  wordLengths = c(3, Inf),
  # Palabras de paro personalizadas
  customstopwords = c('méxico')
)

# Preparar documentos
out = prepDocuments(
  # Extraer documentos
  documents = pluck(procesamiento, 'documents'), 
  # Extraer vocabulario
  vocab = pluck(procesamiento, 'vocab'), 
  # Fijar umbral inferior de aparición en documentos
  lower.thresh = nrow(anuncios_politica_monetaria) * 0.05, 
  # Fijar umbral superior de aparición en documentos
  upper.thresh = nrow(anuncios_politica_monetaria) * 0.95
)

# Ajusta el modelo CTM
modelo_ctm = stm(
  # Documentos
  documents = pluck(out, 'documents'), 
  # Vocabulario
  vocab = pluck(out, 'vocab'), 
  # Número de tópicos
  K = 3, 
  # Tolerancia de interaciones
  max.em.its = 10000,
  # Tipo de búsqueda (Spectral es la más rápida)
  init.type = "Spectral", 
  seed = 123
)

# Mostrar etiquetas
labelTopics(modelo_ctm)

# Resumen del modelo
plot(modelo_ctm, 
     type = "summary", 
     # Tamaño de letra
     text.cex = 1, 
     # Título
     main = "Modelo de Tópicos Correlacionados", 
     # Nombre del eje
     xlab = "Preponderancia promedio")


# Histograma de relación tópico documento
plot(modelo_ctm, type = "hist", main = "Relación tópico-documento")

# Principales términos
plot(modelo_ctm, type = "labels", main = "Términos más relevantes")

# Comparar tópicos
plot(modelo_ctm, type = "perspectives", topics = c(3, 2))

# Correlación del tópico
mod.out.corr = topicCorr(model = modelo_ctm)
plot(mod.out.corr)

# Detras del código
# Probabilidad tópico-documento
gammas = tidy(modelo_ctm, matrix = 'gamma') 


# Correlograma
if(require(GGally) == FALSE){                                                
  install.packages('GGally')                                                 
}
gammas %>% 
  # Formato wide
  spread(key = topic, value = gamma) %>% 
  # Eliminar el id de documento
  select(-document) %>% 
  # Calcular la correlación
  cor() %>% 
  # Correlograma
  GGally::ggcorr(label = T)


# Gráficos ternarios
# Install - ggtern
if(require(ggtern) == FALSE){                                                
  install.packages('ggtern')                                                 
  library(ggtern)                                                            
}else{                                                                          
  library(ggtern)                                                            
}  

gammas %>% 
  # Formato wide
  spread(key = topic, value = gamma, sep = '_') %>% 
  # Gráficos ternarios
  ggtern(aes(x = topic_1, y = topic_2, z = topic_3)) +
  # Agregar direcciones
  theme_showarrows() + 
  #define first data geometry
  stat_density_tern(
    aes(
      fill = after_stat(level), 
      alpha = after_stat(level)
    ),
    bdl = 0.05,
    geom='polygon',
    show.legend = FALSE) +
  # Agregar puntos
  geom_point(alpha = 0.5) +
  labs(
    x = 'Tópico 1',
    y = 'Tópico 2',
    z = 'Tópico 3',
    title = 'Minutas de Política Monetaria',
    subtitle = 'Modelo de Tópicos Correlacionados'
  ) 

# Visualizar correlación entre dos tópicos
gammas %>% 
  # Formato wide
  spread(key = topic, value = gamma, sep = '_') %>% 
  # Crear un lienzo
  ggplot(aes(x = topic_1, y = topic_2)) +
  # Agregar puntos
  geom_point() +
  # Modificar ejes
  scale_y_continuous(
    breaks = c(0.001, 0.01,0.1,0.5), 
    labels = scales::percent
  ) +
  scale_x_continuous(
    breaks = c(0.001, 0.01,0.1,0.5), 
    labels = scales::percent
  ) +
  # Agregar etiquetas
  labs(
    x = 'Tópico 1',
    title = 'Correlación entre tópicos',
    subtitle = 'Tópico 2'
  ) +
  # Modificar coordinadas
  coord_trans(x="log2", y="log2") +
  # Modificar tema
  theme_bw() +
  theme(
    axis.title.y = element_blank()
  )

# Lee & Mimo (2014) -------------------------------------------------------
# Algoritmo de Lee and Mimno (2014)
modelo_ctm = stm(
  documents = pluck(out, 'documents'), 
  vocab = pluck(out, 'vocab'), 
  K = 0, 
  max.em.its = 10000,
  init.type = "Spectral", 
  seed = 123
)

# Mostrar etiquetas
labelTopics(modelo_ctm)

# Resumen del modelo
plot(modelo_ctm, 
     type = "summary", 
     # Tamaño de letra
     text.cex = 1, 
     # Título
     main = "Modelo de Tópicos Correlacionados", 
     # Nombre del eje
     xlab = "Preponderancia promedio"
     )


# Histograma de relación tópico documento
plot(modelo_ctm, type = "hist", main = "Relación tópico-documento")

# Principales términos
plot(modelo_ctm, type = "labels", main = "Términos más relevantes", topics = 1:5)

# Comparar tópicos
plot(modelo_ctm, type = "perspectives", topics = c(2, 36))

# Correlación del tópico
mod.out.corr = topicCorr(model = modelo_ctm)
plot(mod.out.corr)

# Detras del código
# Probabilidad tópico-documento
gammas = tidy(modelo_ctm, matrix = 'gamma') 

gammas %>% 
  # Formato wide
  spread(key = topic, value = gamma) %>% 
  # Eliminar el id de documento
  select(-document) %>% 
  # Calcular la correlación
  cor() %>% 
  # Correlograma
  GGally::ggcorr(label = T, label_size = 2)

gammas %>% 
  # Formato wide
  spread(key = topic, value = gamma, sep = '_') %>% 
  # Gráficos ternarios
  ggtern(aes(x = topic_3, y = topic_20, z = topic_23)) +
  # Agregar direcciones
  theme_showarrows() + 
  #define first data geometry
  stat_density_tern(
    aes(
      fill = after_stat(level), 
      alpha = after_stat(level)
    ),
    bdl = 0.1,
    geom='polygon',
    show.legend = FALSE) +
  # Agregar puntos
  geom_point(alpha = 0.5) +
  labs(
    x = 'Tópico 3',
    y = 'Tópico 20',
    z = 'Tópico 23',
    title = 'Minutas de Política Monetaria',
    subtitle = 'Modelo de Tópicos Correlacionados'
  ) 

# Visualizar correlación entre dos tópicos
gammas %>% 
  # Formato wide
  spread(key = topic, value = gamma, sep = '_') %>% 
  # Crear un lienzo
  ggplot(aes(x =topic_3, y = topic_20)) +
  # Agregar puntos
  geom_point() +
  # Modificar ejes
  scale_y_continuous(
    breaks = c(0.001, 0.01,0.1,0.5), 
    labels = scales::percent
    ) +
  scale_x_continuous(
    breaks = c(0.001, 0.01,0.1,0.5), 
    labels = scales::percent
  ) +
  # Agregar etiquetas
  labs(
    x = 'Tópico 3',
    title = 'Correlación entre tópicos',
    subtitle = 'Tópico 20'
  ) +
  # Modificar coordinadas
  coord_trans(x="log2", y="log2") +
  # Modificar tema
  theme_bw() +
  theme(
    axis.title.y = element_blank()
  )

# Visualizar correlación entre dos tópicos
gammas %>% 
  # Formato wide
  spread(key = topic, value = gamma, sep = '_') %>% 
  # Crear un lienzo
  ggplot(aes(x =topic_20, y = topic_23)) +
  # Agregar puntos
  geom_point() +
  # Modificar ejes
  scale_y_continuous(
    breaks = c(0.001, 0.01,0.1,0.5), 
    labels = scales::percent
  ) +
  scale_x_continuous(
    breaks = c(0.001, 0.01,0.1, 0.5), 
    labels = scales::percent
  ) +
  # Agregar etiquetas
  labs(
    x = 'Tópico 20',
    title = 'Correlación entre tópicos',
    subtitle = 'Tópico 23'
  ) +
  # Modificar coordinadas
  coord_trans(x="log2", y="log2") +
  # Modificar tema
  theme_bw() +
  theme(
    axis.title.y = element_blank()
  )


# Principales términos
plot(modelo_ctm, type = "labels", main = "Términos más relevantes", 
     topics = c(2,20,23))


