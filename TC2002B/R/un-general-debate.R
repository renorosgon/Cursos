# Fijar directorio de trabajo
setwd("~/Desktop/ITESM/Cursos/TC2002B/")

# Instalación y carga de paqueterías
# Instalar - Cargar tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}

# Instalar - Cargar tidytext                                                       
if(require(tidytext) == FALSE){                                                
  install.packages('tidytext')                                                 
  library(tidytext)                                                            
}else{                                                                          
  library(tidytext)                                                            
}   

# Instalar - Cargar udpipe                                                  
if(require(udpipe) == FALSE){                                                
  install.packages('udpipe')                                                 
  library(udpipe)                                                            
  library(udpipe)                                                            
}   

# Install - topicmodels
if(require(topicmodels) == FALSE){                                                
  install.packages('topicmodels')                                                 
  library(topicmodels)                                                            
}else{                                                                          
  library(topicmodels)                                                            
}  



folders = list.files(path = 'data/un_general_debate', full.names = TRUE)
files = map(.x = folders, .f = list.files, full.names = TRUE) %>% 
  flatten_chr()

read_debates = function(file){
  tibble(
    session = str_extract(file, 'Session \\d+'),
    year = str_extract(file, '\\d{4}'),
    country = str_extract(file, '[A-Z]{3}'),
    text = read_file(file)
  )
} 
  
debates = map_df(.x = files, .f = read_debates)

debates = debates %>% 
  mutate(
    year = as.numeric(year),
    text = str_squish(text)
  )

write_rds(debates, file = 'data/un_general_debate.rds')

debates = read_rds('data/un_general_debate.rds') %>% 
  mutate(doc_id = paste(year, country, sep = '-')) 
  filter(year > 2019)



lemmas = udpipe(
  x = debates, 
  object = 'english', 
  parallel.cores = 7
)

tokens = lemmas %>% 
  filter(
    !upos %in% c('SCONJ','CCONJ','SYM','NUM','INTJ','AUX','X'),
    !is.na(upos),
    !lemma %in% stopwords::stopwords('en',source = 'stopwords-iso'),
    nchar(lemma) > 2,
    !str_detect(lemma, '\\.|\\d')
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
    nchar(palabra) > 2
  ) %>%
  # Conteo de palabras por articulo
  count(doc_id, palabra) %>% 
  # Agergar TF-IDF
  bind_tf_idf(term = palabra, document = doc_id, n = n)

# Matriz de documentos y términos
dtm = tokens %>% 
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
  x = dtm, 
  # Número de tópicos
  k = 5,
  # Método de muestreo
  method = 'VEM',
  # Semilla aleatoria
  control = list(seed = 123)
)

terms(modelo_lda, k = 10)

# Visualización Interactiva -----------------------------------------------
# Install - LDAvis
if(require(LDAvis) == FALSE){                                                
  install.packages('LDAvis')                                                 
  library(LDAvis)                                                            
}else{                                                                          
  library(LDAvis)                                                            
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
doc.length = slam::row_sums(minutas_dtm)
# Frecuencia de términos
term.freq = slam::col_sums(minutas_dtm)[match(vocabulario, colnames(minutas_dtm))]

# Crear un json
json = createJSON(
  phi = beta, 
  theta = gamma,
  vocab = vocabulario,
  doc.length = doc.length, 
  term.frequency = term.freq)

# Visualizar
serVis(json)


# Gráficos ternarios ------------------------------------------------------
# Install - ggtern
if(require(ggtern) == FALSE){                                                
  install.packages('ggtern')                                                 
  library(ggtern)                                                            
}else{                                                                          
  library(ggtern)                                                            
}  

# Probabilidad tópico-documento
gammas = tidy(modelo_lda, matrix = 'gamma') 

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
    geom='polygon',
    show.legend = FALSE) +
  # Agregar puntos
  geom_point(alpha = 0.5) +
  labs(
    x = 'Tópico 1',
    y = 'Tópico 2',
    z = 'Tópico 3',
    title = 'Tweets de Periodistas',
    subtitle = 'Alocación Latente de Dirichlet'
  ) 



# Correlated Topic Modelling ----------------------------------------------
library(stm)
procesamiento = textProcessor(
  documents = dtm, 
  language = "en", 
  stem = FALSE
)


out = prepDocuments(
  documents = pluck(procesamiento, 'documents'), 
  vocab = pluck(procesamiento, 'vocab'), 
  lower.thresh = 29, 
  upper.thresh = 565
)

modelo_ctm <- stm(
  documents = pluck(out, 'documents'), 
  vocab = pluck(out, 'vocab'), 
  K = 3, 
  max.em.its = 10000,
  init.type = "Spectral", 
  seed = 123
)

labelTopics(modelo_ctm)

toLDAvis(mod=modelo_ctm, docs=pluck(out,'documents'))

# Probabilidad tópico-documento
gammas = tidy(modelo_ctm, matrix = 'gamma') 

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
    geom='polygon',
    show.legend = FALSE) +
  # Agregar puntos
  geom_point(alpha = 0.5) +
  labs(
    x = 'Tópico 1',
    y = 'Tópico 2',
    z = 'Tópico 3',
    title = 'Minutas de Política Monetaria',
    subtitle = 'Alocación Latente de Dirichlet'
  ) 


plot(modelo_ctm, type = "summary", 
     text.cex = 1, 
     main = "Topic shares on the corpus as a whole", 
     xlab = "estimated share of topics")


plot(modelo_ctm, type = "hist", topics = 1:3, 
     main = "histogram of the topic shares within the documents")


plot(modelo_ctm, type = "labels", topics = 1:3, main = "Topic terms")


modelo_ctm <-  stm(
  documents = pluck(out, 'documents'), 
  vocab = pluck(out, 'vocab'), 
  K = 10, 
  max.em.its = 10000,
  init.type = "Spectral", 
  seed = 123
)

mod.out.corr <- topicCorr(model = modelo_ctm)
plot(mod.out.corr)

labelTopics(modelo_ctm)
# Probabilidad tópico-documento
gammas = tidy(modelo_ctm, matrix = 'gamma') 

gammas %>% 
  # Formato wide
  spread(key = topic, value = gamma, sep = '_') %>% 
  # Gráficos ternarios
  ggtern(aes(x = topic_1, y = topic_2, z = topic_3)) +
  # Agregar direcciones
  theme_showarrows() + 
  #define first data geometry
  geom_density_tern() +
  # Agregar puntos
  geom_point(alpha = 0.5) +
  labs(
    x = 'Tópico 1',
    y = 'Tópico 2',
    z = 'Tópico 3',
    title = 'Minutas de Política Monetaria',
    subtitle = 'Modelo de Tópicos Correlacionados'
  ) 


tweets_tokens = lemmas %>% 
  filter(
    !upos %in% c('SCONJ','CCONJ','SYM','NUM','INTJ','AUX','X'),
    !is.na(upos),
    !lemma %in% stopwords::stopwords('es',source = 'stopwords-iso'),
    nchar(lemma) > 2,
    !str_detect(lemma, '\\.')
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
    nchar(palabra) > 2
  ) %>%
  # Conteo de palabras por articulo
  count(doc_id, palabra) %>% 
  # Agergar TF-IDF
  bind_tf_idf(term = palabra, document = doc_id, n = n)

# Matriz de documentos y términos
tweets_dtm = tweets_tokens %>% 
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
  x = tweets_dtm, 
  # Número de tópicos
  k = 3,
  # Método de muestreo
  method = 'Gibbs',
  # Semilla aleatoria
  control = list(seed = 123)
)

terms(modelo_lda, k = 10)

# Visualización Interactiva -----------------------------------------------
# Install - LDAvis
if(require(LDAvis) == FALSE){                                                
  install.packages('LDAvis')                                                 
  library(LDAvis)                                                            
}else{                                                                          
  library(LDAvis)                                                            
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
doc.length = slam::row_sums(minutas_dtm)
# Frecuencia de términos
term.freq = slam::col_sums(minutas_dtm)[match(vocabulario, colnames(minutas_dtm))]

# Crear un json
json = createJSON(
  phi = beta, 
  theta = gamma,
  vocab = vocabulario,
  doc.length = doc.length, 
  term.frequency = term.freq)

# Visualizar
serVis(json)


# Gráficos ternarios ------------------------------------------------------
# Install - ggtern
if(require(ggtern) == FALSE){                                                
  install.packages('ggtern')                                                 
  library(ggtern)                                                            
}else{                                                                          
  library(ggtern)                                                            
}  

# Probabilidad tópico-documento
gammas = tidy(modelo_lda, matrix = 'gamma') 

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
    geom='polygon',
    show.legend = FALSE) +
  # Agregar puntos
  geom_point(alpha = 0.5) +
  labs(
    x = 'Tópico 1',
    y = 'Tópico 2',
    z = 'Tópico 3',
    title = 'Tweets de Periodistas',
    subtitle = 'Alocación Latente de Dirichlet'
  ) 



# Correlated Topic Modelling ----------------------------------------------
procesamiento = textProcessor(
  documents = pull(debates, text), 
  metadata = debates, 
  language = "en", 
  stem = FALSE,
  removepunctuation = TRUE,
  lowercase = TRUE,
  removestopwords = TRUE,
  removenumbers = TRUE,
  wordLengths = c(3, Inf)
)


out = prepDocuments(
  documents = pluck(procesamiento, 'documents'), 
  vocab = pluck(procesamiento, 'vocab'), 
  lower.thresh = 100, 
  upper.thresh = 10400
)



modelo_ctm <- stm(
  documents = pluck(out, 'documents'), 
  vocab = pluck(out, 'vocab'), 
  K = 5, 
  max.em.its = 10000,
  init.type = "Spectral", 
  seed = 123
)

labelTopics(modelo_ctm)

toLDAvis(mod=modelo_ctm, docs=pluck(out,'documents'))

# Probabilidad tópico-documento
gammas = tidy(modelo_ctm, matrix = 'gamma') 

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
    bdl = 0.1,
    geom='polygon',
    show.legend = FALSE) +
  # Agregar puntos
  geom_point(alpha = 0.5) +
  labs(
    x = 'Tópico 1',
    y = 'Tópico 2',
    z = 'Tópico 3',
    title = 'Minutas de Política Monetaria',
    subtitle = 'Alocación Latente de Dirichlet'
  ) 


plot(modelo_ctm, type = "summary", 
     text.cex = 1, 
     main = "Topic shares on the corpus as a whole", 
     xlab = "estimated share of topics")


plot(modelo_ctm, type = "hist", topics = 1:5, 
     main = "histogram of the topic shares within the documents")


plot(modelo_ctm, type = "labels", topics = 1:5, main = "Topic terms")


modelo_ctm <-  stm(
  documents = pluck(out, 'documents'), 
  vocab = pluck(out, 'vocab'), 
  K = 20, 
  max.em.its = 10000,
  init.type = "Spectral", 
  seed = 123
)

mod.out.corr <- topicCorr(model = modelo_ctm)
GGally::ggcorr(mod.out.corr$cor, label = T)
plot(mod.out.corr)

labelTopics(modelo_ctm)
# Probabilidad tópico-documento 
gammas = tidy(modelo_ctm, matrix = 'gamma') 

gammas %>% 
  # Formato wide
  spread(key = topic, value = gamma, sep = '_') %>% 
  # Gráficos ternarios
  ggtern(aes(x = topic_1, y= topic_5, z = topic_9)) +
  # Agregar direcciones
  theme_showarrows() + 
  #define first data geometry
  stat_density_tern(aes(
    fill = after_stat(level), 
    alpha = after_stat(level)
  ),
  geom='polygon',
  show.legend = FALSE,
  bdl = 0.1
  ) +
  # Agregar puntos
  geom_point(alpha = 0.5) +
  labs(
    x = 'Tópico 1',
    y = 'Tópico 5',
    z = 'Tópico 9',
    title = 'Debate General de Naciones Unidas',
    subtitle = 'Modelo de Tópicos Correlacionados'
  ) 









