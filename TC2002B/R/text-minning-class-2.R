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
url <- "https://candidaturas2021.ine.mx/documentos/descargas/baseDatosCandidatos.xls" 

#Descargar en la carpeta "datos"
download.file(url, destfile = "data/baseDatosCandidatos.xls", mode = "wb")

# Leer el conjunto de datos
candidaturas = readxl::read_xls('data/baseDatosCandidatos.xls') %>% 
  # Homogeneizar los nombres
  janitor::clean_names()

# Seleccionar las propuestas de género
propuestas_genero = candidaturas %>% 
  select(partido_coalicion:distrito, nombre_candidato:genero, escolaridad, propuesta_genero)

# Qué partidos omitieron propuestas de género
propuestas_genero %>% 
  with_groups(
    .groups = partido_coalicion,
    summarise,
    candidaturas = n(),
    sin_propuestas = sum(is.na(propuesta_genero))
  ) %>% 
  ggplot(
    aes(
      x = 100 * sin_propuestas/candidaturas, 
      y = reorder(partido_coalicion,sin_propuestas/candidaturas)
    )
  ) +
  geom_col() +
  labs(x = 'Porcentaje de candidaturas sin propuestas') +
  theme(
    axis.title.y = element_blank()
  )

# Tokenización de propuestas
tokens = propuestas_genero %>% 
  filter(!is.na(propuesta_genero)) %>% 
  unnest_tokens(
    input = propuesta_genero,
    output = "palabra", 
    token = "words"
  ) 

# La ley de Zipf
ley_de_zipf = tokens %>% 
  count(palabra, sort = T) %>% 
  mutate(rango = 1:n()) 

head(ley_de_zipf)

ggplot(ley_de_zipf, aes(x = n)) +
  geom_histogram()

ggplot(ley_de_zipf, aes(x = rango, y = n)) +
  geom_point()

ggplot(ley_de_zipf, aes(x = rango, y = n)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

# La frecuencia es proporcional a cierta potencia del rango
# f = A/r^a
lm(log(n) ~ log(rango), data = ley_de_zipf) 

# Los legonemas
legonemas = ley_de_zipf %>% 
  count(n) %>% 
  rename(
    legonema = n,
    n = nn
  )

# La relación entre legonemas y frecuencias
legonemas %>% 
  ggplot(aes(x = legonema, y = n)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

# n = B/f^b
lm(log(n) ~ log(legonema), data = legonemas)


# Nubes de palabras -------------------------------------------------------
# Importancia para la visualización
# Install - load wordcloud
if(require(wordcloud) == FALSE){                                                
  install.packages('wordcloud')                                                 
  library(wordcloud)                                                            
}else{                                                                          
  library(wordcloud)                                                            
}  

# Cuántas veces aparece cada palabra?
totales = tokens %>% 
  count(palabra, sort = TRUE) 

head(totales)

# Ajustando los márgenes
par(mar = c(0,0,0,0))

# Nuestra primer nube de palabras
wordcloud(
  words = pull(totales, palabra),
  freq = pull(totales, n),
  max.words = 200
)

# Modificando los elementos de la nube de palabras
wordcloud(
  words = pull(totales, palabra),
  freq = log(pull(totales, n)),
  max.words = 150,
  scale=c(3,0.5),
)


# Palabras de paro --------------------------------------------------------
# Install - load stopwords
if(require(stopwords) == FALSE){                                                
  install.packages('stopwords')                                                 
  library(stopwords)                                                            
}else{                                                                          
  library(stopwords)                                                            
}  

# Extraer las palabras de paro
palabras_paro = stopwords('es')

print(palabras_paro)

# Eliminando palabras de paro
totales = tokens %>% 
  count(palabra, sort = TRUE) %>% 
  filter(!palabra %in% palabras_paro) 

# Así luce la nube de palabras
wordcloud(
  words = pull(totales, palabra),
  freq = pull(totales, n),
  max.words = 150,
  scale=c(3,1),
)

# Jugando con la estética
wordcloud(
  words = pull(totales, palabra),
  freq = pull(totales, n),
  max.words = 150,
  scale=c(3,1),
  rot.per = 0,
  random.order = FALSE,
  colors = c('pink','pink3','red','darkred'),
)

# Eliminar NAs
totales = tokens %>% 
  group_by(genero) %>% 
  count(palabra, sort = TRUE) %>% 
  filter(
    !palabra %in% palabras_paro,
    !is.na(palabra)
  ) 


# Comparación entre grupos ------------------------------------------------
# Existe una diferencia entre las propuestas de hombres y mujeres?

# Tokens Mujeres
mujeres = totales %>% 
  filter(genero == 'MUJER') %>%
  top_n(n = 200)

wordcloud(
  words = pull(mujeres, palabra),
  freq = sqrt(pull(mujeres, n)),
  scale=c(3,0.25),
  rot.per = 0,
  random.order = FALSE,
  colors = c('pink','pink3','red','darkred'),
)

# Tokens Hombres
hombres = totales %>% 
  filter(genero == 'HOMBRE') %>% 
  top_n(n = 200)

wordcloud(
  words = pull(hombres, palabra),
  freq = sqrt(pull(hombres, n)),
  scale=c(3,0.25),
  rot.per = 0,
  random.order = FALSE,
  colors = c('lightblue','lightblue4','blue','darkblue'),
)


# Comparar conceptos
comparacion = totales %>% 
  spread(genero,n) %>% 
  mutate_if(is.numeric, replace_na, 0) %>% 
  filter(HOMBRE + MUJER > 10) %>% 
  column_to_rownames('palabra') 


# Nubes de comparación ----------------------------------------------------
comparison.cloud(
  term.matrix = comparacion, 
  random.order = FALSE, 
  colors = c("blue","red"),
  rot.per = 0, 
  scale = c(3, .5),
  title.size = 1.7, 
  max.words = 400
)

comparison.cloud(
  term.matrix = sqrt(comparacion), 
  random.order = FALSE, 
  colors = c("blue","red"),
  rot.per = 0, 
  title.size = 1.7, 
  scale = c(3, .5),
  max.words = 400
)

comparison.cloud(
  term.matrix = mutate_all(comparacion + 1, log), 
  random.order = FALSE, 
  colors = c("blue","red"),
  rot.per = 0, 
  title.size = 1.7, 
  scale=c(3,0.25),
  max.words = 400
)


commonality.cloud(
  term.matrix = comparacion, 
  random.order = FALSE, 
  colors = c("purple","purple2","purple4"),
  rot.per = 0, 
  scale = c(3, .5),
  title.size=1.7, 
  max.words = 400
)


# Similitud de textos -----------------------------------------------------
# Existe alguna similitud entre las propuestas de las distintas candidaturas?

# El caso de AGS
aguascalientes =  tokens %>% 
  filter(entidad == "AGUASCALIENTES") %>% 
  # Conteo de palabras por coalición y candidatura
  count(partido_coalicion, nombre_candidato, palabra) 

# Creamos una matriz con los tokens por candidato
matriz_tokens = aguascalientes %>% 
  select(-partido_coalicion) %>% 
  spread(nombre_candidato, n) %>% 
  mutate_if(is.numeric, replace_na, 0) %>% 
  select(-palabra) 

# La correlación de términos como similitud de discursos
matriz_tokens %>% 
  # Calcular la correlación entre discursos
  cor() %>% 
  # Transformar a tibble
  as_tibble(rownames = 'from') %>% 
  gather('to', 'correlation', - from) %>% 
  ggplot(
    aes(
      x = reorder(from, correlation), 
      y = reorder(to, correlation), 
      fill = correlation
    )
  ) +
  geom_tile(show.legend = F) +
  geom_text(aes(label = round(correlation, 1)), col ='white') +
  scale_fill_gradient2(low = 'white', mid = 'lightblue',high = 'darkblue') +
  # Modificar el tema
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 90)
  )

# La distancia jaccard de los discursos
matriz_tokens = matriz_tokens %>%   
  as.matrix() %>% 
  t()

matriz_tokens[1:8,1:8]

# Caluclar la distancia jaccard  entre discuros
distancias = dist(matriz_tokens, method = "binary")
distancias[1:8]

# Crear un cluster jerárquico
cluster_jerarquico = hclust(distancias)

# Configurar los márgenes
par(mar = c(0,2,2,0))

# Dendograma 
plot(x = cluster_jerarquico,
     # Título
     main = "Disimilitud de Propuestas",
     # Configurar ejes y texto
     hang = -1, cex = 0.6,xlab = '', y = ''
)

# Clusters 
rect.hclust(cluster_jerarquico, k = 3, border = 3:5)
rect.hclust(cluster_jerarquico, k = 30, border = 6:10)


# Qusemos solo 3 clusters
# Dendograma 
plot(x = cluster_jerarquico,
     # Título
     main = "Disimilitud de Propuestas",
     # Configurar ejes y texto
     hang = -1, cex = 0.6,xlab = '', y = ''
)
# Clusters 
rect.hclust(cluster_jerarquico, k = 3, border = 3:8)

# Concatenar los clusters
num_cluster = cutree(cluster_jerarquico, k = 3) %>% 
  enframe(name = 'nombre_candidato', value = 'cluster')

propuestas_genero_ags = propuestas_genero %>% 
  inner_join(num_cluster)

# Tokenizamos las propuestas
tokens_genero_ags =  propuestas_genero_ags %>% 
  unnest_tokens(
    input = propuesta_genero,
    output = "palabra", 
    token = "words",
  ) %>% 
  count(cluster, palabra, sort = TRUE) %>% 
  filter(!palabra %in% palabras_paro) %>% 
  mutate_if(is.numeric, replace_na, 0)


# Hagamos una comparación
comparacion = tokens_genero_ags %>% 
  spread(cluster,n) %>% 
  mutate_if(is.numeric, replace_na, 0) %>% 
  column_to_rownames('palabra') 

comparison.cloud(
  term.matrix = log(comparacion+1), 
  random.order = FALSE, 
  colors = c("blue","red",'orange'),
  rot.per = 0,
  scale = c(2, .5),
  title.size=1.7, 
  max.words = 400
)

# Una visualización más concreta
tokens_genero_ags %>% 
  group_by(cluster) %>% 
  top_n(wt = n, n = 5) %>% 
  ggplot(aes(x = n, y = reorder_within(palabra,n, cluster))) +
  geom_col() +
  facet_wrap(~cluster, scales = 'free_y')


# Jugando con la estetica
tokens_genero_ags %>% 
  group_by(cluster) %>% 
  top_n(wt = n, n = 5) %>% 
  ggplot(aes(x = n, y = reorder_within(palabra,n, cluster))) +
  geom_point(size = 7) +
  geom_text(aes(label = palabra), hjust = -0.35) +
  geom_text(aes(label = n), col = 'white') +
  scale_x_continuous(expand = c(0,0), limits = c(0,50)) +
  facet_wrap(~cluster, scales = 'free_y') +
  labs(title = 'Palabras más frecuentes por cluster') +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank()
  )


