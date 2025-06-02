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


# Generadores de texto ----------------------------------------------------

# Esta función la utilizaremos para normalizar nuestros textos
normalizar = function(texto, vocab = NULL){
  # Sustitur los puntos finales por _ss_
  texto = gsub("\\.\\s*$", "  _ss_", texto)
  # Convertir a minusculas
  texto = tolower(texto)
  # Elmiminar espacios duplicados
  texto = gsub("\\s+", " ", texto)
  # Replazar puntos y seguidos por _ss _s_
  texto = gsub("\\.[^0-9]", " _ss_ _s_ ", texto)
  # Eliminar caracteres especiales
  texto = gsub("[«»¡!¿?-]", "", texto) 
  # Remplazar puntuación por explicitos
  texto = gsub(";", " _punto_coma_ ", texto) 
  texto = gsub("\\:", " _dos_puntos_ ", texto) 
  texto = gsub("\\,[^0-9]", " _coma_ ", texto)
  # Elmiminar espacios duplicados
  texto = gsub("\\s+", " ", texto)
  # Regresa el texto
  return(texto)
}

# Un ejemplo pequeño
corpus_mini = c(
  "Este es un ejemplo: el perro corre, el gato escapa. Este es un número 3.1416, otro número es 1,23.",
  "Este   es otro ejemplo.  " 
  )

print(corpus_mini)
normalizar(corpus_mini)


ejemplo = tibble(texto = corpus_mini) %>%
  mutate(id = row_number()) %>%
  mutate(txt = normalizar(texto)) 

print(ejemplo)

# Un ejemplo más grande
noticias = read_lines('https://s3.amazonaws.com/es-noticias/Es_Newspapers.txt')

# Revisa el total de noticias
length(noticias)

# Lucen así
head(noticias)

# Tomaremos una muestra
set.seed(123)
indice = sample(x = 1:length(noticias), size = 100000)

# Crear un tibble con la muestra
noticias_df = tibble(texto = noticias) %>%
  filter(row_number() %in% indice) %>% 
  mutate(
    # Agregar id
    id = row_number(),
    # Normalizar el texto
    texto = normalizar(texto = texto)
    ) 

# Creamos una función para generar conteos de ngramas
conteo_ngramas = function(corpus, n = 1){
  # Define el inicio 
  inicio = paste(rep("_s_ ", n - 1), collapse = "")
  # Crea una lista de nombres
  token_nombre = paste('palabra', 1:n, sep = '_')
  # Crea una lista de grupos
  token_grupos = syms(token_nombre[-length(token_nombre)])
  
  # Transformación del corpus
  corpus %>% 
    # Agregar inicio
    mutate(texto = paste0(inicio, texto)) %>% 
    # Tokenizar
    unnest_tokens(
      input = texto,
      output = ngrama,
      token = 'ngrams',
      n = n
    ) %>%
    # Conteo de ngramas
    count(ngrama) %>%   
    # Separar ngramas
    separate(ngrama, paste('palabra', 1:n, sep = '_'), sep = ' ') %>% 
    # Agrupar
    group_by(!!!token_grupos) %>% 
    # Calcular el denominador por grupo
    mutate(denominador = sum(n)) %>%
    # Desagrupar
    ungroup() %>%
    # Calcular la log-probabilidad
    mutate(log_p = log(n) - log(denominador))
}


# Creamos una lista de modelos de ngramas
n_gramas =  list(
  unigramas = conteo_ngramas(noticias_df, n = 1),
  bigramas  = conteo_ngramas(noticias_df, n = 2),
  trigramas = conteo_ngramas(noticias_df, n = 3)
  )

# Luce así
print(n_gramas)

# Crear una función para calcular la probabilidad
log_prob = function(textos, n_gramas, n = 2){
  # Crear un inicio
  inicio = paste(rep("_s_ ", n - 1), collapse = "")    
  # Normalizar el texto en un tible
  df_tokens = tibble(texto = textos) %>%
    mutate(
      id = row_number(),
      texto = normalizar(texto),
      texto = paste(inicio, texto, sep = '')
    ) %>% 
    # Tokenizar por grupo 
    group_by(id) %>%
    unnest_tokens(
      input = texto,
      output = ngrama,
      token = 'ngrams',
      n = n
    ) %>%
    # Separar los ngramas
    separate(ngrama, paste('palabra', 1:n, sep = '_'), " ") %>%
    # Concatenar con el modelo de ngramas correspondiente
    left_join(n_gramas[[n]], by = paste('palabra', 1:n, sep = '_'))
  
  # Calcular la log probabilidad
  log_probs = split(pull(df_tokens, log_p), pull(df_tokens,id))
  # Calcular el promedio para cada texto
  sapply(log_probs, mean)
}

# Un ejemplo
textos = c(
  "Otro día muy soleado.",
  "Es este ejemplo un.",
  "Este es un ejemplo.",
  "Esta frase es xxyz."
         )

# Probando los distintos modelos
log_prob(textos, n_gramas, n = 1)
log_prob(textos, n_gramas, n = 2)
log_prob(textos, n_gramas, n = 3)

# Una explicación simple
n = 2
textos = "Otro día muy soleado."

# Crear el tibble normalizado
df_tokens = tibble(texto = textos)%>% 
  mutate(
    # Agregar id
    id = row_number(),
    # Normalizar el texto
    texto = normalizar(texto = texto)
  ) %>% 
  group_by(id) %>%
  unnest_tokens(
    input = texto,
    output = ngrama,
    token = 'ngrams',
    n = n
  ) %>%
  separate(ngrama, paste('palabra', 1:n, sep = '_'), " ") %>%
  left_join(n_gramas[[n]], by = paste('palabra', 1:n, sep = '_'))

df_tokens


# Palabras Faltantes ------------------------------------------------------
# Extraer el vocabulario existente
vocabulario_txt = n_gramas[[1]] %>% 
  filter(n > 1) %>% 
  pull(palabra_1)

length(vocabulario_txt)

# Crear un nuevo ambiente de vocabulario
vocab_env = new.env()
# Agregamos un símbolo para palabras desconocidas
vocab_env[["_unk_"]] <- 1
# Agregamos cada palabra existente
for(palabra in vocabulario_txt){
  vocab_env[[palabra]] <- 1
}

# El número de ngramas
nrow(n_gramas[[1]])
# Total de palabras
sum(n_gramas[[1]]$n)
# El nuevo vocabulario
length(vocab_env)

# Crear una función para restringir el vocabulario
restringir_vocab = function(texto, vocab_env){
  # Separar en palabras
  texto_v = strsplit(texto, " ")[[1]]
  # Aplicar una función temporal a cada palabra
  texto_v <- lapply(texto_v, function(palabra){
    # Si no está vacio
    if(palabra != ""){
      # Buscar la palabra en el vocabulario
      en_vocab = vocab_env[[palabra]]
      # Si no epalabraiste
      if(is.null(en_vocab)){
        # Remplazar por _unk_
        palabra = "_unk_"
      }
      # Regresar la palabra
      palabra
    }
  })
  # Concatenar el texto
  texto <- paste(texto_v, collapse = " ")
  # Regresar el texto
  texto
}

# Así se ve nuestro ejemplo
restringir_vocab(textos, vocab_env)

# Restrigimos el vocabulario de nuestras noticias
noticias_df_unk = noticias_df %>% 
  # Agregamos los desconocidos aplicando nuestra función a cada noticia
  mutate(texto = map_chr(texto, ~restringir_vocab(.x, vocab_env = vocab_env))) %>% 
  select(id, texto) 

# Actualizamos nuestra función de log_probabilidad
log_prob = function(textos, n_gramas, n = 2, 
                    laplace = FALSE, delta = 0.001, 
                    vocab_env = NULL){
  # Crear un inicio
  inicio = paste(rep("_s_ ", n - 1), collapse = "")    
  # Normalizar el texto en un tible
  df = tibble(texto = textos) %>%
    mutate(
      id = row_number(),
      texto = normalizar(texto),
      texto = paste(inicio, texto, sep = '')
    ) 
  
  # Si hay un vocabulario
  if(!is.null(vocab_env)){
    df = df  %>% 
      # Agregamos los desconocidos aplicando nuestra función a cada noticia
      mutate(texto = map_chr(texto, ~restringir_vocab(.x, vocab_env = vocab_env))) %>% 
      select(id, texto) 
  }
  
  df_tokens = df %>% 
    # Tokenizar por grupo 
    group_by(id) %>%
    unnest_tokens(
      input = texto,
      output = ngrama,
      token = 'ngrams',
      n = n
    ) %>%
    # Separar los ngramas
    separate(ngrama, paste('palabra', 1:n, sep = '_'), " ") %>%
    # Concatenar con el modelo de ngramas correspondiente
    left_join(n_gramas[[n]], by = paste('palabra', 1:n, sep = '_'))
  
  # Suavizamiento de laplace
  if(laplace){
    V = nrow(n_gramas[[1]])
    log_probs = log(pull(df_tokens, n) + delta) - log(pull(df_tokens, denominador) + delta * V)
    log_probs[is.na(log_probs)] =  log(1/V)
  } else {
    log_probs = pull(df_tokens, log_p)
  }
  # Calcular la log probabilidad
  log_probs = split(log_probs, pull(df_tokens,id))
  # Calcular el promedio para cada texto
  sapply(log_probs, mean)
}

# Trabajando con palabras desconocidas
n_gramas_u = list(
  unigramas = conteo_ngramas(noticias_df_unk, n = 1), 
  bigramas = conteo_ngramas(noticias_df_unk, n = 2),
  trigramas = conteo_ngramas(noticias_df_unk, n = 3)
)

textos = c(
  "Es un día soleado.",
  "Es este ejemplo un.",
  "Este es un ejemplo.",
  "Esta frase es xxyz."
  )

log_prob(textos, n_gramas_u, n = 1, vocab_env = vocab_env)
log_prob(textos, n_gramas_u, n = 2, vocab_env = vocab_env)
log_prob(textos, n_gramas_u, n = 3, vocab_env = vocab_env)

# Suavizamiento de laplace
log_prob(textos, n_gramas = n_gramas_u, n = 1, laplace = TRUE, delta = 0.01)
log_prob(textos, n_gramas = n_gramas_u, n = 2, laplace = TRUE, delta = 0.01)
log_prob(textos,n_gramas = n_gramas_u, n = 3, laplace = TRUE, delta = 0.01)


# Modelos generativos -----------------------------------------------------
# Unigramas
calc_siguiente_uni = function(texto, n_gramas){
  # Genera un umbral aleatorio
  u = runif(n = 1)
  # Ordena los ngramas en función de la log probabilidad
  unigramas_s = arrange(n_gramas[[1]], log_p)
  # Calcula la probabilidad acumulada
  prob_acum = pull(unigramas_s, log_p) %>% 
    exp() %>% cumsum()
  # Identifica la palabra que cruza el umbral
  palabra_no =  match(TRUE, u < prob_acum)
  # Imprime la palabra
  token = unigramas_s %>% 
    filter(row_number() == palabra_no) %>% 
    pull(palabra_1) 
}
# Comienza un texto
texto = "_s_"
# Condición de paro
fin = FALSE

# Ciclo while
while(!fin){
  # Extraer la siguiente palabra
  siguiente = calc_siguiente_uni(texto, n_gramas_u)
  # Agregar al texto
  texto = c(texto, siguiente)
  # Condición de paro
  if(siguiente == "_ss_"){
    fin <- TRUE
  }
}
paste(texto, collapse = " ")

# Bigramas
calc_siguiente_bi <- function(texto, n_gramas){
  # Genera un umbral aleatorio
  u = runif(n = 1)
  # Define la longitud
  n = length(texto)
  # Mapea la palabra anterior
  anterior = texto[n]
  # Filtra los potenciales candidatos
  siguiente_df = filter(n_gramas[[2]], palabra_1 == anterior) %>% 
    arrange(log_p) 
  # Calcula la probabilidad acumulada
  prob_acum = pull(siguiente_df, log_p) %>% 
    exp() %>% cumsum()
  # Identifica la palabra que cruza el umbral
  palabra_no =  match(TRUE, u < prob_acum)
  # Imprime la palabra
  siguiente_df %>% 
    filter(row_number() == palabra_no) %>% 
    pull(palabra_2) 
}

# Comienza un texto
texto = "_s_"

for(i in 1:40){
  # Extraer la siguiente palabra
  siguiente = calc_siguiente_uni(texto, n_gramas_u)
  # Agregar al texto
  texto = c(texto, siguiente)
  # Condición de paro
  if(siguiente == "_ss_"){
    fin <- TRUE
  }
}

paste(texto, collapse = " ")

# Trigramas
calc_siguiente_tri <- function(texto, n_gramas){
  # Genera un umbral aleatorio
  u = runif(n = 1)
  # Define la longitud
  n = length(texto)
  # Mapea el contexto
  contexto = texto[c(n,n-1)]
  # Filtra los potenciales candidatos
  siguiente_df =  n_gramas[[3]] %>% 
    filter(
      palabra_2 == contexto[1], 
      palabra_1 == contexto[2]
      )%>% 
    arrange(log_p) 
  # Calcula la probabilidad acumulada
  prob_acum = pull(siguiente_df, log_p) %>% 
    exp() %>% cumsum()
  # Identifica la palabra que cruza el umbral
  palabra_no =  match(TRUE, u < prob_acum)
  # Imprime la palabra
  siguiente_df %>% 
    filter(row_number() == palabra_no) %>% 
    pull(palabra_3) 
}

# Inicia el texto
texto = c("_s_", "_s_")

for(i in 1:50){
  # Extraer la siguiente palabra
  siguiente = calc_siguiente_tri(texto, n_gramas_u)
  # Agregar al texto
  texto = c(texto, siguiente)
  # Condición de paro
  if(siguiente == "_ss_"){
    texto = c(texto, "_s_")
  }
}
paste(texto, collapse = " ")


# Evaluación de modelos ---------------------------------------------------
# Tomaremos una muestra
set.seed(123)
indice = sample(x = 1:length(noticias), size = 100000)

set.seed(321)
prueba = sample(x = 1:nrow(noticias_df), size = 1000)

# Conjuntos de entrenamiento y prueba
textos_entrenamiento = noticias_df %>% 
  sample_n(size = 1000) %>% 
  pull(texto) %>% 
  str_c(collapse = ' ')

# Conjuntos de entrenamiento y prueba
textos_prueba = tibble(texto = noticias) %>%
  filter(row_number() %in% prueba) %>% 
  mutate(
    # Agregar id
    id = row_number(),
    # Normalizar el texto
    texto = normalizar(texto = texto)
  ) %>% 
  pull(texto) %>% 
  str_c(collapse = ' ')


# Evaluación del entrenamniento
exp(-log_prob(textos_entrenamiento, n_gramas_u, n = 1, laplace = T))
exp(-log_prob(textos_entrenamiento, n_gramas_u, n = 2, laplace = T))
exp(-log_prob(textos_entrenamiento, n_gramas_u, n = 3, laplace = T))


exp(-log_prob(textos_prueba, n_gramas_u, n = 1, laplace = T))
exp(-log_prob(textos_prueba, n_gramas_u, n = 2, laplace = T))
exp(-log_prob(textos_prueba, n_gramas_u, n = 3, laplace = T))




