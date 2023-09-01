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

# Sesiones ----------------------------------------------------------------
#Definir la url base
url = 'https://www.milenio.com'

# Un GET normal
response = GET(url = url)

# Estado de la respuesta
response

# Cómo se hizo la petición 
pluck(response, 'request')

#Definir usuario-agente
user_agent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/105.0.0.0 Safari/537.36"

# Identificarte como una computadora (usuario-agente)
response = GET(url = url, user_agent(user_agent))

# Estado de la respuesta
response

# Cómo se hizo la petición 
pluck(response, 'request')

# Identificarte como una computadora (usuario-agente)
response = GET(
  url = url, 
  user_agent(user_agent),
  query = list(text = 'libros de texto')
)

titulos = response %>%
  read_html() %>% 
  html_nodes(xpath = '//h2/a/text()') %>%
  html_text() 

print(titulos)

# Iniciar una sesion
mi_session = session(url = url, user_agent(user_agent))

# Usa la sesión 
busqueda = mi_session %>% 
  # Ejecuta la búsqueda
  session_jump_to('buscador?text=libros+de+texto') %>% 
  # Lee el documento html
  read_html() 

titulos = busqueda %>%
  html_nodes(xpath = '//h2[@class="lr-list-row-row-news__title"]/a') %>%
  html_text() %>% 
  str_squish()

print(titulos)


# Usa la sesión 
busqueda = mi_session %>% 
  # Ejecuta la búsqueda
  session_jump_to('buscador?page=2&text=libros+de+texto') %>% 
  # Lee el documento html
  read_html() 

titulos = buscador %>%
  html_nodes(xpath = '//h2[@class="lr-list-row-row-news__title"]/a') %>%
  html_text() %>% 
  str_squish()

print(titulos)

# Extrae el número de páginas
articulos_totales = busqueda %>% 
  # Fija la ruta al texto de interés
  html_nodes(xpath = '//div[@class="search-controls__results"]//text()') %>% 
  # Extrae el texto
  html_text() %>% 
  # Concatena el texto
  str_c(collapse = '') %>% 
  # Extrae los dígitos
  str_extract('\\d+') %>% 
  # Transforma a numéricos 
  as.numeric()

articulos_por_pagina = busqueda %>%
  html_nodes(xpath = '//h2[@class="lr-list-row-row-news__title"]/a') %>%
  html_text() %>% 
  length()

# Define el numero de paginas
paginas = articulos_totales / articulos_por_pagina

# Iniciar la barra de progreo
progreso = progress_bar$new(total = paginas)

# Creamos un tibble vacio
articulos_tb = c()

# Iniciar una sesion
mi_session = session(url = url, user_agent(user_agent))

# Iteramos por pagina
for (pagina in 1:paginas){
  # Actualiza el progreso
  progreso$tick()
  
  # Usa la sesion
  buscador = mi_session %>%
    # Salta a la página del buscador
    session_jump_to(url = paste0('buscador?page=', pagina, '&text=libros+de+texto'),
                    # Agrega los encabezados
                    add_headers('referer'='https://www.milenio.com/buscador?text=libros+de+texto')
    )
  
  # Lee el documento html
  buscador_html = read_html(buscador)
  
  # Extrae las urls de los articulos
  articulos = buscador_html %>%
    html_nodes(xpath = '//h2[@class="lr-list-row-row-news__title"]/a/@href') %>%
    html_text()
  
  #Itera sobre las urls
  for(articulo in articulos){
    # Generar la url del articulo
    articulo_url = paste0 (url, articulo)
    # Hacer el request
    articulo_response = GET(articulo_url,  user_agent(user_agent))
    
    # Revisamos el estado de la respuesta
    if(articulo_response$status_code < 400){
      # Leemos nuestra estructura html
      articulo_html = read_html(articulo_response)
      
      # Extraer el titulo
      titulo = articulo_html %>%
        html_nodes(xpath = '//h1[@class="nd-title-headline-title-headline-base__title"]') %>%
        html_text()
      
      # Extraer la autoria
      autoria = articulo_html %>%
        html_nodes(xpath = '//span[@class="author"]') %>%
        html_text()
      
      # Extraer el texto
      texto = articulo_html %>%
        html_nodes(xpath = '//div[@class="media-container news"]/p') %>%
        html_text() %>%
        paste(collapse = " ")
      
      # Extraer la fecha
      fecha = articulo_html %>%
        html_nodes(xpath ='//time[@itemprop="datePublished"]') %>%
        html_text()
      
      # Creamos una fila 
      fila = tibble(
        fuente = articulo_url,
        fecha = fecha,
        autoria = autoria,
        titulo = titulo,
        texto = texto
      ) 
      # Concatenamos el artículo
      articulos_tb = bind_rows(articulos_tb, fila)
      
      # Dar un respiro
      Sys.sleep(runif(max = 0.05))
    }
  }
}

#Este es el resultado
glimpse(articulos_tb)



