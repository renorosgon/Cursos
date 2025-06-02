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
  session_jump_to('buscador?text=jalisco') %>% 
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
  session_jump_to('buscador?page=2&text=jalisco') %>% 
  # Lee el documento html
  read_html() 

titulos = busqueda %>%
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
    session_jump_to(url = paste0('buscador?page=', pagina, '&text=jalisco'),
                    # Agrega los encabezados
                    add_headers('referer'='https://www.milenio.com/buscador?text=jalisco')
    )
  
  # Lee el documento html
  buscador_html = read_html(buscador)
  
  # Extrae las urls de los articulos
  articulos = buscador_html %>%
    html_nodes(xpath = '//h2/a/@href') %>% 
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
        html_nodes(xpath = '//h1[contains(@class,"nd-title-headline-title-headline")]') %>%
        html_text()
      
      # Extraer la autoria
      autoria = articulo_html %>%
        html_nodes(xpath = '//span[@class="author"] | //h2[@data-mrf-recirculation="Nota Autor"]/a/text()') %>%
        html_text(trim = TRUE)
      
      # Imagen
      imagen = articulo_html %>% 
        html_nodes(xpath = '//img[@class="nd-media-detail-base__img"]/@src') %>% 
        html_text() %>% 
        str_c(collapse = '')
      
      # Extraer el texto
      texto = articulo_html %>%
        html_nodes(xpath = '//div[@id="content-body"]/p//text()') %>% 
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
        imagen = imagen,
        titulo = titulo,
        texto = texto
      ) 
      # Concatenamos el artículo
      articulos_tb = bind_rows(articulos_tb, fila)
      
      # Dar un respiro
      Sys.sleep(runif(n = 1, max = 0.05))
    }
  }
}

#Este es el resultado
glimpse(articulos_tb)


final = articulos_tb %>% 
  mutate(  
    medio = 'milenio_jalisco',
    tipo_medio = "portal de noticias digital",
    ubicacion = 'seccion',
    jerarquia = 'cronológica',
    fecha = as.Date(dmy_hms(fecha)),
    autoria = str_trim(autoria, 'both'),
    seccion = str_remove(str_extract(fuente, 'https://www.milenio.com/\\w+'), 'https://www.milenio.com/'),
    genero_periodistico = ifelse(seccion == 'opinion', 'Opinión','Nota Informativva')
  )  %>%
  unique() %>% 
  filter(
    !seccion %in% c('deportes','ciencia','futbol'),
    fecha == today()-2
  ) %>% 
  with_groups(
    .groups = fecha,
    mutate,
    # Crear id por día
    articulo_id = 1:n(),
    articulo_id = case_when(
      nchar(articulo_id) < 2 ~ paste0('00', articulo_id),
      nchar(articulo_id) < 3 ~ paste0('0', articulo_id),
      TRUE ~ as.character(articulo_id)
    ),
    articulo_id = paste(fecha, articulo_id, sep = '-'),
  ) %>% 
  mutate(
    campaña = str_detect(texto,'campañas?'),
    jalisco = str_detect(texto,'de Jalisco|de jalisco'),
    candidatura = str_detect(texto,'[Cc]andidat[aoex]s?|[Cc]andidaturas?'),
    candidata = str_detect(texto,'[Cc]andidatas?'),
    gubernatura = str_detect(texto,'[Gg][ou]bernadora?|[Gg][ou]bernaturas?'),
    gobernador = str_detect(texto,'[Gg]obernador'),
    pablo_lemus_navarro = str_detect(texto,'[Pp]ablo [Ll]emus|[Ll]emus [Nn]avarro|[Ll]emus'),
    claudia_delgadillo_gonzalez = str_detect(texto,'[Cc]laudia [Dd]elgadillo|[Dd]elgadillo [Gg]onzález|[Dd]elgadillo'),
    laura_lorena_haro_ramirez = str_detect(texto,'[Ll]aura [Ll]orena [Hh]aro|[Ll]aura [Hh]aro|[Hh]aro [Rr]amírez|[Hh]aro'),
    pri = str_detect(texto,'PRI'),
    pan = str_detect(texto,'PAN'),
    prd = str_detect(texto,'PRD'),
    mc = str_detect(texto,'[Mm]ovimiento [Cc]iudadano|MC'),
    morena = str_detect(texto,'MORENA|Morena'),
    hagamos = str_detect(texto,'HAGAMOS|Hagamos'),
    futuro = str_detect(texto,'FUTURO|Futuro'),
    pt = str_detect(texto,'PT|Partido del Trabajo'),
    pvem = str_detect(texto,'Partido Verde|PVEM'),
    coalicion =  str_detect(texto,'[cC]oalición'),
    alianza =  str_detect(texto,'[Aa]lianza'),
    propuesta = str_detect(texto,'[pP]ropuestas?|[pP]ropone')
  )  %>%  
  mutate_if(is.logical, as.numeric) %>% 
  filter(
    !is.na(titulo),
    pablo_lemus_navarro + claudia_delgadillo_gonzalez + laura_lorena_haro_ramirez > 0,
  ) %>%
  select(tipo_medio, medio, fecha, ubicacion, seccion, jerarquia, articulo_id, url = fuente, imagen, titulo, autoria, genero_periodistico, texto, campaña:propuesta) 


write_excel_csv(final, 'milenio_marzo_01a09.csv')
