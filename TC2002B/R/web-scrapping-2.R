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

# Definimos la url que queremos raspar
path = 'https://www.jornada.com.mx/2023/08/14/'

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

### Obtener los links de las secciones
# Usando XPATH
xpath_jornada = jornada_html %>%
  # Obtenemos los nodos de la tabla que contiene hipervinculos
  html_nodes(xpath = '//tr/td/a/@href') %>%
  # Obtenemos el texto
  html_text() %>% 
  # Remover la fecha 
  str_remove_all('[/\\d+]+')

# Usando XPATH
css_jornada = jornada_html %>%
  # Obtenemos los nodos de la tabla que contiene hipervinculos
  html_nodes(css = 'tr td a') %>%
  # Obtener el atributo que queremos
  html_attr('href') %>% 
  # Remover la fecha 
  str_remove_all('[/\\d+]+')

# Vamos a trabajar las secciones
secciones = jornada_html %>%
  # Obtenemos los nodos de la tabla que contiene hipervinculos
  html_nodes(css = 'tr td a') %>%
  # Obtener el atributo que queremos
  html_attr('href') %>% 
  # Remover la fecha 
  str_remove_all('[/\\d+]+')

secciones = secciones[!str_equal(secciones, '')]
secciones = secciones[!str_detect(secciones, 'https')]

# Luce así
print(secciones)

# Seleccionemos la primera
seccion = secciones[1]

# Scrapper para páginas Editorial
# Definimos la url que queremos raspar
path = 'https://www.jornada.com.mx/2023/08/14/'

# Construimos nuestra url
seccion_url = paste0(path,seccion)

# Hacemos nuestro request
seccion_response = GET(seccion_url)

# Verificamos el status
pluck(seccion_response, 'status_code')

# Leemos el html
seccion_html = read_html(seccion_response)

# Obtenemos el título
# Xpath
titulo = seccion_html %>%
  # Obtenemos los nodos de la tabla que contiene hipervinculos
  html_nodes(xpath = '//div[@class="cabeza"]/em') %>%
  # Obtenemos el texto
  html_text()

# CSS
titulo = seccion_html %>%
  html_nodes(css = 'div.cabeza em') %>%
  html_text() 


# Obtenemos el texto
# Xpath
texto = seccion_html %>%
  html_nodes(xpath = '//div[@class="text"]') %>%
  html_text()

# CSS
texto = seccion_html %>%
  html_nodes(css = 'div.text') %>%
  html_text() 

# Construimos un tibble
fila =  tibble(
  fecha = '2023/08/14',
  seccion = seccion,
  autor = 'Editorial',
  titulo = titulo,
  texto_raw = texto
)

### Hagamoslo generablizable
# Fijamos una fecha de inicio
fecha_inicio = rstudioapi::askForSecret("input", message = "Inroduce la fecha en formato yyyy/mm/dd")

# Creamos nuestra secuencia de fechas
fechas = str_replace_all(
  # Genera una secuencia
  seq(
    as.Date(fecha_inicio), # Desde la Fecha de Inicio
    Sys.Date(), # Hasta Fecha del sistema
    by = "day" # Sequencia por día
  ), 
  # Remplaza - por /
  '-', '/'
)

# Definimos nuestraa url base
jornada_url = 'https://www.jornada.com.mx'

# Creamos tibbles vacíos
textos_df = c()
errores = c()

# Inicializamos nuestra barra de progreso
bar = progress_bar$new(total = length(fechas))

# Por cada fecha en fechas
for(fecha in fechas){
  # Marca el progreso
  bar$tick()
  # Definimos la url que queremos raspar
  path = paste(jornada_url, fecha, 'edito', sep = '/')
  
  # Realizamos nuestra solicitud
  response = GET(path)
  
  # Si la respuesta existe
  if(response$status_code != 404){
    
    # Leemos nuestra estructura html
    jornada_html = read_html(response)
    
    # Obtenemos la sección
    seccion = jornada_html %>%
      html_nodes(xpath = '//img[@class="title"]/@alt') %>%
      html_text()
    
    # Obtenemos el título
    titulo = jornada_html %>%
      html_nodes(xpath = '//div[@id="article-cont"]/div[@class="cabeza"]') %>%
      html_text()
    
    # Obtenemos el texto
    texto = jornada_html %>%
      html_nodes(xpath = '//div[@class="text"]') %>%
      html_text()
    
    # Construimos una fila para el tibble
    fila =  tibble(
      url = path,
      fecha = fecha,
      seccion = seccion,
      autor = 'Editorial',
      titulo = titulo,
      texto_raw = texto
    )
    
    # La concatenamos
    textos_df = bind_rows(textos_df,fila)
  }else{
    # Imprime el error
    print(paste0("Error ",response$status_code,' en ', path))
    # Concatenalo a errores
    errores = c(errores,fecha) 
  }
}