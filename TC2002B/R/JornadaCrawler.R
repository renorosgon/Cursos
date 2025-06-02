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

# Crear la araña ----------------------------------------------------------
# Definimos a nuestra anaña como una funcion que tiene como 
# fecha predeterminada el día de hoy y un grupo de secciones predefinidad
SpiderLaJornada = function(
  # Inputs
  fecha_inicio = Sys.Date(),
  fecha_final = Sys.Date(),
  secciones = c("edito","opinion","politica","economia","mundo","estados",
                "capital","deportes","cultura","espectaculos")
  ){
  
  # Comienza en el url de la jornada
  jornada_url = 'https://www.jornada.com.mx'
  
  # Crea nuestros dataframes vacios
  articulos_df = c()
  errores = c()

  # Crea la secuencia de fechas
  fechas = str_replace_all(
    # Genera una secuencia
    seq(
      as.Date(fecha_inicio), # Desde la Fecha de Inicio
      as.Date(fecha_final), # Hasta Fecha Final
      by = "day" # Sequencia por día
    ), 
    # Remplaza - por /
    '-', '/'
  )
  
  # Iteramos sobre las fechas
  for(fecha in fechas){
    
    # Iteramos sobre las secciones
    for(seccion in secciones){
      
      # Definimos la url que queremos raspar
      path = paste(jornada_url, fecha, seccion, sep = '/')
      
      # Realizamos nuestra solicitud
      response = GET(path)
      
      if(response$status_code < 400){
        
        # Leer estrucutra html
        seccion_html = read_html(response)
        
        # Obtener el nombre de la seccion
        nombre_seccion = seccion_html %>%
          html_nodes(xpath = '//img[@class="title"]/@alt') %>%
          html_text()
        
        if(seccion == 'edito'){

          # Obtenemos el título
          titulo = seccion_html %>%
            html_nodes(xpath = '//div[@id="article-cont"]/div[@class="cabeza"]') %>%
            html_text()
          
          # Obtenemos el texto
          texto = seccion_html %>%
            html_nodes(xpath = '//div[@class="text"]') %>%
            html_text()
          
          # Construimos una fila para el tibble
          fila = tibble(
            fuente = path,
            fecha = fecha,
            seccion = nombre_seccion,
            autoria = 'Editorial',
            titulo = titulo,
            texto = texto
          ) 
          
          # La concatenamos
          articulos_df = bind_rows(articulos_df,fila)
          
        }else{
          
          # Obtenemos los hipervinculos a los articulos
          articulos = seccion_html %>%
            html_nodes(xpath = '//div[@id="section-cont"]/div[contains(@class,"item")]//a/@href') %>%
            html_text() %>%
            unique()
          
          for(articulo in articulos){
            # Generar la url del articulo
            articulo_url = paste(jornada_url,fecha,articulo, sep = '/')
            # Hacer el request
            articulo_response = GET(articulo_url)
            
            # Revisamos el estado de la respuesta
            if(articulo_response$status_code < 400){
              # Leemos nuestra estructura html
              articulo_html = read_html(articulo_response)
              
              # Extraer el titulo
              titulo = articulo_html %>%
                html_nodes(css = 'div.cabeza')%>%
                html_text()
              
              # Extraer la autoria
              autoria = articulo_html %>%
                html_nodes(css = 'span[itemprop="name"]') %>%
                html_text()
              
              # Extraer el texto
              texto = articulo_html %>%
                html_nodes(css = 'div#article-text') %>%
                html_text()
              
              fila = tibble(
                fuente = articulo_url,
                fecha = fecha,
                seccion = nombre_seccion,
                autoria = autoria,
                titulo = titulo,
                texto = texto
              ) 
              articulos_df = bind_rows(articulos_df, fila)            
              }else{
              errores = c(errores,articulo_url)
              }
          }
          }
      }
    }
  }
  return(articulos_df)
}
