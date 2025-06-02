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

# Instalar - Cargar progress                                                       
if(require(progress) == FALSE){                                                
  install.packages('progress')                                                 
  library(progress)                                                            
}else{                                                                          
  library(progress)                                                            
}    

# Instalar - Cargar rjson                                                       
if(require(rjson) == FALSE){                                                
  install.packages('rjson')                                                 
}

# Instalar - Cargar httr                                                       
if(require(httr) == FALSE){                                                
  install.packages('httr')                                                 
}

# Funciones auxiliares ----------------------------------------------------
select_columns = function(lista){
  # Convertir lista a data frame
  df = flatten_df(
    # Seleccionar columnas de interes
    lista[c('idPartido','nombrePartido','colorPartido','nombreCandidatoPropietario','total','porcentaje')]
  )
  return(df)
}


# Queries -----------------------------------------------------------------
# Fijar usuario agente
user_agent = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36'

# Inicializar vectores vacios
actas = c()
distritos = c()

# Barra de progreso
bar = progress_bar$new(total = 32)

# Iterar por cada entidad
for(cve_ent in 1:32){
  # GET Request
  ent_metadata = httr::GET(
    # Página principal
    url = "https://computos2024.ine.mx",
    # Dirección de la consulta
    path  = sprintf('assets/presidencia/rutas/%s.json', cve_ent),
    # Encabezado usario-agente
    user_agent(user_agent)
  ) %>% 
    # Leer el html
    read_html() %>% 
    # Extraer el texto
    html_text() %>% 
    # Estructurar el json
    rjson::fromJSON()
  
  # Obtener metadata de distritos
  distritos_ent = pluck(ent_metadata, 'distritos') %>% 
    # Concatenar en data frame
    bind_rows() %>% 
    # Agregar metadata de entidad
    mutate( 
      cve_ent = cve_ent,
      nom_ent = pluck(ent_metadata, 'migas')
    ) 
  
  # Concatenar a tibbld de distritos
  distritos = distritos_ent %>% 
    bind_rows(distritos)
  
  # Iterar por cada id de distrito
  for (id_distrito in  pull(distritos_ent, id)) {
    # GET request
    response = httr::GET(
      # Página principal
      url = "https://computos2024.ine.mx",
      # Dirección de la consulta
      path  = sprintf('assets/JSON/PRESIDENTE/DISTRITAL/Presidente_DISTRITAL_%s_%s.json', cve_ent, id_distrito),
      # Encabezado uusario-agente
      user_agent(user_agent)
    ) %>% 
      # Leer el html
      read_html() %>% 
      # Extraer el texto
      html_text() %>% 
      # Estructurar el json
      rjson::fromJSON() 
    
    # Aplicar a cada elemento de la lista
    actas = map_df(
      # Lista de interéa
      .x = pluck(response, 'votosCandidatoPartidoCoalicion'), 
      # Función
      .f = select_columns
    ) %>% 
      # Agregar metadatos
      mutate(
        cve_ent = cve_ent,
        id_distrito = id_distrito
      ) %>% 
      # Concatenar
      bind_rows(actas)
  }
  bar$tick()
}

write_excel_csv(x = actas, file = 'data/resultados_2024.csv')
