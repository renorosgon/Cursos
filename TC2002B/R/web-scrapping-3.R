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


# Queries -----------------------------------------------------------------
# Fijar usuario agente
user_agent = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99.0.4844.84 Safari/537.36'

# El GET request con un query
siem = GET(
  # Página principal
  url = "https://siem.economia.gob.mx",
  # Dirección de la consulta
  path  = 'establecimientos-publicos-x-criterios',
  # Encabezado uusario-agente
  user_agent(user_agent),
  # Consulta
  query = list(id="",
               catEntidadFederativaFk = 0, 
               catActividad = 0,
               catCamaraFk = "",
               nombreComercial = "",
               importa = 2,
               exporta = 2,
               publico = 2,
               catEdoEstablecimientoFk = 0,
               pageNum = 1,
               orderBy = "",
               desc = 0)
) %>% 
  # Leer el html
  read_html() %>% 
  # Extraer el texto
  html_text() %>% 
  # Estructurar el json
  rjson::fromJSON()

# Obtener páginas
pages = pluck(siem, 'pages')

# Crear un tibble
siem_tibble = siem %>% 
  # Extraer el diccionario de lista
  pluck('list') %>% 
  # Concatenar por filas
  bind_rows() %>% 
  # Limpiar nombres
  janitor::clean_names() 

glimpse(siem_tibble)


# Web scrapping ------------------------------------------------------------
# Crear una barra de progreso
bar = progress_bar$new(total = pages)

# Iterar por página
for (page in 2:pages) {
  # Actualziar la barra
  bar$tick()
  # Make a GET request
  siem_tibble = GET(
    # Página principal
    url = "https://siem.economia.gob.mx",
    # Dirección de la consulta
    path  = 'establecimientos-publicos-x-criterios',
    # Encabezado uusario-agente
    user_agent(user_agent),
    # Consulta
    query = list(id="",
                 catEntidadFederativaFk = 0, 
                 catActividad = 0,
                 catCamaraFk = "",
                 nombreComercial = "",
                 importa = 2,
                 exporta = 2,
                 publico = 2,
                 catEdoEstablecimientoFk = 0,
                 # Aquí está nuestro iterador
                 pageNum = page, 
                 orderBy = "",
                 desc = 0)
  ) %>% 
    # Leer el html
    read_html() %>% 
    # Extraer el texto
    html_text() %>% 
    # Estructurar el json
    rjson::fromJSON() %>% 
    # Extraer el diccionario de lista
    pluck('list') %>% 
    # Concatenar por filas
    bind_rows() %>% 
    # Limpiar nombres
    janitor::clean_names() %>% 
    # Concatenar por filas
    bind_rows(siem_tibble)
}

# Por ahora seguimos guardando nuestros datos así
write_excel_csv(siem_tibble,'directorio_siem.csv')



# Web crawling ------------------------------------------------------------
# Crear listas vacias
company_details = c()
company_location = c()
company_profile = c()
company_products = c()
company_complements = c()
company_countries = c()

# Extraer ids
lista_ids = pull(siem_tibble, id)

# Crear una barra de progreso
bar = progress_bar$new(total = length(lista_ids))

# Iterar para cada id en la lista
for (id in lista_ids) {
  bar$tick()
  # Intentar
  try(
    {
      company_details = GET(
        # Set main url
        url = "https://siem.economia.gob.mx",
        # Set search path 
        path  = 'detalle-establecimiento',
        # Set query for the iterator
        query = list(id = id),
        # Add user_agent header
        user_agent(user_agent)
      ) %>% 
        # Read html document
        read_html() %>% 
        # Get text
        html_text() %>% 
        # Structure the json
        rjson::fromJSON() %>% 
        # Bind by rows into a tibble
        bind_rows() %>% 
        # Clean names
        janitor::clean_names() %>% 
        # Bind rows into the main tibble
        bind_rows(company_details)
    },
    silent = TRUE
  )
  
  
  try(
    {
      # Make a GET request
      company_location = GET(
        # Set main url
        url = "https://siem.economia.gob.mx",
        # Set search path 
        path  = 'establecimiento-ubicacion.json',
        # Set query for the iterator
        query = list(id = id),
        # Add user_agent header
        user_agent(user_agent)
      ) %>% 
        # Read html document
        read_html() %>% 
        # Get text
        html_text() %>% 
        # Structure the json
        rjson::fromJSON() %>% 
        # Bind by rows into a tibble
        bind_rows() %>% 
        # Clean names
        janitor::clean_names() %>% 
        # Bind rows into the main tibble
        bind_rows(company_location)
    }, 
    silent = T
  )
  
  try(
    {
      # Make a GET request
      company_profile = GET(
        # Set main url
        url = "https://siem.economia.gob.mx",
        # Set search path 
        path  = 'detalle-establecimiento-perfil',
        # Set query for the iterator
        query = list(id = id),
        # Add user_agent header
        user_agent(user_agent)
      ) %>% 
        # Read html document
        read_html() %>% 
        # Get text
        html_text() %>% 
        # Structure the json
        rjson::fromJSON() %>% 
        # Bind by rows into a tibble
        bind_rows() %>% 
        # Clean names
        janitor::clean_names() %>% 
        # Bind rows into the main tibble
        bind_rows(company_profile)
    },
    silent = TRUE
  )
  
  try(
    {
      # Make a GET request
      company_products = GET(
        # Set main url
        url = "https://siem.economia.gob.mx",
        # Set search path 
        path  = 'establecimientoproducto-por-establecimiento',
        # Set query for the iterator
        query = list(idEstablecimiento = id),
        # Add user_agent header
        user_agent(user_agent)
      ) %>% 
        # Read html document
        read_html() %>% 
        # Get text
        html_text() %>% 
        # Structure the json
        rjson::fromJSON() %>% 
        # Bind by rows into a tibble
        bind_rows() %>% 
        # Clean names
        janitor::clean_names() %>% 
        # Bind rows into the main tibble
        bind_rows(company_products)
    },
    silent = TRUE
  )
  
  try(
    {
      # Make a GET request
      company_complements = GET(
        # Set main url
        url = "https://siem.economia.gob.mx",
        # Set search path 
        path  = 'establecimiento-complemento',
        # Set query for the iterator
        query = list(id = id),
        # Add user_agent header
        user_agent(user_agent)
      ) %>% 
        # Read html document
        read_html() %>% 
        # Get text
        html_text() %>% 
        # Structure the json
        rjson::fromJSON() %>% 
        # Bind by rows into a tibble
        bind_rows() %>% 
        # Clean names
        janitor::clean_names() %>% 
        # Bind rows into the main tibble
        bind_rows(company_complements)
    },
    silent = TRUE
  )
  
  try(
    {
      # Make a GET request
      company_countries = GET(
        # Set main url
        url = "https://siem.economia.gob.mx",
        # Set search path 
        path  = 'establecimiento-pais-por-establecimiento',
        # Set query for the iterator
        query = list(id = id),
        # Add user_agent header
        user_agent(user_agent)
      ) %>% 
        # Read html document
        read_html() %>% 
        # Get text
        html_text() %>% 
        # Structure the json
        rjson::fromJSON() %>% 
        # Bind by rows into a tibble
        bind_rows() %>% 
        # Clean names
        janitor::clean_names() %>% 
        # Bind rows into the main tibble
        bind_rows(company_countries)
    },
    silent = TRUE
  )
  Sys.sleep(runif(1, min = 0, max = 0.0001))
}

# Guardar los datos
write_excel_csv(company_complements,'complements.csv')
write_excel_csv(company_countries,'countries.csv')
write_excel_csv(company_details,'details.csv')
write_excel_csv(company_location,'location.csv')
write_excel_csv(company_products,'products.csv')
write_excel_csv(company_profile,'profile.csv')









