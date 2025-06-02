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

# Instalar - Cargar DBI                                                       
if(require(DBI) == FALSE){       
  install.packages('dbplyr')                                          
  install.packages('DBI')                                                 
  library(DBI)                                                            
}else{                                                                          
  library(DBI)                                                            
}         

# Instalar - Cargar RPostgres                                                  
if(require(RPostgres) == FALSE){                                                
  install.packages('RPostgres')                                                 
  library(RPostgres)                                                            
}else{                                                                          
  library(RPostgres)                                                            
}   

# Instalar - Cargar rstudioapi                                                       
if(require(rstudioapi) == FALSE){                                                
  install.packages('rstudioapi')                                                 
  library(rstudioapi)                                                            
}else{                                                                          
  library(rstudioapi)                                                            
}   


# Conexión ----------------------------------------------------------------
conexion = dbConnect(
  # Driver
  drv = RPostgres::Postgres(),
  # Base de datos
  dbname = 'postgres',
  # Host
  host = 'localhost',
  # Puerto
  port = 5432,
  # Usuario
  user = 'postgres',
  # Constraseña
  password = rstudioapi::askForPassword()
  )


# Operaciones con DBI -----------------------------------------------------
# Enlistar tablas
dbListTables(conn = conexion)

# Lista de campos
dbListFields(conn = conexion, name = "casillas")

# Leer una tabla 
dbReadTable(conn = conexion, "casillas")


# Queries con SQL ---------------------------------------------------------
query = "SELECT SUM(opinion_si) AS opinion_si FROM casillas"

# Solicitar una respuesta
response = dbSendQuery(conn = conexion, query)
# Recolectar la respuesta
opinion_si = dbFetch(response)
# Limpiar el resultado
dbClearResult(response)


# Queries con tidyverse ---------------------------------------------------
tidy_query = tbl(conexion,'casillas') %>% 
  summarise(opinion_si = sum(opinion_si)) 

# Luce así
print(tidy_query)
  
# Muestra el query
show_query(tidy_query)

# Collecta la consulta
tb = collect(tidy_query)


# Joins -------------------------------------------------------------------
totales = tbl(conexion,'casillas') %>% 
  group_by(id_municipio) %>% 
  summarise(
    opinion_si = sum(opinion_si),
    opinion_no = sum(opinion_no),
    nulos = sum(nulos),
    total_opiniones = sum(total_opiniones),
    lista_nominal = sum(lista_nominal),
    secciones = n(),
    casillas = n()
    ) 

# Leer tablas
municipios = tbl(conexion, "municipios")
estados = tbl(conexion, "estados")

# Join en tidyverse
join = estados %>% 
  left_join(municipios, by = 'id_estado') %>% 
  left_join(totales, by = 'id_municipio')

# Cómo se ve el query
show_query(join)

# Realizar el join
casillas = collect(join)

# Crear una tabla
dbWriteTable(conn = conexion, 'tabla_final', casillas)

# Enlistar tablas
dbListTables(conn = conexion)

# Desconectar
dbDisconnect(conexion)

