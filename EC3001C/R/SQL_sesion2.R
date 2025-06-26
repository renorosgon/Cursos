# Fijar directorio de trabajo
setwd("~/Desktop/ITESM/Cursos/EC3001C/data/envi_2020_base_de_datos_csv/Bases de datos")

# Cargar librearias
library(tidyverse)
library(duckdb)

# Establecer conexión
conexion = dbConnect(
  # Motor de base de datos
  drv = duckdb::duckdb(),
  # Usuario
  # user = rstudioapi::askForPassword(),
  # Password
  # password = rstudioapi::askForPassword(),
  # Puerto de conexión
  # port = '1234',
  # Ubicación de la base de datos en un servidor
  # host = 'https//aws....'
  # VAMOS A TRABAJAR EN LOCAL
  dbdir = ':memory:'
)

# Sentencia para crear una tabla a partir de un csv
query = "
  CREATE TABLE vivienda AS
  -- Esto es un comentario en SQL
    SELECT * FROM read_csv_auto('TVIVIENDA.csv');
"
# Executamos nuesta sentencia
tabla_vivienda = dbExecute(
  conn = conexion,
  statement = query
  )

# Lista de tablas
dbListTables(conn = conexion)

# Conocer el esquema de mi base de datos
query = "PRAGMA table_info('vivienda');"
info = dbGetQuery(conn = conexion, query)
print(info)

# Cuántas viviendas hay en México?
query = "
  SELECT 
    SUM(FACTOR) AS total_viviendas
  FROM 
    vivienda;
"
# Traer la consulta a memoria
total_viviendas = dbGetQuery(
  conn = conexion, 
  statement = query
  )

# Equivalente en tidyverse
#read_csv('TVIVIENDA.csv') %>% 
  #summarise(total_viviendas = sum(FACTOR))

# Version tidyverse de query
dplyr_query = tbl(src = conexion, 'vivienda') %>% 
  summarise(total_viviendas = sum(FACTOR))

show_query(dplyr_query)


# Cuántas viviendas hay en México por entidad federativa?
query = "
  SELECT 
    ENT AS clave_entidad, SUM(FACTOR) AS total_viviendas
  FROM 
    vivienda
  GROUP BY 
    ENT
  ORDER BY 
    total_viviendas DESC;
"
viviendas_ent = dbGetQuery(conn = conexion, statement = query) 

# Cuántas viviendas hay en México por entidad federativa en localidades 
# de más de 100 mil habitantes?
query = "
  SELECT
    ENT AS clave_entidad, SUM(FACTOR) AS total_viviendas
  FROM 
    vivienda
  WHERE 
    TLOC = 1
  GROUP BY 
    ENT
  ORDER BY 
    total_viviendas DESC;
"
localidades_grandes = dbGetQuery(
  conn = conexion, 
  statement = query
  ) 

# Version tidyverse
dplyr_query = tbl(src = conexion, 'vivienda') %>% 
  filter(TLOC == 1) %>% 
  group_by(ENT) %>% 
  summarise(total_viviendas = sum(FACTOR)) %>% 
  arrange(total_viviendas) 

# Traducir a SQL
show_query(dplyr_query)

# Taer los datos a memoria.
localidades_grandes = collect(dplyr_query)

# Hay que tener modales
dbDisconnect(conn = conexion)






