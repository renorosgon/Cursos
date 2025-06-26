# Fijar directorio de trabajo
setwd("~/Desktop/ITESM/Cursos/EC3001C/")

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

# Instalar - Cargar RSQLite                                                  
if(require(RSQLite) == FALSE){                                                
  install.packages('RSQLite')                                                 
  library(RSQLite)                                                            
}else{                                                                          
  library(RSQLite)                                                            
}   

# Conexión ----------------------------------------------------------------
conexion <- dbConnect(RSQLite::SQLite(), "data/mibase.db")


# Trabajar con bases de datos ---------------------------------------------
# Enlistar tablas
dbListTables(conn = conexion)

# Creación de tablas
# CREATE TABLE [IF NOT EXISTS] [schema_name].table_name (
#     column_1 data_type PRIMARY KEY,
#     column_2 data_type NOT NULL,
#     column_3 data_type DEFAULT 0,
#     table_constraints
#     );

statement = "CREATE TABLE IF NOT EXISTS estudiantes(
          matricula VARCHAR(9) PRIMARY KEY,
          nombre VARCHAR, 
          apellido VARCHAR,
          edad INTEGER,
          sexo VARCHAR(2)
          );"


dbSendQuery(conn = conexion, statement = statement)

# Eliminación de tablas 
# (OJO al equipo de ingeniería no le gusta este comando)
# DROP TABLE [schema_name].table_name ;
statement = 'DROP TABLE estudiantes;'
dbSendQuery(conn = conexion, statement = statement)

# Agregar observaciones
# INSERT INTO table (column1,column2 ,..)
# VALUES( value1,	value2 ,...);

statement = "INSERT INTO estudiantes (matricula, nombre, apellido, edad, sexo)
         VALUES ('A01334554','RENE','ROSADO', 30, 'H');"

dbSendQuery(conn = conexion, statement = statement)

# Consulta de bases de datos (QUERY)
# SELECT [clauses]
# FROM [schema_name].table_name
# Use DISTINCT clause to query unique rows in a table
# Use WHERE clause to filter rows in the result set
# Use ORDER BY clause to sort the result set
# Use LIMIT OFFSET clauses to constrain the number of rows returned
# Use INNER JOIN or LEFT JOIN to query data from multiple tables using join.
# Use GROUP BY to get the group rows into groups and apply aggregate function for each group.
# Use HAVING clause to filter groups

query = "SELECT *
         FROM estudiantes
         WHERE sexo = 'H' AND edad > 25
         ORDER BY matricula
         LIMIT 3;"

# Creamos un objeto de respuesta
response = dbSendQuery(conn = conexion, statement = query)

# Transaccinar
estudiantes = dbFetch(response)

glimpse(estudiantes)


