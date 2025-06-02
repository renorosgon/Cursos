# Esto solo se corre la primera vez que estoy instalando la libreria
# install.packages('duckdb')

# Cargar la paqueteria
library(duckdb)

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

# Enlista tablas
dbListTables(conn = conexion)

# Crear una tabla
query = "
CREATE TABLE estudiantes(
    id VARCHAR,
    nombre VARCHAR,
    programa VARCHAR,
    edad DOUBLE,
    semestre DOUBLE
  );
"
# Ejecutar un query
dbExecute(conn = conexion, statement = query)
# Revisar la lista de tablas
dbListTables(conn = conexion)

# Hacer una consulta
query = 'SELECT * FROM estudiantes;'
dbGetQuery(conn = conexion, statement = query)

# Poblar una tabla
query = "
INSERT INTO estudiantes VALUES
  ('A01747933', 'FRIDA', 'LEC', 21, 7),
  ('A01771301', 'JATZIRI', 'LEC', 21, 7),
  ('A01368578', 'ANDRES', 'LEC', 21, 7),
  ('A01029985', 'DANIEL', 'LEC', 23, 7),
  ('A01783366', 'DAVID', 'LEC', 22, 7),
  ('A01654207', 'ALEXA', 'LEC-LRI', 22, 9);
"
dbExecute(conn = conexion, statement = query)

# Eliminar una tabla (PROHIBIDÍSIMO! [Co_oC])
query = "
DROP TABLE estudiantes;
"
# Ejecutar un query
dbExecute(conn = conexion, statement = query)
# Revisar la lista de tablas
dbListTables(conn = conexion)

# Hacer una consulta
query = 'SELECT * FROM estudiantes;'
estudiante = dbGetQuery(conn = conexion, statement = query)

dbDisconnect(conn = conexion)



