# Fijar directorio de trabajo
setwd("~/Desktop/ITESM/Cursos/EC3001C")

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

# Nos devuelve una base vacía
dbListTables(conn = conexion)

# Me deconecto
dbDisconnect(conn = conexion)

# Crear una base de datos (archivo.db)
conexion = dbConnect(
  # Motor de base de datos
  drv = duckdb::duckdb(),
  # El nombre de mi archivo (base de datos)
  dbdir = 'data/envi_2020_base_de_datos_csv/Bases de datos/ENVI.duckdb'
)

# Cascaron vacío
dbListTables(conn = conexion)

# Lista de archivos
archivos = list.files(pattern = '.csv')

# Iterar para cada archivo en archivos
for(archivo in archivos){
  # Crear el nombre de la tabla
  nombre_tabla = archivo %>% 
    str_remove('.csv') %>% 
    str_remove('T') %>% 
    tolower()
  
  # Crear un query con formato
  # %s siginifica: Literamente por este string
  query = sprintf(
    fmt = "CREATE TABLE %s AS SELECT * FROM read_csv_auto('%s');",
    # Primer %s
    nombre_tabla,
    # Segundo %s
    archivo
  )
  # Luce así
  print(query)
  
  # Executar el query
  dbExecute(conn = conexion, statement = query)
}

# Cuántas viviendas existen en México por entidad federativa?
query = "
  SELECT 
      ENT AS clave_entidad, SUM(FACTOR) AS total_vivienda
  FROM
    vivienda
  GROUP BY
    ENT;
"
viviendas = dbGetQuery(conn = conexion, statement = query)

head(viviendas)

# Cuántas hogares existen en México por entidad federativa?
query = "
  SELECT 
      ENT AS clave_entidad, SUM(FACTOR) AS total_hogares
  FROM
    hogar
  GROUP BY
    ENT;
"
hogares = dbGetQuery(conn = conexion, statement = query)

head(hogares)

# Concatenar tablas en tidyverse
left_join(
  x = viviendas, 
  y = hogares, 
  by = 'clave_entidad'
  ) %>% 
  mutate(
    hogares_por_vivienda = total_hogares/total_vivienda
  )
  
# Concatenar tablas en SQL (ESTO ES LO QUE DEBEMOS HACER SIEMPRE!!)
query = "
  SELECT 
    vivienda.clave_entidad, 
    vivienda.total_vivienda,
    hogar.total_hogares, 
    hogar.total_hogares/vivienda.total_vivienda AS hogares_por_vivienda

  -- Consulta a la tabla de vivienda
  FROM 
    (SELECT 
        ENT AS clave_entidad, SUM(FACTOR) AS total_vivienda
    FROM
      vivienda
    GROUP BY
      ENT) vivienda

  -- Concatenar con mi consulta de hogar
  LEFT JOIN
    (SELECT 
        ENT AS clave_entidad, SUM(FACTOR) AS total_hogares
    FROM
      hogar
    GROUP BY
      ENT) hogar

  -- Llave primaria/foreana
  ON 
    vivienda.clave_entidad = hogar.clave_entidad

  -- Ordenar de forma descendente
  ORDER BY
    hogares_por_vivienda DESC;
"

# El resultado final 
df = dbGetQuery(conn = conexion, statement = query)
glimpse(df)

# Cuántas viviendas tienen condiciones de hacinamiento?
query = "
  SELECT
    entidad, 
    indice_hacinamiento, 
    -- Promedio ponderado
    SUM(hacinamiento * FACTOR)/SUM(FACTOR) AS hacinamiento_promedio,
    SUM(FACTOR) AS total_viviendas
  FROM
    -- Subconsulta de la tabla de vivienda
    (SELECT
        ENT AS entidad, 
        CAST(P1_1 AS NUMERIC) AS personas,
        CAST(P4_10 AS NUMERIC) AS dormitorios, 
        FACTOR, 
        personas/dormitorios AS hacinamiento,
    
        -- Crear una nueva variable a partir de casos
        CASE 
            WHEN hacinamiento < 2.5 THEN 'Sin hacinamiento'
            WHEN hacinamiento BETWEEN 2.5 AND 5 THEN 'Hacinamiento medio'
            ELSE 'Hacinamiento crítico'
        -- Termina de evaluar los casos y crea la variable cómo...
        END AS indice_hacinamiento
      FROM
        vivienda) hacinamiento
    GROUP BY
        entidad, indice_hacinamiento;
"
# Traer la consulta a memoria
hacinamiento = dbGetQuery(conn = conexion, statement = query)

# Un gráfico sencillo
hacinamiento %>% 
  group_by(entidad) %>% 
  mutate(porcentaje = total_viviendas/sum(total_viviendas)) %>% 
  ggplot(
    aes(
      x = porcentaje, 
      y = entidad
      )
    ) +
  geom_col() +
  scale_x_continuous(labels = scales::percent) +
  labs(
    x = 'Viviendas (%)', 
    title = 'Porcentaje de viviendas según condición de hacinamiento'
    ) +
  facet_wrap(~indice_hacinamiento, scales = 'free') +
  theme(axis.title.y = element_blank())

# No hay que olvidar desconectarnos
dbDisconnect(conexion)


































