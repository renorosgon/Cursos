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

# Carga de conjunto de datos ----------------------------------------------
# Resultados por distrito federal de las elecciones 2024
resultados = read_csv('data/resultados_2024.csv') %>% 
  janitor::clean_names()

# Revisar nuestros datos
glimpse(resultados)

# Tabla con información de entidades, municipios y distritos
distritos_federales = read_delim(
  file = 'https://storage.googleapis.com/mapoteca/catalogos-jun2024/CMDEF/CMDEF.txt',
  delim = '|'
) %>% 
  janitor::clean_names()

# Normalización de bases de datos -----------------------------------------
# Necesitamos
# 1) Tabla de entidades
entidades = distritos_federales %>% 
  select(clave_entidad = entidad, nombre_entidad) %>% 
  unique()

head(entidades)

# 2) Tabla de municipios
municipios = distritos_federales %>% 
  select(clave_entidad = entidad, clave_municipio = municipio, nombre_municipio) %>% 
  unique()

head(municipios)

# 3) Tabla de distritos
distritos = distritos_federales %>% 
  select(clave_entidad = entidad, clave_municipio = municipio, distrito) %>% 
  unique()

head(distritos)

# 4) Tabla de partidos
partidos = resultados %>% 
  select(id_partido, nombre_partido, color_partido, nombre_candidato_propietario) %>% 
  unique()

head(partidos)

# 5) Tabla de partidos
partidos = resultados %>% 
  select(id_partido, nombre_partido, color_partido, nombre_candidato_propietario) %>% 
  unique()

head(partidos)

# 6) Tabla de votos
votos = resultados %>% 
  select(cve_ent, id_distrito, id_partido, total) %>% 
  unique()

head(votos)

# Por qué molestarnos en hacer esto?
object.size(resultados)
# Reduciomos a la mitad el peso de nuestra base de datos
object.size(votos) + object.size(partidos)


# Hasta ahora respondiamos preguntas cargando todos los datos en R
# Cuántos votos tuvo cada candidatura?
votos %>% 
  # Agrupa por id_partido
  group_by(id_partido) %>% 
  # Suma los votos por grupo
  summarise(total = sum(total)) %>% 
  # Concatenar por la derecha
  right_join(
    # Selecciona las columnas de la tabla partidos
    select(partidos, id_partido, nombre_partido),
    # Concatenar en id_partido
    by = join_by(id_partido)
    )

# El problema es que tenemos mucha información en memoria y podríamos usar 
# solo los datos que nos interesan


# Manejo de bases de datos ------------------------------------------------
# Instalar - Cargar duckdb                                                       
if(require(duckdb) == FALSE){                                                
  install.packages('duckdb')                                                 
  library(duckdb)                                                            
}else{                                                                          
  library(duckdb)                                                            
}  


# Crear una base de datos (archivo.db)
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
  # El nombre de mi archivo (base de datos)
  dbdir = 'data/elecciones_2024.duckdb'
)

# Cascaron vacío
dbListTables(conn = conexion)

# Crear una tabla a partir de leer un archivo
query = "CREATE TABLE actas_raw AS SELECT * FROM read_csv_auto('data/resultados_2024.csv');"

# Executar el query
dbExecute(conn = conexion, statement = query)

# Ahora existe una tabla
dbListTables(conn = conexion) 


# Crear una tabla a partir de una tabla en R (o python/rust/etc)
query = "SELECT * FROM actas_raw;"

# Executar el query
dbGetQuery(conn = conexion, statement = query)

# Podemos repetir todo lo que hicimos anteriormente directamente en nuestra base SQL


# Hacer una consulta equivalente a nuestro tibble partidos
query = "-- Seleccionar Distintos valores
        SELECT DISTINCT
          idPartido AS id_partido,
          nombrePartido AS nombre_partido,
          colorPartido AS color,
          nombreCandidatoPropietario AS nombre_candidato_propietario
        FROM 
          actas_raw
;"

# Executar el query
dbGetQuery(conn = conexion, statement = query)

# Crear la tabla partidos
# Hacer una consulta equivalente a nuestro tibble partidos
query = "-- Crear una tabla a partir de una consulta
          CREATE TABLE partidos AS
            SELECT DISTINCT
              idPartido AS id_partido,
              nombrePartido AS nombre_partido,
              colorPartido AS color,
              nombreCandidatoPropietario AS nombre_candidato_propietario
            FROM 
              actas_raw
;"
# Executar el query
dbExecute(conn = conexion, statement = query)

# Lista de tablas
dbListTables(conn = conexion) 

# Hacer la consulta de la tabla partidos
query = "-- Seleccionar Distintos valores
        SELECT 
          *
        FROM 
          partidos
;"

# Executar el query
partidos_sql = dbGetQuery(conn = conexion, statement = query)

# Obtenemos lo mismo
print(partidos_sql)
print(partidos)

# Para no repetir todo en SQL podemos cargar todo desde R
# Crear la tabla entidades a partir del tibble entidades
dbWriteTable(conn = conexion, name = 'entidades', value = entidades)

# Crear la tabla municipios a partir del tibble municipios
dbWriteTable(conn = conexion, name = 'municipios', value = municipios)
# Aparece este error
# Error: Invalid Input Error: Invalid unicode (byte sequence mismatch) detected in segment statistics update
# Si revisamos los nombre de municipios hay caracteres atípicos
View(municipios)
# Esto es porque la codificación es UTF-8
Encoding(municipios$nombre_municipio) 
# Solo hay que indicarle en este caso que es latin1
Encoding(municipios$nombre_municipio) = 'latin1'
# Voilá
dbWriteTable(conn = conexion, name = 'municipios', value = municipios)


# Crear la tabla municipios a partir del tibble municipios
dbWriteTable(conn = conexion, name = 'distritos', value = distritos)

# Crear la tabla votos a partir del tibble votos
dbWriteTable(conn = conexion, name = 'votos', value = votos)

# Lista de tablas
dbListTables(conn = conexion) 

# Vamos a desconectarnos por ahora
dbDisconnect(conn = conexion)

# Dale click en memory usage para ver toda la memoria que estamos utilizando
# Después borra todos los objetos en ambiente
rm(list = ls())
# Liberemos el espacio basura (garbage collector)
gc()

# Volvamos conectarnos a nuestra base de datos
# Crear una base de datos (archivo.db)
conexion = dbConnect(
  # Motor de base de datos
  drv = duckdb::duckdb(),
  dbdir = 'data/elecciones_2024.duckdb'
)

# Revisa cuanta memoria estás utilizando

# Ahora trabajemos en SQL para solucionar las siguientes preguntas
# Cuántos votos se emitieron?
query = '
    -- Seleccionar
    SELECT 
      -- Suma una columna y asigna un nombre
      SUM(total) AS total 
    -- De la tabla
    FROM 
      votos
;'
dbGetQuery(conn = conexion, statement = query)

# Cuántos votos se emitieron por entidad federativa?
query = '
    -- Seleccionar
    SELECT 
      -- Suma una columna y asigna un nombre
      cve_ent,
      SUM(total) AS total 
    -- De la tabla
    FROM 
      votos
    -- Agrupando por
    GROUP BY 
      cve_ent
;'
dbGetQuery(conn = conexion, statement = query)

# Cuántos votos se emitieron por entidad federativa? Ordenado
query = '
  -- Seleccionar
  SELECT 
    cve_ent, SUM(total) AS total 
  -- De la tabla
  FROM 
    votos
  -- Agrupando por
  GROUP BY 
    cve_ent
  -- Ordenando por
  ORDER BY 
    total DESC
;'
dbGetQuery(conn = conexion, statement = query)

# Cuántos votos nulos se emitieron por entidad federativa? Ordenado
query = '
  -- Seleccionar
  SELECT 
    cve_ent, SUM(total) AS total 
  -- De la tabla
  FROM 
    votos
  -- Filtrando por
  WHERE  
    id_partido = 91
  -- Agrupando por
  GROUP BY 
    cve_ent
  -- Ordenando por
  ORDER BY 
    total DESC
;'

dbGetQuery(conn = conexion, statement = query)

# Qué porcentaje de votos tuvo cada partido por distrido? Ordenado
query = '
  -- Seleccionar
  SELECT 
    *,
    -- Hacer un calculo por grupos
    total / SUM(total) OVER (PARTITION BY cve_ent, id_distrito) AS porcentaje
  -- De la tabla
  FROM 
    votos
;'

dbGetQuery(conn = conexion, statement = query)

# Qué porcentaje de votos tuvo cada partido por entidad?
query = '
  -- Seleccionar
  SELECT 
    *,
    -- Hacer un calculo por grupos
    total / SUM(total) OVER (PARTITION BY cve_ent) AS porcentaje
  -- De la consulta
  FROM 
    ( -- Selección de atributos
      SELECT cve_ent, id_partido, SUM(total) AS total
      -- Tabla de consulta
      FROM votos
      -- Agrupado por
      GROUP BY cve_ent, id_partido
    )
;'

dbGetQuery(conn = conexion, statement = query)

# Qué porcentaje de votos tuvo cada partido por entidad?
query = '
  -- Seleccionar
  SELECT 
    entidades.nombre, partidos.nombre_partido, votos.*,
    -- Hacer un calculo por grupos
    total / SUM(total) OVER (PARTITION BY cve_ent) AS porcentaje
  -- De la consulta
  FROM 
    ( -- Selección de atributos
      SELECT cve_ent, id_partido, SUM(total) AS total
      -- Tabla de consulta
      FROM votos
      -- Agrupado por
      GROUP BY cve_ent, id_partido
    -- Asignar un nombre a la tabla temporal
    ) votos
  -- Concatenar por la derecha
  RIGHT JOIN
    partidos
  -- En las columnas que coincidad
  ON 
    partidos.id_partido = votos.id_partido
  -- Concatenar por la izquierda
  RIGHT JOIN
    entidades
  -- En las columnas que coincidan
  ON 
    entidades.clave = votos.cve_ent
;'

dbGetQuery(conn = conexion, statement = query)

# Cómo podemos pegarle los nombres a la tabla anterior
query = '
  -- Seleccionar
  SELECT 
    -- Atributos de la tabla entidades
    entidades.nombre AS entidad, 
    -- Atributos de la tabla partidos
    partidos.nombre_partido,
    -- Atributos de la tabla temporal votos
    votos.*,
    -- Hacer un calculo por grupos
    total / SUM(total) OVER (PARTITION BY cve_ent) AS porcentaje
  -- De la consulta
  FROM 
    ( -- Selección de atributos
      SELECT cve_ent, id_partido, SUM(total) AS total
      -- Tabla de consulta
      FROM votos
      -- Agrupado por
      GROUP BY cve_ent, id_partido
    -- Asignar un nombre a la tabla temporal
    ) votos
  -- Concatenar por la derecha
  RIGHT JOIN
    partidos
  -- En las columnas que coincidad
  ON 
    partidos.id_partido = votos.id_partido
  -- Concatenar por la izquierda
  RIGHT JOIN
    entidades
  -- En las columnas que coincidan
  ON 
    entidades.clave = votos.cve_ent
;'

dbGetQuery(conn = conexion, statement = query)

# Oye René, eso está muy complicado...
# Podemos trabajar nuestra base de datos con tidyverse
# tidyverse está pensado como un lenguaje SQL
# Conectar la tabla
tidy_query = tbl(src = conexion, 'votos') %>% 
  # Agrupar por entidad y partido
  group_by(cve_ent, id_partido) %>% 
  # Calcular totales
  summarise(total = sum(total)) %>% 
  # Agrupar por entidad
  group_by(cve_ent) %>% 
  # Calcular porcentaje
  mutate(porcentaje = total/ sum(total)) %>% 
  # Unir la tabla de partidos
  right_join(
    x = tbl(src = conexion, 'partidos') %>% 
    # Selecciona las columnas de la tabla partidos
    select(id_partido, nombre_partido),
    # Concatenar en id_partido
    by = join_by(id_partido)
  ) %>% 
  # Unir la tabla entidades
  left_join(
    y = tbl(src = conexion, 'entidades') %>% 
      # Selecciona las columnas de la tabla 
      select(clave, nombre),
    # Concatenar en donde cve_ent coincide con clave
    by = join_by( cve_ent == clave)
  )

# Esta consulta es una previsualización (lazy evaluation)
tidy_query

# Esto ocurre tras bambalinas
show_query(tidy_query)

# Para traerlo a memoria (a R) hay que ejecutar la consulta
collect(tidy_query)

# Podemos cargar el resultado en un tibble
votos_entidad = dbGetQuery(conn = conexion, statement = query)
head(votos_entidad)

# Hacer una gráfica directamente sin guardar los datos
dbGetQuery(conn = conexion, statement = query) %>% 
  # Crear un lienzo en blanco
  ggplot(
    aes(
      x = porcentaje, 
      y = factor(cve_ent),
      fill = reorder(nombre_partido, porcentaje)
      )
  ) +
  # Agregar columnas
  geom_col() +
  # Modificar colores
  scale_fill_manual(values = c('#F1F1F1','#D9D9D9','#F78A30','#779ACD','#E59492')) +
  # Separar por entidad
  facet_wrap(~entidad, scales = 'free', ncol = 8) + 
  # Transformar a coordenada polar (pie charts)
  coord_polar() +
  # Modificar etiquetas
  labs(
    title = 'Resultados de la elección 2024',
    caption = '@renorosgon'
  ) + 
  # Modificar tema
  theme_void() +
  theme(
    # Posición de la leyenda
    legend.position = 'top',
    # Eliminar título de la leyenda
    legend.title = element_blank(),
    # Tamaño texto
    legend.text = element_text(size = 8)
  )


# Agregar otras tablas y datos a nuestra base de datos ------------------
# Descargar el índice de marginación en la carpeta "data"
download.file(
  url = "http://www.conapo.gob.mx/work/models/CONAPO/Marginacion/Datos_Abiertos/Municipio/IMM_2020.xls",
  destfile = "data/IMM_2020.xls", 
  mode = "wb"
  )

# Cargar a R el índice de marginación
marginacion = readxl::read_excel(
  path = "data/IMM_2020.xls",
  sheet = 'IMM_2020'
  ) %>% 
  # Limpiar nombres
  janitor::clean_names() %>% 
  # Eliminar columnas de nombres
  select(- nom_ent, - nom_mun) %>% 
  # Transformar las claves
  mutate(
    cve_ent = as.numeric(cve_ent),
    # Extraer los últimos tres dígitos de clave mun
    cve_mun = str_extract(cve_mun, '\\d{3}$'),
    cve_mun = as.numeric(cve_mun)
  )

glimpse(marginacion)

# Agregar tabla 
dbWriteTable(conn = conexion, name = 'indice_marginacion', value = marginacion)

# Podemos aproximar al votante mediano por municipio en función de la marginación?
# Conectar la tabla
tidy_query = tbl(src = conexion, 'votos') %>% 
  # Agregar distritos 
  left_join(
    y = tbl(src = conexion, 'distritos'),
    by = join_by(cve_ent == clave_entidad, id_distrito == distrito)
  ) %>% 
  # Agregar municipios 
  left_join(
    y = tbl(src = conexion, 'municipios'),
    by = join_by(cve_ent == clave_entidad, clave_municipio)
  ) %>% 
  # Agrupar por entidad, municipio y partido
  group_by(cve_ent, clave_municipio, id_partido) %>% 
  # Calcular totales
  summarise(total = sum(total)) %>% 
  # Agrupar por entidad y municipio
  group_by(cve_ent, clave_municipio) %>% 
  # Calcular porcentaje
  mutate(porcentaje = total/ sum(total)) %>% 
  # Unir la tabla de partidos
  right_join(
    x = tbl(src = conexion, 'partidos') %>% 
      # Selecciona las columnas de la tabla partidos
      select(id_partido, nombre_partido),
    # Concatenar en id_partido
    by = join_by(id_partido)
  ) %>% 
  # Unir la tabla entidades
  left_join(
    y = tbl(src = conexion, 'entidades') %>% 
      # Selecciona las columnas de la tabla 
      select(clave, entidad = nombre),
    # Concatenar en donde cve_ent coincide con clave
    by = join_by(cve_ent == clave)
  ) %>% 
  # Agregar datos de marginación
  left_join(
    y = tbl(src = conexion, 'indice_marginacion') %>% 
      select(cve_ent, cve_mun, im_2020, gm_2020, imn_2020),
    by = join_by(cve_ent, clave_municipio == cve_mun)
  )

# Algo así podría ser el query
show_query(tidy_query)

# Hacemos la consulta y la traemos a memoria (R)
municipios_votos = collect(tidy_query)

glimpse(municipios_votos)

# Creamos un modelo logistico
# Recordemos que la liga logistica = LN(P/(1-P))
# Supongamos que P = %
# Tomemos solo los votantes de la coalición SIGAMOS HACIENDO HISTORIA PVEM_PT_MORENA
shh = municipios_votos %>% 
  filter(
    nombre_partido == 'SIGAMOS HACIENDO HISTORIA PVEM_PT_MORENA',
    !is.na(gm_2020)
  ) %>% 
  mutate(
    gm_2020 = factor(gm_2020, levels = c('Muy bajo','Bajo','Medio','Alto','Muy alto'))
  )

# Instalar - Cargar ggdist                                                       
if(require(ggdist) == FALSE){                                                
  install.packages('ggdist')                                                 
  library(ggdist)                                                            
}else{                                                                          
  library(ggdist)                                                            
}

# Hacer un gráfico de nubes de lluvia
ggplot(shh, aes(x = porcentaje, y = gm_2020, fill = gm_2020)) +
  # Densidad en la parte superior
  stat_halfeye(adjust = 3) +
  # Puntos en la parte inferior
  stat_dots(side = "left") +
  # Caja en el centro
  stat_pointinterval(width = 0.5) +
  # Modificar eje x
  scale_x_continuous(
    limits = c(0,1), 
    labels = scales::percent_format(),
    expand = c(0,0)
    ) +
  # Agregar Etiquetas
  labs(
    title = 'Votos por municipio en favor de Claudia Sheinbaum (%)',
    subtitle = 'Índice de Marginación Municipal 2020',
    caption = 'Fuente: Elaboración propia con datos de CONAPO e INE\nAutor:@renorosgon'
  ) +
  # Modificar el color
  scale_fill_manual(values = c("#9d2449",'#b8556c','#d17f90','#e9aab6','#ffd5dd')) +
  # Modificar tema
  theme_bw(base_family = 'Avenir Next') +
  theme(
    axis.title = element_blank(),
    legend.position = 'none'
  )

# Estimamos nuestra regresión logistica agregada LN(P/(1-P)) ~ im_2020
modelo_shh = shh %>% 
  lm(
  formula = log(porcentaje/(1-porcentaje)) ~ imn_2020
  )

# Resultados (recuerda que entre más grande el índice, menor la marginación)
summary(modelo_shh)

# Ahora necesitamos extraer las probabilidades
# Podemos demostrar que P = exp(x)/(1+exp(x))
# Calcular los momios
exp_x = predict(modelo_shh,shh) %>% 
  exp()
# Agregar a la predicción
shh = shh %>% 
  mutate(
    momios = exp_x,
    probabilidad = exp_x / (1 + exp_x)
    )

# Podemos comparar el porcentaje observado vs la probabilidad estimada
ggplot(shh) +
  stat_halfeye(aes(x = porcentaje, fill = 'Porcentaje Observado')) +
  stat_halfeye(aes(x = probabilidad, fill = 'Probabilidad Estimada'), side = "left") 


# Ahora podemos correr un modelo para cada una de las candidaturas
modelos = municipios_votos %>% 
  # Filtrar NA
  filter(!is.na(porcentaje)) %>% 
  # Agrupar por partido
  group_by(nombre_partido) %>% 
  # Anidar losd atos
  nest() %>%
  mutate(
    # Correr un modelo para cada grupo de datos
    modelo = map(data, ~ lm(log(porcentaje/(1-porcentaje)) ~ imn_2020, data = .x)),
    # Extraer los coefficientes de cada modelo
    coefficients = map(modelo, coefficients),
    # Extraer los coeficientes individuales
    intercept = map_dbl(coefficients, 1),
    beta = map_dbl(coefficients, 2)
    )

# Realizar predicciones para cada grupo
predicciones = modelos %>% 
  # Aplicar el modelo a cada grupo de datos
  mutate(predicciones = map2(modelo, data, ~ predict(.x, newdata = .y))) %>%
  # Desanidar los datos y las predicciones
  unnest(c(data, predicciones)) %>% 
  # Calcular momios y probabilidades
  mutate(
    # p / 1-p
    momios = exp(predicciones),
    # p = exp(x) / 
    probabilidad = momios / (1 + momios),
    # Nombrar candidaturas
    candidatura = case_when(
      nombre_partido == 'SIGAMOS HACIENDO HISTORIA PVEM_PT_MORENA' ~ 'Claudia Sheinbaum Pardo',
      nombre_partido == 'MOVIMIENTO CIUDADANO' ~ 'Jorge Álvares Máynez',
      nombre_partido == 'FUERZA  Y CORAZÓN POR MÉXICO PAN_PRI_PRD' ~ 'Xochitl Gálvez Ruíz',
      TRUE ~ NA
    ),
    # Convertir a factor
    gm_2020 = factor(gm_2020, levels = c('Muy bajo','Bajo','Medio','Alto','Muy alto'))
  ) %>% 
  filter(!is.na(candidatura), !is.na(gm_2020)) 

# Instalar - Cargar ggtext                                                       
if(require(ggtext) == FALSE){                                                
  install.packages('ggtext')                                                 
  library(ggtext)                                                            
}else{                                                                          
  library(ggtext)                                                            
}

# Instalar - Cargar ggdensity                                                       
if(require(ggdensity) == FALSE){                                                
  install.packages('ggdensity')                                                 
  library(ggdensity)                                                            
}else{                                                                          
  library(ggdensity)                                                            
}

# Gráfico superior
superior = predicciones %>% 
  # Porcentaje vs IMN_2020
  ggplot(aes(x = porcentaje, y = imn_2020, fill = candidatura)) +
  # Densidad coloreada
  geom_hdr(show.legend = F) +
  # Agregar texto
  annotate( 
    # Definir geometría y coordenadas
    geom = 'text', x = 0.88, y = c(0.51, 0.99),
    # Agregar etiquetas
    label = c('Mayor marginación','Menor marginación'),
    # Modificar detalles
    col = 'gray40', size = 3.5, family = 'Avenir Next'
  ) +
  # Agregar flechas
  annotate(
    # Definir geometría y coordenadas
    geom = 'segment', x = 0.97, xend = 0.97, y = c(0.5, 0.999), yend = c(0.499, 1),
    # Agregar flecha
    arrow = arrow(type = "closed", length = unit(0.2, "cm")), col = 'gray40'
  ) +
  # Modificar colores
  scale_fill_manual(values = c("#9d2449",'orange',"pink")) +
  scale_color_manual(values = c("#9d2449",'orange',"pink")) +
  # Agregar Etiquetas
  labs(
    # Usamos css y html para modificar los colores del título
    title = '<strong>¿La marginaión municipal influyó en los resultados de la elección presidencial 2024?</strong>',
    subtitle = "A continuación se muestra el porcentaje de votación efectiva a nivel municipal y la probabilidad de voto <br>
    en favor de <strong style='color:#F7A724;'>Jorge Álvarez Máynez</strong>, <strong style='color:#F9C0CC;'>Xochitl Gálvez Ruíz</strong> 
    y <strong style='color:#9E234A;'>Claudia Sheinbaum Pardo</strong> en función del <br><strong>Índice de Marginación Municipal 2020.<strong>",
    x = 'Porcentaje de votos a nivel de municipal'
  ) +
  # Modificar eje x
  scale_x_continuous(
    limits = c(0,1), 
    labels = scales::percent_format(),
    expand = c(0,0.008)
  ) +
  # Modificar el tema
  theme_classic(base_family = 'Avenir Next') +
  theme(
    # Para poder modificar el téxto con css y html
    plot.title = element_markdown(size = 14, lineheight = 1.5),  
    plot.subtitle = element_markdown(size = 12, lineheight = 1.2),
    # Márgenes angostos
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
    # Modificar color del texto
    text = element_text(colour = 'gray30'),
    # Modificar lineas y ejes
    axis.line.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(colour = 'gray45', linewidth = 0.25),
    axis.ticks.x = element_line(colour = 'gray45', linewidth = 0.25),
    axis.text.x = element_text(colour = 'gray45')
  ) 

# Gráfico intermedio
intermedio = predicciones %>% 
  ggplot() +
  # Agregar densidad en puntos
  stat_dots(aes(x = porcentaje, col = candidatura)) +
  # Agregar densidar en distribución orientada hacia abajo
  stat_halfeye(aes(x = probabilidad, fill = candidatura), side = "left") +
  # Modificar colores
  scale_fill_manual(values = c("#9d2449",'orange',"pink")) +
  scale_color_manual(values = c("#9d2449",'orange',"pink")) +
  # Modificar eje x
  scale_x_continuous(
    limits = c(0,1), 
    labels = scales::percent_format(),
    expand = c(0,0.008)
  ) +
  # Modificar tema
  theme_void(base_family = 'Avenir Next') +
  theme(
    # Quitar elementos de eje y
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    # Quitar leyenda
    legend.position = 'none',
    # Márgenes angostos
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
  )


# Gráfico inferior
inferior = predicciones %>% 
  # IMN_2020 (invertido) vs Probabilidad estimada
  ggplot(aes(y = - imn_2020, x = probabilidad, fill = candidatura)) +
  # Densidad con color
  geom_hdr(show.legend = F) +
  # Agregar texto
  annotate( 
    # Definir geometría y coordenadas
    geom = 'text', x = 0.88, y = c(-0.51, -0.99),
    # Agregar etiquetas
    label = c('Mayor marginación','Menor marginación'),
    # Modificar detalles
    col = 'gray40', size = 3.5, family = 'Avenir Next'
  ) +
  # Agregar flechas
  annotate(
    # Definir geometría y coordenadas
    geom = 'segment', x = 0.97, xend = 0.97, y = c(-0.5, -0.999), yend = c(-0.499, -1),
    # Agregar flecha
    arrow = arrow(type = "closed", length = unit(0.2, "cm")), col = 'gray40'
  ) +
  # Modificar colores
  scale_fill_manual(values = c("#9d2449",'orange',"pink")) +
  scale_color_manual(values = c("#9d2449",'orange',"pink")) +
  # Modificar eje x
  scale_x_continuous(
    limits = c(0,1), 
    labels = scales::percent_format(),
    expand = c(0,0.008) ,
    position = "top"
    ) +
  # Agregar etiquetas
  labs(
    x = 'Probabilidad de recibir un voto',
    caption = 'Fuente: Elaboración propia con datos de CONAPO e INE\nAutor:@renorosgon'
  ) +
  # Modificar el tema
  theme_classic(base_family = 'Avenir Next') +
  theme(
    # Modificar eje x
    axis.line.x = element_line(colour = 'gray45', linewidth = 0.25),
    axis.ticks.x = element_line(colour = 'gray45', linewidth = 0.25),
    axis.text.x = element_text(colour = 'gray45'),
    # Eliminar eje y
    axis.line.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    # Márgenes angostos
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
  )


# Instalar - Cargar patchwork                                                       
if(require(patchwork) == FALSE){                                                
  install.packages('patchwork')                                                 
  library(patchwork)                                                            
}else{                                                                          
  library(patchwork)                                                            
}

# Resultado final
superior / intermedio / inferior

