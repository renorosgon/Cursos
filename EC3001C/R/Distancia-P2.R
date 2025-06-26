# Fijar directorio de trabajo
setwd("~/Desktop/ITESM/Cursos/EC3001C")

# Librerías ---------------------------------------------------------------
# Instalar - Cargar tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}

# Instalar - Cargar duckdb                                                       
if(require(duckdb) == FALSE){                                                
  install.packages('duckdb')                                                 
  library(duckdb)                                                            
}else{                                                                          
  library(duckdb)                                                            
}

# Conexión con SQL --------------------------------------------------------
# Establecer conexión
conexion = dbConnect(
  # Motor de base de datos
  drv = duckdb::duckdb(),
  # El nombre de mi archivo (base de datos)
  dbdir = 'data/envi_2020_base_de_datos_csv/Bases de datos/ENVI.duckdb'
)

# Identificar
dbListTables(conn = conexion)

# Cuáles son los niveles de satisfacción de la vivienda en México?
query = "
  SELECT
    -- Concatenar valores de dos o más columnas con ||
    FOLIO || VIV_SEL AS folio,
    P6_3_1 AS satisfaccion_pisos,
    P6_3_2 AS satisfaccion_paredes_techos,
    P6_3_3 AS satisfaccion_acabados,
    P6_3_4 AS satisfaccion_iluminacion,
    P6_3_5 AS satisfaccion_ventilacion,
    P6_3_6 AS satisfaccion_proteccion,
    FACTOR AS ponderador
  FROM
    vivienda
  WHERE 
    -- Filtrar valores nulos
    satisfaccion_pisos IS NOT NULL 
  ;"

# Traer a memoria
satisfaccion = dbGetQuery(conn = conexion, statement = query)
head(satisfaccion)

# Desconectar por ahora
dbDisconnect(conn = conexion)

# Modelar los datos -------------------------------------------------------
# Instalar - Cargar Hmisc  
if(require(Hmisc) == FALSE){    
  install.packages('Hmisc')                                                 
}
# Instalar - Cargar GGally  
if(require(GGally) == FALSE){    
  install.packages('GGally')                                                 
}

# Revisar el resumen de los datos
summary(satisfaccion)

# Análisis de correlación
satisfaccion %>% 
  select(satisfaccion_pisos:satisfaccion_proteccion) %>% 
  GGally::ggcorr(label = T)

# Análisis muestrales
count(satisfaccion, satisfaccion_paredes_techos) %>% 
  mutate(porcentaje = 100 *  n/sum(n))

# Análisis poblacionales
satisfaccion %>% 
  count(satisfaccion_paredes_techos, wt = ponderador) %>% 
  mutate(porcentaje = 100 *  n/sum(n))


# Distancia P2 ------------------------------------------------------------
# Seleccionar variables
variables =  select(satisfaccion, satisfaccion_pisos:satisfaccion_proteccion)
pesos = pull(satisfaccion, ponderador)

# Definir metriz de referencia
referencia = mutate_all(variables, function(x) x = 4)

# Cálculo de distancias ponderadas
diferencia = abs(variables - referencia)

# Desviaciones muestrales 
desviaciones = variables %>% 
  # Calcular las varianzas ponderadas
  mutate_all(Hmisc::wtd.var, weights = pesos) %>% 
  # Calcular la raíz cuadrada (desviaciones estandar)
  sqrt()

# Inversa de la desviación estandar
ponderador = 1 / desviaciones

# Calcular las ditancias
distancias = diferencia * ponderador

# Distancia inicial (índice de Freshet)
distancia_p2 = rowSums(distancias)

# Así luce nuestra primer distancia
summary(distancia_p2)

# Iterar hasta encontrar convergencia
iteracion = 1
repeat{
  # Imprimir contador
  print(sprintf('Iteración: %s', iteracion))
  
  # Punto de partida
  distancia_p2_aux = distancia_p2
  
  # Ordenar por correlación
  orden = order(abs(cor(distancias, distancia_p2)), decreasing = TRUE)
  distancias = select(distancias, all_of(orden)) 
  
  # Calcular ponderadores 1-r2
  columnas = colnames(distancias)
  
  ponderador_r2 = tibble(
    V1 = rep(1, nrow(distancias))
  )
  
  # Regresíon 1 a varios del más correlacionado al menos correlacionado
  for (i in 1:(ncol(distancias) - 1)){
    # Generar la formula
    formula = paste(columnas[i+1], '~', paste(columnas[1:i], collapse = ' + '))

    # Regresión lineal simple
    r = lm(formula = formula, data = distancias, weights = pesos) %>% 
      # Extraer r cuadrada
      summary() %>% pluck('r.squared')
    
    # Agregar pondeerador
    ponderador_r2 = mutate(ponderador_r2, 'V{i+1}' := 1 - r)
  }
  
  names(ponderador_r2) = columnas
  
  # Actualizar distancia p2
  distancia_p2 = rowSums(distancias * ponderador_r2)
  
  # Condiciones de paro
  if((iteracion >= 1000) || (0.0001 >= mean(abs(distancia_p2_aux - distancia_p2)))){
    break
  }
  
  # Actualizar iteracion
  iteracion = iteracion + 1
}

summary(distancia_p2)

# Distribución de nuestra distancia P2
ggplot() + 
  geom_histogram(aes(x = distancia_p2,  y = after_stat(density)))

# Estratificación Dalenius-Hodges -----------------------------------------
# Instalar - Cargar stratification                                                       
if(require(stratification) == FALSE){                                                
  install.packages('stratification')                                                 
  library(stratification)                                                            
}else{                                                                          
  library(stratification)                                                            
}

# Se identifican los casos extremos en el índice y se aplica el método de caja 
# propuesto por Hubert y Vandervieren, para establecer los límites 
# con los que se debe trabajar.
outliers = boxplot.stats(distancia_p2)
cota_inferior =  pluck(outliers, 'stats') %>% 
  first()

# Utilizaremos este índice para la estratificación
dp2_outliers = ifelse(distancia_p2 > cota_inferior, distancia_p2, cota_inferior)

# Estratificación con base en la calibración  
strata_dh = strata.cumrootf(
    x = dp2_outliers,
    CV = 0.05,
    Ls = 5,
    # Neyman allocation
    alloc = c(0.5, 0, 0.5), 
    nclass = 20
    )

# Extraenos los estratos y asignamos las etiquetas
estratos = factor(
  x = pluck(strata_dh, 'stratumID'), 
  levels = 1:5, 
  c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto")
)

# Agregar a nuestro tibble
satisfaccion = satisfaccion %>% 
  mutate(nivel_satisfaccion = estratos)

# Análisis de los datos ---------------------------------------------------

# Establecer conexión
conexion = dbConnect(
  # Motor de base de datos
  drv = duckdb::duckdb(),
  # El nombre de mi archivo (base de datos)
  dbdir = 'data/envi_2020_base_de_datos_csv/Bases de datos/ENVI.duckdb'
)

query = "
  SELECT 
    -- Concatenar valores de dos o más columnas con ||
    FOLIO || VIV_SEL AS folio,
    CASE 
        WHEN TLOC = 1 THEN '100,000 y más habitantes'
        WHEN TLOC = 2 THEN '15,000 a 99,999 habitantes'
        WHEN TLOC = 3 THEN '2,500 a 14,999 habitantes'
        WHEN TLOC = 4 THEN 'menor a 2,500 habitantes'
    END AS habitantes_localidad,
    
    CASE 
        WHEN P4_4 = 1 THEN 'Material de desecho'
        WHEN P4_4 = 2 THEN 'Lámina de cartón'
        WHEN P4_4 = 3 THEN 'Lámina de asbesto o metálica'
        WHEN P4_4 = 4 THEN 'Carrizo, bambú o palma'
        WHEN P4_4 = 5 THEN 'Embarro o bajareque'
        WHEN P4_4 = 6 THEN 'Madera'
        WHEN P4_4 = 7 THEN 'Adobe'
        WHEN P4_4 = 8 THEN 'Tabique, ladrillo, block, piedra,\n cantera, cemento o concreto'
    END AS material_paredes,
    
    CASE 
        WHEN ENT = 1 THEN 'AGUASCALIENTES'
        WHEN ENT = 2 THEN 'BAJA CALIFORNIA'
        WHEN ENT = 3 THEN 'BAJA CALIFORNIA SUR'
        WHEN ENT = 4 THEN 'CAMPECHE'
        WHEN ENT = 5 THEN 'COAHUILA'
        WHEN ENT = 6 THEN 'COLIMA'
        WHEN ENT = 7 THEN 'CHIAPAS'
        WHEN ENT = 8 THEN 'CHIHUAHUA'
        WHEN ENT = 9 THEN 'CIUDAD DE MEXICO'
        WHEN ENT = 10 THEN 'DURANGO'
        WHEN ENT = 11 THEN 'GUANAJUATO'
        WHEN ENT = 12 THEN 'GUERRERO'
        WHEN ENT = 13 THEN 'HIDALGO'
        WHEN ENT = 14 THEN 'JALISCO'
        WHEN ENT = 15 THEN 'ESTADO DE MEXICO'
        WHEN ENT = 16 THEN 'MICHOACAN'
        WHEN ENT = 17 THEN 'MORELOS'
        WHEN ENT = 18 THEN 'NAYARIT'
        WHEN ENT = 19 THEN 'NUEVO LEON'
        WHEN ENT = 20 THEN 'OAXACA'
        WHEN ENT = 21 THEN 'PUEBLA'
        WHEN ENT = 22 THEN 'QUERETARO'
        WHEN ENT = 23 THEN 'QUINTANA ROO'
        WHEN ENT = 24 THEN 'SAN LUIS POTOSI'
        WHEN ENT = 25 THEN 'SINALOA'
        WHEN ENT = 26 THEN 'SONORA'
        WHEN ENT = 27 THEN 'TABASCO'
        WHEN ENT = 28 THEN 'TAMAULIPAS'
        WHEN ENT = 29 THEN 'TLAXCALA'
        WHEN ENT = 30 THEN 'VERACRUZ'
        WHEN ENT = 31 THEN 'YUCATAN'
        WHEN ENT = 32 THEN 'ZACATECAS'
    END AS entidad
  FROM 
    vivienda
  WHERE 
    -- Filtrar valores nulos
    P6_3_1 IS NOT NULL 
  ;
"

# Agregar datos
satisfaccion = satisfaccion %>% 
  # Concatenar la consulta por la izquierda
  left_join(y = dbGetQuery(conn = conexion, statement = query), by = join_by(folio)) %>% 
  mutate(
    # Agregar la distancia P2
    distancia_p2 = distancia_p2,
    # Normalizar los valores
    p2_scale = 100 * (distancia_p2 - min(distancia_p2)) / (max(distancia_p2) - min(distancia_p2)),
    
    habitantes_localidad = factor(
      x = habitantes_localidad, 
      levels =  c(
        'menor a 2,500 habitantes',
        '2,500 a 14,999 habitantes',
        '15,000 a 99,999 habitantes',
        '100,000 y más habitantes'
        )
      ),
    material_paredes = factor(
      x = material_paredes, 
      levels =  c(
        'Material de desecho', 'Lámina de cartón', 'Lámina de asbesto o metálica',
        'Carrizo, bambú o palma', 'Embarro o bajareque', 'Madera', 'Adobe',
        'Tabique, ladrillo, block, piedra,\n cantera, cemento o concreto'
        )
    ),
    entidad = factor(
      x = entidad,
      levels = c(
       'AGUASCALIENTES','BAJA CALIFORNIA','BAJA CALIFORNIA SUR','CAMPECHE',
       'COAHUILA','COLIMA','CHIAPAS','CHIHUAHUA','CIUDAD DE MEXICO',
       'DURANGO','GUANAJUATO','GUERRERO','HIDALGO','JALISCO','ESTADO DE MEXICO',
       'MICHOACAN','MORELOS','NAYARIT','NUEVO LEON','OAXACA','PUEBLA',
       'QUERETARO','QUINTANA ROO','SAN LUIS POTOSI','SINALOA','SONORA',
       'TABASCO','TAMAULIPAS','TLAXCALA','VERACRUZ','YUCATAN','ZACATECAS'
      )
    )
  ) 


# Desconectarse de los valores
dbDisconnect(conn = conexion)



# Análisis de Resultados --------------------------------------------------
# Porcentaje de viviendas
satisfaccion %>% 
  # Total de viviendas
  group_by(nivel_satisfaccion) %>% 
  reframe(viviendas = sum(ponderador)) %>% 
  # Câlculo de porcentajes
  mutate(viviendas = viviendas/sum(viviendas)) %>% 
  # Crear un lienzo
  ggplot(aes(x = '', y = viviendas, fill = nivel_satisfaccion)) +
  # Columnas apiladas
  geom_col(position = 'stack', show.legend = FALSE) +
  # Agregar texto
  geom_text(
    aes(x = 1.15, label = paste0(nivel_satisfaccion, '\n', round(100 * viviendas, 2),'%')),
    # Modificar la posición
    position = position_stack(vjust = 0.5),
    # Color y tipo de letra
    col = 'white', family = 'Avenir Next'
  ) +
  # Expandir la gráfica
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  # Coordenadas polares
  coord_polar(theta = 'y', clip = 'off')  +
  # Agregar etiquetas
  labs(
    title = '¿Cuál es el nivel de satisfación con la vivienda en México?',
    caption = 'Autor: René Rosado González\nFuente: Elaboración propia con datos de la ENVI2021'
  ) + 
  # Modificar colores
  scale_fill_manual(values = c('darkred','darkorange', 'yellow3','green4','darkgreen')) +
  # Modificat tema
  theme_void(base_size = 12, base_family = 'Avenir Next') +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "inches"),
    # Transparencias
    rect = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent"), 
    plot.background = element_rect(fill = "transparent", color = NA),
  )

# Guardar pie
ggsave('images/pie.png', units = 'cm', 
       height = 30, width = 30, 
       dpi = 150, bg = "transparent")

# Porcentaje de viviendas
satisfaccion %>% 
  # Total de viviendas
  group_by(entidad, nivel_satisfaccion) %>% 
  reframe(viviendas = sum(ponderador)) %>% 
  # Câlculo de porcentajes
  group_by(entidad) %>% 
  mutate(viviendas = viviendas/sum(viviendas)) %>% 
  # Crear un lienzo
  ggplot(aes(x = '', y = viviendas, fill = nivel_satisfaccion)) +
  # Columnas apiladas
  geom_col(position = 'stack') +
  # Agregar texto
  geom_text(
    aes(x = 1.18, label = paste0(round(100 * viviendas, 2),'%')),
    # Modificar la posición
    position = position_stack(vjust = 0.5),
    # Color y tipo de letra
    col = 'white', family = 'Avenir Next',
    size = 3
  ) +
  # Expandir la gráfica
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  facet_wrap(~entidad, ncol = 8) +
  # Coordenadas polares
  coord_polar(theta = 'y', clip = 'off')  +
  # Agregar etiquetas
  labs(
    title = '¿Cuál es el nivel de satisfación con la vivienda en México?',
    caption = 'Autor: René Rosado González\nFuente: Elaboración propia con datos de la ENVI2021'
  ) + 
  # Modificar colores
  scale_fill_manual(values = c('darkred','darkorange', 'yellow3','green4','darkgreen')) +
  # Modificat tema
  theme_void(base_size = 12, base_family = 'Avenir Next') +
  theme(
    legend.position = 'top',
    legend.title = element_blank(),
    legend.justification = c("left", "top"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.box.margin = margin(5, 0, 1, 0),
    # Transparencias
    rect = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent"), 
    plot.background = element_rect(fill = "transparent", color = NA),
  )

# Barras
# Porcentaje de viviendas
satisfaccion %>% 
  # Total de viviendas
  group_by(nivel_satisfaccion) %>% 
  reframe(viviendas = sum(ponderador)) %>% 
  # Câlculo de porcentajes
  mutate(viviendas = viviendas/sum(viviendas)) %>% 
  # Crear un lienzo
  ggplot(aes(x = nivel_satisfaccion, y = viviendas, fill = nivel_satisfaccion)) +
  # Columnas apiladas
  geom_col(show.legend = FALSE) +
  # Agregar texto
  geom_text(
    aes(label = paste0(round(100 * viviendas, 2),'%')),
    # Modificar la posición
    vjust = 2,
    # Color y tipo de letra
    col = 'white', family = 'Avenir Next'
  ) +
  # Expandir la gráfica
  scale_x_discrete(expand = c(0.11, 0.11)) +
  scale_y_continuous(expand = c(0,0,0,0.01), labels = scales::percent) +
  # Agregar etiquetas
  labs(
    title = '¿Cuál es el nivel de satisfación con la vivienda en México?',
    caption = 'Autor: René Rosado González\nFuente: Elaboración propia con datos de la ENVI2021'
  ) + 
  # Modificar colores
  scale_fill_manual(values = c('darkred','darkorange', 'yellow3','green4','darkgreen')) +
  # Modificat tema
  theme_bw(base_size = 12, base_family = 'Avenir Next') +
  theme(
    legend.position = 'top',
    legend.title = element_blank(),
    legend.justification = c("left", "top"),
    axis.title = element_blank(),
    legend.box.margin = margin(5, 0, 1, 0),
    # Transparencias
    rect = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent"), 
    plot.background = element_rect(fill = "transparent", color = NA),
  )


satisfaccion %>% 
  # Total de viviendas
  group_by(material_paredes, nivel_satisfaccion) %>% 
  reframe(viviendas = sum(ponderador)) %>% 
  # Câlculo de porcentajes
  group_by(material_paredes) %>% 
  mutate(viviendas = viviendas/sum(viviendas)) %>% 
  # Crear un lienzo
  ggplot(aes(x = viviendas, y = material_paredes, fill = nivel_satisfaccion)) +
  # Columnas apiladas
  geom_col() +
  # Agregar texto
  geom_text(
    aes(label = paste0(round(100 * viviendas, 2),'%')),
    # Modificar la posición
    position = position_stack(vjust = 0.5),
    # Color y tipo de letra
    col = 'white', family = 'Avenir Next'
  ) +
  # Expandir la gráfica
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0,0), labels = scales::percent) +
  # Agregar etiquetas
  labs(
    title = '¿Cuál es el nivel de satisfación con la vivienda en México?',
    subtitle = 'Satisfacción de la vivienda según material de los muros',
    caption = 'Autor: René Rosado González\nFuente: Elaboración propia con datos de la ENVI2021'
  ) + 
  # Modificar colores
  scale_fill_manual(values = c('darkred','darkorange', 'yellow3','green4','darkgreen')) +
  # Modificat tema
  theme_bw(base_size = 12, base_family = 'Avenir Next') +
  theme(
    legend.position = 'top',
    legend.title = element_blank(),
    legend.justification = c("left", "top"),
    axis.title = element_blank(),
    legend.box.margin = margin(0, 0, 0, 0),
    # Transparencias
    rect = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent"), 
    plot.background = element_rect(fill = "transparent", color = NA),
  )



























