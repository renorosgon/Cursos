# Fijar el directorio de trabajo
setwd("~/Desktop/ITESM/Cursos/TC2001B")

# Librerías ---------------------------------------------------------------
# Instalar - Cargar tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}

# Data --------------------------------------------------------------------
# Cargar los delitos de 2023 a nivel municipal
delitos = readxl::read_excel("data/delitos_2023.xlsx") %>% 
  # Limpiar nombres
  janitor::clean_names()

# Luce así
glimpse(delitos) 

# Ingeniería de características
delitos = delitos %>% 
  # Mutar el dataframe
  mutate(
    # Suma por filas de las columnas enero a diciembre
    total_anual = rowSums(select(delitos, enero:diciembre))
  ) %>% 
  # Resumir por grupos
  with_groups(
    .groups = c(ano, clave_ent, entidad, bien_juridico_afectado),
    summarise,
    # Sumar totales anuales
    delitos = sum(total_anual)
  )

# Luce así
glimpse(delitos) 

delitos_long = delitos

# Pasar los bienes jurídicos afectados a columnas
delitos = delitos %>% 
  spread(
    key = bien_juridico_afectado, 
    value = delitos, 
    fill = 0
    ) 

# Luce así
glimpse(delitos) 


# Análisis de correlación 
delitos %>% 
  select(`El patrimonio`:`Otros bienes jurídicos afectados (del fuero común)`) %>% 
  GGally::ggpairs()

# Agregar datos de la población
# https://www.gob.mx/cms/uploads/attachment/file/918028/BD_municipales_portada_regiones_FINAL.pdf
poblacion = readxl::read_excel('data/poblacion_conapo.xlsx') %>% 
  janitor::clean_names()

# Así lucen nuestros datos
glimpse(poblacion)

# Agregar la población por entidad federativa
poblacion = poblacion %>% 
  with_groups(
    .groups = c(ano, clave_ent),
    summarise,
    poblacion = sum(pob_total)
  )


# Concatenar información
incidencia = delitos %>% 
  # Concatenación interna en las columnas año y clave_ent
  inner_join(y = poblacion, by = c('ano','clave_ent')) %>% 
  # Mutar el tibble
  mutate(
    across(
      # Aplicar en las siguientes columnas
      .cols = c(`El patrimonio`:`Otros bienes jurídicos afectados (del fuero común)`), 
      # La siguiente fórmula (.x se refiere a la columna a la que se aplica)
      .fns = ~ 100000 * .x / poblacion)
  )

# Luce así
glimpse(incidencia)

# Análisis de correlación
incidencia %>% 
  select(`El patrimonio`:`Otros bienes jurídicos afectados (del fuero común)`) %>% 
  GGally::ggpairs()


# Aglomeración jerárquica -------------------------------------------------
# Cector de distancias
distancias = incidencia %>% 
  # Colocar el nombre de la entidad en el nombre de las filas
  column_to_rownames(var = 'entidad') %>% 
  # Seleccionar columnas
  select(`El patrimonio`:`Otros bienes jurídicos afectados (del fuero común)`) %>% 
  # Calcular de distancias
  dist(method = "euclidean")

# Obtenemos este vector
distancias[1:5]

# Construir aglomerados con vínculo simple
single_fit = hclust(distancias, method = "single")  

# Graficar
plot(single_fit, family="Arial")
# Agregar recuadros para 4 clusters
rect.hclust(single_fit, k = 4, border = 2:6)

# Alternativamente
dendograma = as.dendrogram(single_fit)
dendograma = color_branches(single_fit, h = 4)
plot(dendograma)

# Construir aglomerados con vínculo de Ward2 (mínima varianza entre grupos)
complete_fit = hclust(distancias, method = "ward.D2")  #Hierarchical clustering

#Plot the clusters
dendograma = as.dendrogram(complete_fit)
dendograma = color_branches(complete_fit, h = 3)

plot(
  x = dendograma, 
  family="Arial", 
  main = 'Aglomeración por incidencia de delitos',
  ylab = 'Distancia entre cluster'
)

rect.hclust(complete_fit, k=3, border=1:6)


# Extraer clusters
clusters = cutree(complete_fit, k = 3) %>% 
  # Convertir a tibble
  as_tibble(rownames = 'entidad') %>% 
  # Renombrar columna
  rename(cluster = value) %>% 
  # Convertir a factor
  mutate(cluster = factor(cluster))

# Concatenar la información
incidencia = incidencia %>% 
  left_join(clusters, by = join_by(entidad))

# Análisis de correlación
incidencia %>% 
  select(cluster,`El patrimonio`:`Otros bienes jurídicos afectados (del fuero común)`) %>% 
  GGally::ggpairs(aes(col = cluster))

