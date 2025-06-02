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

# Instalar - Cargar p2distance                                                       
if(require(p2distance) == FALSE){                                                
  install.packages("https://cran.r-project.org/src/contrib/Archive/p2distance/p2distance_1.0.1.tar.gz")                                      
  library(p2distance)                                                            
}else{                                                                          
  library(p2distance)                                                            
}

# Instalar - Cargar stratification                                                       
if(require(stratification) == FALSE){                                                
  install.packages('stratification')                                                 
  library(stratification)                                                            
}else{                                                                          
  library(stratification)                                                            
}

# Instalar - Cargar GGally                                                       
if(require(GGally) == FALSE){                                                
  install.packages('GGally')                                                 
}

# Rutinas de cálculo ------------------------------------------------------
# Indice de marginación con distancia P2
source('R/conapo_ime.R')

# Data --------------------------------------------------------------------
# Enlistar los temas 
para_borrar = ls()

# Guardar los resultados
ime = IME_2020_resultados

# Borrar lo que ya no nos sirve
rm(list = para_borrar)
rm(para_borrar)

# Aglomeración jerárquica -------------------------------------------------
# Cector de distancias
distancias = ime %>% 
  # Colocar el nombre de la entidad en el nombre de las filas
  column_to_rownames(var = 'NOM_ENT') %>% 
  # Seleccionar columnas
  select(ANALF:PO2SM) %>% 
  # Calcular de distancias
  dist(method = "euclidean")

# Construir aglomerados con vínculo de Ward2 (mínima varianza entre grupos)
complete_fit = hclust(distancias, method = "ward.D2")  #Hierarchical clustering

# Extraer clusters
clusters = cutree(complete_fit, k = 5) %>% 
  # Convertir a tibble
  as_tibble(rownames = 'NOM_ENT') %>% 
  # Renombrar columna
  rename(cluster_jerarquico = value) %>% 
  # Convertir a factor
  mutate(cluster_jerarquico = factor(cluster_jerarquico))

# Concatenar la información
data = ime %>% 
  left_join(clusters, by = join_by(NOM_ENT))

# Análisis de correlación
GGally::ggpairs(
  data = select(data,ANALF:cluster_jerarquico),
  mapping = aes(colour = GM_2020)
  )

# Análisis de comparar categorías
GGally::ggpairs(
  data = select(data, IM_2020:cluster_jerarquico),
  mapping = aes(colour = GM_2020)
  )

# Reordenar clusters
data = data %>% 
  # Convertir a factor
  mutate(
    cluster_jerarquico = factor(
      x = cluster_jerarquico, 
      levels = c(2,1,3,5,4)
      )
    )



# Mapas -------------------------------------------------------------------
# Instalar - Cargar sf                                                       
if(require(sf) == FALSE){                                                
  install.packages('sf')                                                 
  library(sf)                                                            
}else{                                                                          
  library(sf)                                                            
}

# Instalar - Cargar patchwork                                                       
if(require(patchwork) == FALSE){                                                
  install.packages('patchwork')                                                 
  library(patchwork)                                                            
}else{                                                                          
  library(patchwork)                                                            
}

# Cargar los polígonos
mexico = st_read('data/entidades')

glimpse(mexico)

mapas = mexico %>% 
  left_join(
    y = select(data, - NOM_ENT), by = c('CVEGEO' = 'CVE_ENT'))

