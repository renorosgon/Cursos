# Para importar fuentes
install.packages(c('remotes','extrafont'))
remotes::install_version("Rttf2pt1", version = "1.3.8")
library(extrafont)
font_import()

# Fijar el directorio de trabajo
setwd("~/Desktop/ITESM/Cursos/EC3001C")

# 1.  Librerías -----------------------------------------------------------
# Instalar - Cargar tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}
# Instalar - Cargar tidygraph                                                       
if(require(tidygraph) == FALSE){                                                
  install.packages('tidygraph')                                                 
  library(tidygraph)                                                            
}else{                                                                          
  library(tidygraph)                                                            
}
# Instalar - Cargar igraph                                                       
if(require(igraph) == FALSE){                                                
  install.packages('igraph')                                                 
  library(igraph)                                                            
}else{                                                                          
  library(igraph)                                                            
}
# Instalar - Cargar ggraph                                                       
if(require(ggraph) == FALSE){                                                
  install.packages('ggraph')                                                 
  library(ggraph)                                                            
}else{                                                                          
  library(ggraph)                                                            
}


# 2. Minería y análisis de datos ------------------------------------------

# Cargar los datos
ejes = read_csv('data/los_miserables.csv')

# Resumen
summary(ejes)

# Histograma con las interacciones entres personajes
ggplot(ejes) +
  geom_histogram(aes(x = value)) +
  labs(x = 'Interacciones entre personajes', y = 'Conteo') +
  ggtitle('Los Miserables', 'Escenas compartidas entre personajes')


# Calcular la importancia de los personajes
importancia_personajes = ejes %>% 
  # Agrupar por nombre
  gather(nodo, nombre, source:target) %>% 
  group_by(nombre) %>% 
  # Sumar pesos
  summarise(peso = sum(value)) %>% 
  # Arreglar de forma descendente
  arrange(desc(peso))
  
  
# Grafico de Columnas con la importancia de personajes
ggplot(importancia_personajes) +
  geom_col(aes(x = peso, y = reorder(nombre,peso))) +
  labs(x = 'Número de Escenas') +
  ggtitle('Los Miserables', subtitle = 'Aparariciones') +
  scale_x_continuous(expand = c(0,0))+
  theme_bw() +
  theme(
    text = element_text(family = 'Bebas Neue'),
    axis.title.y = element_blank()
  )


# Matriz de adjacencia (gráficamente)
ggplot(ejes,
       aes(
         x = reorder(source, value),
         y = reorder(target, value),
         fill = value
         )
       ) +
  geom_tile(show.legend = FALSE) +
  geom_text(aes(label = value), col = 'white') +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 90)
  )
  
  
# Crear un table-graph no dirigido
red = as_tbl_graph(ejes, directed = FALSE)

# Luce así  
red

# Visualizando una red
ggraph(red) +
  # Agregar ejes
  geom_edge_link(
    # Modificar esteticas de los ejes
    aes(alpha = value, col = value),
    # Modificar estilos de los ejes
    show.legend = FALSE) +
  # Agregar nodos
  geom_node_text(
    # Modificar esteticas de los ejes
    aes(label = name), 
    # Modificar estilos
    size = 2) +
  # Modificar tema
  theme_graph()

# Diametro de una red: logitudo del camino más corto entre los dos nodos más separados
diameter(red)
# Cuáles son esos nodos?
farthest_vertices(red)

# Modificar elementos de una red
red = red %>% 
  # Activar el elemento
  activate(nodes) %>% 
  # Algunas propiedades de los nodos
  mutate(
    # Grado: número de nodos con los que estoy conectado
    vecinos = degree(red),
    # Fuerza: suma de los pesosv(importancia)
    importancia = strength(red, weights = pull(ejes, value)),
    # Intermediación: Que tan seguido el nodo se encuentra en el camino mas corto
    intermediacion = betweenness(red),
    # Cercanīa: Que tan cerca esta de otros nodos 
    cercania = closeness(red),
    # Triangulos: relación entre triadas
    triangulos = count_triangles(red),
    # Transitividad: porcentaje de triangulos a los que pertenece respecto al potencial al que puede pertener
    transitividad = transitivity(red,type = "local"),
    # Centralidad de mis vecinos
    eigen_centralidad = eigen_centrality(red, weights = pull(ejes, value)) %>% pluck('vector')
  ) %>% 
  # Activar el elemento
  activate(edges) %>% 
  # Algunas propiedades de los ejes
  mutate(
    # Intermediación: Que tan seguido el eje se encuentra en el camino mas corto
    intermediacion = edge_betweenness(red, weights = 1/pull(ejes, value))
    )
  
# Exraer los nodos como un tibble
nodos = red %>% 
  activate(nodes) %>% 
  as_tibble() %>% 
  arrange(desc(cercania))

# La relación entre las propiedades de los vertices
ggplot(nodos, aes(x = vecinos, y = intermediacion)) +
  geom_point(aes(size = importancia, col = cercania),
             show.legend = FALSE) +
  geom_text(aes(label = name), size = 2) +
  scale_y_log10() +
  scale_x_log10()

ggplot(nodos, aes(x = triangulos, y = transitividad)) +
  geom_point(aes(size = vecinos), show.legend = FALSE, col = 'white') +
  geom_text(aes(label = name), size = 2) +
  scale_size(range = c(5,10))


neighbors(red, 'Gavroche')
neighbors(red, 'Valjean')


# Exraer los ejes como un tibble
ejes = red %>% 
  activate(edges) %>% 
  as_data_frame()

# Grafica la relacion entre elementos
ejes %>% 
  mutate(name = paste(from, to, sep = '-')) %>% 
  ggplot(aes(x = value, y = intermediacion)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10()


# Como sabemos si un eje es importante
conteo_ejes = ejes %>% 
  # Conteo de valores
  count(value) %>%
  # Frecuencias
  mutate(porcentaje = 100 * n / nrow(ejes),
         acumulado = cumsum(porcentaje)) 

# Agregamos esto a la red
red = red %>% 
  activate(edges) %>% 
  mutate(debil = value < 7,
         fuerte = value > 6)

# Visualizando una red
ggraph(red) +
  # Agregar ejes
  geom_edge_link(
    # Modificar esteticas de los ejes
    aes(alpha = value, col = intermediacion, filter = fuerte),
    # Modificar estilos de los ejes
    show.legend = FALSE) +
  # Agregar nodos
  geom_node_point(
    aes(size = importancia, col = log(intermediacion)
    ),
    show.legend = FALSE
  ) +
  geom_node_text(
    # Modificar esteticas de los ejes
    aes(label = name), 
    # Modificar estilos
    size = 3, repel = T) +
  # Modificar tema
  theme_graph()



# Calculo de similiridad --------------------------------------------------
matriz_adjacencia = as_adjacency_matrix(red)
similaridad = cor(as.matrix(matriz_adjacencia))
diag(similaridad) = 0

qplot(as.vector(similaridad))


# Convert weighted similarity matrix to a graph
red_similaridad = similaridad %>% 
  as_tibble(rownames = 'from') %>% 
  gather(to,similaridad, -from) %>% 
  as_tbl_graph(directed = FALSE)

# Visualizando una red
ggraph(red_similaridad, layout ='circle') +
  # Agregar ejes
  geom_edge_link(
    # Modificar esteticas de los ejes
    aes(alpha = weight, filter = weight > 0.75),
    # Modificar estilos de los ejes
    show.legend = FALSE) +
  geom_node_text(
    # Modificar esteticas de los ejes
    aes(label = name), repel = T
    ) +
  # Modificar tema
  theme_graph()


# Detección de comunidades ------------------------------------------------

# Matriz de distancias
matriz_distancias = 1-similaridad

# Obtain a distance object 
distancias = as.dist(matriz_distancias)
dendograma = hclust(distancias, method = 'average')
plot(dendograma)

# Crear aglomerados
clusters = cutree(dendograma, k = 5) %>% 
  as_tibble(rownames = 'name') %>% 
  rename(cluster = value)

# Agregamos a nuestra lista de nodos
nodos = nodos %>% 
  left_join(clusters, by = 'name')

# Agregamos a nuestra red
red = red %>% 
  activate(nodes) %>% 
  left_join(clusters, by = 'name')


# Visualizando una red
ggraph(red) +
  # Agregar ejes
  geom_edge_link(
    # Modificar esteticas de los ejes
    aes(alpha = value, col = intermediacion, filter = debil),
    # Modificar estilos de los ejes
    show.legend = FALSE) +
  # Agregar nodos
  geom_node_point(
    aes(size = importancia, col = factor(cluster)
    ),
    show.legend = FALSE
  ) +
  geom_node_text(
    # Modificar esteticas de los ejes
    aes(label = name), 
    # Modificar estilos
    size = 3, repel = T) +
  # Modificar tema
  theme_graph()

# Agregamos a los ejes
ejes = ejes %>% 
  left_join(clusters, by = c('from'='name')) %>% 
  left_join(clusters, by = c('to'='name'), suffix = c('_from','_to')) %>% 
  mutate(type = ifelse(cluster_from==cluster_to,'Homogeneo','Heterogeneo'))

ejes %>% 
  pull(type) %>% 
  table()

nodos %>% 
  pull(cluster) %>% 
  table()

# Conectividad
conetividad = 2 * nrow(ejes) / nrow(nodos) / (nrow(nodos) - 1)

# Homophilia: que tanto se relacionan personas de la misma comunidad
# Diadicidad: conectividad dentro del mismo grupo
# D > 1, Aleatorio D ~ 1 , Antidiadica < 1
nodos_4 = nodos %>% 
  filter(cluster == 4) %>% 
  nrow()
ejes_4 = ejes %>% 
  filter(type == 'Homogeneo', 
         cluster_from == 4) %>% 
  nrow()
ejes_4_potenciales = nodos_4 * (nodos_4 -1) * conetividad / 2
diad_4 = ejes_4 / ejes_4_potenciales

# Heterophilia:conectividad entre grupos
# Heterophilida > 1  Heterofobica H<1
heterofilia_esperada = nodos_4 * (nrow(nodos) - nodos_4) * conetividad
ejes_mixtos = ejes %>% 
  filter(type == 'Heterogeneo', 
         cluster_from == 4 | cluster_to == 4) %>% 
  nrow()

heterofilia_4 = ejes_mixtos / heterofilia_esperada 


homofilia = function(ejes, nodos, clusters = c(1:5)){
  conectividad = 2 * nrow(ejes) / nrow(nodos) / (nrow(nodos) - 1)
  
  nodos_grupo = nodos %>% 
    filter(cluster %in% clusters) %>% 
    nrow()
  
  ejes_grupo = ejes %>% 
    filter(
      (cluster_from %in% clusters) & (cluster_to %in% clusters)
        ) %>% 
    nrow()
  
  diadicidad_potencial = nodos_grupo * (nodos_grupo -1) * conetividad / 2
  diadicidad = ejes_grupo / diadicidad_potencial
  
  ejes_mixtos = ejes %>% 
    filter(
      (cluster_from %in% clusters) | (cluster_to %in% clusters) & (cluster_from != cluster_to)
      ) %>% 
    nrow()
  
  heterofilia_esperada =  nodos_grupo * (nrow(nodos) - nodos_grupo) * conetividad
  heterofilia = ejes_mixtos / heterofilia_esperada 
  
  homofilia = tibble(
    conectividad,
    nodos_grupo,
    ejes_grupo,
    diadicidad_potencial,
    diadicidad,
    ejes_mixtos,
    heterofilia_esperada,
    heterofilia
  )
  return(homofilia)
}

homoflia_tb = homofilia(ejes, nodos, clusters = 3)

t(homoflia_tb)




