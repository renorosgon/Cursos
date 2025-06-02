# Fijar el directorio de trabajo
setwd("~/Desktop/ITESM/Cursos/TC2001B")

# Libraries ---------------------------------------------------------------
# Install-Load tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}
# Install-Load tidygraph                                                       
if(require(tidygraph) == FALSE){                                                
  install.packages('tidygraph')                                                 
  library(tidygraph)                                                            
}else{                                                                          
  library(tidygraph)                                                            
}
# Install-Load igraph                                                       
if(require(igraph) == FALSE){                                                
  install.packages('igraph')                                                 
  library(igraph)                                                            
}else{                                                                          
  library(igraph)                                                            
}
# Install-Load ggraph                                                       
if(require(ggraph) == FALSE){                                                
  install.packages('ggraph')                                                 
  library(ggraph)                                                            
}else{                                                                          
  library(ggraph)                                                            
}
# Install-Load patchwork                                                       
if(require(patchwork) == FALSE){                                                
  install.packages('patchwork')                                                 
  library(patchwork)                                                            
}else{                                                                          
  library(patchwork)                                                            
}
# Install-Load ggrepel                                                       
if(require(ggrepel) == FALSE){                                                
  install.packages('ggrepel')                                                 
}
# Install-Load GGally                                                       
if(require(GGally) == FALSE){                                                
  install.packages('GGally')                                                 
}
# Data --------------------------------------------------------------------
data = readxl::read_excel('data/redes.xlsx') %>% 
  mutate(carrera = str_replace_all(carrera,'-',' '))

# Las relaciones de-hacia se representan como ejes de una red   
edges = data %>% 
  # Seleccionar la información de-hacia
  select(from, to) %>% 
  unique()

glimpse(edges)

# Los nodos de una red son los elementos que contruyen las relaciones
nodos = data %>% 
  # Seleccionar la información de los nodos
  select(name = from, carrera:signo_zodical) %>% 
  unique()

# Para representar redes en tidyverse usamos tidygraph
network = as_tbl_graph(edges, directed = TRUE)

# Observa que es una lista de nodos y ejes
glimpse(network)

# Para trabajar con los elementos de una red
network = network %>% 
  # Activamos el elemento
  activate(nodes) %>% 
  # Concatenamos la información
  left_join(nodos, by = 'name')

str(network)
print(network)

# Visualizing networks
ggraph(network) +
  # Graficar ejes con geom_edge_*
  geom_edge_parallel(
    # Para modificar las flechas
    arrow = arrow(
      # Ángulo de la punta
      angle = 10,
      # Ancho
      length = unit(0.5, "cm"),
      # C
      ends = "last",
      type = "closed"
      )
    ) +
  # Agregar nodos con etiquetas de texto
  geom_node_label(aes(label = name), label.size = 0.1) +
  # Modificar el tema
  theme_graph(base_size = 12) 

# Diámetro de una red: longitud del camino más corto entre los dos nodos más separados
diameter(network)
# Quiénes son
farthest_vertices(network)

# Modificar una red
network = network %>% 
  # Activar los nodos
  activate(nodes) %>% 
  # Propiedades de una red
  mutate(
    # Grado: número de nodos con los que estoy conectado
    # En este caso solo nos interesa el grado interior (cuántas personas me declararon)
    vecinos = degree(network, mode = 'in'),
    # Intermediación: Con qué frecuencia el nodo se encuentra en la ruta más corta
    betweenness = betweenness(network),
    # Cercanía: Qué tan rápido puedes llegar a otros nodos
    closeness = closeness(network),
    # Triángulos: relación entre tríadas
    triangles = count_triangles(network),
    # Transitividad: porcentaje de triángulos al que pertenece respecto al potencial al que puede pertenecer
    transitivity = transitivity(network, type = "local"),
    # Centralidad propia: centralidad a partir de la centralidad de mis vecinos (valores propios)
    eigen_centrality = eigen_centrality(network) %>% 
      pluck('vector')
  ) %>% 
  # Activar ejes
  activate(edges) %>% 
  # Propiedades de los ejes
  mutate(
    # Betweenness: How often the axis is on the shortest path
    edge_betweenness = edge_betweenness(network)
  )

# Extraer un tibble de la red
nodes = network %>% 
  # Activar nodos
  activate(nodes) %>% 
  # Extraer el tibble
  as_tibble() %>% 
  # Filtrar NAs
  filter(!is.na(carrera))

# Podemos ver la correlación entre las métricas
nodes %>% 
  select_if(is.numeric) %>% 
  GGally::ggcorr(label = T)

# La relación entre las propiedades de los vertices
ggplot(
  # Agregar nuestra data
  data = nodes, 
  # Definir variables a graficar
  aes(
      x = vecinos, 
      y = betweenness, 
      size = eigen_centrality,
      fill = closeness
    )
  ) +
  # Agregar etiquetas
  ggrepel::geom_label_repel(
    aes(label = name), 
    # Modificar estéticas
    size = 5, col = 'gray90'
    ) +
  # Definir color de relleno
  scale_fill_gradient(
    low = 'red4',
    high = 'red'
  ) +
  # Etiquetas
  labs(
    x = 'Grado de entrada',
    y = 'Grado de intermediación',
    fill = 'Cercanía',
    title = 'LTP - Generación 2023-2027'
  ) +
  # Modificar el tema
  theme_classic(base_size = 12) 


# Extraer ejes
edges = network %>% 
  activate(edges) %>% 
  as_data_frame()

# Graficar la relación más importante
# el número de caminos más cortos que pasan por un eje 
# en un gráfico o red
ggplot(
  data = edges,
  aes(
    x = reorder(to, edge_betweenness), 
    y = reorder(from, edge_betweenness), 
    fill = edge_betweenness
    )
) +
  # Mapa de calor
  geom_tile() +
  # Agregar texto
  geom_text(
    aes(label = round(edge_betweenness,1)), 
    col = 'white'
    ) +
  # Definir color de relleno
  scale_fill_gradient(
    low =  'gray90',
    high = 'red3'
  ) +
  # Agregar etiquetas
  labs(
    fill = 'Intermediación',
    title = 'La amistad más importante'
  ) +
  # Modificar el tema
  theme_classic(base_size = 12) +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 45)
  )

# Calculo de similiridad --------------------------------------------------
matriz_adjacencia = as_adjacency_matrix(network)
# Similaridad como la correlación
similaridad = cor(as.matrix(matriz_adjacencia))
# Volver 0 la relación con uno mismo
diag(similaridad) = 0

GGally::ggcorr(similaridad, label = T)

# Rem remplazar na
similaridad[is.na(similaridad)] = 0 

# Crear una red de similitudes
network_similaridad = similaridad %>% 
  as_tibble(rownames = 'from') %>% 
  gather(to,similaridad, -from) %>% 
  as_tbl_graph(directed = FALSE)

# Visualizando una red de similaridad
similares = ggraph(network_similaridad) +
  # Agregar edges
  geom_edge_link(
    aes(
      # Mapear la similaridad
      alpha = similaridad, 
      col = similaridad, 
      # Filtrar ejes de similitud
      filter = similaridad > 0
      ),
    # Modificar estilos de los edges
    show.legend = FALSE) +
  geom_node_label(
    # Modificar esteticas de los edges
    aes(label = name)
  ) +
  scale_edge_colour_gradient(low = 'lightblue', high = 'darkblue') +
  # Modificar tema
  theme_graph() +
  labs(title = 'Similitud con base en relaciones')

# Visualizando una red de similaridad
disimilares = ggraph(network_similaridad) +
  # Agregar edges
  geom_edge_link(
    aes(
      # Mapear la similaridad
      alpha = similaridad, 
      color = similaridad, 
      # Filtrar ejes de similitud
      filter = similaridad < 0
    ),
    # Modificar estilos de los edges
    show.legend = F) +
  geom_node_label(
    # Modificar esteticas de los edges
    aes(label = name)
  ) +
  scale_edge_colour_gradient(low = 'pink', high = 'darkred') +
  # Modificar tema
  theme_graph() +
  labs(title = 'Disimilitud con base en relaciones')

similares + disimilares
# Detección de comunidades ------------------------------------------------
# Matriz de distancias
matriz_distancias = 1 - similaridad

# Obtain a distance object 
distancias = as.dist(matriz_distancias)
dendograma = hclust(distancias, method = 'average')
plot(dendograma)
rect.hclust(dendograma, k = 5, border=1:6)

# Crear aglomerados
clusters = cutree(dendograma, k = 5) %>% 
  as_tibble(rownames = 'name') %>% 
  rename(cluster = value)

# Agregamos a nuestra lista de nodos
nodos = nodes %>% 
  left_join(clusters, by =  'name')

# Agregamos a nuestra network
network = network %>% 
  activate(nodes) %>% 
  left_join(clusters, by = c('name'))


# Visualizando una network
ggraph(network) +
  # Graficar ejes con geom_edge_*
  geom_edge_parallel(
    # Para modificar las flechas
    arrow = arrow(
      # Ángulo de la punta
      angle = 10,
      # Ancho
      length = unit(0.5, "cm"),
      # C
      ends = "last",
      type = "closed"
    ),
    # Colorear los lados
    aes(col = edge_betweenness)
  ) +
  # Agregar nodos con etiquetas de texto
  geom_node_point(
    # Colorear por cluster
    aes(col = factor(cluster)),
    # No mostrar leyenda
    show.legend = FALSE,
    # Remover el tamaño
    size = 10
    ) +
  # Agregar etiquetas
  geom_node_label(
    # Estéticas basadas en datos
    aes(label = name, fill = factor(cluster)),
    # Estéticas no vinculadas a datos
    col = 'white', show.legend = FALSE
    ) +
  # Modificar colores
  scale_edge_colour_gradient(
    low = 'pink', 
    high = 'darkred', 
    name = 'Grado de\nIntermediación'
    ) +
  scale_color_manual(
    values = c('yellow4','yellow3','orange','orange3','orange4')
    ) +
  scale_fill_manual(
    values = c('yellow4','yellow3','orange','orange3','orange4')
    ) + 
  # Modificar el tema
  theme_graph(base_size = 12) +
  theme(legend.position = 'bottom')

# Agregamos a los clusters a los ejes
edges = edges %>% 
  # Concatenar los clusters a los ejes
  left_join(clusters, by = c('from'='name')) %>% 
  left_join(clusters, by = c('to'='name'), suffix = c('_from','_to')) %>% 
  # Definir si es homogeneo o heterogeneo
  mutate(
    type = ifelse(cluster_from == cluster_to,'Homogeneo','Heterogeneo')
    )

# Ejes
edges %>% 
  pull(type) %>% 
  table()

# Clusters
nodos %>% 
  pull(cluster) %>% 
  table()

# Conectividad
conetividad = 2 * nrow(edges) / nrow(nodos) / (nrow(nodos) - 1)

# Homophilia: que tanto se relacionan personas de la misma comunidad
# Diadicidad: conectividad dentro del mismo grupo
# D > 1, Aleatorio D ~ 1 , Antidiadica < 1
nodos_d = nodos %>% 
  filter(cluster == 4) %>% 
  nrow()

print(nodos_d)

edges_d = edges %>% 
  filter(type == 'Homogeneo', 
         cluster_from == 4) %>% 
  nrow()

print(edges_d)

edges_potenciales = nodos_d * (nodos_d -1) * conetividad / 2
print(edges_potenciales)

diad_d = edges_d / edges_potenciales
print(diad_d)

# Heterofilia:conectividad entre grupos
# Heterofílica > 1  Heterofobica H < 1
heterofilia_esperada = nodos_d * (nrow(nodos) - nodos_d) * conetividad
edges_mixtos = edges %>% 
  filter(type == 'Heterogeneo', 
         cluster_from == 4 | cluster_to == 4) %>% 
  nrow()

heterofilia = edges_mixtos / heterofilia_esperada 

print(heterofilia)

