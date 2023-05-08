# To import fonts into R
# install.packages(c('remotes','extrafont'))
# remotes::install_version("Rttf2pt1", version = "1.3.8")
# library(extrafont)
# extrafont::font_import()

# Fijar el directorio de trabajo
setwd("~/Desktop/ITESM/Cursos/EC3001C")


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


# Data --------------------------------------------------------------------
# Les Miserables edges
edges = read_csv('data/los_miserables.csv')

# Look like thise
summary(edges)
glimpse(edges)

# Interactions between characters
ggplot(edges) +
  # Plot histogram
  geom_histogram(aes(x = value)) +
  # Add labels
  labs(
    x = 'Interactions', 
    y = 'Count', 
    title = 'Les Miserables - Shared scenes between characters'
    ) 


# Character Importance
importance = edges %>% 
  # Reorder data
  gather(node, name, source:target) %>% 
  with_groups(
    .group = name,
    summarise,
    weight = sum(value)
  ) %>%  
  arrange(desc(weight))
  
  
# Plot character importance
ggplot(importance) +
  # Add Columns
  geom_col(aes(x = weight, y = reorder(name,weight))) +
  # Add labels
  labs(x = 'Number of scenes', title = 'Les Miserables - Character Importance') +
  # Expand x-axis
  scale_x_continuous(expand = c(0,0)) +
  # Modify theme
  theme_bw() +
  theme(
    text = element_text(family = 'Bebas Neue'),
    axis.title.y = element_blank()
  )


# Adjacency Matrix
ggplot(edges,
       aes(
         x = reorder(source, value),
         y = reorder(target, value),
         fill = value
         )
       ) +
  # Add heatmap
  geom_tile(show.legend = FALSE) +
  # Add text
  geom_text(aes(label = value), col = 'white') +
  # Modify theme
  theme_bw() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 90)
  )
  
  
# Create a non-directed network with tidygraph
network = as_tbl_graph(edges, directed = FALSE)

str(network)
print(network)

# Visualizing networks
ggraph(network) +
  # Add edges
  geom_edge_link( aes(alpha = value, col = value), show.legend = FALSE) +
  # Add nodes
  geom_node_text(aes(label = name), repel = TRUE) +
  # Modify theme
  theme_graph(base_size = 12) 

# Diameter of a network: length of the shortest path between the two most separated nodes
diameter(network)
# Who are they?
farthest_vertices(network)

# Modify elements of a network
network = network %>% 
  # Activate notes
  activate(nodes) %>% 
  # Some node properties
  mutate(
    # Degree: number of nodes with which I am connected
    neighbors = degree(network),
    # Strength: sum of the weights (importance)
    strength = strength(network, weights = pull(edges, value)),
    # Betweenness: How often the node is on the shortest path
    betweenness = betweenness(network),
    # Closeness: How close it is to other nodes
    closeness = closeness(network),
    # Triangles: relationship between triads
    triangles = count_triangles(network),
    # Transitivity: percentage of triangles to which it belongs with respect to 
    # the potential to which it can belong
    transitivity = transitivity(network,type = "local"),
    # Eigencentrality (neighbors centrality)
    eigen_centrality = eigen_centrality(network, weights = pull(edges, value)) %>% 
                       pluck('vector')
  ) %>% 
  # Activate edges
  activate(edges) %>% 
  # Edges properties 
  mutate(
    # Betweenness: How often the axis is on the shortest path
    edge_betweenness = edge_betweenness(network, weights = 1/pull(edges, value))
    )
  
# Extracting variables
nodes = network %>% 
  activate(nodes) %>% 
  as_tibble() %>% 
  arrange(desc(closeness))

nodes %>% 
  select_if(is.numeric) %>% 
  cor()

# La relación entre las propiedades de los vertices
ggplot(nodes, aes(x = neighbors, y = betweenness)) +
  geom_point(aes(size = strength, col = closeness)) +
  geom_text(aes(label = name), size = 2) +
  scale_y_log10() +
  scale_x_log10()


neighbors(network, 'Gavroche')
neighbors(network, 'Valjean')


# Extract edges
edges = network %>% 
  activate(edges) %>% 
  as_data_frame()

# How important an edge is
count_edges = edges %>% 
  # Count values
  count(value) %>%
  # See frequencies
  mutate(percentage = 100 * n / nrow(edges),
         cummulate = cumsum(percentage)) 

# Add a classification to the network
network = network %>% 
  activate(edges) %>% 
  mutate(weak = value < 7,
         strong = value >= 7)

# Visualizing strong links
strong = ggraph(network) +
  # Add edges
  geom_edge_link(aes(alpha = value, col = edge_betweenness, filter = strong),
                 show.legend = FALSE) +
  # Add nodes
  geom_node_point(aes(size = strength, col = log(betweenness)),
                  show.legend = FALSE) +
  geom_node_text(aes(label = name), size = 3, repel = T) +
  labs(title = 'Strong Ties') +
  # Modify theme
  theme_graph()

# Visualizing weak links
weak = ggraph(network) +
  # Add edges
  geom_edge_link(aes(alpha = value, col = edge_betweenness, filter = weak),
                 show.legend = FALSE) +
  # Add nodes
  geom_node_point(aes(size = strength, col = log(betweenness)),
                  show.legend = FALSE) +
  geom_node_text(aes(label = name), size = 3, repel = T) +
  labs(title = 'Weak Ties') +
  # Modify theme
  theme_graph()

library(patchwork)
strong + weak

# Calculo de similiridad --------------------------------------------------
matriz_adjacencia = as_adjacency_matrix(network)
similaridad = cor(as.matrix(matriz_adjacencia))
diag(similaridad) = 0

qplot(as.vector(similaridad))
GGally::ggcorr(similaridad, label = T)

# Convert weighted similarity matrix to a graph
network_similaridad = similaridad %>% 
  as_tibble(rownames = 'from') %>% 
  gather(to,similaridad, -from) %>% 
  as_tbl_graph(directed = FALSE)

# Visualizando una network
ggraph(network_similaridad, layout ='circle') +
  # Agregar edges
  geom_edge_link(
    # Modificar esteticas de los edges
    aes(alpha = similaridad, filter = similaridad > 0.3),
    # Modificar estilos de los edges
    show.legend = FALSE) +
  geom_node_text(
    # Modificar esteticas de los edges
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
clusters = cutree(dendograma, k = 3) %>% 
  as_tibble(rownames = 'nombre') %>% 
  rename(cluster = value)

# Agregamos a nuestra lista de nodos
nodos = nodos %>% 
  left_join(clusters, by =  'nombre')

# Agregamos a nuestra network
network = network %>% 
  activate(nodes) %>% 
  left_join(clusters, by = c('name'= 'nombre'))


# Visualizando una network
ggraph(network) +
  # Agregar edges
  geom_edge_link(
    # Modificar esteticas de los edges
    aes(alpha = value, col = intermediacion, filter = debil),
    # Modificar estilos de los edges
    show.legend = FALSE) +
  # Agregar nodos
  geom_node_point(
    aes(size = importancia, col = factor(cluster)
    ),
    show.legend = FALSE
  ) +
  geom_node_text(
    # Modificar esteticas de los edges
    aes(label = name), 
    # Modificar estilos
    size = 3, repel = T) +
  # Modificar tema
  theme_graph()

# Agregamos a los edges
edges = edges %>% 
  left_join(clusters, by = c('from'='name')) %>% 
  left_join(clusters, by = c('to'='name'), suffix = c('_from','_to')) %>% 
  mutate(type = ifelse(cluster_from==cluster_to,'Homogeneo','Heterogeneo'))

edges %>% 
  pull(type) %>% 
  table()

nodos %>% 
  pull(cluster) %>% 
  table()

# Conectividad
conetividad = 2 * nrow(edges) / nrow(nodos) / (nrow(nodos) - 1)

# Homophilia: que tanto se relacionan personas de la misma comunidad
# Diadicidad: conectividad dentro del mismo grupo
# D > 1, Aleatorio D ~ 1 , Antidiadica < 1
nodos_4 = nodos %>% 
  filter(cluster == 4) %>% 
  nrow()
edges_4 = edges %>% 
  filter(type == 'Homogeneo', 
         cluster_from == 4) %>% 
  nrow()
edges_4_potenciales = nodos_4 * (nodos_4 -1) * conetividad / 2
diad_4 = edges_4 / edges_4_potenciales

# Heterophilia:conectividad entre grupos
# Heterophilida > 1  Heterofobica H<1
heterofilia_esperada = nodos_4 * (nrow(nodos) - nodos_4) * conetividad
edges_mixtos = edges %>% 
  filter(type == 'Heterogeneo', 
         cluster_from == 4 | cluster_to == 4) %>% 
  nrow()

heterofilia_4 = edges_mixtos / heterofilia_esperada 


homofilia = function(edges, nodos, clusters = c(1:5)){
  conectividad = 2 * nrow(edges) / nrow(nodos) / (nrow(nodos) - 1)
  
  nodos_grupo = nodos %>% 
    filter(cluster %in% clusters) %>% 
    nrow()
  
  edges_grupo = edges %>% 
    filter(
      (cluster_from %in% clusters) & (cluster_to %in% clusters)
        ) %>% 
    nrow()
  
  diadicidad_potencial = nodos_grupo * (nodos_grupo -1) * conetividad / 2
  diadicidad = edges_grupo / diadicidad_potencial
  
  edges_mixtos = edges %>% 
    filter(
      (cluster_from %in% clusters) | (cluster_to %in% clusters) & (cluster_from != cluster_to)
      ) %>% 
    nrow()
  
  heterofilia_esperada =  nodos_grupo * (nrow(nodos) - nodos_grupo) * conetividad
  heterofilia = edges_mixtos / heterofilia_esperada 
  
  homofilia = tibble(
    conectividad,
    nodos_grupo,
    edges_grupo,
    diadicidad_potencial,
    diadicidad,
    edges_mixtos,
    heterofilia_esperada,
    heterofilia
  )
  return(homofilia)
}

homoflia_tb = homofilia(edges, nodos, clusters = 2)

t(homoflia_tb)




