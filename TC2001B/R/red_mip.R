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
# Data --------------------------------------------------------------------
# Cargar los delitos de 2023 a nivel municipal
mip = readxl::read_excel("data/mip_2018.xlsx") %>% 
  # Generar formato largo
  gather(key = 'to', value = 'porcentaje', - sector) %>% 
  rename(from = sector)

# Luce así
glimpse(mip) 

# Constgruir Red
network = as_tbl_graph(x = mip,directed = T)

network %>% 
  # Mutar nodos
  activate(nodes) %>% 
  # Extraer código SCIAN
  mutate(scian = str_extract(name,'[\\d-]+')) %>% 
  activate(edges) %>% 
  # Filtrar por índice de sector/subsector/rama/clase
  filter(
    from %in% c(5) | to %in% c(5)  
      ) %>% 
  # Visualizando una network
  ggraph(layout = 'linear') +
  # Graficar ejes con geom_edge_*
  geom_edge_arc(
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
    aes(
      col = porcentaje, 
      filter = porcentaje > 0,
    #  linewidth = porcentaje
      )
  ) +
  # Agregar nodos con etiquetas de texto
  geom_node_point() +
  # Agregar etiquetas
  geom_node_label(
    # Estéticas basadas en datos
    aes(label = scian)
  ) +
  # Modificar el tema
  theme_graph(base_size = 12) +
  theme(legend.position = 'bottom')

