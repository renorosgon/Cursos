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
# Install-Load sf                                                       
if(require(sf) == FALSE){                                                
  install.packages('sf')                                                 
  library(sf)                                                            
}else{                                                                          
  library(sf)                                                            
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
# Install - load ggspatial                                                       
if(require(ggspatial) == FALSE){                                                
  install.packages('ggspatial')                                                 
  library(ggspatial)                                                            
}else{                                                                          
  library(ggspatial)                                                            
}
# Install - load rnaturalearth                                                       
if(require(rnaturalearth) == FALSE){                                                
  install.packages('rnaturalearth')                                                 
  library(rnaturalearth)                                                            
}else{                                                                          
  library(rnaturalearth)                                                            
}
# Install - load rnaturalearthdata                                                       
if(require(rnaturalearthdata) == FALSE){                                                
  install.packages('rnaturalearthdata')                                                 
  library(rnaturalearthdata)                                                            
}else{                                                                          
  library(rnaturalearthdata)                                                            
}

# Data --------------------------------------------------------------------
# BACI Country Codes
country_codes = read_csv('data/country_codes_V202401b.csv')
# BACI Product Codes
product_codes = read_csv('data/product_codes_HS17_V202401b.csv')

# BACI trade data
baci = read_csv('data/BACI_HS17_Y2022_V202401b.csv') %>% 
  # Filter HS for a specific product
  filter(k == '970600') %>% 
  # Select and rename columns
  transmute(
    # Trade network directions
    country_code_from = i,
    country_code_to = j,
    # HS id
    hs_id = k,
    # Value in million of USD
    value = v,
    # Quantity of trade (some products are not quantifiable)
    quantity = q
  ) %>% 
  # Join contry codes for node `from`
  left_join(
    y = country_codes %>% 
      select(country_code_from = country_code, from = country_iso3),
    by = join_by(country_code_from)
  ) %>% 
  # Join contry codes for node `to`
  left_join(
    y = country_codes %>% 
      select(country_code_to = country_code, to = country_iso3),
    by = join_by(country_code_to),
    suffix = c('_from','_to')
  )

# Load world map from naturalearth
world = ne_countries(scale = "medium", returnclass = "sf") %>% 
  filter(sovereignt != 'Antarctica')

# Get point coords
country_coords = world %>%
  # Select relevant columns
  select(admin, iso_a3_eh, geometry) %>%
  # Create centroids
  st_centroid() %>%
  mutate(
    # Get longitude coord
    long = st_coordinates(.)[, 1],
    # Get latitude coord
    lat = st_coordinates(.)[, 2]
    ) %>%
  # Drop sf
  st_drop_geometry() %>%
  # Rename column
  rename(country = admin)

glimpse(country_coords)


# Add coordinates for 'from' and 'to' countries
trade_coords = baci %>%
  # Joing `from` coords
  left_join(
    y = country_coords, 
    by = c("from" = "iso_a3_eh")
    ) %>%
  rename(
    long_from = long, 
    lat_from = lat
    ) %>%
  # Joing `to` coords
  left_join(
    y = select(country_coords, - country), 
    by = c("to" = "iso_a3_eh")
    ) %>%
  rename(
    long_to = long, 
    lat_to = lat
    ) %>% 
  # Filter NAs
  filter(
    !is.na(long_from),
    !is.na(long_to)
    ) %>%
  # Set import - export
  mutate(
    trade = ifelse(from == "MEX", 'Export', 'Import')
  )

glimpse(trade_coords)

# List of comercial partners
partners = baci %>% 
  # Filter Mexico's comercial partners
  filter(from == 'MEX' | to == 'MEX') %>% 
  # Select columns
  select(from, to) %>% 
  # Make list
  gather() %>% 
  select(value) %>% 
  # Remove 'MEX'
  filter(value != 'MEX') %>% 
  pull(value) %>% 
  unique() 

# Base world map
base_map = ggplot() +
  geom_sf(
    data = world, 
    fill = "antiquewhite3", 
    color = "gray50"
    ) 

plot(base_map)

# Add trade plot
trade_plot = base_map +
  # This is the edge curve
  geom_curve(
    # Filter country by `MEX`
    data = filter(trade_coords, from == 'MEX' | to == 'MEX'),
    # Modify aesthetics
    aes(
      # Draw the edge from - to
      x = long_from, xend = long_to, 
      # Draw the edge from - to
      y = lat_from, yend = lat_to, 
      # Set size
      size = value,
      color = trade,
      ),
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
    curvature = 0.2,
    alpha = 0.7
  ) +
  geom_point(
    data = country_coords %>% 
      filter(iso_a3_eh %in% partners),
    aes(x = long, y = lat),
    color = "orange3", size = 2
  ) +
  # Modify colors
  scale_color_manual(
    values = c('brown', 'orange2'),
    name = 'Trade direction'
    ) +
  # Modify Sizes
  scale_size_continuous(
    range = c(0.3, 2.5), 
    name = "Million of USD"
    ) +
  # Add Labels
  labs(
    title = "Mexico's Trade Network", 
    subtitle = "Antiques of an age exceeding one hundred years",
    captions = 'Source: own elaboration with BACI'
    ) +
  # Add geográfic scale
  annotation_scale(
    # Location
    location = "bl",
    # Bar colors
    bar_cols = c('antiquewhite4','antiquewhite'),
    # Text
    text_col = 'antiquewhite4',
    # Adjust vertically
    pad_y = unit(0.03, "npc"),
    line_width = 0.1
  ) +
  # Compass rose
  annotation_north_arrow(
    # Locarion
    location = "bl",        
    # Style
    style = north_arrow_fancy_orienteering(
      # Fill
      fill = c('antiquewhite4','antiquewhite'),
      # Line
      line_col = "antiquewhite4",
      # Text
      text_col = 'antiquewhite4'
    ),
    # Adjust vertically
    pad_y = unit(0.06, "npc"),
    pad_x = unit(0.006, "npc")
  ) +
  # Modify Theme
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "antiquewhite"),
    axis.title = element_blank()
  )

# Plot
print(trade_plot)

