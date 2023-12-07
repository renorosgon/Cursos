# Fijar directorio de trabajo
setwd("~/Desktop/ITESM/Cursos/TC2001B")

# Libraries ---------------------------------------------------------------
# install.packages(c('terra','leafem','sf'))
# Install - load tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}
# Install - load sf                                                       
if(require(sf) == FALSE){                                                
  install.packages('sf')                                                 
  library(sf)                                                            
}else{                                                                          
  library(sf)                                                            
}
# Install - load ggsn                                                       
if(require(ggsn) == FALSE){                                                
  install.packages('ggsn')                                                 
  library(ggsn)                                                            
}else{                                                                          
  library(ggsn)                                                            
}
# Install - load patchwork                                                       
if(require(patchwork) == FALSE){                                                
  install.packages('patchwork')                                                 
  library(patchwork)                                                            
}else{                                                                          
  library(patchwork)                                                            
}

# Simple Features Geometries ----------------------------------------------
# Points
point_2D = st_point(c(1,2))
str(point_2D)
plot(point_2D)

point_3D = st_point(c(1,2,3))
str(point_3D)

# Multipoints
coords = rbind(c(0,3),c(0,4),c(1,5),c(2,5))
multipoints = st_multipoint(coords)
str(multipoints)
plot(multipoints)

# Linestring
linestring = st_linestring(coords)
str(linestring)
plot(linestring)

# Multilinestring
coords_2 = rbind(c(0.2,3), c(0.2,4), c(1,4.8), c(2,4.8))
coords_3 = rbind(c(0,4.4), c(0.6,5))

multilinestring = st_multilinestring(list(coords, coords_2, coords_3))
str(multilinestring)
plot(multilinestring)

# Polygons
pol_coords_1 = rbind(c(0,0), c(1,0), c(3,2), c(2,4), c(1,4), c(0,0))
polygon_1 = st_polygon(list(pol_coords_1))
str(polygon_1)
plot(polygon_1, col = 'gray')

pol_coords_2 = rbind(c(1,1), c(1,2), c(2,2), c(1,1))
polygon_2 = st_polygon(list(pol_coords_1, pol_coords_2))
str(polygon_2)
plot(polygon_2, col = 'gray')

# Multipolygons
pol_coords_3 = rbind(c(3,0), c(4,0), c(4,1), c(3,1), c(3,0))
pol_coords_4 = rbind(c(3.3,0.3), c(3.8,0.3), c(3.8,0.8), c(3.3,0.8), c(3.3,0.3))[5:1,]
pol_coords_5 = rbind(c(3,3), c(4,2), c(4,3), c(3,3))

multipolygon = st_multipolygon(
  list(
    # Polygon 1
    list(pol_coords_1,pol_coords_2),
    # Poligon 2
    list(pol_coords_3,pol_coords_4),
    # Polygon 3
    list(pol_coords_5)
  )
)
str(multipolygon)

plot(multipolygon, col = 'gray')

# Geometry collection
geometry_collection =- st_geometrycollection(list(multipoints, multilinestring, multipolygon))
str(geometry_collection)
plot(geometry_collection, col ='gray')

# Centroids
centroid = st_centroid(polygon_1)
str(centroid)
plot(polygon_1)
plot(centroid, add = TRUE)


# Create shp
#write_sf(geometry_collection, 'example.shp')

# Working with shapefiles -------------------------------------------------
# Loading a shape file
imuc = read_sf('data/imuc/colonias_imc2020.shp') %>% 
  filter(CVE_ENT == '09')

# What is this?
class(imuc)

# GIS reference
st_crs(imuc) 

# Where is the SF? in the geometry
attr(imuc, 'sf_column')

glimpse(imuc)

st_geometry(imuc)

# What can I do to it?
methods(class = "sf")

# Mapping Margination Index
imuc %>% 
  # Mutate strata to factor
  mutate(
    GM_2020 = factor(GM_2020, levels =  c("Muy alto", "Alto", "Medio", "Bajo", "Muy bajo"))
  ) %>% 
  # Maps with ggplot
  ggplot() +
    # Choropleth
    geom_sf(aes(fill = GM_2020)) +
    # Modify colors
    scale_fill_viridis_d(option = 'B', direction =  -1) +
    # Add labels 
    labs(
      title = 'Indice de Marginación Urbana en CDMX',
      fill = 'Nivel de Marginación',
      caption = 'Elaboración propia con datos de CONAPO'
    ) +
    # Add compass Rose
    north(imuc, location = 'topright') +
    # Add geographic scale
    scalebar(imuc, transform = FALSE, dist_unit = 'km', dist = 5, 
           location = "bottomleft", st.size = 3) +
   # Modify theme
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_rect(fill = "transparent"),
      legend.position = c(0.125, 0.9),
      legend.key.height = unit(0.5,'cm')
      )

# Spatial Operations ------------------------------------------------------
# Load Mexico City Basic Geostatistical Area
agebs = c('data/poligono_ageb_urbanas_cdmx/poligono_ageb_urbanas_cdmx.shp',
          'data/poligono_ageb_rurales_cdmx/poligono_ageb_rurales_cdmx.shp')
agebs = map_df(
  .x = agebs,
  .f = read_sf
)


# GIS reference
st_crs(agebs) 

# Is is the same as IMUC crs?
st_crs(agebs) == st_crs(imuc)

# What does this mean
ggplot(imuc) +
  # Choropleth
  geom_sf(fill = 'blue', alpha = 0.5, col = NA) +
  # Choropleth
  geom_sf(data = agebs, fill = 'white', alpha = 0.5, col = NA) 
  
# How do we solve it?
# Transform CRS
imuc = imuc  %>% 
  mutate(
    # Mutate strata to factor
    GM_2020 = factor(GM_2020, levels =  c("Muy alto", "Alto", "Medio", "Bajo", "Muy bajo")),
    # Calculate area
    AREA = st_area(geometry)
  ) %>% 
  # Change crs (projection)
  st_transform(crs = st_crs(agebs))

# Checking they have the same projection
st_crs(agebs) == st_crs(imuc)

# This is how it looks
ggplot(imuc) +
  # Choropleth
  geom_sf(fill = 'blue', alpha = 0.5, col = NA) +
  # Choropleth
  geom_sf(data = agebs, fill = 'white', alpha = 0.5, col = NA) 

# Validity 
# Simple feature validity refers to a number of properties that polygons should 
# have, such as non-self intersecting, holes being inside polygons. 
st_is_valid(imuc) 

imuc %>% 
  filter(!st_is_valid(imuc)) %>% 
  st_is_valid(reason = T) 

# Make valid polygons
imuc = st_make_valid(imuc)

# Load metro stations (Points)
metro = read_sf('data/stcmetro_shp/STC_Metro_estaciones_utm14n.shp') %>% 
  st_transform(crs = st_crs(imuc))

# Maps with ggplot
ggplot(imuc) +
  # Choropleth
  geom_sf(aes(fill = GM_2020)) +
  # Modify colors
  scale_fill_viridis_d(option = 'B', direction =  -1, guide = guide_legend(order = 1)) +
  # Dot map
  geom_sf(data = metro, aes(col = 'Estación del Metro'), 
          fill = 'white', shape = 21,size = 2) +
  scale_color_manual(values = 'black', guide = guide_legend(title = NULL, order = 2)) +
  # Add labels 
  labs(
    title = 'Indice de Marginación Urbana en CDMX',
    fill = 'Nivel de Marginación',
    caption = 'Elaboración propia con datos de CONAPO'
  ) +
  # Add compass Rose
  north(imuc, location = 'topright') +
  # Add geographic scale
  scalebar(imuc, transform = TRUE, dist_unit = 'km', dist = 5, 
           location = "bottomleft", st.size = 3) +
  # Modify theme
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(fill = "transparent", color = NA),
    legend.position = c(0.125, 0.85),
    legend.key.height = unit(0.5,'cm'),
    
  )

# Filtering shape files
centro_colonia = filter(imuc, COLONIA == 'Centro', MUN == '015')
# Intersections
centro_agebs = st_intersection(agebs, centro_colonia)

# What does this mean
agebs_plot = ggplot(centro_agebs) +
  # Choropleth
  geom_sf(col = 'black') +
  labs(title = 'Mexico City Center - AGEBS') 

suburb_plot = ggplot(centro_colonia) +
  # Choropleth
  geom_sf(col = 'black') +
  labs(title = 'Mexico City Center - Suburb')

agebs_plot + suburb_plot

# Interseccion of polythong with point
intersection = st_intersects(centro_agebs, metro)
# Number of hostpitals per ageb
num_metro_stations = sapply(intersection, length)
# Add to the tibble
centro_agebs = centro_agebs %>% 
  mutate(metro_stations = num_metro_stations)
# Looks like this
ggplot(centro_agebs) +
  # Choropleth
  geom_sf(aes(fill = factor(metro_stations))) 

# Interseccion of point with polygon
intersection = st_intersects(metro, centro_agebs)
metro_index = sapply(intersection, length)
# Get the hospital
metro_stations = metro %>% 
  filter(row_number() %in% which(metro_index == 1))

# Plotting points
ggplot(centro_agebs) +
  # Choropleth
  geom_sf(fill = NA, col = 'black') + 
  geom_sf(data = metro_stations, size = 3) + 
  geom_sf_label(data = metro_stations, aes(label = NOMBRE), vjust = 1)

# Distance Matrix
distances = st_distance(metro_stations, metro_stations, ) 
rownames(distances) = pull(metro_stations, NOMBRE)
colnames(distances) = pull(metro_stations, NOMBRE)
distances

# Buffering
buffer = st_buffer(metro_stations, dist = units::as_units(500, 'meters')) %>% 
  st_cast("LINESTRING")

# Plotting points
ggplot(centro_agebs) +
  # Choropleth
  geom_sf(fill = NA, col = 'black') + 
  geom_sf(data = buffer, aes(col = NOMBRE), size = 1.5 , show.legend = FALSE) + 
  geom_sf(data = metro_stations, aes(col = NOMBRE), size = 3, show.legend = FALSE) + 
  geom_sf_text(data = metro_stations, aes(label = NOMBRE), vjust = 1.2, show.legend = FALSE) 

 
# Interseccion of polythong with point
intersection = st_intersects(centro_agebs, buffer)
# Number of hostpitals per ageb
num_metro_stations = sapply(intersection, length)
# Add to the tibble
centro_agebs = centro_agebs %>% 
  mutate(metro_stations = num_metro_stations)
# Looks like this
ggplot(centro_agebs) +
  # Choropleth
  geom_sf(aes(fill = metro_stations)) +
  labs(
    title = 'Metro Coverage in Mexico City Downtown',
    fill = 'Available metro access \n(Less than 500 m perimeter)'
    ) +
  # Modify colors
  scale_fill_viridis_c(option = 'B', breaks = seq(0,9,3)) +
  # Modify theme
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_rect(fill = "transparent", color = NA),
    legend.position = c(0.125, 0.85),
    legend.key.height = unit(0.5,'cm'),
  )

