# Set working directory
setwd("~/Desktop/ITESM/Cursos/TC2001B")

# Libraries ---------------------------------------------------------------
# Install - load tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}
# Install - load factoextra                                                       
if(require(factoextra) == FALSE){                                                
  install.packages('factoextra')                                                 
  library(factoextra)                                                            
}else{                                                                          
  library(factoextra)                                                            
}
# Install - load FactoMineR                                                       
if(require(FactoMineR) == FALSE){                                                
  install.packages('FactoMineR')                                                 
  library(FactoMineR)                                                            
}else{                                                                          
  library(FactoMineR)                                                            
}
# Install - load DataExplorer                                                       
if(require(DataExplorer) == FALSE){                                                
  install.packages('DataExplorer')                                                 
  library(DataExplorer)                                                            
}else{                                                                          
  library(DataExplorer)                                                            
}
# Install - janitor                                        
if(require(janitor) == FALSE){                                                
  install.packages('janitor')                                                 
}
# Install - GGally                                        
if(require(GGally) == FALSE){                                                
  install.packages('GGally')                                                 
}

# Data --------------------------------------------------------------------
# V-Dem provides a multidimensional and disaggregated dataset that reflects the 
# complexity of the concept of democracy as a system of rule that goes beyond 
# the simple presence of elections. It distinguishes between five high-level 
# principles of democracy: electoral, liberal, participatory, deliberative, 
# and egalitarian, and collect data to measure these principles.
# For mor info visit: https://www.v-dem.net/

# Read the data
vdem = read_csv("data/V-Dem-CY-Full+Others-v14.csv")  %>% 
  # Filter the most recent year
  filter(year == max(year)) %>% 
  # Select and rename columns
  select(
    country_name, 
    iso3 = country_text_id, 
    electoral = v2x_polyarchy,
    liberal = v2x_libdem, 
    participatory = v2x_partipdem,
    deliberative = v2x_delibdem,
    egalitarian = v2x_egaldem
  ) 


# Exploratory data analysis -----------------------------------------------
glimpse(vdem)

# Metrics on the structure
plot_intro(vdem)

# Missing values
plot_missing(vdem)

#To visualize distributions for all continuous features
plot_histogram(vdem)

# Correlation plot
vdem %>% 
  # Select only numeric features
  select_if(is.numeric) %>% 
  GGally::ggpairs()

# Principal Component Analysis --------------------------------------------
pca = vdem  %>% 
  # Set rownames
  column_to_rownames(var = 'iso3') %>% 
  # Select only numeric features
  select_if(is.numeric) %>% 
  # Center and Scale the data
  prcomp(center = T, scale. = T)

# We already have a democracy index
fviz_pca_ind(
  # PCA objext
  X = pca,
  # PCs to be ploted
  axes = c(1,2),
  # Geoms to plot
  geom = c("point", "text"),
  # Avoid text overlapping
  repel = TRUE,
  # Title
  title = 'PCA - Individuals representation'
)

# K-Means -----------------------------------------------------------------
scaled_data = vdem  %>% 
  # Change row names
  column_to_rownames(var = 'iso3') %>% 
  # Select only numeric dada
  select_if(is.numeric) %>% 
  # Scale and center data
  scale(center = T, scale = T)

# Optimal number of centroids by silhoutte width
fviz_nbclust(
    x = scaled_data, 
    FUNcluster = kmeans, 
    method = "silhouette",
  ) 

# Optimal number of centroids by gap stat
fviz_nbclust(
    x = scaled_data, 
    FUNcluster = kmeans, 
    method = "gap_stat", 
    nboot = 1000
  ) 

# Optimal number of centroids by Withung Sum of Square
fviz_nbclust(
  x = scaled_data, 
    FUNcluster = kmeans, 
    method = "wss"
  ) 

# K-mean fitting
kmean_results = kmeans(
  # Data
  x = scaled_data,
  # Number of clusters
  centers = 3, 
  # Number of random starts
  nstart = 100, 
  # Number of max iterations
  iter.max = 1000
)

kmean_results

# Cluster per observation
pluck(kmean_results, 'cluster')
# Observation per cluster
pluck(kmean_results, 'size')
# Centroids
pluck(kmean_results, 'centers')


# Ploting data
fviz_cluster(
  # Cluster object
  object = kmean_results, 
  # Reference data (we need scale data)
  data = scaled_data,
  # Set pallete
  palette = c("green4", "orange1", "red3"),
  # Concentration ellipse
  ellipse.type = "euclid", 
  ellipse.alpha = 0.05,
  # Avoid label overplotting (slows the process)
  repel = TRUE, 
  # Modify theme
  ggtheme = theme_minimal(),
  # Title
  main = 'Aglomeración por K-Medias'
  ) +
  theme(legend.position = 'top')


# Lets get all our data together
final_data = vdem %>% 
  # Join PCs
  left_join(
    # Extract pca scores
    y = pluck(pca, 'x') %>% 
      # Get rownames to column
      as_tibble(rownames = 'iso3'),
    # Join by iso3
    by = join_by(iso3)
    )  %>% 
  left_join(
    # Extract cluster number
    y = pluck(kmean_results, 'cluster') %>% 
      # Get rownames to column
      as_tibble(rownames = 'iso3') %>% 
      # Rename cluster column
      rename(cluster = value) %>% 
      # Mutate to factor
      mutate(cluster = factor(cluster)
      ),
    # Join by iso3
    by = join_by(iso3)
  )

# Final plot
final_data %>% 
  # Select data of interest
  select(electoral:cluster) %>% 
  # Correlation plot colored by cluster
  GGally::ggpairs(aes(col = cluster, fill = cluster)) +
  # Modify color pallete
  scale_color_manual(values = c("green4", "orange1", "red3")) +
  scale_fill_manual(values = c("green4", "orange1", "red3"))


# We can get rid of the noisy components
final_data %>% 
  # Select data of interest
  select(electoral:PC1, cluster) %>% 
  # Correlation plot colored by cluster
  GGally::ggpairs(aes(col = cluster, fill = cluster)) +
  # Modify color pallete
  scale_color_manual(values = c("green4", "orange1", "red3")) +
  scale_fill_manual(values = c("green4", "orange1", "red3"))



