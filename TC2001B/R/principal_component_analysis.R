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
# For more info visit: https://www.v-dem.net/

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

# This is a PCA object
str(pca)

# Means
pluck(pca, 'center')
# Standar deviations
pluck(pca, 'scale')

# PCA standard deviations
pluck(pca, 'sdev')

# PCAs (eigenvectors)
pluck(pca, 'rotation')

# PCA Scores
pluck(pca, 'x')

# The PCA scores are orthogonal
pluck(pca, 'x') %>% 
  GGally::ggpairs()

summary(pca)

# Hoe many componets
fviz_eig(pca,  addlabels = TRUE)


# PCA for Variables (factominer) ------------------------------------------
# Results for variables
pca_var_results = get_pca(res.pca = pca, element = 'var')
pca_var_results

# Coordinates for the variables
pluck(pca_var_results,'coord')


# Correlations between variables and dimensions
pluck(pca_var_results,'cor')

# Add PCA
data = bind_cols(vdem, pluck(pca,'x'))
# Correlation plot
data %>% 
  # Select only numeric features
  select_if(is.numeric) %>% 
  GGally::ggpairs()

# Correlation plot
data %>% 
  # Select only numeric features
  select(electoral:PC1) %>% 
  GGally::ggpairs()

# Contributions of the variables to the PCA   
pluck(pca_var_results,'contrib')

# Top variable contribution PC1
fviz_contrib(pca, choice = "var", axes = 1)
# Top variable contribution PC2
fviz_contrib(pca, choice = "var", axes = 2)
# Top variable contribution PC3
fviz_contrib(pca, choice = "var", axes = 3)

# Visualizing multiple PCs
fviz_pca_var(
  # PCA objext
  X = pca,
  # PCs to be ploted
  axes = c(1,2),
  # Geoms to plot
  geom = c("point", "arrow", "text"),
  # Avoid text overlapping
  repel = TRUE,
  # Color by contribution
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  # Title
  title = 'PCA - Variable Contribution'
)


# How much of the variable’s variance is captured by a given PC.
pluck(pca_var_results,'cos2')

fviz_pca_var(
  # PCA objext
  X = pca,
  # PCs to be ploted
  axes = c(1,2),
  # Geoms to plot
  geom = c("point", "arrow", "text"),
  # Avoid text overlapping
  repel = TRUE,
  # Color by representation
  col.var = "cos2",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  # Title
  title = 'PCA - Variable representation'
)


# PCA for Individuals (factominer) ----------------------------------------
# Results for individuals
pca_ind_results = get_pca(res.pca = pca, element = 'ind')
pca_ind_results

# Coordinates for the individuals
pluck(pca_ind_results,'coord')

# Contributions of the ind to the PCA   
pluck(pca_ind_results,'contrib')

pluck(pca_ind_results,'contrib') %>% 
  colSums()

# Top contributors
fviz_contrib(pca, choice = "ind", axes = 1)
# Top contributors
fviz_contrib(pca, choice = "ind", axes = 2) 

# Visualizing multiple PCs
fviz_pca_ind(
  # PCA objext
  X = pca,
  # PCs to be ploted
  axes = c(1,2),
  # Geoms to plot
  geom = c("point", "text"),
  # Avoid text overlapping
  repel = TRUE,
  # Color by contribution
  col.ind = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  # Title
  title = 'PCA - Individual contribution'
)


# How much of the variable’s variance is captured by a given PC.
pluck(pca_ind_results,'cos2')

fviz_pca_ind(
  # PCA objext
  X = pca,
  # PCs to be ploted
  axes = c(1,2),
  # Geoms to plot
  geom = c("point", "text"),
  # Avoid text overlapping
  repel = TRUE,
  # Color by representation
  col.ind = "cos2",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  # Title
  title = 'PCA - Individuals representation'
)
