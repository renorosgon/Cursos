setwd("~/Desktop/ITESM/Cursos/EC3001C")

# 1.  Libraries -----------------------------------------------------------
# Install - load DBI                                                       
if(require(DBI) == FALSE){                                                
  install.packages('DBI')                                                 
  library(DBI)                                                            
}else{                                                                          
  library(DBI)                                                            
}
# Install - load RSQLite                                                       
if(require(RSQLite) == FALSE){                                                
  install.packages('RSQLite')                                                 
  detach("package:RSQLite", unload = TRUE)
}
# Install - load tidyverse                                                       
if(require(tidyverse) == FALSE){                                                
  install.packages('tidyverse')                                                 
  library(tidyverse)                                                            
}else{                                                                          
  library(tidyverse)                                                            
}

# Load Base Model Simulator
source('R/social_mobility_model.R')
# Building your Data Base -------------------------------------------------
# Create a connection to our new database, social_mobility_abm.db
# you can check that the .db file has been created on your working directory
connection <- dbConnect(RSQLite::SQLite(), "data/social_mobility_abm.db")

# List all the tables available in the database
dbListTables(connection)

# DELETE TABLES
statement = "DROP TABLE IF EXISTS social_mobility_model;"
dbExecute(conn = connection, statement = statement)

# List availabe tables
dbListTables(connection)

# CREATE TABLE
# In relational databases, a table is a list of rows. Each row has the same 
# column structure that consists of cells. Each row also has a consecutive 
# rowid sequence number used to identify the row. Therefore, you can consider 
# a table as a list of pairs: (rowid, row).
statement = "CREATE TABLE social_mobility_model
          (
            run_id INTEGER, 
            agent_id INTEGER,
            time INTEGER,
            wealth REAL
          );"

dbExecute(conn = connection, statement = statement)
  
# List availabe tables
dbListTables(connection)

# CREATE UNIQUE INDEX
# Unlike a table, an index has an opposite relationship: (row, rowid). 
# An index is an additional data structure that helps improve the performance 
# of a query. Querying using equality (=) and ranges (>, >=, <,<=) on the 
# indexes are very efficient.

# Each index must be associated with a specific table. An index consists of one 
# or more columns, but all columns of an index must be in the same table. A 
# table may have multiple indexes.
query = "CREATE UNIQUE INDEX idx_run_agent_time ON base_model (run_id, agent_id, time);"
dbExecute(conn = connection, statement = query)

query = "CREATE INDEX idx_run_time ON base_model (run_id, time);"
dbExecute(conn = connection, statement = query)

query = "CREATE INDEX idx_time ON base_model time;"
dbExecute(conn = connection, statement = query)

# Run multiple simulations
for (run in 1:5000) {
  # Transform the time series to long format
  social_mobility_model() %>% 
    mutate(
      # Add run_id
      run_id = run
    ) %>% 
    # Order table structure
    select(run_id, agent_id = id, time, wealth) %>% 
  # Store results in our database
  dbWriteTable(con = connection, name = "social_mobility_model", value = ., 
               append = TRUE, row.names=FALSE, overwrite=FALSE)
  
}


# List availabe tables
dbListTables(connection)

# Create query
dplyr_query = tbl(connection, 'base_model') %>% 
  # Keep last obs
  filter(time == 1000) %>% 
  # Group by run id
  group_by(run_id) %>% 
  # Summarise
  summarise(
    average = mean(wealth),
    std = sd(wealth),
    max = max(wealth),
    min = min(wealth)
  )

# Show the query
show_query(dplyr_query)

# See result
result = collect(dplyr_query)

# Disconnect
dbDisconnect(conn = connection)
