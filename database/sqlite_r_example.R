#
library(RSQLite)
library(DBI) 

# connect to the database 
con <- dbConnect(RSQLite::SQLite(), "./Acanthonus.sqlite")

# -------- Interact with the data using database functions and SQL -------------

# list all tables in the database
dbListTables(con) 

# list the fields in a table
dbListFields(con, "collections") 

# read an entire table into R
collections_table <- dbReadTable(con, "collections") 
nrow(collections_table)

# read a subset of rows of a table into R
result <- dbGetQuery(con, "SELECT * FROM collections WHERE Year = 2012;") 
head(result)

# ------------------ Interact with the data using dplyr functions --------------
library(dbplyr)

# create a connection to a table 
collections_table <- tbl(con, "collections")

# access a subset of the columns of a table
collections_table %>% select(Year:Day, Collectors, Method)

# access a subset of the rows of a table
collections_table %>% filter(Year > 2015)

# calculate summary statistics
collections_table %>% 
  group_by(Year) %>%
  summarise(effort = n())

# view the SQL query translation of the dplyer code
fish_targeted_db <- collections_table %>% filter(Fish == 1)
fish_targeted_db %>% show_query()

# pull data from database into R
fish_targeted_local %>% collect()

dbDisconnect(con) # disconnect from database
