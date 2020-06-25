# author: Maxwell Kleinhans (maxwell.kleinhans@gmail.com)

# read in libraries
library(shiny)
library(leaflet) # for mapping
library(RSQLite) # SQLite support
library(DBI) # database functions

# change default to read strings in from tables as characters instead of factors
options(stringsAsFactors = FALSE)

# record current date and time in POSIX format
current_time <- as.POSIXlt(Sys.time())

# get just the date component of the date and time
current_date <- as.Date(current_time) 

# get just the year component of the date
current_year <- as.integer(gsub("-.*", "", current_date))

# coordinates of geographical centroid of Georgia in WGS 84
# used to set default view of map
GA_lat <- 32.646111
GA_lon <- -83.431667

# read in HUC8 shapes as SpatialPolygonsDataFrame from previously saved rds file
huc8s <- readRDS("/Users/kylenconnelly/Documents/Kyle_(E)/UGA/Projects/RBC_Biodiv_Database/database/huc8s.rds")

# directory path to .sqlite file database
db_path <- "/Users/kylenconnelly/Documents/Kyle_(E)/UGA/Projects/RBC_Biodiv_Database/database/Acanthonus.sqlite"

# open connection to database
con <- dbConnect(RSQLite::SQLite(), db_path)

# get unique sampling dates from collections table
ymd <- dbGetQuery(con, "SELECT DISTINCT year, month, day FROM collections;")

# get unique sample target taxon from collections table
tgt <- dbGetQuery(con, "SELECT DISTINCT Target_Taxon FROM collections;")[[1]]

# get all unique species names from species table
spp <- dbGetQuery(con, "SELECT Scientific_Name FROM species;")[[1]]

dbDisconnect(con) # disconnect from database

# get rid of dates that are missing a component (year, month, or day)
ymd <- na.omit(ymd)

# get rid of dates before 1700, pretty sure any of these are errors
ymd <- ymd[ymd[,"Year"] > 1700,]

# unique collection dates as unix time (seconds since Jan 1, 1970)
date_ints <- as.double(as.POSIXlt(paste(ymd[,"Year"], ymd[,"Month"], ymd[,"Day"], sep = "-"), format = "%Y-%m-%d"))

# earliest date of a collection in the database
min_date <- as.POSIXlt(min(na.omit(date_ints)), origin = "1970-01-01")

#' get_query_data
#'
#' get records for collections of the species included in the 'species' parameter 
#' between the 'start_year' and 'end_year' 
#'
#' @param start_year four-digit integer - get records from this year and later
#' @param end_year four-digit integer - get records from this year and earlier
#' @param species string vector - get records for collections of these species
#'
#' @return dataframe of records from occurrence database
#'
#' 
get_query_data <- function(db_path, start_year, end_year, species, taxon){

  # if species are not specified in the user input
  if(length(species) < 1){
    
    # construct SQL query as string to return records within specified year range
    get_record <- paste0("SELECT * FROM collections INNER JOIN subcollections 
                         ON subcollections.COLLECT_ID = collections.COLLECT_ID 
                         WHERE Year BETWEEN '", start_year, "' AND '", end_year, "';")
  # if species are specified in user input
  }else{
    # construct SQL query as string to return records of collections of species selected within specified year range
    get_record <- paste0("SELECT * FROM collections INNER JOIN subcollections 
                         ON subcollections.COLLECT_ID = collections.COLLECT_ID 
                         WHERE Scientific_Name IN ('", paste(species, collapse = "','"), "') 
                         AND Year BETWEEN '", start_year, "' AND '", end_year, "';")
  }
  
  # create connection to database
  con <- dbConnect(RSQLite::SQLite(), db_path)
  
  # execute SQL query and return resulting records
  result <- dbGetQuery(con, get_record)

  dbDisconnect(con) # disconnect from database
  
  return(result) # return resulting records from function
}

# Define UI ----
ui <- fluidPage(
  title = "Acanthonus Database",
  
  # set up the multiple tab interface
  tabsetPanel(id = "tabs",
             
              # table view tab
              tabPanel("Data Viewer", id = "table",
                       
                       # set up data query interface
                       fluidRow(
                         column(12, # set to twelve to allow the input selector use the entire width of the page
                                sliderInput(inputId = "year", 
                                            label = "Years", 
                                            min = min_date, 
                                            max = current_time, 
                                            value = c(min_date,current_time), 
                                            width = "100%",
                                            timeFormat = "%Y" # only show years, not months or days
                                            ) 
                         )
                       ),
                       fluidRow(
                         column(12,
                                selectInput(inputId = "taxon", 
                                                   label = "Target Taxon", 
                                                   choices = tgt, 
                                                   width = "100%",
                                                   multiple = TRUE
                                                   )
                         )
                       ),
                       fluidRow(
                         column(12,
                                selectInput(inputId = "species", 
                                            label = "Species", 
                                            choices = spp, 
                                            width = "100%", 
                                            multiple = TRUE
                                            )
                         )
                       ),
                       fluidRow(
                         column(6,
                                # this button triggers querying the database
                                # for retrieval of records
                                actionButton(inputId = "query", 
                                             label = "View Data")
                         ),
                         column(6,
                                # this button allows the user to download 
                                # the records from the database query as a text file
                                downloadButton(outputId = "dl", 
                                               label = "Download Data")
                         ),
                         br()
                       ),
                       hr(),
                       fluidRow(
                         column(12,
                                # display the records from the database query
                                DT::dataTableOutput("table_out")
                         )
                       )
              ),
              # mapping tab
              tabPanel("Map Viewer", id = "map",

                       # sidebar allows the user to adjust inputs, main panel shows map display based on user inputs
                       sidebarLayout(
                         
                         sidebarPanel(
                           sliderInput(inputId = "map_year", 
                                       label = "Years", 
                                       min = min_date, 
                                       max = current_time, 
                                       value = c(min_date,current_time), 
                                       width = "100%",
                                       timeFormat = "%Y"
                           ),
                           selectInput(inputId = "map_species",
                                       label = "Species",
                                       choices = spp, 
                                       multiple = TRUE
                           ),
                           # button triggers execution of query on database
                           actionButton(inputId = "map_query", 
                                        label = "View Data"
                           ),
                           # button allows the user to download a text version of the
                           # records resulting from the database query
                           downloadButton(outputId = "map_dl", label = "Download Data"
                                          ),
                           # map shows hucs when this box is selected
                           checkboxInput(inputId = "show_hucs",
                                         label = "Show HUC8s"
                           ),
                           # allows user to change base map of map display
                           radioButtons(inputId = "base_map",
                                        label = "Base Map",
                                        choices = list("Streets" = 1, 
                                                       "Imagery" = 2,
                                                       "Topography" = 3
                                                       ),
                                        selected = 1
                           ),
                           hr(),
                           h4("Summary"), 
                           
                           # print summary statistics for selected data
                           uiOutput("spatial_summary")
                         ),
                         mainPanel(
                           # show map
                           leafletOutput("map_plot", height = "700px")
                         )
                       )

              )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  # update current time when user interface refreshes
  current_time <- as.POSIXlt(Sys.time())
  
  # current time as number of seconds since 1970-01-01
  current_unix_time <- as.double(current_time)
  
  # set up map output
  output$map_plot <- renderLeaflet({

    map <- leaflet()

    # input == 1 is street map, 2 is satellite imagery, 3 is topography
    if(input$base_map == 2){
      
      # add satellite imagery base layer to map
      map <- addProviderTiles(map, 
                                  providers$Esri.WorldImagery, 
                                  group = "Imagery")
    }else if(input$base_map == 3){
      
      # add topography base layer to map
      map <- addProviderTiles(map,
                              providers$OpenTopoMap, 
                              group = "Topography")
    }else{
      
      # add default openstreet map base layer to map
      map <- addTiles(map,
                      group = "Streets")
    }
    
    # set the default view to the state of Georgia
    map <-  setView(map,
                    GA_lon,
                    GA_lat,
                    zoom = 7
    )
    
    # if the checkbox is selected, add trasparent huc 8 boundaries to map
    if(input$show_hucs){
      map <- addPolygons(map,
                         data = huc8s,
                         opacity = .18, 
                         fillOpacity = .1, 
                         color = "darkblue", 
                         weight = 3.5,
                         label = huc8s@data$HUC8,
                         group = "hucs"
      )
    }
    
    # if the database has been queried for records to plot
    if(nrow(map_data_out()) > 0){
      
      # set up popups to show additional information about the corresponding record 
      # when the user clicks on a point on the map
      
      # initialize empty string vector with an entry for each record in query result
      popups <- rep("", nrow(map_data_out())) 
      
      # populate each entry of the vector of popups with HTML formatted info on records
      for(i in 1:length(popups)){
        
        # dl, dt, and dd tags are used to make a formatted list with terms and descriptions
        popups[i] <- paste0("<dl><dt>Species: </dt><dd>",
                            map_data_out()[i,"Scientific_Name"],
                            " </dd><dt>Date: </dt><dd>",
                            paste(map_data_out()[i,"Year"],map_data_out()[i,"Month"],map_data_out()[i,"Day"], sep = "-"),
                            " </dd><dt>Collectors: </dt><dd>",
                            map_data_out()[i,"Collectors"],
                            " </dd><dt></dd></dl>"
        )
      }
      
      # add markers to map representing locations of collection records in query
      map <- addCircleMarkers(map,
                              lat = map_data_out()[,"Og_Lat"],
                              lng = map_data_out()[,"Og_Lon"],
                              label = map_data_out()[,"Scientific_Name"],
                              popup = popups,
                              radius = 2
      )
      
    }
    
    # show map
    map
  })
  
  # construct query result for map tab 
  map_data_out <- eventReactive(input$map_query, {
    
    # convert POSIX dates to integer years
    year1 <- as.integer(format(input$map_year[1], "%Y"))
    year2 <- as.integer(format(input$map_year[2], "%Y"))
    
    # pass user inputs to query function
    dtout <- get_query_data(db_path, year1, year2, input$map_species)
    
    dtout
  })
  
  # set up download of map data as a comma-separated values text file
  output$map_dl <- downloadHandler(
    filename = function() {
      paste0("acanthonus_data_download-", gsub(" ", "_", Sys.Date()), ".csv")
    },
    content = function(file) {
      write.csv(map_data_out(), file, row.names = FALSE)
    }
  )
  
  # assemble metadata describing records selected by query in a format for printing to the UI
  output$spatial_summary <- renderUI({

    HTML(paste0(
      "<div style='font-weight:bold;padding-top:20px;'>",
      "<span>Number of Records: <span style='text-decoration: underline;'>&nbsp;&nbsp;&nbsp;",
      nrow(map_data_out()),
      "&nbsp;&nbsp;&nbsp;</span></span></div>")
    )
  })
  
  # construct query result for data table tab 
  data_out <- eventReactive(input$query, {
    
    # convert POSIX dates to integer years
    year1 <- as.integer(format(input$year[1], "%Y"))
    year2 <- as.integer(format(input$year[2], "%Y"))
    
    # pass user inputs to query function
    dtout <- get_query_data(db_path, year1, year2, input$species)
    
    dtout
  })
  
  # set up data table display for data table viewer tab
  output$table_out <- DT::renderDataTable({
    DT::datatable(data_out(), 
                  options = list(pageLength = 50)
    )
  })
  
  # set up download of table data as a comma-separated values text file
  output$dl <- downloadHandler(
    filename = function() {
      paste0("acanthonus_data_download-", gsub(" ", "_", Sys.Date()), ".csv")
    },
    content = function(file) {
      write.csv(data_out(), file, row.names = FALSE)
    }
  )
}

# Run the app ----
shinyApp(ui = ui, server = server)
