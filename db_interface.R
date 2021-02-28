# author: Maxwell Kleinhans (maxwell.kleinhans@gmail.com)

# read in libraries
library(shiny)
library(shinyjs) # more dynamic user interface options
library(xlsx) # read in excel files
library(leaflet) # for mapping
library(RSQLite) # SQLite support
library(DBI) # database functions

source("./interface_functions.R")

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
huc8s <- readRDS("./data/huc8s.rds")

# directory path to .sqlite file database
db_path <- "./data/Acanthonus.sqlite"

# open connection to database
con <- dbConnect(RSQLite::SQLite(), db_path)

# get unique sampling dates from collections table
ymd <- dbGetQuery(con, "SELECT DISTINCT year, month, day FROM collections;")

# get all unique species names from species table
spp <- dbGetQuery(con, "SELECT Scientific_Name FROM taxonomy_lookup;")[[1]]

# get unique sites and coordinates already in the database
site_lookup <- dbGetQuery(con, "SELECT DISTINCT SITE_ID, Og_site, Og_Lat, Og_Lon FROM collections")

dbDisconnect(con) # disconnect from database

# eliminate sets of coordinates not associated with site names
site_lookup <- site_lookup[!is.na(site_lookup[,"Og_Site"]),]
site_lookup <- site_lookup[site_lookup[,"Og_Site"] != "",]
site_lookup <- unique(site_lookup) # eliminate duplicates

# get rid of dates that are missing a component (year, month, or day)
ymd <- na.omit(ymd)

# get rid of dates before 1700, pretty sure any of these are errors
ymd <- ymd[ymd[,"Year"] > 1700,]

# unique collection dates as unix time (seconds since Jan 1, 1970)
date_ints <- as.double(as.POSIXlt(paste(ymd[,"Year"], ymd[,"Month"], ymd[,"Day"], sep = "-"), format = "%Y-%m-%d"))

# earliest date of a collection in the database
min_date <- as.POSIXlt(min(na.omit(date_ints)), origin = "1970-01-01")

# Define UI ----
ui <- fluidPage(
  
  useShinyjs(), # this line is necessary if any functions from the shinyjs package are used in the app
  
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
                       
              ),
              # tab for upload of kickset-level data
              
              tabPanel("Kickset Entry", id = "kicksets",
                       fluidRow(column(12,HTML("<span>&nbsp</span>"))), # spacer
                       fluidRow(
                         column(12,
                                fileInput("kickset_file", 
                                          "Choose kickset data excel file",
                                )
                         ),
                       ),
                       fluidRow(
                         column(4,
                                actionButton(inputId = "kickset_submit", label = "Submit")
                         ),
                         column(4,
                                actionButton(inputId = "kickset_import", label = "Import to Database")
                         )
                       ),
                       hr(),
                       fluidRow(
                         column(12,
                                # this div is hidden by default and becomes visible
                                # after records have been successfully formatted for import to the database
                                hidden(
                                  div(
                                    id = "kickset_working",
                                    h4("Working...")
                                  )
                                )
                         )
                       ),
                       hr(),
                       fluidRow(
                         column(12,
                                # this div is hidden by default and becomes visible
                                # after records have been successfully formatted for import to the database
                                hidden(
                                  div(
                                    id = "kickset_processed",
                                    h4("Kickset data QAQC and formatting successful")
                                  )
                                )
                         )
                       ),
                       fluidRow(
                         column(12,
                                # this div is hidden by default and becomes visible
                                # if errors or unaccepted values are detected in the entered data
                                hidden(
                                  div(
                                    id = "kickset_processing_error",
                                    h4("There are problems with the entered data that prevented them from being imported into the database.",
                                       HTML("<div style='text-decoration:underline'>Resolve the errors listed below before attempting to resubmit.</div>")
                                    )
                                  )
                                )
                         )
                       ),
                       hr(),
                       fluidRow(
                         column(12,
                                # display errors encountered in QAQC and formatting of input data
                                htmlOutput("kickset_processing_errs")
                         )
                       ),
                       hr(),
                       fluidRow(
                         column(12,
                                # this div is hidden by default and becomes visible
                                # after records have been successfully added to the database
                                hidden(
                                  div(
                                    id = "kickset_imported",
                                    h4("Kickset data imported to the database successfully")
                                  )
                                )
                         )
                       ),
                       fluidRow(
                         column(12,
                                # display errors encountered in QAQC and formatting of input data
                                htmlOutput("kickset_import_errs")
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
  
  # hide all success and error messages to begin with
  disable("kickset_import")
  shinyjs::hide("kickset_working")
  shinyjs::hide("kickset_processed")
  shinyjs::hide("kickset_processing_error")
  shinyjs::hide("kickset_imported")
  
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
    
    # pass user inputs to query function - defined at top of file
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
    
    # pass user inputs to query function - defined at the top of this file
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
  
  # this line prevents a person from having to write 'input$kickset_file$datapath'
  # over and over again, might be unneccesary
  kickset_data_path <- reactive({input$kickset_file$datapath})
  
  # hide all success and error messages when a new file is selected for processing
  observeEvent(input$kickset_file$datapath, {
    disable("kickset_import")
    shinyjs::hide("kickset_working")
    shinyjs::hide("kickset_processed")
    shinyjs::hide("kickset_processing_error")
    shinyjs::hide("kickset_imported")
  })
  
  # when the user
  err_out <- eventReactive(input$kickset_submit, {
    
    shinyjs::show("kickset_working")
    
    errs <- ""
    
    if(is.null(input$kickset_file)){
      # no action needed here, a file hasn't been selected
    }else{
      
      if(grepl("[.]xlsx$", kickset_data_path())[[1]]){
        
        # check necessary tables exist
        # if 'read.xlsx' cannot find the table, it will throw an error
        fish <- tryCatch({
          read.xlsx(kickset_data_path(), 2)
        }, error = function(e) {
          NULL
        })
        
        if(is.null(fish)){
          errs <- paste0(errs, "<br/>FISH SPREADSHEET NOT FOUND!")
        }
        
        habitat <- tryCatch({
          read.xlsx(kickset_data_path(), 3)
        }, error = function(e) {
          NULL
        })
        
        if(is.null(habitat)){
          errs <- paste0(errs, "<br/>HABITAT SPREADSHEET NOT FOUND!")
        }
        
        # if either table cannot be read, display error messages, and don't attempt to QAQC data
        if(is.null(fish) | is.null(habitat)){
          
          # input file doesn't include the necessary sheets
          shinyjs::hide("kickset_working")
          shinyjs::show("kickset_processing_error")
          shinyjs::hide("kickset_processed")
          disable("import")
          
        }else{
          
          # get rid of rows of all NA's that the above xlsx functions read in for some reason
          # these lines work by creating a matrix of booleans (is.na(df)),
          # summing this matrix row-wise to create a vector of integers (apply(,MARGIN = 1, FUN = sum)
          # and then comparing these integers to the number of columns in the data frame.
          # a row where every value is NA will be all TRUE which will sum to the number of columns in the data frame
          # if there are fewer NA's than columns, the sum will be less than the number of columns, and the row will be kept
          fish <- fish[apply(is.na(fish), MARGIN = 1, FUN = sum) < ncol(fish),]
          habitat <- habitat[apply(is.na(habitat), MARGIN = 1, FUN = sum) < ncol(habitat),]
          
          # run the QAQC function from interface_functions.R
          errs <- QAQC_kicksets(habitat, fish, site_lookup, spp)
          
          # if the qaqc function returns errors, show errors and disable import
          if(nchar(errs) > 0){
            
            shinyjs::hide("kickset_working")
            shinyjs::show("kickset_processing_error")
            shinyjs::hide("kickset_processed")
            disable("kickset_import")
            
            # if qaqc function returns no errors, enable import
          }else{
            
            shinyjs::hide("kickset_working")
            shinyjs::show("kickset_processed")
            shinyjs::hide("kickset_processing_error")
            enable("kickset_import")
          }
        }
        
      }else{ # submitted file doesn't have the proper file type
        
        shinyjs::hide("kickset_working")
        shinyjs::show("kickset_processing_error")
        shinyjs::hide("kickset_processed")
        disable("kickset_import")
      }
    }
    # remove initial line break from error string to save space in interface
    errs <- gsub("^<br/>", "", errs)
    
    errs
  })
  
  # format QAQC errors for display in interface
  output$kickset_processing_errs <- renderUI({
    HTML(paste0("<div style='color:red;font-size:large;'>",err_out(),"</div>"))
  })
  
  kickset_habitat <- reactive({
    
    # read in habitat data if no errors are detected in the QAQC step
    if(nchar(err_out()) == 0){
      habitat <- read.xlsx(kickset_data_path(), 3)
      habitat <- habitat[apply(is.na(habitat), MARGIN = 1, FUN = sum) < ncol(habitat),]
      habitat
    }else{
      NULL
    }
  })
  
  kickset_fish <- reactive({
    
    # read in fish capture data if no errors are detected in the QAQC step
    if(nchar(err_out()) == 0){
      fish <- read.xlsx(kickset_data_path(), 2)
      fish <- fish[apply(is.na(fish), MARGIN = 1, FUN = sum) < ncol(fish),]
      fish
    }else{
      NULL
    }
  })
  
  # create to associate kickset records with collection ids
  kickset_collect_id_lookup <- reactive({
    
    con <- dbConnect(RSQLite::SQLite(), db_path)
    
    # get the highest collection id assigned to a record in the database
    max_collect_id <- dbGetQuery(con, "SELECT MAX(COLLECT_ID) FROM collections")[[1]]
    
    dbDisconnect(con) # disconnect from database
    
    # create a lookup table for associating collection id's with kickset records
    # based on field number, date, and site
    lookup <- unique(kickset_habitat()[,c("Field..","Date","Site.no.")])
    
    # placeholder for current new collection id to be added to the database
    curr_collect_id <- max_collect_id
    
    for(i in 1:nrow(lookup)){
      
      collect_id <- tryCatch({match_collect_id(lookup[i,"Field.."], 
                                               lookup[i,"Date"], 
                                               lookup[i,"Site.no."], 
                                               db_path)}, 
                             error = function(e) {
                               NULL
                             })
      
      # if the field number, date, and site cannot be associated with an existing collection record,
      # assign a new collection id
      if(is.null(collect_id)){
        
        # increment collection id
        curr_collect_id <- curr_collect_id + 1
        collect_id <- curr_collect_id
      }
      
      lookup[i,"COLLECT_ID"] <- collect_id
    }
    lookup
  })
  
  kickset_habitat_out <- eventReactive(input$kickset_import, {
    
    shinyjs::show("kickset_working")
    
    habitat <- kickset_habitat()
    
    habitat <- merge(habitat, kickset_collect_id_lookup())
    
    # set up data frame for habitat data matching formatting of corresponding database table
    hab_out <- data.frame(
      COLLECT_ID = habitat[,"COLLECT_ID"],
      Set_Number = habitat[,"Kickset"],
      COLLECTxSET_ID = as.double(habitat[,"COLLECT_ID"]) + as.double(habitat[,"Kickset"] * .001),
      Set_Type = rep("kickset", nrow(habitat)),
      Set_Length_m = habitat[,"Haul.length..m."],
      Set_AvgWidth_m = rep("", nrow(habitat)),
      Set_AvgDepth_m = habitat[,"Depth"],
      Set_AvgVelocity_ms = habitat[,"Velocity"],
      No_Fish = habitat[,"No.fish"],
      Dominant_Substrate = rep("", nrow(habitat)),
      Sediment_Code = habitat[,"Sediment.Code"],
      Moveable_Gravel = habitat[,"Moveable"] == "M",
      Embedded_Sediment = rep("", nrow(habitat)), # can't find this in the input data
      Justicia_Present = habitat[,"J"] == "J",
      Woody_Present = habitat[,"WD"] == "WD",
      Podostemum_Present = habitat[,"P"] == "P",
      Depth_Code = habitat[,"Depth.Code"],
      Velocity_Category = habitat[,"Velocity.category"],
      Habitat_Other = rep("", nrow(habitat)),
      Subcollections_Habitat_Notes = habitat[,"Notes"],
      Subcollections_Habitat_QAQC = rep(0, nrow(habitat))
    )
    
    con <- dbConnect(RSQLite::SQLite(), db_path)
    
    # number of records already in the database with id's overlapping with the new records
    habmatches <- dbGetQuery(con, 
                             paste0("SELECT COUNT(*) FROM subcollections_habitat WHERE COLLECTxSET_ID IN (",
                                    paste(unique(hab_out[,"COLLECTxSET_ID"]), 
                                          collapse = ","),
                                    ");"))
    
    dbDisconnect(con) # disconnect from database
    
    # if records have already been imported to the database, do not duplicate them
    if(habmatches > 0){
      NULL
    }else{
      
      hab_out
    }
  })
  
  kickset_fish_out <- eventReactive(input$kickset_import, {
    
    fish <- kickset_fish()
    
    # set NA's to '0' so they can be used in arithmetic operations
    fish[is.na(fish[,"Ad.count"]), "Ad.count"] <- 0
    fish[is.na(fish[,"YOY"]), "YOY"] <- 0
    
    # do some basic formatting steps for species names - see interface_functions.R
    fish[,"genus_species"] <- format_species(fish[,"genus_species"])
    
    # convert columns that should store integers to integers
    # they might have been read in as characters
    integer_columns_fish <- c("Kickset", "Ad.count", "YOY", "Formalin.total", "Released.total", "ETOH.clip", "ETOH.whole", "Total", "SL.mm.")
    for(col_name in integer_columns_fish){
      fish[,col_name] <- as.integer(fish[,col_name])
    }
    
    lookup <- kickset_collect_id_lookup()
    
    # change column names to be consistent with fish spreadsheet
    colnames(lookup)[c(1,3)] <- c("Field.No.","Site.No.")
    fish <- merge(fish, lookup)
    
    # set up data frame for fish data matching formatting of corresponding database table
    fish_out <- data.frame(
      COLLECTxSET_ID = fish[,"COLLECT_ID"] + (fish[,"Kickset"] * .001),
      Scientific_Name = fish[,"genus_species"],
      Standard_Length_mm = fish[,"SL.mm."],
      Total_Count = fish[,"Total"],
      Adult_Count = fish[,"Ad.count"],
      YOY_Count = fish[,"YOY"],
      Preserved_Formalin = fish[,"Formalin.total"],
      Preserved_Ethanol = fish[,"ETOH.whole"],
      Genetic_Clip = fish[,"ETOH.clip"],
      Total_Released = fish[,"Released.total"],
      Sex = fish[,"Sex"],
      Subcollections_Organism_Notes = fish[,"Notes"],
      Subcollections_Organism_QAQC = rep(0,nrow(fish))
    )
    
    con <- dbConnect(RSQLite::SQLite(), db_path)
    
    # number of records already in the database with id's overlapping with the new records
    fishmatches <- dbGetQuery(con, 
                              paste0("SELECT COUNT(*) FROM subcollections_organisms WHERE COLLECTxSET_ID IN (",
                                     paste(unique(fish_out[,"COLLECTxSET_ID"]), 
                                           collapse = ","),
                                     ");"))
    
    dbDisconnect(con) # disconnect from database
    
    # if records have already been imported to the database, do not duplicate them
    
    if(fishmatches > 0){
      NULL
      
    }else{
      
      fish_out
    }
    
  })
  
  collection_out <- eventReactive(input$kickset_import, {
    
    lookup <- kickset_collect_id_lookup()
    
    # if the habitat and fish data are missing, no need to make collection records
    if(is.null(kickset_habitat_out()) | is.null(kickset_fish_out())){
      NULL
      
    }else{
      
      # check if collection records in database
      con <- dbConnect(RSQLite::SQLite(), db_path)
      
      # number of collection records in the database sharing id with new records to be imported
      collectionmatches <- dbGetQuery(con,
                                      paste0("SELECT COLLECT_ID FROM collections WHERE COLLECT_ID IN (",
                                             paste(unique(lookup[,"COLLECT_ID"]),
                                                   collapse = ","),
                                             ");"))
      
      dbDisconnect(con) # disconnect from database
      
      # if the collection id's associated with the new subcollection records to be imported
      # are not associated with existing collection records, new collection records must be added
      # if(collectionmatches[[1]] < 1){
      
      lookup <- lookup[!lookup[,"COLLECT_ID"] %in% collectionmatches[[1]],]
      
      # change column names to match the collection id lookup table
      collections_site_lookup <- site_lookup
      colnames(collections_site_lookup)[2] <- "Site.no."
      lookup <- merge(lookup, collections_site_lookup)
      
      # set up data frame for fish data matching formatting of corresponding database table
      collections_out <- data.frame(
        COLLECT_ID = lookup[,"COLLECT_ID"],
        SITE_ID = lookup[,"SITE_ID"],
        Year = format(as.Date(lookup[,"Date"]), "%Y"),
        Month = format(as.Date(lookup[,"Date"]), "%m"),
        Day =  format(as.Date(lookup[,"Date"]), "%d"),
        Collectors = rep("", nrow(lookup)),
        SOURCE_ID = rep("", nrow(lookup)),
        Method = rep("Kicksets and Hauls", nrow(lookup)),
        Start_Time = rep("", nrow(lookup)),
        End_Time = rep("", nrow(lookup)),
        Target_Taxon = rep("Fish", nrow(lookup)),
        Field_Numbers = lookup[,"Field.."],
        Og_Lat = rep("", nrow(lookup)),
        Og_Lon = rep("", nrow(lookup)),
        Og_Site = lookup[,"Site.no."],
        Locality = rep("", nrow(lookup)),
        Reliability_Notes = rep("", nrow(lookup)),
        Collection_Notes = rep("", nrow(lookup)),
        Fish = rep(1, nrow(lookup)),
        Invertebrate = rep(0, nrow(lookup)),
        Plant = rep(0, nrow(lookup)),
        Reptile =  rep(0, nrow(lookup)),
        Amphibian = rep(0, nrow(lookup)),
        Collection_QAQC = rep(0, nrow(lookup))
      )
      
      collections_out
      
    }
  })
  
  # aggregate habitat and kickset data for subcollections table
  # subcollection_out <- reactive({
  subcollection_out <- eventReactive(input$kickset_import, {
    
    if(is.null(kickset_habitat_out()) | is.null(kickset_fish_out())){
      
      NULL
      
    }else{
      
      hab_and_sets <- merge(kickset_habitat_out(), kickset_fish_out())
      
      # subcollection records are separated by life stage and aggregated across kicksets at a given site
      # construct data frame of just YOY counts per species and site visit
      yoy_out <- aggregate(list(Count = hab_and_sets[,"YOY_Count"]), 
                           by = list(COLLECT_ID = hab_and_sets[,"COLLECT_ID"],
                                     Scientific_Name = hab_and_sets[,"Scientific_Name"]),
                           FUN = sum)
      
      yoy_out[,"Life_Stage"] <- rep("YOY", nrow(yoy_out))
      
      # don't need records for which no YOY fish were captured
      yoy_out <- yoy_out[yoy_out[,"Count"] > 0,]
      
      # construct data frame of just counts of adult fish per species and site visit
      adult_out <- aggregate(list(Count = hab_and_sets[,"Adult_Count"]), 
                             by = list(COLLECT_ID = hab_and_sets[,"COLLECT_ID"],
                                       Scientific_Name = hab_and_sets[,"Scientific_Name"]),
                             FUN = sum)
      
      adult_out[,"Life_Stage"] <- rep("adult", nrow(adult_out))
      
      # don't need records for which no adult fish were captured
      adult_out <- adult_out[adult_out[,"Count"] > 0,]
      
      # recombine the YOY and adult fish records
      subcollection_out <- rbind(adult_out, yoy_out)
      subcollection_out <- subcollection_out[order(subcollection_out[,"COLLECT_ID"], 
                                                   subcollection_out[,"Scientific_Name"]),]
      
      # aggregate notes by site visit
      subcollection_notes <- aggregate(list(Subcollection_Notes = hab_and_sets[,"Subcollections_Organism_Notes"]), 
                                       by = list(COLLECT_ID = hab_and_sets[,"COLLECT_ID"],
                                                 Scientific_Name = hab_and_sets[,"Scientific_Name"]),
                                       FUN = paste_, sep = ", ")
      
      subcollection_out <- merge(subcollection_out, subcollection_notes)
      
      # this column isn't really populated yet in the database
      subcollection_out[,"Native"] <- rep("", nrow(subcollection_out))
      subcollection_out[,"Subcollection_QAQC"] <- rep(0, nrow(subcollection_out))
      
      con <- dbConnect(RSQLite::SQLite(), db_path)
      
      # number of subcollection records in the database sharing a collection ID with the new records
      subcollection_matches <- dbGetQuery(con, 
                                          paste0("SELECT COUNT(*) FROM subcollections WHERE COLLECT_ID IN (",
                                                 paste(unique(subcollection_out[,"COLLECT_ID"]), 
                                                       collapse = ","),
                                                 ");"))
      
      dbDisconnect(con) # disconnect from database
      
      if(subcollection_matches > 0){
        
        # if subcollection records overlap with the ones to be imported, 
        # do not import them to avoid duplicates
        NULL
        
      }else{
        subcollection_out
      }
      
    }
  })
  
  # import records to the database and store any errors if there are problems
  output$kickset_import_errs <- renderUI({
    
    # the only conditions under which the subcollections data frame would be NULL
    # is if importing records to the database would lead to duplicate records
    if(is.null(subcollection_out())){
      import_errs <- "Records already appear in database. No database import performed to avoid duplicate records."
      shinyjs::hide("kickset_working")
    }else{
      
      con <- dbConnect(RSQLite::SQLite(), db_path)
      
      # write all tables to database
      dbWriteTable(con, "subcollections_habitat", kickset_habitat_out(), append = T)
      dbWriteTable(con, "subcollections_organisms", kickset_fish_out(), append = T)
      dbWriteTable(con, "subcollections", subcollection_out(), append = T)
      
      # only import collection records if needed
      if(nrow(collection_out()) > 0){
        dbWriteTable(con, "collections", collection_out(), append = T)
      }
      
      dbDisconnect(con) # disconnect from database
      
      # show success message
      shinyjs::show("kickset_imported")
      shinyjs::hide("kickset_working")
      # no import errors
      import_errs <- ""
    }
    
    # format import errors for display in the interface
    HTML(paste0("<div style='color:red;font-size:large;'>",import_errs,"</div>"))
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
