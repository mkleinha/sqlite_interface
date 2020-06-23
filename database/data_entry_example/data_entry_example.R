#Water quality entry form example
#will use this to test the entry form
#branch
# author: Maxwell Kleinhans (maxwell.kleinhans@gmail.com)

# this line shouldn't be necessary if you're using r 4.0
options(stringsAsFactors = FALSE)

# read in packages
library(shiny)
library(shinyjs)
library(RSQLite)
library(DBI)

current_time <- as.POSIXlt(Sys.time()) # get current local time and date 
current_date <- as.Date(current_time) # date component of current time and date
first_date <- "2010-01-01" # earliest date for data entry 

# path to .sqlite database file
db_path <- "./wq_test.sqlite"

default_style <- "color:black" # CSS for good input values
error_style <- "color:red" # CSS for bad input values

# maximum and minimum values defining the ranges of inputs allowed by QAQC rules
max_temp <- 99 
min_temp <- 0
max_ph <- 14
min_ph <- 0
max_do <- 30
min_do <- 0
max_spc <- 10000
min_spc <- 0
max_turb <- 10000
min_turb <- 0

# list of streams for input options
streams <- c("Holly", "Etowah", "Conasauga")

#' update_hab
#'
#' @param stream character string stream name
#' @param location character string describing sampling location
#' @param date character string date in the form yyyy-mm-dd
#' @param site_num integer site number
#' @param observers character string listing collectors of the data
#' @param temp numeric temperature in degrees celcius
#' @param pH integer pH
#' @param DO numeric dissolved oxygen concentration in mg / L
#' @param spc numeric specific conductivity
#' @param turb numeric turbidity in NTU
#' @param db_path character string path to .sqlite database file
#'
#' @return error messages pasted into a single string separated by line breaks for printing
#'
#' @examples
#' 
update_hab <- function(stream, location, date, site_num, observers, temp, pH, DO, spc, turb, db_path){

  # replace one single quote in location descripton with two for SQL query formatting reasons
  location <- gsub("'", "''", location)
  date <- gsub(" *UTC$", "", date) # remove time from date

  # empty string to which error messages will be pasted
  msg <- ""
  
  # the measurements are useless if they aren't associated with a stream
  if(is.na(stream)){
    msg <- paste0(msg, "No stream selected.<br/>")
  }
  
  # if temperature measurement is missing, don't throw an error, 
  # but also skip bounds check to avoid 'missing value where TRUE/FALSE needed' error
  if(is.na(temp)){
    # no action needed here, 'if' statement just prevents conditions below from
    # throwing 'missing value where TRUE/FALSE needed' error if temp is NA
  }else if(temp > max_temp | temp < min_temp){ # check if temp is within a reasonable range of values
    # if temp is outside specified acceptable range, add error message to output
    msg <- paste0(msg, "Entered temperature outside reasonable range (",min_temp,"-",max_temp,").<br/>")
  }
  
  if(is.na(pH)){
    
  }else if(pH > max_ph | pH < min_ph){
    msg <- paste0(msg, "Entered pH value outside reasonable range (",min_ph,"-",max_ph,").<br/>")
  }
  
  if(is.na(DO)){
    
  }else if(DO > max_do | DO < min_do){
    msg <- paste0(msg, "Entered dissolved oxygen value outside reasonable range (",min_do,"-",max_do,").<br/>")
  }
  
  if(is.na(spc)){
    
  }else if(spc > max_spc | spc < min_spc){
    msg <- paste0(msg, "Entered conductivity value outside reasonable range (",min_spc,"-",max_spc,").<br/>")
  }
  
  if(is.na(turb)){
    
  }else if(turb > max_turb | turb < min_turb){
    msg <- paste0(msg, "Entered turbidity value outside reasonable range (",min_turb,"-",max_turb,").<br/>")
  }
  
  # if there are no error messages (the length of the messages string is 0), 
  # add data to database as new record
  if(nchar(msg) == 0){
    
    # set up SQL insert query structure specifying fields and values (see usage examples of sqlInterpolate function)
    sql <- "INSERT INTO habitat (stream, location, Date, site_num, observers, tempc, pH, domgl, spc, turb) VALUES (?stream, ?location, ?date, ?site_num, ?observers, ?temp, ?pH, ?domgl, ?spc, ?turb);"
    
    # connect to database
    con <- dbConnect(RSQLite::SQLite(), db_path)
    
    # construct query using sqlInterpolate to prevent SQL injection attacks
    query <- sqlInterpolate(con, sql, 
                            stream = stream, 
                            location = location, 
                            date = date, 
                            site_num = site_num,
                            observers = observers,
                            temp = temp, 
                            pH = pH, 
                            domgl = DO,
                            spc = spc,
                            turb = turb)
    
    # finally execute query to add record to database
    dbExecute(con, query)
    
    dbDisconnect(con) # disconnect from database
    
  }
  # return error messages
  return(msg)
}


#' style_switch
#'
#' @param value numeric value to test
#' @param min numeric lower bound of acceptable range of values
#' @param max numeric upper bound of acceptable range of values
#' @param style1 character string css for value within acceptable range
#' @param style2 character string css for value outside of acceptable
#'
#' @return style1 if value is within range defined by 'min' and 'max' parameters, else style2
#'
#' @examples
style_switch <- function(value, min, max, style1, style2){
  
  # missing value accepted
  if(is.na(value)){
    style <- style1
    
  # value outside of range
  }else if(value > max | value < min){
    style <- style2
    
  # value within acceptable range
  }else{
    style <- style1
  }
  
  return(style)
}

# Define UI ----
ui <- fluidPage(
  useShinyjs(), # this line is necessary if any functions from the shinyjs package are used in the app
  title = "Water Quality Example Interface",
  
  tabsetPanel(id = "tabs",
              
              # page 51 of "PN, Mon Guidelines & Perf Stds_11.8.18.pdf"
              tabPanel("Field Data Sheet Entry", id = "single", 
                       # first row, location, date, site
                       fluidRow(
                         column(2,
                                selectInput(inputId = "stream", 
                                            label = "Stream", 
                                            # added empty string to options for streams in order to prevent errors 
                                            # that could occur if users submit data without changing the stream name from the default stream,
                                            # leading to data misattributed to the default stream
                                            choices = c("", streams)) 
                         ),
                         column(4,
                                textInput(
                                  inputId = "location",
                                  label = "Location/road:",
                                  value = ""
                                )
                         ),
                         column(2,
                                dateInput(
                                  inputId = "date",
                                  label = "Date",
                                  format = "yyyy-mm-dd",
                                  value = current_date,
                                  max = current_date,
                                  min = first_date
                                )
                         ),
                         column(4,
                                numericInput(inputId = "site_num",
                                             label = "Site#",
                                             value = NULL,
                                             min = 0,
                                             max = 1000000000 # this seems like a lot of sites, but who knows how people number their sites
                                )
                         )
                         
                       ),
                       hr(),
                       # second row, observers
                       fluidRow(
                         column(12,
                                textInput(
                                  inputId = "observers",
                                  label = "Observer(s)",
                                  value = "",
                                  # entry window takes up the entire width of its container / the browser window 
                                  # to allow for long lists of data collectors to be visible
                                  width = "100%" 
                                )
                         )
                       ),
                       hr(),
                       # third row, water quality
                       fluidRow(
                         column(2,
                                htmlOutput("water_quality")
                         ),
                         
                         # the 'min' and 'max' arguments to the input functions 
                         # specified here are only enforced by the function 
                         # if the user sets the value using the up and down arrows, 
                         # not if the user enters a specific value with the keyboard, 
                         # necessitating additional input validation
                         column(2,
                                # this line allows changing of the style of the field label dynamically
                                # based on whether the entered value is within the acceptable range
                                htmlOutput("temp_div"), 
                                numericInput(inputId = "temp", 
                                             label = "", # label is replaced by 'htmlOutput()' above
                                             value = NULL, 
                                             min = min_temp, 
                                             max = max_temp,
                                             step = .01)
                         ),
                         column(2,
                                htmlOutput("ph_div"),
                                numericInput(inputId = "pH", 
                                             label = "", 
                                             value = NULL, 
                                             min = min_ph, 
                                             max = max_ph,
                                             step = .01)
                         ),
                         column(2,
                                htmlOutput("do_div"),
                                numericInput(inputId = "do", 
                                             label = "", 
                                             value = NULL, 
                                             min = min_do, 
                                             max = max_do,
                                             step = .01)
                         ),
                         column(2,
                                htmlOutput("spc_div"),
                                numericInput(inputId = "spc", 
                                             label = "", 
                                             value = NULL, 
                                             min = min_spc, 
                                             max = max_spc,
                                             step = .01)
                         ),
                         column(2,
                                htmlOutput("turb_div"),
                                numericInput(inputId = "turb", 
                                             label = "", 
                                             value = NULL, 
                                             min = min_turb, 
                                             max = max_turb,
                                             step = .01)
                         )
                       ),
                       hr(),                       
                       fluidRow(
                         column(12,
                                actionButton(inputId = "submit", label = "Submit")
                         )
                       ),
                       hr(),
                       fluidRow(
                         column(12,
                                # this div is hidden by default and becomes visible 
                                # after a record is successfully added to the database table
                                hidden(
                                  div(
                                    id = "success",
                                    h4("Data submitted successfully")
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
                                    id = "error",
                                    h4("There are problems with the entered data that prevented them from being imported into the database.", 
                                       HTML("<div style='text-decoration:underline'>Resolve these errors before attempting to resubmit.</div>"))
                                  )
                                )
                         )
                       ),
                       fluidRow(
                         # display error messages returned from the update_hab function
                         column(12,
                                htmlOutput("entry_errs")
                         )
                       ),
                       hr(),
                       fluidRow(
                         # display records entered into database table 
                         DT::dataTableOutput("table_out")
                       )
              )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  # update current time - changes every time something in the inputs changes
  current_time <- as.POSIXlt(Sys.time())
  
  # convert current time to number of seconds since January 1, 1970
  current_unix_time <- as.double(current_time)
  
  # CSS of the temperature entry field reacts to value entered
  temp_style <- eventReactive(input$temp, {
    style_switch(input$temp, min_temp, max_temp, default_style, error_style)
  })
  
  # create temperature entry field label incorporating changing CSS
  output$temp_div <- renderUI({
    HTML(paste0("<div style='font-weight:bolder;",temp_style(), "'>Temperature (degrees C)</div>"))
  })
  
  ph_style <- eventReactive(input$pH, {
    style_switch(input$pH, min_ph, max_ph, default_style, error_style)
  })
  
  output$ph_div <- renderUI({
    HTML(paste0("<div style='font-weight:bolder;",ph_style(), "'>pH</div>"))
  })
  
  do_style <- eventReactive(input$do, {
    style_switch(input$do, min_do, max_do, default_style, error_style)
  })
  
  output$do_div <- renderUI({
    HTML(paste0("<div style='font-weight:bolder;",do_style(), "'>DO (mg/L)</div>"))
  })
  
  spc_style <- eventReactive(input$spc, {
    style_switch(input$spc, min_spc, max_spc, default_style, error_style)
  })
  
  output$spc_div <- renderUI({
    HTML(paste0("<div style='font-weight:bolder;",spc_style(), "'>Specific Conductance (uS/cm)</div>"))
  })
  
  # format label for row of water quality values
  output$water_quality <- renderUI({
    HTML("<div style='font-weight:bolder;padding-top:20px;text-align: right;'>Water Quality: </div>")
  })
  
  turb_style <- eventReactive(input$turb, {
    style_switch(input$turb, min_turb, max_turb, default_style, error_style)
  })
  
  output$turb_div <- renderUI({
    HTML(paste0("<div style='font-weight:bolder;",turb_style(), "'>Turbidity (ntu)</div>"))
  })
  
  # store errors for printing to display
  err_out <- eventReactive(input$submit, {
    
    # While the water quality measurements are optional, 
    # the data are useless without associating them with a stream.
    # a stream is required in order to attempt a record addition
    if(input$stream == ""){ 
      result <- "No Stream selected.<br/>"
    }else{
      
      # attempt to add a record composed of the entered values and retrieve any error messages
      result <- update_hab(input$stream, 
                           input$location, 
                           input$date, 
                           input$site_num,
                           input$observers, 
                           input$temp, 
                           input$pH, 
                           input$do, 
                           input$spc, 
                           input$turb,
                           db_path
                           )
    }
    
    # if there are no error messages, show success message and hide error message
    if(nchar(result) == 0){
      shinyjs::show("success")
      shinyjs::hide("error")
      
    }else{ # if there are error messages, show error message and hide success message
      shinyjs::show("error")
      shinyjs::hide("success")
    }
    
    # disable the submit button until inputs are altered to avoid adding duplicate records
    disable("submit")
    
    result
  })
  
  # format error messages for display in interface
  output$entry_errs <- renderUI({
    HTML(paste0("<div style='color:red;font-size:large;'>",err_out(),"</div>"))
  })
  
  # combine all inputs to monitor for changes
  check_all_inputs <- reactive({
    list(input$stream, 
         input$location, 
         input$date, 
         input$site_num,
         input$observers, 
         input$temp, 
         input$pH, 
         input$do, 
         input$spc, 
         input$turb)
  })
  
  # if any of the inputs change, enable the submit button and hide the success message
  observeEvent(check_all_inputs(), {
    shinyjs::hide("success")
    enable("submit")
  })
  
  # query database records for display in table form
  data_out <- eventReactive(input$submit, {
    
    # construct SQL SELECT query
    get_records <- paste0("SELECT * FROM habitat")
    
    # connect to database
    con <- dbConnect(RSQLite::SQLite(), db_path)
    
    # execute query
    result <- dbGetQuery(con, get_records)
    
    # disconnect from database
    dbDisconnect(con)
    
    # return table of database records
    result
  })
  
  # construct table display
  output$table_out <- DT::renderDataTable({DT::datatable(data_out(), options = list(pageLength = 50))})
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
