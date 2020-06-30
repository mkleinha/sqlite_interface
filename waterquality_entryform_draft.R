#' This is script for the Water Quality Date Entry Interface tab of the Acanthonus database
#' This form is for entering data primarily associated with Section 6 and NFWF projects in the Upper Coosa Basin
# author: Maxwell Kleinhans (maxwell.kleinhans@gmail.com) and Phillip Bumpers (bumpersp@uga.edu)

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
max_temp <- 40 
min_temp <- 0
max_ph <- 14
min_ph <- 0
max_do <- 20
min_do <- 0
max_spc <- 10000
min_spc <- 0
max_turb <- 10000
min_turb <- 0
max_dissolved<-40
min_dissolved<-0
max_totals<-40
min_totals<-0


# lists for input options for parameters with defined but multiple options. 
basins <- c("Holly", "Etowah", "Conasauga")
substrates<-c("not determined", "bedrock", "boulder", "cobble", "gravel", "sand", "silt", "mud", "concrete", "wood", "leaves")
labs<-c("CAIS-UGA", "Analytical Chemistry Lab, Ecology, UGA", "Dalton Utilities","Laboratory for Environmental Analysis, Hassan Lab, UGA", "NA", "Other" )
collect_types<-c("wading", "bucket")
instream_locations<-c("left bank", "right bank", "thalweg", "open channel", "bridge")
flow_types<-c("riffle", "run", "pool", "backwater", "other")
flow_conditions<-c("not determined", "stable-low", "stable-high", "stable-normal", "rising", "falling", "peak")
weather_conditions<-c("NA", "heavy rain", "hot", "cold", "sunny", "cloudy", "partly cloudy", "light rain", "snow")
buffer_conditions<-c("cleared", "fringe", "canopy")

#usgs_gages<c()#may eventually add USGS gage ID choices that has built in code to fetch discharge on the date entered. 


#' The parameters to be included in the form. 
#' update_hab
#'
#' @param Date character string date in the form yyyy-mm-dd
#' @param Basin character string stream name
#' @param Sample_Time character string time in the form hh:mm 24:00
#' @param Og_Site integer site number
#' @param Observers character string listing collectors of the data
#' @param Temperature_c numeric temperature in degrees celcius
#' @param ph integer pH
#' @param Dissolved_Oxygen_mgl numeric dissolved oxygen concentration in mg / L
#' @param Specific_Conductivity_uscm numeric specific conductivity
#' @param Turbidity_ntu numeric turbidity in NTU
#' @param Dissolved_Nitrate_mgl numeric nitrate in mg/L
#' @param Dissolved_Ammonium_mgl numeric ammonium in mg/L
#' @param Dissolved_Phosphorus_mgl numeric phosphorus in mg/L
#' @param Total_Nitrogen_mgl numeric total nitrogen in mg/L
#' @param Total_Phosphorus_mgl numeric total phosphours in mg/L
#' @param Sodium_mgl numeric sodium in mg/L
#' @param Calcium_mgl numeric calcium in mg/L
#' @param Potassium_mgl numeric potassium in mg/L
#' @param Magnesium_mgl numeric magnesium in mg/L
#' @param Analytical_Lab character string stating where chemical analyses were performed
#' @param Collection_Type character string describing if sample was collected by wading or bucket
#' @param Instream_Location character string describing location of sample in the channel
#' @param Flow_Type character string describing flow type, riffle, run, pool, thalweg
#' @param Channel_Width_m integer of approximate wetted width of stream in meters
#' @param Substrate character string describing dominant substrate types
#' @param Stage_Condition character string describing flow condition
#' @param Water_Odor character string describing the presence of stream odor
#' @param Water_Color character string describing color of water
#' @param Weather_Conditions character string describing weather conditions during sampling
#' @param RiverRight_Buffer character string describing river right riparian buffer
#' @param RiverLeft_Buffer character string describing river left buffer
#' @param Water_Quality_Notes character string of additinal notes recorded
#' @param USGS_Gage_cfs integer of discharge from appropriate USGS gage in cfs
#' @param USGS_Gage_ID integer of the USGS gage ID for appropriate USGS gage
#' @param db_path character string path to .sqlite database file
#'
#' @return error messages pasted into a single string separated by line breaks for printing
#'
#' @examples
#' 

#create a function that will update the habitat table. 
update_hab <- function(Date,Basin, Sample_Time, Og_Site, Observers, Temperature_c, ph, Dissolved_Oxygen_mgl, Specific_Conductivity_uscm, Turbidity_ntu, Dissolved_Nitrate_mgl, 
                       Dissolved_Ammonium_mgl, Dissolved_Phosphorus_mgl, Total_Nitrogen_mgl, Total_Phosphorus_mgl, Sodium_mgl,Calcium_mgl, Potassium_mgl, Magnesium_mgl, Analytical_Lab, 
                       Collection_Type, Instream_Location, Flow_Type, Channel_Width_m,  Substrate, Stage_Condition, Water_Odor,
                       Water_Color, Weather_Conditions, RiverRight_Buffer, RiverLeft_Buffer, Water_Quality_Notes, USGS_Gage_cfs, USGS_Gage_ID, db_path){
  
  # replace one single quote in location descripton with two for SQL query formatting reasons
  #location <- gsub("'", "''", location)
  #Water_Quality_Notes <- gsub("'", "''", Water_Quality_Notes)
  Date <- gsub(" *UTC$", "", Date) # remove time from date
  
  # empty string to which error messages will be pasted
  #parameters where Multiple=TRUE in the UI need to if the length bit or the submit form 
  #will throw an error expecting lenght 1 but returning 0. 
  msg <- ""
  if(length(Substrate)<1){
    msg <- paste0(msg, "No substrate selected.")
  }
  
  msg <- ""
  if(length(Weather_Conditions)<1){
    msg <- paste0(msg, "No Weather selected.")
  }
  #Set QAQC rules for data entry
  # if temperature measurement is missing, don't throw an error, 
  # but also skip bounds check to avoid 'missing value where TRUE/FALSE needed' error
  if(is.na(Temperature_c)){
    # no action needed here, 'if' statement just prevents conditions below from
    # throwing 'missing value where TRUE/FALSE needed' error if temp is NA
  }else if(Temperature_c > max_temp | Temperature_c < min_temp){ # check if temp is within a reasonable range of values
    # if temp is outside specified acceptable range, add error message to output
    msg <- paste0(msg, "Entered temperature outside reasonable range (",min_temp,"-",max_temp,").<br/>")
  }
  
  if(is.na(ph)){
    
  }else if(ph > max_ph | ph < min_ph){
    msg <- paste0(msg, "Entered pH value outside reasonable range (",min_ph,"-",max_ph,").<br/>")
  }
  
  if(is.na(Dissolved_Oxygen_mgl)){
    
  }else if(Dissolved_Oxygen_mgl > max_do | Dissolved_Oxygen_mgl < min_do){
    msg <- paste0(msg, "Entered dissolved oxygen value outside reasonable range (",min_do,"-",max_do,").<br/>")
  }
  
  if(is.na(Specific_Conductivity_uscm)){
    
  }else if(Specific_Conductivity_uscm > max_spc | Specific_Conductivity_uscm < min_spc){
    msg <- paste0(msg, "Entered conductivity value outside reasonable range (",min_spc,"-",max_spc,").<br/>")
  }
  
  if(is.na(Turbidity_ntu)){
    
  }else if(Turbidity_ntu > max_turb | Turbidity_ntu < min_turb){
    msg <- paste0(msg, "Entered turbidity value outside reasonable range (",min_turb,"-",max_turb,").<br/>")
  }
  
  if(is.na(Dissolved_Nitrate_mgl)){
    
  }else if(Dissolved_Nitrate_mgl > max_dissolved | Dissolved_Nitrate_mgl < min_dissolved){
    msg <- paste0(msg, "Entered dissolved nitrate value outside reasonable range (",min_dissolved,"-",max_dissolved,").<br/>")
  }
  
  if(is.na(Dissolved_Ammonium_mgl)){
    
  }else if(Dissolved_Ammonium_mgl > max_dissolved | Dissolved_Ammonium_mgl < min_dissolved){
    msg <- paste0(msg, "Entered dissolved ammonium value outside reasonable range (",min_dissolved,"-",max_dissolved,").<br/>")
  }
  
  if(is.na(Dissolved_Phosphorus_mgl)){
    
  }else if(Dissolved_Phosphorus_mgl > max_dissolved | Dissolved_Phosphorus_mgl < min_dissolved){
    msg <- paste0(msg, "Entered dissolved phosphorus value outside reasonable range (",min_dissolved,"-",max_dissolved,").<br/>")
  }
  
  if(is.na(Total_Nitrogen_mgl)){
    
  }else if(Total_Nitrogen_mgl > max_totals | Total_Nitrogen_mgl < min_totals){
    msg <- paste0(msg, "Entered total nitrogen value outside reasonable range (",min_totals,"-",max_totals,").<br/>")
  }
  
  if(is.na(Total_Phosphorus_mgl)){
    
  }else if(Total_Phosphorus_mgl > max_totals | Total_Phosphorus_mgl < min_totals){
    msg <- paste0(msg, "Entered total phosphorus value outside reasonable range (",min_totals,"-",max_totals,").<br/>")
  }
  
  # records are uniquely identified by date, basin and site number in this example db, so require these fields
  if(is.na(Date)){
    msg <- paste0(msg, "Please specify a date before attempting to submit records.<br/>")
  }
  
  if(is.na(Basin)){
    msg <- paste0(msg, "Please specify a Basin before attempting to submit records.<br/>")
  }
  
  if(is.na(Og_Site)){
    msg <- paste0(msg, "Please specify a site number before attempting to submit records.<br/>")
  }
  
  
  # if there are no error messages (the length of the messages string is 0), 
  # add data to database as new record
  if(nchar(msg) == 0){
    
    get_records <- paste0("SELECT * FROM habitat WHERE Date = '",Date,"' AND Basin = '",Basin,"' AND Og_Site = ",Og_Site)
    
    # connect to database
    con <- dbConnect(RSQLite::SQLite(), db_path)
    
    result <- dbGetQuery(con, get_records)
    
    if(nrow(result) > 0){ # record with unique identifiers site, stream and date already exists, so update this existing record
      
      sql <- "UPDATE habitat
                SET Observers = ?Observers,
                    Temperature_c = ?Temperature_c,
                    ph = ph,    
                    Dissolved_Oxygen_mgl = ?Dissolved_Oxygen_mgl,
                    Specific_Conductivity_uscm = ?Specific_Conductivity_uscm,
                    Turbidity_ntu = ?Turbidity_ntu,
                    Dissolved_Nitrate_mgl = ?Dissolved_Nitrate_mgl, 
                    Dissolved_Ammonium_mgl = ?Dissolved_Ammonium_mgl, 
                    Dissolved_Phosphorus_mgl = ?Dissolved_Phosphorus_mgl, 
                    Total_Nitrogen_mgl = ?Total_Nitrogen_mgl, 
                    Total_Phosphorus_mgl = ?Total_Phosphorus_mgl, 
                    Sodium_mgl= ?Sodium_mgl, 
                    Calcium_mgl = ?Calcium_mgl, 
                    Potassium_mgl = ?Potassium_mgl,
                    Magnesium_mgl = ?Magnesium_mgl, 
                    Analytical_Lab = ?Analytical_Lab, 
                    Collection_Type = ?Collection_Type,
                    Instream_Location = ?Instream_Location,
                    Flow_Type = ?Flow_Type, 
                    Channel_Width_m =? Channel_Width_m,
                    Substrate = ?Substrate, 
                    Stage_Condition = ?Stage_Condition, 
                    Water_Odor = ?Water_Odor,
                    Water_Color = ?Water_Color, 
                    Weather_Conditions = ?Weather_Conditions, 
                    RiverRight_Buffer = ?RiverRight_Buffer, 
                    RiverLeft_Buffer = ?RiverLeft_Buffer, 
                    Water_Quality_Notes =?Water_Quality_Notes, 
                    USGS_Gage_cfs = ?USGS_Gage_cfs,
                    USGS_Gage_ID = ?USGS_Gage_ID,
                  WHERE Date = ?Date
                  AND Basin = ?Basin
                  AND Og_Site = ?Og_Site;"
    
    
    # construct query using sqlInterpolate to prevent SQL injection attacks
    query <- sqlInterpolate(con, sql, 
                            Date = Date,
                            Basin = Basin,
                            Sample_Time=Sample_Time,
                            Og_Site = Og_Site,
                            Observers = Observers,
                            Temperature_c = Temperature_c, 
                            ph = ph, 
                            Dissolved_Oxygen_mgl = Dissolved_Oxygen_mgl,
                            Specific_Conductivity_uscm = Specific_Conductivity_uscm,
                            Turbidity_ntu = Turbidity_ntu,
                            Dissolved_Nitrate_mgl = Dissolved_Nitrate_mgl,
                            Dissolved_Ammonium_mgl = Dissolved_Ammonium_mgl,
                            Dissolved_Phosphorus_mgl = Dissolved_Phosphorus_mgl,
                            Total_Nitrogen_mgl = Total_Nitrogen_mgl,
                            Total_Phosphorus_mgl = Total_Phosphorus_mgl,
                            Sodium_mgl = Sodium_mgl, 
                            Calcium_mgl = Calcium_mgl, 
                            Potassium_mgl = Potassium_mgl,
                            Magnesium_mgl = Magnesium_mgl,
                            Analytical_Lab = Analytical_Lab,
                            Collection_Type = Collection_Type,
                            Instream_Location = Instream_Location, 
                            Flow_Type = Flow_Type,
                            Channel_Width_m = Channel_Width_m, 
                            Substrate = Substrate,
                            Stage_Condition = Stage_Condition, 
                            Water_Odor = Water_Odor, 
                            Water_Color = Water_Color, 
                            Weather_Conditions = Weather_Conditions, 
                            RiverRight_Buffer = RiverRight_Buffer,
                            RiverLeft_Buffer = RiverLeft_Buffer,
                            Water_Quality_Notes = Water_Quality_Notes, 
                            USGS_Gage_cfs = USGS_Gage_cfs, 
                            USGS_Gage_ID = USGS_Gage_ID)
    
      # finally execute query to add record to database
      dbExecute(con, query)
    
    }else{ # record doesn't exist so append record to existing database
      
      # This strategy only works if the inputs match up exactly to the fields of the database
      
      vals <- data.frame(
        Date,
        Basin,
        Sample_Time,
        Og_Site, 
        Observers, 
        Temperature_c, 
        ph, 
        Dissolved_Oxygen_mgl, 
        Specific_Conductivity_uscm, 
        Turbidity_ntu, 
        Dissolved_Nitrate_mgl, 
        Dissolved_Ammonium_mgl,
        Dissolved_Phosphorus_mgl,
        Total_Nitrogen_mgl, 
        Total_Phosphorus_mgl, 
        Sodium_mgl,
        Calcium_mgl, 
        Potassium_mgl, 
        Magnesium_mgl, 
        Analytical_Lab, 
        Collection_Type, 
        Instream_Location, 
        Flow_Type, 
        Channel_Width_m,  
        Substrate, 
        Stage_Condition,
        Water_Odor,
        Water_Color, 
        Weather_Conditions, 
        RiverRight_Buffer, 
        RiverLeft_Buffer, 
        Water_Quality_Notes, 
        USGS_Gage_cfs, 
        USGS_Gage_ID
        )
      
      dbWriteTable(con, "habitat", vals, append = T)
    }
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
                       # first row, Basin, date, site, observers
                       fluidRow(
                         column(2,
                                selectInput(inputId = "Basin", 
                                                   label = "Basin", 
                                                   # added empty string to options for streams in order to prevent errors 
                                                   # that could occur if users submit data without changing the stream name from the default stream,
                                                   # leading to data misattributed to the default stream
                                                   choices = c("", basins))
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
                         column(3,
                                textInput(
                                  inputId = "Sample_Time",
                                  label = "Sample Time (hh:mm, 24hr)",
                                  value = ""
                                  # entry window takes up the entire width of its container / the browser window 
                                  # to allow for long lists of data collectors to be visible
                                )
                                ),
                         column(1,
                                numericInput(inputId = "Og_Site",
                                             label = "Site#",
                                             value = NULL,
                                             min = 0,
                                             max = 1000000000 # this seems like a lot of sites, but who knows how people number their sites
                                )
                                ),
                         column(4,
                                textInput(
                                  inputId = "Observers",
                                  label = "Observer(s)",
                                  value = "",
                                  # entry window takes up the entire width of its container / the browser window 
                                  # to allow for long lists of data collectors to be visible
                                  width = "100%")
                                )
                         ),
                       hr(),
                       # second row, begin water quality
                       fluidRow(
                         column(2,
                                htmlOutput("water_quality")
                                )
                         ),
                       fluidRow(
                         # the 'min' and 'max' arguments to the input functions 
                         # specified here are only enforced by the function 
                         # if the user sets the value using the up and down arrows, 
                         # not if the user enters a specific value with the keyboard, 
                         # necessitating additional input validation
                         column(2,
                                # this line allows changing of the style of the field label dynamically
                                # based on whether the entered value is within the acceptable range
                                htmlOutput("temp_div"), 
                                numericInput(inputId = "Temperature_c", 
                                             label = "", # label is replaced by 'htmlOutput()' above
                                             value = NULL, 
                                             min = min_temp, 
                                             max = max_temp,
                                             step = .01)
                                ),
                         column(1,
                                htmlOutput("ph_div"),
                                numericInput(inputId = "ph", 
                                             label = "", 
                                             value = NULL, 
                                             min = min_ph, 
                                             max = max_ph,
                                             step = .01)
                                ),
                         column(1,
                                htmlOutput("do_div"),
                                numericInput(inputId = "Dissolved_Oxygen_mgl", 
                                             label = "", 
                                             value = NULL, 
                                             min = min_do, 
                                             max = max_do,
                                             step = .1)
                                ),
                         column(3,
                                htmlOutput("spc_div"),
                                numericInput(inputId = "Specific_Conductivity_uscm", 
                                             label = "", 
                                             value = NULL, 
                                             min = min_spc, 
                                             max = max_spc,
                                             step = .01 
                                             )
                                ),
                         column(2,
                                htmlOutput("turb_div"),
                                numericInput(inputId = "Turbidity_ntu", 
                                             label = "", 
                                             value = NULL, 
                                             min = min_turb, 
                                             max = max_turb,
                                             step = .1 
                                             )
                                )
                         ),
                       
                       # third row, nutrients
                       fluidRow(
                         column(2,
                                selectInput(inputId = "Analytical_Lab", 
                                            label = "Analytical Lab", 
                                            # added empty string to options for streams in order to prevent errors 
                                            # that could occur if users submit data without changing the stream name from the default stream,
                                            # leading to data misattributed to the default stream
                                            choices = c("", labs)) 
                                ),
                         column(2,
                                htmlOutput("nitrate_div"),
                                numericInput(inputId = "Dissolved_Nitrate_mgl", 
                                             label = "", 
                                             value = NULL, 
                                             min = min_dissolved, 
                                             max = max_dissolved,
                                             step = .01)
                                ),
                         column(2,
                                htmlOutput("ammonium_div"),
                                numericInput(inputId = "Dissolved_Ammonium_mgl", 
                                             label = "", 
                                             value = NULL, 
                                             min = min_dissolved, 
                                             max = max_dissolved,
                                             step = .01)
                                ),
                         column(2,
                                htmlOutput("srp_div"),
                                numericInput(inputId = "Dissolved_Phosphorus_mgl", 
                                             label = "", 
                                             value = NULL, 
                                             min = min_dissolved, 
                                             max = max_dissolved,
                                             step = .01)
                                ),
                         column(2,
                                htmlOutput("totn_div"),
                                numericInput(inputId = "Total_Nitrogen_mgl", 
                                             label = "", 
                                             value = NULL, 
                                             min = min_totals, 
                                             max = max_totals,
                                             step = .01)
                                ),
                         column(2,
                                htmlOutput("totp_div"),
                                numericInput(inputId = "Total_Phosphorus_mgl", 
                                             label = "", 
                                             value = NULL, 
                                             min = min_totals, 
                                             max = max_totals,
                                             step = .01)
                                )
                         ),
                       #ions
                       fluidRow(
                         column(2,
                                htmlOutput("calc_div"),
                                numericInput(inputId = "Calcium_mgl", 
                                             label = "", 
                                             value = NULL, 
                                             step = .01)
                                ),
                         column(2,
                                htmlOutput("sod_div"),
                                numericInput(inputId = "Sodium_mgl", 
                                             label = "", 
                                             value = NULL, 
                                             step = .01)
                                ),
                         column(2,
                                htmlOutput("pot_div"),
                                numericInput(inputId = "Potassium_mgl", 
                                             label = "", 
                                             value = NULL, 
                                             step = .01)
                                ),
                         column(2,
                                htmlOutput("mag_div"),
                                numericInput(inputId = "Magnesium_mgl", 
                                             label = "", 
                                             value = NULL, 
                                             step = .01)
                                )
                         ),
                       hr(),
                       #4th row. Sample information
                       fluidRow(
                         column(2, 
                                selectInput(inputId = "Instream_Location", 
                                               label = "Instream Location", 
                                               # added empty string to options for streams in order to prevent errors 
                                               # that could occur if users submit data without changing the stream name from the default stream,
                                               # leading to data misattributed to the default stream
                                               choices = c("", instream_locations))
                                ),
                         column(2, 
                                selectInput(inputId = "Collection_Type", 
                                               label = "Collection Type", 
                                               # added empty string to options for streams in order to prevent errors 
                                               # that could occur if users submit data without changing the stream name from the default stream,
                                               # leading to data misattributed to the default stream
                                               choices = c("", collect_types))
                                ),
                         column(2, 
                                selectInput(inputId = "Flow_Type", 
                                               label = "Flow Type", 
                                               # added empty string to options for streams in order to prevent errors 
                                               # that could occur if users submit data without changing the stream name from the default stream,
                                               # leading to data misattributed to the default stream
                                               choices = c("", flow_types))
                                ),
                         column(2, 
                                selectInput(inputId = "Stage_Condition", 
                                               label = "Stage Condition", 
                                               # added empty string to options for streams in order to prevent errors 
                                               # that could occur if users submit data without changing the stream name from the default stream,
                                               # leading to data misattributed to the default stream
                                               choices = c("", flow_conditions))
                                )
                       ),
                      #5th row
                       hr(),
                       fluidRow(
                         column(2, 
                                selectInput(inputId = "Substrate", 
                                               label = "Substrate", 
                                               # added empty string to options for streams in order to prevent errors 
                                               # that could occur if users submit data without changing the stream name from the default stream,
                                               # leading to data misattributed to the default stream
                                               choices = c("", substrates), 
                                               multiple = TRUE)
                                ),
                         column(2,
                                numericInput(inputId = "Channel_Width_m",
                                             label = "Wetted Channel Width (m)",
                                             value = NULL,
                                             min = 0,
                                             max = 10000 #
                                             )
                                ),
                         column(3,
                                textInput(inputId = "Water_Odor",
                                          label = "Water Odor",
                                          value = ""
                                          # entry window takes up the entire width of its container / the browser window 
                                          # to allow for long lists of data collectors to be visible
                                          
                                          )
                                ),
                         column(3,
                                textInput(inputId = "Water_Color",
                                  label = "Water Color",
                                  value = ""
                                  # entry window takes up the entire width of its container / the browser window 
                                  # to allow for long lists of data collectors to be visible
                                  )
                                )
                         ),
                       hr(),
                      #6th Row
                       fluidRow(
                         column(3, 
                                selectInput(inputId = "Weather_Conditions", 
                                               label = "Weather", 
                                               # added empty string to options for streams in order to prevent errors 
                                               # that could occur if users submit data without changing the stream name from the default stream,
                                               # leading to data misattributed to the default stream
                                               choices = c("", weather_conditions), 
                                               multiple = TRUE)
                                ),
                         
                         column(4, 
                                selectInput(inputId = "RiverRight_Buffer", 
                                               label = "River Right Riparian Buffer Conition", 
                                               # added empty string to options for streams in order to prevent errors 
                                               # that could occur if users submit data without changing the stream name from the default stream,
                                               # leading to data misattributed to the default stream
                                               choices = c("",buffer_conditions))
                                ),
                         
                         column(4, 
                                selectInput(inputId = "RiverLeft_Buffer", 
                                               label = "River Left Riparian Buffer Conition", 
                                               # added empty string to options for streams in order to prevent errors 
                                               # that could occur if users submit data without changing the stream name from the default stream,
                                               # leading to data misattributed to the default stream
                                               choices = c("",buffer_conditions))
                                )
                         ),
                       hr(),
                      #7th row
                       fluidRow(
                         column(3,
                                numericInput(inputId = "USGS_Gage_cfs",
                                             label = "USGS Gage Discharge (cfs)",
                                             value = NULL)
                                ),
                         column(3,
                                numericInput(inputId = "USGS_Gage_ID",
                                             label = "USGS Gage ID",
                                             value = NULL)
                                )
                         ),
                       hr(),
                      #8th row. notes
                       fluidRow(
                         column(8,
                                textInput(
                                  inputId = "Water_Quality_Notes",
                                  label = "Water Quality Notes",
                                  value = "",
                                  # entry window takes up the entire width of its container / the browser window 
                                  # to allow for long lists of data collectors to be visible
                                  width = "100%")
                                )
                         ),
                       hr(),
                      #9th row Buttons
                       fluidRow(
                         column(12,
                                actionButton(inputId = "submit", label = "Submit")
                         ),
                         column(1,
                                actionButton(inputId = "query", label = "Query")
                         ),
                         column(2,
                                actionButton(inputId = "clear", label = "Clear")
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
  
  # CSS of the temperature entry field reacts to value entered (changes color if value out of range)
  temp_style <- eventReactive(input$Temperature_c, {
    style_switch(input$Temperature_c, min_temp, max_temp, default_style, error_style)
  })
  
  # create temperature entry field label incorporating changing CSS
  output$temp_div <- renderUI({
    HTML(paste0("<div style='font-weight:bolder;",temp_style(), "'>Temperature (degrees C)</div>"))
  })
  
  ph_style <- eventReactive(input$ph, {
    style_switch(input$ph, min_ph, max_ph, default_style, error_style)
  })
  
  output$ph_div <- renderUI({
    HTML(paste0("<div style='font-weight:bolder;",ph_style(), "'>pH</div>"))
  })
  
  do_style <- eventReactive(input$Dissolved_Oxygen_mgl, {
    style_switch(input$Dissolved_Oxygen_mgl, min_do, max_do, default_style, error_style)
  })
  
  output$do_div <- renderUI({
    HTML(paste0("<div style='font-weight:bolder;",do_style(), "'>DO (mg/L)</div>"))
  })
  
  spc_style <- eventReactive(input$Specific_Conductivity_uscm, {
    style_switch(input$Specific_Conductivity_uscm, min_spc, max_spc, default_style, error_style)
  })
  
  output$spc_div <- renderUI({
    HTML(paste0("<div style='font-weight:bolder;",spc_style(), "'>Specific Conductance (uS/cm)</div>"))
  })
  
  # format label for row of water quality values
  output$water_quality <- renderUI({
    HTML("<div style='font-weight:bolder;padding-top:25px;text-align: right;'>Water Quality: </div>")
  })
  
  nitrate_style <- eventReactive(input$Dissolved_Nitrate_mgl, {
    style_switch(input$Dissolved_Nitrate_mgl, min_dissolved, max_dissolved, default_style, error_style)
  })
  
  output$nitrate_div <- renderUI({
    HTML(paste0("<div style='font-weight:bolder;",nitrate_style(), "'>Dissolved NO3 (mg/L)</div>"))
  })
  
  turb_style <- eventReactive(input$Turbidity_ntu, {
    style_switch(input$Turbidity_ntu, min_turb, max_turb, default_style, error_style)
  })
  
  output$turb_div <- renderUI({
    HTML(paste0("<div style='font-weight:bolder;",turb_style(), "'>Turbidity (ntu)</div>"))
  })
  
  ammonium_style <- eventReactive(input$Dissolved_Ammonium_mgl, {
    style_switch(input$Dissolved_Ammonium_mgl, min_dissolved, max_dissolved, default_style, error_style)
  })
  
  output$ammonium_div <- renderUI({
    HTML(paste0("<div style='font-weight:bolder;",ammonium_style(), "'>Dissolved NH4 (mg/L)</div>"))
  })
  
  srp_style <- eventReactive(input$Dissolved_Phosphorus_mgl, {
    style_switch(input$Dissolved_Phosphorus_mgl, min_dissolved, max_dissolved, default_style, error_style)
  })
  
  output$srp_div <- renderUI({
    HTML(paste0("<div style='font-weight:bolder;",srp_style(), "'>Dissolved PO4 (mg/L)</div>"))
  })
  
  totn_style <- eventReactive(input$Total_Nitrogen_mgl, {
    style_switch(input$Total_Nitrogen_mgl, min_totals, max_totals, default_style, error_style)
  })
  
  output$totn_div <- renderUI({
    HTML(paste0("<div style='font-weight:bolder;",totn_style(), "'>Total N (mg/L)</div>"))
  })
  
  totp_style <- eventReactive(input$Total_Phosphorus_mgl, {
    style_switch(input$Total_Phosphorus_mgl, min_totals, max_totals, default_style, error_style)
  })
  
  output$totp_div <- renderUI({
    HTML(paste0("<div style='font-weight:bolder;",totp_style(), "'>Total P (mg/L)</div>"))
  })
  
  calc_style <- eventReactive(input$Calcium_mgl, {
    style_switch(input$Calcium_mgl,min_totals, max_totals,  default_style, error_style)
  })
  
  output$calc_div <- renderUI({
    HTML(paste0("<div style='font-weight:bolder;",calc_style(), "'>Calcium (mg/L)</div>"))
  })
  
  sod_style <- eventReactive(input$Sodium_mgl, {
    style_switch(input$Sodium_mgl,min_totals, max_totals, default_style, error_style)
  })
  
  output$sod_div <- renderUI({
    HTML(paste0("<div style='font-weight:bolder;",sod_style(), "'>Sodium (mg/L)</div>"))
  })
  
  pot_style <- eventReactive(input$Potassium_mgl, {
    style_switch(input$Potassium_mgl,min_totals, max_totals, default_style, error_style)
  })
  
  output$pot_div <- renderUI({
    HTML(paste0("<div style='font-weight:bolder;",sod_style(), "'>Potassium (mg/L)</div>"))
  })
  mag_style <- eventReactive(input$Magnesium_mgl, {
    style_switch(input$Magnesium_mgl,min_totals, max_totals, default_style, error_style)
  })
  
  output$mag_div <- renderUI({
    HTML(paste0("<div style='font-weight:bolder;",mag_style(), "'>Magnesium (mg/L)</div>"))
  })
  
  turb_style <- eventReactive(input$Turbidity_ntu, {
    style_switch(input$Turbidity_ntu, min_turb, max_turb, default_style, error_style)
  })
  
  output$turb_div <- renderUI({
    HTML(paste0("<div style='font-weight:bolder;",turb_style(), "'>Turbidity (ntu)</div>"))
  })
  
 
  # store errors for printing to display
  err_out <- eventReactive(input$submit, {
    
    # While the water quality measurements are optional, 
    # the data are useless without associating them with a stream.
    # a stream is required in order to attempt a record addition
    if(input$Basin == ""){ 
      result <- "No Basin selected.<br/>"
    }else{
      
      # attempt to add a record composed of the entered values and retrieve any error messages
      result <- update_hab(input$date,
                           input$Basin,
                           input$Sample_Time,
                           input$Og_Site,
                           input$Observers, 
                           input$Temperature_c, 
                           input$ph, 
                           input$Dissolved_Oxygen_mgl, 
                           input$Specific_Conductivity_uscm, 
                           input$Turbidity_ntu,
                           input$Dissolved_Nitrate_mgl,
                           input$Dissolved_Ammonium_mgl,
                           input$Dissolved_Phosphorus_mgl,
                           input$Total_Nitrogen_mgl,
                           input$Total_Phosphorus_mgl,
                           input$Sodium_mgl,
                           input$Calcium_mgl, 
                           input$Potassium_mgl,
                           input$Magnesium_mgl,
                           input$Analytical_Lab,
                           input$Collection_Type,
                           input$Instream_Location, 
                           input$Flow_Type, 
                           input$Channel_Width_m, 
                           input$Substrate,
                           input$Stage_Condition, 
                           input$Water_Odor, 
                           input$Water_Color, 
                           input$Weather_Conditions, 
                           input$RiverRight_Buffer,
                           input$RiverLeft_Buffer,
                           input$Water_Quality_Notes, 
                           input$USGS_Gage_cfs, 
                           input$USGS_Gage_ID,
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
    list(input$date,
         input$Basin,
         input$Sample_Time,
         input$Og_Site,
         input$Observers, 
         input$Temperature_c, 
         input$ph, 
         input$Dissolved_Oxygen_mgl, 
         input$Specific_Conductivity_uscm, 
         input$Turbidity_ntu,
         input$Dissolved_Nitrate_mgl,
         input$Dissolved_Ammonium_mgl,
         input$Dissolved_Phosphorus_mgl,
         input$Total_Nitrogen_mgl,
         input$Total_Phosphorus_mgl,
         input$Sodium_mgl, 
         input$Calcium_mgl, 
         input$Potassium_mgl,
         input$Magnesium_mgl,
         input$Analytical_Lab,
         input$Collection_Type, 
         input$Instream_Location, 
         input$Flow_Type, 
         input$Channel_Width_m, 
         input$Substrate,
         input$Stage_Condition, 
         input$Water_Odor, 
         input$Water_Color, 
         input$Weather_Conditions, 
         input$RiverRight_Buffer,
         input$RiverLeft_Buffer,
         input$Water_Quality_Notes, 
         input$USGS_Gage_cfs, 
         input$USGS_Gage_ID)
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
  observeEvent(input$clear, {
    updateTextInput(session, "Basin", value = "")
    updateDateInput(session, "date", value = current_date)
    updateTextInput(session, "Sample_Time", value = "")
    updateNumericInput(session, "Og_Site", value = "")
    updateTextInput(session, "Observers", value = "")
    updateNumericInput(session, "Temperature_c", value = "")
    updateNumericInput(session, "ph", value = "")     
    updateNumericInput(session, "Dissolved_Oxygen_mgl", value = "")
    updateNumericInput(session, "Specific_Conductivity_uscm", value = "")
    updateNumericInput(session, "Turbidity_ntu", value = "")
    updateNumericInput(session, "Dissolved_Nitrate_mgl", value = "")
    updateNumericInput(session, "Dissolved_Ammonium_mgl", value = "")
    updateNumericInput(session, "Dissolved_Phosphorus_mgl", value = "")
    updateNumericInput(session, "Total_Nitrogen_mgl", value = "")
    updateNumericInput(session, "Total_Phosphorus_mgl", value = "")
    updateNumericInput(session, "Sodium_mgl", value = "")
    updateNumericInput(session, "Calcium_mgl", value = "")
    updateNumericInput(session, "Potassium_mgl", value = "")
    updateNumericInput(session, "Magnesium_mgl", value = "")
    updateTextInput(session, "Analytical_Lab", value = "")
    updateTextInput(session, "Collection_Type", value = "")
    updateTextInput(session, "Instream_Location", value = "")
    updateTextInput(session, "Flow_Type", value = "")
    updateNumericInput(session, "Channel_Width_m", value = "")
    updateTextInput(session, "Substrate", value = "")
    updateTextInput(session, "Stage_Condition", value = "")
    updateTextInput(session, "Water_Odor", value = "")
    updateTextInput(session, "Water_Color", value = "")
    updateTextInput(session, "Weather_Conditions", value = "")
    updateTextInput(session, "RiverRight_Buffer", value = "")
    updateTextInput(session, "RiverLeft_Buffer", value = "")
    updateTextInput(session, "Water_Quality_Notes", value = "")
    updateNumericInput(session, "USGS_Gage_cfs", value = "")
    updateNumericInput(session, "USGS_Gage_ID", value = "")
    
  })
  
  # format error messages for display in interface
  output$query_errs <- renderUI({
    HTML(paste0("<div style='color:red;font-size:large;'>",query_out(),"</div>"))
  })
  
  query_out <- eventReactive(input$query, {
    queryerrs <- ""  
    
    if(is.na(input$date)){
      queryerrs <- paste0(queryerrs, "Please specify a date before attempting to query records.<br/>")
    }
    
    if(is.na(input$Basin)){
      queryerrs <- paste0(queryerrs, "Please specify a stream before attempting to query records.<br/>")
    }
    
    if(is.na(input$Og_Site)){
      queryerrs <- paste0(queryerrs, "Please specify a site number before attempting to query records.<br/>")
    }
    
    if(nchar(queryerrs) == 0){
      # construct SQL SELECT query
      get_records <- paste0("SELECT * FROM habitat WHERE Date = '",input$date,"' AND Basin = '",input$Basin,"' AND Og_Site = ",input$Og_Site)
      
      # connect to database
      con <- dbConnect(RSQLite::SQLite(), db_path)
      
      # execute query
      result <- dbGetQuery(con, get_records)
      
      # disconnect from database
      dbDisconnect(con)
      
      if(nrow(result) < 1){
        queryerrs <- paste0(queryerrs, paste0("No records found matching Date: ", input$date, ", Basin: ", input$Basin, ", and Og_Site: ", input$Og_Site,".<br/>"))
        
      }else if(nrow(result) > 1){
        queryerrs <- paste0(queryerrs, paste0("Multiple records found matching Date: ", input$date, ", Basin: ", input$Basin, ", and Og_Site: ", input$Og_Site,".<br/>"))
        
      }else{
        
        updateTextInput(session, "Sample_Time", value = result$Sample_Time)
        updateTextInput(session, "Observers", value = result$Observers)
        updateNumericInput(session, "Temperature_c", value = result$Temperature_c)
        updateNumericInput(session, "ph", value = result$ph)     
        updateNumericInput(session, "Dissolved_Oxygen_mgl", value = result$Dissolved_Oxygen_mgl)
        updateNumericInput(session, "Specific_Conductivity_uscm", value = result$Specific_Conductivity_mgl)
        updateNumericInput(session, "Turbidity_ntu", value = result$Turbidity_ntu)
        updateNumericInput(session, "Dissolved_Nitrate_mgl", value = result$Dissolved_Nitrate_mgl)
        updateNumericInput(session, "Dissolved_Ammonium_mgl", value = result$Dissolved_Ammonium_mgl)
        updateNumericInput(session, "Dissolved_Phosphorus_mgl", value = result$Dissolved_Phosphorus_mgl)
        updateNumericInput(session, "Total_Nitrogen_mgl", value = result$Total_Nitrogen_mgl)
        updateNumericInput(session, "Total_Phosphorus_mgl", value = result$Total_Phosphorus_mgl)
        updateNumericInput(session, "Sodium_mgl", value = result$Sodium_mgl)
        updateNumericInput(session, "Calcium_mgl", value = result$Calcium_mgl)
        updateNumericInput(session, "Potassium_mgl", value = result$Potassium_mgl)
        updateNumericInput(session, "Magnesium_mgl", value = result$Magnesium_mgl)
        updateTextInput(session, "Analytical_Lab", value = result$Analytical_Lab)
        updateTextInput(session, "Collection_Type", value = result$Collection_Type)
        updateTextInput(session, "Instream_Location", value = result$Instream_Location)
        updateTextInput(session, "Flow_Type", value = result$Flow_Type)
        updateNumericInput(session, "Channel_Width_m", value = result$Channel_With_m)
        updateTextInput(session, "Substrate", value = result$Substrate)
        updateTextInput(session, "Stage_Condition", value = result$Stage_Condition)
        updateTextInput(session, "Water_Odor", value = result$Water_Odor)
        updateTextInput(session, "Water_Color", value = result$Water_Color)
        updateTextInput(session, "Weather_Conditions", value =result$Weather_Conditions)
        updateTextInput(session, "RiverRight_Buffer", value = result$RiverRight_Buffer)
        updateTextInput(session, "RiverLeft_Buffer", value = result$RiverLeft_Buffer)
        updateTextInput(session, "Water_Quality_Notes", value = result$Water_Quality_Notes)
        updateNumericInput(session, "USGS_Gage_cfs", value = result$USGS_Gage_cfs)
        updateNumericInput(session, "USGS_Gage_ID", value = result$USGS_Gage_ID)
      }
    }
    queryerrs
  })
  
  
  # construct table display
  output$table_out <- DT::renderDataTable({DT::datatable(data_out(), options = list(pageLength = 50))})
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
