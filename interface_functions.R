# not vectorized
#' check_date
#' 
#' check if a date is of the format 'YYYY-MM-DD' 
#'
#' @param date character string representing a date of any format
#'
#' @return vector of character strings describing problems with 
#' the formatting of the input string 
#' If nor formatting problems are detected, returns the empty string ""
#'
#' @examples
#' 
#' check_date("2020-04-05")
#' check_date("1995-7-8")
#' check_date("44-5/8")
#' check_date("2021-13-40")
#' 
check_date <- function(date){
  
  errs <- ""
  
  
  # check if date is of the form 'YYYY-MM-DD'
  if(!grepl("^\\d{4}[-]\\d{1,2}[-]\\d{1,2}$", date)){
    
    errs <- c(errs, "bad date format")
    
  }else{
    
    # extract year component of date and convert to integer for range check
    year <- as.integer(gsub("[-].*", "", date))
    
    # check if year is within a reasonable range (1900 to current year)
    if(any(year > format(Sys.Date(), "%Y") | year < 1900)){
      errs <- c(errs, "year out of range")
    }
    
    # extract month component of date and convert to integer for range check
    month <- as.integer(gsub("^[^-]*[-]|[-][^-]*$", "", date))
    
    # check if month is within a reasonable range
    if(month > 12 | month < 1){
      errs <- c(errs, "month out of range")
    }
    
    # extract day component of date and convert to integer for range check
    day <- as.integer(gsub("^.*[-]", "", date))
    
    # check if day is within a reasonable range
    if(day > 31 | day < 1){
      errs <- c(errs, "day out of range")
    }
    
    # if there are no errors so far, it shouldn't be a problem to convert the 
    # year to a date
    if(length(errs) == 1){
      
      # check if date is in the future
      if(as.Date(date) > Sys.Date()){
        errs <- c(errs, "date in the future")
      }
    }
    
  }
  
  # if there are errors, eliminate the empty string in the vector
  if(length(errs) > 1){
    errs <- errs[errs != ""]
  }
  
  return(errs)
}

#' match_collect_id
#'
#' @param field_num character string field number of record
#' @param date character string date
#' @param site character string site number / name
#' @param db_path character string file path to database
#'
#' @return integer collection ID number associated with supplied combination of 
#' field number, date, and site, or NULL 
#' if no collection matching these criteria was found
#' 
match_collect_id <- function(field_num, date, site, db_path){
  
  con <- dbConnect(RSQLite::SQLite(), db_path)
  
  # attempt to associate kicksets with collection records based on field numbers
  
  # get lookup table of field numbers and collection ID's from database collections table
  query_fnums <- paste0("SELECT DISTINCT COLLECT_ID, Field_Numbers FROM collections WHERE Field_Numbers != ''")
  fnum_lookup <- dbGetQuery(con, query_fnums)
  
  # eliminate field numbers not matching function parameter
  fnum_lookup <- fnum_lookup[grepl(field_num, 
                                   fnum_lookup[,"Field_Numbers"],
                                   fixed = TRUE),]
  
  dbDisconnect(con) # disconnect from database
  
  # if only one record matches the function parameter field_num, 
  # return the collection ID associated with this record
  if(nrow(fnum_lookup) == 1){
    collect_id <- fnum_lookup[,"COLLECT_ID"]
    
  # throw an error if the field number matches more than one collection record
  }else if(nrow(fnum_lookup) > 1){
    
    stop(paste0("Field number matches ", nrow(fnum_lookup), " records."))
    
    # if the field numbers cannot be associated with existing collection records,
    # attempt to use site and date
  }else{
    
    con <- dbConnect(RSQLite::SQLite(), db_path)
    
    # get lookup table of collection id's, sites, and dates from database
    query_sdate <- paste0("SELECT DISTINCT COLLECT_ID, Year, Month, Day, Og_Site FROM collections WHERE Og_Site != ''")
    sdate_lookup <- dbGetQuery(con, query_sdate)
    
    # construct date column from component parts
    sdate_lookup[,"Date"] <- paste(sdate_lookup[,"Year"], 
                                   sdate_lookup[,"Month"], 
                                   sdate_lookup[,"Day"], 
                                   collapse = "-")
    
    # restrict lookup table to dates and sites present in function parameters
    sdate_lookup <- sdate_lookup[sdate_lookup[,"Date"] %in% date & sdate_lookup[,"Og_Site"] %in% site,]
    
    dbDisconnect(con) # disconnect from database
    
    # if only one record matches the function parameter site and date
    # return the collection ID associated with this record
    if(nrow(sdate_lookup) == 1){
      collect_id <- sdate_lookup[,"COLLECT_ID"]
      
      # throw an error if the field number matches more than one collection record
    }else if (nrow(sdate_lookup) > 1){
      stop(paste0("Site and date match ", nrow(sdate_lookup), " records."))
      
      # collection ID could not be found based on the supplied parameters
    }else{
      collect_id <- NULL
    }
  }
  
  return(collect_id)
  
} # end match_collect_id function

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
get_query_data <- function(db_path, start_year, end_year, species){
  
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

# ------------------------------------------------------------------------------
#' format_whitespace
#'
#' @param string any string
#'
#' @return string with all sequences of whitespace characters replaced with one space character 
#' and with leading and trailing whitespace removed
#'
#' @examples
#' 
#' format_whitespace("  This string has leading and trailing whitespace  ")
#' format_whitespace("This string has     multiple adjacent spaces      ")
#' format_whitespace("This string has \t other whitespace \n characters")
#'
format_whitespace <- function(string){
  result <- gsub("[ \t\r\n]", " ", string)
  result <- gsub(" +", " ", result)
  result <- gsub("^ +", "", result)
  result <- gsub(" +$", "", result)
  return(result)
}

# ------------------------------------------------------------------------------
#' format_species
#' 
#' consistently format taxonomic names replacing multiple space characters with just one, 
#' removing leading and trailing whitespace, 
#' and replacing sp., sp, spp., etc with "" except in cases of hybrids (eg L. sp. X L. sp.)
#'
#' @param vec vector of character strings of species names
#'
#' @return vector of character strings of species names consistently formatted
#'
#' @examples
#' 
#' species <- c("Boops  boops","Boops sp.","Boops boops x B. sp.")
#' format_species(species)
format_species <- function(vec){
  vec <- format_whitespace(vec)
  vec <- gsub(" x ", " X ", vec)
  vec[grep("^[^X]* *s+p+[.]*$", vec)] <- gsub(" *s+p+[.]*$", "", vec[grep("^[^X]* *s+p+[.]*$", vec)])
  vec <- format_whitespace(vec)
  vec[grep("^\\w+$", vec)] <- paste(vec[grep("^\\w+$", vec)], "sp.")
  vec <- gsub(" sp$", " sp.", vec)
  vec <- gsub(" sp ", " sp. ", vec)
  return(vec)
}

# ------------------------------------------------------------------------------
#' paste_
#' concatenate only non-empty strings from a vector of strings
#'
#' @param str_vec a vector of character strings or a data frame
#' @param sep character or string used as a separator, as in paste()
#'
#' @return
#' if str_vec is a vector, returns a string composed of the non-empty component
#' strings of str_vec separated by sep
#' if str_vec is a data frame, returns a vector of strings, each composed of the
#' non-empty component strings of a given row in str_vec
#'
#' @examples
#' str <- c("a","b","","c")
#' paste_(str)
#' 
#' df <- data.frame(a = c("1",""), b = c("2","3"), c = c("","4"))
#' paste_(df[,c("a","b")], sep = ",")
#' 
paste_ <- function(str_vec, sep = " "){
  if(is.null(nrow(str_vec))){ # test if the input is only one vector
    
    # get character strings from the input vector with > 0 characters (not the empty string)
    non_empty <- str_vec[nzchar(str_vec)]
    
    result <- paste(non_empty, collapse = sep)   
  }else{ # if input is a data frame
    result <- apply(str_vec, MARGIN = 1, FUN = function(x, sep){return(paste(x[nzchar(x)], collapse = sep))}, sep = sep)
  }      
  return(result)
}

#' QAQC_kicksets
#'
#' @param habitat data frame of kickset habitat data 
#' @param fish data frame of kickset fish data
#' @param site_lookup data frame of site information from database collections table
#' @param spp data frame of species name information from database taxonomy_lookup table
#'
#' @return character string of errors describing formatting issues detected in the fish and habitat data,
#' Individual errors are separated by HTML line breaks.
#'
QAQC_kicksets <- function(habitat, fish, site_lookup, spp){
  
  errs <- ""
  
  # check spreadsheets include all the necessary columns
  kickset_colnames <- c('Field.No.','Site.No.','Date','Kickset','SpeciesCode','Ad.count','YOY','Genus','species','genus_species','Formalin.total','Released.total','ETOH.clip','ETOH.whole','Total','SL.mm.','Notes','Sex')
  habitat_colnames <- c('Field..','Site.no.','Location','Date','Kickset','Sediment.Code','Moveable','J','WD','P','Depth','Velocity','No.fish','Depth.Code','Velocity.category','Pod.','Haul.length..m.','Notes')
  
  # necessary columns missing from input data
  missing_kickset_cols <- kickset_colnames[!kickset_colnames %in% colnames(fish)]
  missing_habitat_cols <- habitat_colnames[!habitat_colnames %in% colnames(habitat)]
  
  if(length(missing_kickset_cols) > 0){
    errs <- paste0(errs, paste("ERROR: Necessary columns missing from fish spreadsheet:", paste(missing_kickset_cols, collapse = ", ")))
  }
  
  if(length(missing_habitat_cols) > 0){
    errs <- paste0(errs, "<br/>", paste("ERROR: Necessary columns missing from habitat spreadsheet:", paste(missing_habitat_cols, collapse = ", ")))
  }
  
  # check if all kicksets are associated with sites
  if(any(is.na(fish[,"Site.No."]))){
    errs <- paste0(errs, "<br/>", "NA appears in site number column of fish spreadsheet.")
  }
  
  if(any(is.na(habitat[,"Site.no."]))){
    errs <- paste0(errs, "<br/>", "NA appears in site number column of habitat spreadsheet.")
  }
  
  # check if all kicksets are associated with dates
  if(any(is.na(fish[,"Date"]))){
    errs <- paste0(errs, "<br/>", "NA appears in Date column of fish spreadsheet.")
  }
  
  if(any(is.na(habitat[,"Date"]))){
    errs <- paste0(errs, "<br/>", "NA appears in Date column of habitat spreadsheet.")
  }
  
  # check if all kicksets are associated with field numbers
  if(any(is.na(fish[,"Field.No."]))){
    errs <- paste0(errs, "<br/>", "NA appears in field number column of fish spreadsheet.")
  }
  
  if(any(is.na(habitat[,"Field.."]))){
    errs <- paste0(errs, "<br/>", "NA appears in field number column of habitat spreadsheet.")
  }
  
  # check date formatting --------------------------------------------------------
  dates <- unique(habitat[,"Date"])
  
  # separate dates into component parts
  years <- as.integer(format(as.Date(dates), "%Y"))
  months <- as.integer(format(as.Date(dates), "%m"))
  days <- as.integer(format(as.Date(dates), "%d"))
  
  current_date <- Sys.Date()
  current_year <- as.integer(format(current_date, "%Y"))
  
  # check if years are missing or unreasonable
  if(any(is.na(years))){
    errs <- paste0(errs, "<br/>", paste0("Dates missing or improperly formatted in habitat spreadsheet: ",
                                        paste(unique(dates[is.na(years)]), collapse = ", ")))
  }else if(any(years < 1900 | years > current_year)){
    errs <- paste0(errs, "<br/>", paste0("Years in habitat spreadsheet outside reasonable range: ", 
                                        paste(unique(years[years < 1900 | years > current_year]), collapse = ", ")))
  }
  
  # check months
  if(any(is.na(months))){
    errs <- paste0(errs, "<br/>", paste0("Dates missing or improperly formatted in habitat spreadsheet: ",
                                        paste(unique(dates[is.na(months)]), collapse = ", ")))
  }else if(any(months < 1 | months > 12)){
    errs <- paste0(errs, "<br/>", paste0("Months in habitat column outside reasonable range: ", 
                                        paste(months[months < 1 | months > 12], collapse = ", ")))
  }
  
  # check days
  if(any(is.na(days))){
    errs <- paste0(errs, "<br/>", paste0("Dates missing or improperly formatted in habitat spreadsheet:",
                                        paste(unique(dates[is.na(days)]), collapse = ", ")))
  }else if(any(days < 1 | days > 31)){
    errs <- paste0(errs, "<br/>", paste0("Days in habitat column outside reasonable range: ", 
                                        paste(days[days < 1 | days > 31], collapse = ", ")))
  }
  
  # end date formatting ----------------------------------------------------------
  
  sites <- unique(habitat[,"Site.no."])
  field_nums <- unique(habitat[,"Field.."])
  
  if(length(field_nums) != nrow(unique(habitat[,c("Site.no.","Date")]))){
    errs <- paste0(errs, "<br/>", "Number of field numbers supplied does not match number of unique site and date combinations.")
  }
  
  if(all(sites %in% site_lookup[,"Og_Site"])){
    site_lookup <- site_lookup[site_lookup[,"Og_Site"] %in% sites,]
  }else{
    errs <- paste0(errs, "<br/>", paste0("Site number cannot be associated with coordinates: ", paste(unique(sites[!sites %in% site_lookup[,"Og_Site"]]), sep = ", ")))
  }
  
  # check species are present in the lookup table
  fish[,"genus_species"] <- format_species(fish[,"genus_species"])
  
  if(!all(fish[,"genus_species"] %in% spp)){
    errs <- paste0(errs, "<br/>", paste0("Species not present in database taxonomy_lookup table: ", 
                                        paste(unique(fish[!fish[,"genus_species"] %in% spp,"genus_species"]), collapse = ", ")))
  }
  
  # check integer columns of fish spreadsheet for non-integer characters
  integer_columns_fish <- c("Kickset", "Ad.count", "YOY", "Formalin.total", "Released.total", "ETOH.clip", "ETOH.whole", "Total", "SL.mm.")
  
  # true if any integer column contains non-integer values
  bad_integer_column <- FALSE
  
  for(col_name in integer_columns_fish){
    if(any(grepl("\\D", fish[,col_name]))){
      errs <- paste0(errs, "<br/>", paste("Non-integer or negative values present in",col_name,"column of fish spreadsheet,", 
                                         paste(unique(fish[grepl("\\D", fish[,col_name]),col_name]), collapse = ", ")))
      bad_integer_column <- TRUE
    }else{
      fish[,col_name] <- as.integer(fish[,col_name])
    }
  }
  
  if(!bad_integer_column){
    # check total counts add up to sum of integer count rows
    if(any(fish[,"Total"] != rowSums(fish[,c("Formalin.total","Released.total","ETOH.whole")]))){
      
      # get field numbers and kickset numbers for rows where counts don't match
      error_rows <- apply(fish[fish[,"Total"] != rowSums(fish[,c("Formalin.total","Released.total","ETOH.whole")]),
                               c("Field.no","Kickset")], MARGIN = 1, FUN = paste, sep = ",")
      
      errs <- paste0(errs, "<br/>", paste("Total column in fish spreadsheet does not equal sum of 
             'Formalin.total', 'Released.total', and 'ETOH.whole' cols at field numbers and kicksets:",
                                         error_rows))
    }
    
    # Convert NA's to 0's to avoid erros thrown when used in arithmetic operations
    fish[is.na(fish[,"Ad.count"]), "Ad.count"] <- 0
    fish[is.na(fish[,"YOY"]), "YOY"] <- 0
    
    if(any(fish[,"Total"] != rowSums(fish[,c("Ad.count","YOY")]))){
      
      # get field numbers and kickset numbers for rows where counts don't match
      error_rows <- apply(fish[fish[,"Total"] != rowSums(fish[,c("Ad.count","YOY")]),
                               c("Field.no","Kickset")], MARGIN = 1, FUN = paste, sep = ",")
      
      errs <- paste0(errs, "<br/>", paste("Total column in fish spreadsheet does not equal sum of 
             'Ad.count' and 'YOY' cols at field numbers and kicksets:",
                                         error_rows))
    }
  }
  
  
  # check all kickset numbers are integers
  if(any(is.na(as.integer(fish[,"Kickset"])))){
    errs <- paste0(errs, "<br/>", paste("Non-integer values present in Kickset number column of fish spreadsheet,", 
                                       paste(unique(fish[is.na(as.integer(fish[,"Kickset"])),"Kickset"]), collapse = ", ")))
  }
  
  # check all standard lengths are positive and numeric
  if(any(grepl("[^.0-9]", fish[,"SL.mm."]))){
    errs <- paste0(errs, "<br/>", paste("Non-numeric or negative values present in SL.mm. column of fish spreadsheet,", 
                                       paste(unique(fish[grepl("[^.0-9]", fish[,"SL.mm."]),"SL.mm."]), collapse = ", ")))
  }
  
  # columns in the habitat spreadsheet which should only contain numeric values
  numeric_columns_habitat <- c("Depth", "Velocity","Haul.length..m.")
  
  for(col_name in numeric_columns_habitat){
    
    # velocity column can be negative, others should not be negative
    if(col_name == "Velocity"){
      pattern <- "[^-.0-9]"
    }else{
      pattern <- "[^.0-9]"
    }
    
    if(any(grepl(pattern, habitat[,col_name]))){
      errs <- paste0(errs, "<br/>", paste("Non-numeric values present in",col_name,"column of habitat spreadsheet:", 
                                         paste(unique(habitat[grepl(pattern, habitat[,col_name]),col_name]), collapse = ", ")))
    }
  }
  
  # check all kickset numbers are integers
  if(any(is.na(as.integer(habitat[,"Kickset"])))){
    errs <- paste0(errs, "<br/>", paste("Non-integer values present in Kickset number column of habitat spreadsheet,", 
                                       paste(unique(habitat[is.na(as.integer(habitat[,"Kickset"])),"Kickset"]), collapse = ", ")))
  }
  
  # check 'Sex' column contains only two accepted values
  if(any(!fish[,"Sex"] %in% c("","M","F"))){
    errs <- paste0(errs, "<br/>", paste("Unaccepted values in 'Sex' column of fish spreadsheet,", 
                                       paste(unique(fish[!fish[,"Sex"] %in% c("","M","F"),"Sex"]), collapse = ", ")))
  }
  
  # check habitat columns have only accepted values
  if(any(grepl("[^1-5]", habitat[,"Sediment.Code"]))){
    errs <- paste0(errs, "<br/>", paste("Values out of range of accepted integers present in 'Sediment.Code' column of habitat spreadsheet,", 
                                       paste(unique(habitat[grepl("[^1-5]", habitat[,"Sediment.Code"]),"Sediment.Code"]), collapse = ", ")))
  }
  
  if(any(!habitat[,"Moveable"] %in% c("","M"))){
    errs <- paste0(errs, "<br/>", paste("Unaccepted values in 'Moveable' column of habitat spreadsheet,", 
                                       paste(unique(habitat[!habitat[,"Moveable"] %in% c("","M"),"Moveable"]), collapse = ", ")))
  }
  
  if(any(!habitat[,"J"] %in% c("","J"))){
    errs <- paste0(errs, "<br/>", paste("Unaccepted values in 'J' column of habitat spreadsheet,", 
                                       paste(unique(habitat[!habitat[,"J"] %in% c("","J"),"J"]), collapse = ", ")))
  }
  
  if(any(!habitat[,"WD"] %in% c("","WD"))){
    errs <- paste0(errs, "<br/>", paste("Unaccepted values in 'WD' column of habitat spreadsheet,", 
                                       paste(unique(habitat[!habitat[,"WD"] %in% c("","WD"),"WD"]), collapse = ", ")))
  }
  
  if(any(!habitat[,"P"] %in% c("","P"))){
    errs <- paste0(errs, "<br/>", paste("Unaccepted values in 'P' column of habitat spreadsheet,", 
                                       paste(unique(habitat[!habitat[,"P"] %in% c("","P"),"P"]), collapse = ", ")))
  }
  
  # check field numbers match in both spreadsheets
  missing_kickset_fnums <- unique(fish[!fish[,"Field.No."] %in% habitat[,"Field.."],"Field.No."])
  missing_habitat_fnums <- unique(habitat[!habitat[,"Field.."] %in% fish[,"Field.No."],"Field.."])
  
  if(length(missing_kickset_fnums) > 0){
    errs <- paste0(errs, "<br/>", paste0("ERROR: Field Numbers from fish spreadsheet missing from habitat spreadsheet: ", 
                                        paste(missing_kickset_fnums, collapse = ", ")))
  }
  
  if(length(missing_habitat_fnums) > 0){
    errs <- paste0(errs, "<br/>", paste0("ERROR: Field Numbers from habitat spreadsheet missing from fish spreadsheet: ", 
                                        paste(missing_habitat_fnums, collapse = ", ")))
  }
  
  return(errs)
} # end QAQC function
