suppressMessages(suppressWarnings(library(stringr)))
suppressMessages(suppressWarnings(library(splitstackshape)))

#' Convert latitude hemispheres to pos/neg
#' 
#' @description If latitude ends in "N", drop "N" and convert latitude to numeric.
#' If latitude ends in "S", drop "S" and convert latitude to negative numeric.
#'
#' @param df TC data frame, where lat column has N/S at end
#'
#' @return Latitude column, converted to numeric
convert_lat <- function(df){
  lat <- as.character(df$lat)
  lat <- as.numeric(substr(lat, 1, nchar(lat) - 1)) * 
                    (1 - 2*I(substr(lat, nchar(lat), nchar(lat))=="S"))
  return(lat)
}

#' Convert longitude hemispheres to pos/neg
#' 
#' @description If longitude ends in "E", drop "E" and convert longitude to numeric.
#' If longitude ends in "W", drop "W" and convert longitude to negative numeric.
#'
#' @param df TC data frame, where long column has E/W at end
#'
#' @return Longitude column, converted to numeric
convert_long <- function(df){
  long <- as.character(df$long)
  long <- as.numeric(substr(long, 1, nchar(long) - 1)) * 
                    (1 - 2*I(substr(long, nchar(long), nchar(long))=="W"))
  long[long <= -180] <- round(long[long <= -180] + 360, 1)
  return(long)
}

#' Pull TC data from HURDAT database
#'
#' @description This function reads in data from the online HURDAT database and
#' converts it into a list of data frames, with one data frame per TC. 
#' Only data on TCs from 1951 onward are returned.
#'
#' @return TC data as a list of data frames
#' @export
pull_data <- function(){
  # Read hurricane data from website
  hurdat <- readLines('http://www.aoml.noaa.gov/hrd/hurdat/hurdat2-1851-2016-apr2017.txt')

  # Get entries w/ 3 commas in line (signifying a TC title)
  tc_indices <- which(stringr::str_count(hurdat, ",") == 3)

  # Get names of TCs
  tc_names <- stringr::str_extract(hurdat[tc_indices], "[^,]+")

  # Get order of first TC in year >= 1951
  first_tc <- which(substr(tc_names, 5, 8) >= 1951)[1]

  # Only save TCs in years >= 1951
  hurdat <- hurdat[tc_indices[first_tc]:length(hurdat)]

  # Get entries w/ 3 commas in line (signifying a TC title)
  tc_indices <- which(str_count(hurdat, ",") == 3)
  
  # Get names of TCs in years >= 1951
  tc_names <- stringr::str_extract(hurdat[tc_indices], "[^,]+")

  # Split data into list with one entry for each TC's data, named by TC
  tc_list <- split(hurdat, findInterval(1:length(hurdat), tc_indices))
  names(tc_list) <- tc_names

  # Remove first entry (title) of each TC vector in list
  tc_list <- lapply(tc_list, FUN = function(x) x[2:length(x)])

  # Modify TC vectors into data frames
  tc_list <- lapply(tc_list, 
                    FUN = function(x) splitstackshape::cSplit(data.frame(x), 
                                             splitCols = colnames(data.frame(x)), 
                                             sep = ","))

  # TC data frame column names
  cols <- c("date", "time", "record_id", "status", "lat", "long", "max_sust_wind", 
            "min_pressure", "wind_NE_34", "wind_SE_34", "wind_SW_34", "wind_NW_34", 
            "wind_NE_50", "wind_SE_50", "wind_SW_50", "wind_NW_50", "wind_NE_64", 
            "wind_SE_64", "wind_SW_64", "wind_NW_64")

  # Apply column names to list of TC data frames
  tc_list <- lapply(tc_list, setNames, cols)

  # Convert lat/long to numeric, taking hemisphere into account
  tc_list <- lapply(tc_list, FUN = function(x) {x$lat <- convert_lat(x); x})
  tc_list <- lapply(tc_list, FUN = function(x) {x$long <- convert_long(x); x})

  # Correct a typo
  tc_list$AL041956[37,'long'] <- 16

  return(tc_list)
}

