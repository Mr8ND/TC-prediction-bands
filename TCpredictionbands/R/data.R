# Documentation of Data

#' Example TC, AL032009 
#'
#' A data frame of all recorded time steps of a single TC, a slightly cleaned 
#' version than that provided by the National Oceanic and Atmospheric 
#' Administration's data set.
#'
#' @format A data frame with 69 rows and  variables:
#' \describe{
#'   \item{date}{Date time step recorded (format "YYYYmmdd")}
#'   \item{time}{Time of day time step was recorded, 
#'   (military time, e.g. 600, 1200, 1600, 1800, 0) }
#'   ...
#'   \item{lat}{latitude of the eye of the storm}
#'   \item{long}{longitude of the eye of the storm}
#'   ...
#' }
#' @source \url{http://www.aoml.noaa.gov/hrd/hurdat/hurdat2-1851-2015-070616.txt}
#' \url{http://www.aoml.noaa.gov/hrd/hurdat/Data_Storm.html}
"sample_tc"

#' Example List of 350 Simulated Curves, AL032009 
#'
#' A list of simulated curves, each element in the list is matrix
#' (with different length), given the format below.
#'  
#' Created by using the first 3 points of \code{sample_tc}, with
#' name AL032009.
#' 
#' This simulation was created using the Autoregressive change in bearing and
#' speed models with Kernel-based Lysis (death) models.
#'
#' @format A list of simulated curves, each element is a matrix with format:
#' \describe{
#'   \item{long}{longitude of the eye of the simulated curve}
#'   \item{lat}{latitude of the eye of the simulated curve}
#' }
"sample_sim"

#' Name of sample TC, AL032009 
#' 
#' @format string with TC name associated with \code{sample_tc} and the 
#' true TC who's first 3 points were used to create the simulated curves 
#' in \code{sample_sim}.
#'
"sample_tc_name"