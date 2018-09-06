# Documentation of Data

#' Example TC, AL032009 
#'
#' A data frame of all recorded time steps of a single TC, a slightly cleaned 
#' version than that provided by the National Oceanic and Atmospheric 
#' Administration's data set. "AL032009" means that the TC was in the 
#' Atlantic Basin (AL), the 3rd TC that year (03), in year 2009 (2009).
#'
#' @format A data frame with 69 rows and  variables:
#' \describe{
#'   \item{date}{Date time step recorded (format "YYYYmmdd")}
#'   \item{time}{Time of day time step was recorded, 
#'   (military time, e.g. 600, 1200, 1600, 1800, 0) in UTC (Universal Time 
#'   Coordinate) }
#'   \item{record_id}{Record Identifiers, L - eye cross coastline (landfall),
#'   others possible - see 
#'   \url{http://www.aoml.noaa.gov/hrd/hurdat/newhurdat-format.pdf}}
#'   \item{status}{type of TC, see details}
#'   \item{lat}{latitude of the eye of the storm}
#'   \item{long}{longitude of the eye of the storm}
#'  ...
#'   \item{max_sust_wind}{Maximum sustained wind (in knots) }
#'   \item{min_pressure}{Minimum Pressure (in millibars) }
#'   \item{wind_NE_34}{34 kt wind radii maximum extent in northeastern 
#'   quadrant (in nautical miles) }
#'   \item{wind_SE_34}{... in southeastern quadrant ...}    
#'   \item{wind_SW_34}{... in southwestern quadrant ...}   
#'   \item{wind_NW_34}{... in northwestern quadrant ...}   
#'   \item{wind_NE_50}{50 kt wind radii maximum extent in northeastern 
#'   quadrant (in nautical miles) }    
#'   \item{wind_SE_50}{... in southeastern quadrant ...}   
#'   \item{wind_SW_50}{... in southwestern quadrant ...}   
#'   \item{wind_NW_50}{... in northwestern quadrant ...}  
#'   \item{wind_NE_64}{– 64 kt wind radii maximum extent in northeastern 
#'   quadrant (in nautical miles) }    
#'   \item{wind_SE_64}{... in southeastern quadrant ...}    
#'   \item{wind_SW_64}{... in southwestern quadrant ...}    
#'   \item{wind_NW_64}{... in northwestern quadrant ...}  
#' }
#' 
#' @details
#' \code{status} has possible options:
#' \describe{
#'   \item{TD}{Tropical cyclone of tropical depression intensity (\eqn{<}
#'   34 knots) }
#'   \item{TS}{Tropical cyclone of tropical storm intensity (34-63 knots) }
#'   \item{HU}{Tropical cyclone of hurricane intensity (\eqn{\geq} 64 knots) }
#'   \item{EX}{Extratropical cyclone (of any intensity) }
#'   \item{SD}{– Subtropical cyclone of subtropical depression intensity 
#'   (\eqn{<} 34 knots) }
#'   \item{SS}{Subtropical cyclone of subtropical storm intensity 
#'   (\eqn{\geq} 34 knots) }
#'   \item{LO}{– A low that is neither a tropical cyclone, a subtropical 
#'   cyclone, nor an extratropical cyclone (of any intensity) }
#'   \item{WV}{Tropical Wave (of any intensity)}
#'   \item{DB}{– Disturbance (of any intensity) }
#' }
#' 
#' @source \url{http://www.aoml.noaa.gov/hrd/hurdat/hurdat2-1851-2015-070616.txt}
#' \url{http://www.aoml.noaa.gov/hrd/hurdat/newhurdat-format.pdf}
#' \url{http://www.aoml.noaa.gov/hrd/hurdat/Data_Storm.html}
"sample_tc"

#' Example List of 350 Simulated Curves, AL032009 
#'
#' A list of simulated curvesx, each element in the list is matrix
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
#' "AL032009" means that the TC was in the Atlantic Basin (AL), the 3rd TC that
#'  year (03), in year 2009 (2009).
#' 
#' @format string with TC name associated with \code{sample_tc} and the 
#' true TC who's first 3 points were used to create the simulated curves 
#' in \code{sample_sim}.
#'
"sample_tc_name"

#' Output of PB pipeline for sample TC 
#' 
#' This object is the result of running the PB papeline on the sample TC 
#' "AL032009", using the 350 simulations, which generated 350 Autoregressive
#' simulations with Kernel Death modelling for TC lysis.
#' 
#' @format list with 1 element, which name is "Auto_NoDeathRegs", which in turns
#' has 5 elements, "kde", "bubble_ci", "delta_ball", "convex_hull",
#' "depth_vector" and "time", which are the result of the PB pipeline run with
#' \code{sample_tc} as true TC and its simulations \code{sample_sim}.
#' 
"sample_output_pipeline"

#' Output of simulation process for TC "AL032009"
#' 
#' This object is the result of running the simulation curve for the TC
#' "AL032009". We first train the model by pulling the data from HURDAT and
#' we then simulate 2 curves per each combination of AR/nonAR model and Kernel/
#' logistic regression modeled lysis.
#' 
#' @format list with 1 element, named "AL032009", which in turns
#' has 4 elements, one for each combination of the of AR/nonAR model and 
#' Kernel/ logistic regression modeled lysis, each one of length 2 (2 curve
#' simulated).
#' 
"test_sims"
