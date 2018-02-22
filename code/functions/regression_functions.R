suppressMessages(suppressWarnings(library(lubridate))) 
suppressMessages(suppressWarnings(library(geosphere)))

#' Sanitize TC data for analysis
#' 
#' @description This function takes in an original TC data frame. It saves
#' only the rows on the six-hour marks and checks that all sequential observations
#' are six hours apart. Then it returns a new data frame containing only the
#' latitude and longitude columns.
#'
#' @param df Original TC data frame
#'
#' @return Sanitized TC data frame, w/ six-hour latitude/longitude observations
#' @export
data_sanitize <- function(df){
  
  # Save obs collected on six-hour marks
  df <- subset(df, time %in% c("0", "600", "1200", "1800"))
  
  # Give an error if obs not all six hours apart
  Sys.setenv(TZ = "UTC")
  dates <- lubridate::ymd(df$date)
  lubridate::hour(dates) <- df$time / 100
  stopifnot(identical(as.numeric(diff(dates)), rep(6, length(dates) - 1)))
  
  # Data frame containing only lat and long of six hour obs
  df_san <- data.frame(matrix(nrow = nrow(df), ncol = 2))
  colnames(df_san) <- c("lat", "long")
  df_san$lat <- df$lat 
  df_san$long <- df$long
  
  return(df_san)
}

#' Shift vector forward by 1
#' 
#' @description Shift vector x forward by 1, so that x_shift[1] = NA,
#' x_shift[2] = x[1], ..., x_shift[n] = x[n-1]. 
#'
#' @param x Vector
#'
#' @return \code{x} vector shifted forward by 1
#' @export
shift <- function(x){
  x_shift <- c(NA, x[1:(length(x)-1)])
  return(x_shift)
}

#' Compute bearing and change in bearing
#'
#' @description Compute bearing and change in bearing between all consecutive 
#' (latitude, longitude) pairs. Also determine previous bearing at each
#' observation. 
#' 
#' @details At position i, the bearing (bear) is the bearing 
#' between (lat[i], long[i]) and (lat[i+1], long[i+1]). Bearing is calculated 
#' using geodesic path (shortest path on ellipsoid).
#' Formula for bearing on http://www.movable-type.co.uk/scripts/latlong.html
#' 
#' @param lat Latitude vector
#' @param long Longitude vector
#' 
#' @return List of bearing, bearing_change, and bearing_prev vectors.
#' \code{bearing}: bearing[i] = Bearing between (lat[i], long[i]) 
#' and (lat[i+1], long[i+1]).
#' \code{bearing_change}: bearing_change[i] = bearing[i] - bearing[i-1],
#' with corrections to ensure -180 < bearing_change <= 180.
#' \code{bearing_prev}: bearing_prev[i] = bearing[i-1].
#' 
#' @export
get_bearing <- function(lat, long){
  
  # Number of TC observations
  n <- length(long)
  
  # Compute bearings of all consecutive observations, from 1:2 to n-1:n
  bear <- geosphere::bearing(cbind(long[1:(n-1)], lat[1:(n-1)]),
                             cbind(long[2:n], lat[2:n]))
  
  # Ensure 0 <= bearing < 360
  bear[bear < 0] <- bear[bear < 0] + 360
  bear <- c(bear, NA)
  
  # Get bearing_change, with differences in consecutive bearings
  bearing_change <- c(NA, diff(bear))
  
  # Ensure -180 < bearing_change <= 180
  # If bearing_change <= -180, add 360
  inds <- which(bearing_change <= -180 & !is.na(bearing_change))
  bearing_change[inds] <- bearing_change[inds] + 360
  
  # If bearing_change > 180, subtract 360
  inds <- which(bearing_change > 180 & !is.na(bearing_change))
  bearing_change[inds] <- bearing_change[inds] - 360
  
  # Get bearing_prev, with bearings at previous observations
  bearing_prev <- shift(bear)
  
  return(list(bearing = bear, bearing_change = bearing_change, 
              bearing_prev = bearing_prev))
}

#' Determine whether bearing indicates east or west
#' 
#' @description If 0 <= bearing < 180, then bearing indicates eastward travel.
#' If 180 <= bearing < 360, then bearing indicates westward travel.
#'
#' @param bearing Vector of bearings, where bearing_prev[i] is the bearing
#' between (lat[i], long[i]) and (lat[i+1], long[i+1])
#' 
#' @return \code{eastwest[i]} stores "E" if \code{bearing[i]} indicates eastward
#' travel and "W" if \code{bearing[i]} indicates westward travel
#' @export
get_east_west <- function(bearing){
  
  # Traveling east if bearing < 180 (where 0 <= bearing < 360)
  east <- bearing < 180
  
  # Store "E" and "W" in east_west vector 
  east_west <- rep(NA, length(east))
  east_west[east] <- "E"
  east_west[!east] <- "W"

  return(east_west)
}

#' Compute speed and change in speed
#'
#' @description Determine speed (meters/hr) of TC and change in speed between all
#' consecutive (latitude, longitude) pairs. Also determine previous speed at
#' each observation.
#'
#' @details The speed at observation i (in meters per hour) is the distance 
#' between points i and i+1 (in meters) divided by 6 (hours). This is because
#' there are 6 hours between consecutive observations.
#'
#' @param lat Latitude vector
#' @param long Longitude vector
#'
#' @return List of speed, speed_change, and speed_prev vectors.
#' \code{speed}: Estimated speed at (lat[i], long[i]). Assumes travel at this
#' speed from (lat[i], long[i]) to (lat[i+1], long[i+1]).
#' \code{speed_change}: speed_change[i] = speed[i] - speed[i-1]
#' \code{speed_prev}: speed_prev[i] = speed[i-1]
#' @export
get_speed <- function(lat, long){
  
  # Number of TC observations
  n <- length(lat)
  
  # Distance between consecutive TC observations 
  dist <- geosphere::distGeo(cbind(long[1:(n-1)], lat[1:(n-1)]),
                             cbind(long[2:n], lat[2:n]))
  dist <- c(dist, NA)
  
  # Compute/append estimate of speed as distance (in meters) divided by 6 (hours)
  speed <- dist / 6
  
  # Append changes in speed and previous speeds
  speed_change <- c(NA, diff(speed))
  speed_prev <- c(NA, speed[1:(n-1)])
  return(list(speed = speed, speed_change = speed_change, speed_prev = speed_prev))
}

#' Determine block of TC observation
#' 
#' @details For an individual observation of a TC, block is determined by
#' longitude, latitude, and whether TC traveled east or west from previous
#' observation. Blocks were determined by creating 10 degree latitude by
#' 10 degree longitude regions and combining regions with few observations.
#'
#' @param long Longitude vector
#' @param lat Latitude vector
#' @param east_west_prev "E"/"W" vector, indicating whether TC traveled 
#' east or west from previous observation
#'
#' @return Vector of blocks of TC observations
#' @export
get_block <- function(long, lat, east_west_prev){
  
  # Initialize block. block stays NA for initial observations 
  # (with no info on whether TC traveled east/west from previous obs)
  block <- rep(NA, length(long))
  
  for(i in 1:length(block)){
    if(!is.na(east_west_prev[i]))
    {
      # Blocks for TCs that traveled west from previous observation
      if(long[i] < -90 && lat[i] < 20 && east_west_prev[i] == "W"){
        block[i] <- "A1_west"
      } else if(-90 <= long[i] && long[i] < -80 && lat[i] < 20 && east_west_prev[i] == "W"){
        block[i] <- "A2_west"
      } else if(-80 <= long[i] && long[i] < -70 && lat[i] < 20 && east_west_prev[i] == "W"){
        block[i] <- "A3_west"
      } else if(-70 <= long[i] && long[i] < -60 && lat[i] < 20 && east_west_prev[i] == "W"){
        block[i] <- "A4_west"
      } else if(-60 <= long[i] && long[i] < -50 && lat[i] < 20 && east_west_prev[i] == "W"){
        block[i] <- "A5_west"
      } else if(-50 <= long[i] && long[i] < -40 && lat[i] < 20 && east_west_prev[i] == "W"){
        block[i] <- "A6_west"
      } else if(-40 <= long[i] && long[i] < -30 && lat[i] < 20 && east_west_prev[i] == "W"){
        block[i] <- "A7_west"
      } else if(-30 <= long[i] && lat[i] < 20 && east_west_prev[i] == "W"){
        block[i] <- "A8_west"
      } else if(long[i] < -90 && 20 <= lat[i] && lat[i] < 30 && east_west_prev[i] == "W"){
        block[i] <- "B1_west"
      } else if(-90 <= long[i] && long[i] < -80 && 20 <= lat[i] && lat[i] < 30 && east_west_prev[i] == "W"){
        block[i] <- "B2_west"
      } else if(-80 <= long[i] && long[i] < -70 && 20 <= lat[i] && lat[i] < 30 && east_west_prev[i] == "W"){
        block[i] <- "B3_west"
      } else if(-70 <= long[i] && long[i] < -60 && 20 <= lat[i] && lat[i] < 30 && east_west_prev[i] == "W"){
        block[i] <- "B4_west"
      } else if(-60 <= long[i] && long[i] < -50 && 20 <= lat[i] && lat[i] < 30 && east_west_prev[i] == "W"){
        block[i] <- "B5_west"
      } else if(-50 <= long[i] && long[i] < -40 && 20 <= lat[i] && lat[i] < 30 && east_west_prev[i] == "W"){
        block[i] <- "B6_west"
      } else if(-40 <= long[i] && 20 <= lat[i] && lat[i] < 30 && east_west_prev[i] == "W"){
        block[i] <- "B7_west"
      } else if(long[i] < -90 && 30 <= lat[i] && lat[i] < 40 && east_west_prev[i] == "W"){
        block[i] <- "C1_west"
      } else if(-90 <= long[i] && long[i] < -80 && 30 <= lat[i] && lat[i] < 40 && east_west_prev[i] == "W"){
        block[i] <- "C2_west"
      } else if(-80 <= long[i] && long[i] < -70 && 30 <= lat[i] && lat[i] < 40 && east_west_prev[i] == "W"){
        block[i] <- "C3_west"
      } else if(-70 <= long[i] && long[i] < -60 && 30 <= lat[i] && lat[i] < 40 && east_west_prev[i] == "W"){
        block[i] <- "C4_west"
      } else if(-60 <= long[i] && long[i] < -50 && 30 <= lat[i] && lat[i] < 40 && east_west_prev[i] == "W"){
        block[i] <- "C5_west"
      } else if(-50 <= long[i] && long[i] < -40 && 30 <= lat[i] && lat[i] < 40 && east_west_prev[i] == "W"){
        block[i] <- "C6_west"
      } else if(long[i] < -40 && 40 <= lat[i] && east_west_prev[i] == "W"){
        block[i] <- "D1_west"
      } else if(-40 <= long[i] && 30 <= lat[i] && east_west_prev[i] == "W"){
        block[i] <- "D2_west"
      }
    
      # blocks for TCs that traveled east from previous observation
      if(long[i] < -80 && lat[i] < 20 && east_west_prev[i] == "E"){
        block[i] <- "A1_east"
      } else if(-80 <= long[i] && long[i] < -70 && lat[i] < 20 && east_west_prev[i] == "E"){
        block[i] <- "A2_east"
      } else if(-70 <= long[i] && long[i] < -60 && lat[i] < 20 && east_west_prev[i] == "E"){
        block[i] <- "A3_east"
      } else if(long[i] < -90 && 20 <= lat[i] && lat[i] < 30 && east_west_prev[i] == "E"){
        block[i] <- "B1_east"
      } else if(-90 <= long[i] && long[i] < -80 && 20 <= lat[i] && lat[i] < 30 && east_west_prev[i] == "E"){
        block[i] <- "B2_east"
      } else if(-80 <= long[i] && long[i] < -70 && 20 <= lat[i] && lat[i] < 30 && east_west_prev[i] == "E"){
        block[i] <- "B3_east"
      } else if(-70 <= long[i] && long[i] < -60 && 20 <= lat[i] && lat[i] < 30 && east_west_prev[i] == "E"){
        block[i] <- "B4_east"
      } else if(-60 <= long[i] && long[i] < -50 && lat[i] < 30 && east_west_prev[i] == "E"){
        block[i] <- "B5_east"
      } else if(-50 <= long[i] && long[i] < -40 && lat[i] < 30 && east_west_prev[i] == "E"){
        block[i] <- "B6_east"
      } else if(-40 <= long[i] && lat[i] < 30 && east_west_prev[i] == "E"){
        block[i] <- "B7_east"
      } else if(long[i] < -90 && 30 <= lat[i] && lat[i] < 40 && east_west_prev[i] == "E"){
        block[i] <- "C1_east"
      } else if(-90 <= long[i] && long[i] < -80 && 30 <= lat[i] && lat[i] < 40 && east_west_prev[i] == "E"){
        block[i] <- "C2_east"
      } else if(-80 <= long[i] && long[i] < -70 && 30 <= lat[i] && lat[i] < 40 && east_west_prev[i] == "E"){
        block[i] <- "C3_east"
      } else if(-70 <= long[i] && long[i] < -60 && 30 <= lat[i] && lat[i] < 40 && east_west_prev[i] == "E"){
        block[i] <- "C4_east"
      } else if(-60 <= long[i] && long[i] < -50 && 30 <= lat[i] && lat[i] < 40 && east_west_prev[i] == "E"){
        block[i] <- "C5_east"
      } else if(-50 <= long[i] && long[i] < -40 && 30 <= lat[i] && lat[i] < 40 && east_west_prev[i] == "E"){
        block[i] <- "C6_east"
      } else if(-40 <= long[i] && long[i] < -30 && 30 <= lat[i] && lat[i] < 40 && east_west_prev[i] == "E"){
        block[i] <- "C7_east"
      } else if(-30 <= long[i] && long[i] < -20 && 30 <= lat[i] && lat[i] < 40 && east_west_prev[i] == "E"){
        block[i] <- "C8_east"
      } else if(-60 <= long[i] && long[i] < -50 && 40 <= lat[i] && lat[i] < 50 && east_west_prev[i] == "E"){
        block[i] <- "D1_east"
      } else if(-50 <= long[i] && long[i] < -40 && 40 <= lat[i] && lat[i] < 50 && east_west_prev[i] == "E"){
        block[i] <- "D2_east"
      } else if(-40 <= long[i] && long[i] < -30 && 40 <= lat[i] && lat[i] < 50 && east_west_prev[i] == "E"){
        block[i] <- "D3_east"
      } else if(-30 <= long[i] && long[i] < -20 && 40 <= lat[i] && lat[i] < 50 && east_west_prev[i] == "E"){
        block[i] <- "D4_east"
      } else if(-20 <= long[i] && 30 <= lat[i] && lat[i] < 50 && east_west_prev[i] == "E"){
        block[i] <- "D5_east"
      } else if(long[i] < -70 && 40 <= lat[i] && east_west_prev[i] == "E"){
        block[i] <- "E1_east"
      } else if(-70 <= long[i] && long[i] < -60 && 40 <= lat[i] && east_west_prev[i] == "E"){
        block[i] <- "E2_east"
      } else if(-60 <= long[i] && long[i] < -40 && 50 <= lat[i] && east_west_prev[i] == "E"){
        block[i] <- "F1_east"
      } else if(-40 <= long[i] && long[i] < -20 && 50 <= lat[i] && east_west_prev[i] == "E"){
        block[i] <- "F2_east"
      } else if(-20 <= long[i] && 50 <= lat[i] && east_west_prev[i] == "E"){
        block[i] <- "F3_east"
      }
    }
  }
  
  return(block)
}

#' Appends regression variables
#' 
#' @description Appends columns used in bearing, speed, and death
#' regressions. These columns are lat, long, bearing, bearing_change,
#' bearing_prev, east_west_prev, speed, speed_change, speed_prev,
#' timestep, death, and block. Also includes bearing_change_lag1 and
#' speed_change_lag1 columns for autoregressive models (auto = T).
#'
#' @param df TC data frame
#' @param auto Boolean for whether to append lag change in bearing/speed
#' columns (for autoregressive models)
#'
#' @return TC data frame, including columns for performing bearing, speed,
#' and death regressions
#' @export
get_reg_df <- function(df, auto = T){
  
  # Convert data to lat and long columns on six-hour marks
  df <- data_sanitize(df)
  
  # Append bearing, bearing_change, and bearing_prev
  bearing_cols <- get_bearing(df$lat, df$long)
  df$bearing <- bearing_cols$bearing
  df$bearing_change <- bearing_cols$bearing_change
  df$bearing_prev <- bearing_cols$bearing_prev
  
  # Append east_west_prev
  df$east_west_prev <- get_east_west(df$bearing_prev)
  
  # Append speed, speed_change, and speed_prev
  speed_cols <- get_speed(df$lat, df$long)
  df$speed <- speed_cols$speed
  df$speed_change <- speed_cols$speed_change
  df$speed_prev <- speed_cols$speed_prev
  
  # Append timestep
  df$timestep <- 1:nrow(df)
  
  # Append death indicator column (1 on final observation)
  df$death <- c(rep(0, nrow(df) - 1), 1)
  
  # Append block
  df$block <- get_block(df$long, df$lat, df$east_west_prev)
  
  # For AR models, append bearing_change_lag1 and speed_change_lag1
  if(auto){
    df$bearing_change_lag1 <- shift(df$bearing_change)
    df$speed_change_lag1 <- shift(df$speed_change)
  }
  return(df)
}

#' Determine bearing and speed regressions
#' 
#' @description Compute block-specific regression models for change in
#' bearing and change in speed
#'
#' @param dflist_unlist All TC observations, including regression columns, 
#' as a single data frame
#' @param auto Boolean for whether to compute autoregressive models
#'
#' @return List of bearing_regs and speed_regs.
#' \code{bearing_regs}: Block-specific regressions for change in bearing.
#' \code{speed_regs}: Block-specific regressions for change in speed.
#' @export
get_bearing_speed_regs <- function(dflist_unlist, auto){
  
  if(auto){
    # Remove obs w/o lag change in bearing (or speed)
    dflist_unlist <- subset(dflist_unlist, !is.na(bearing_change_lag1))
    
    # Store a separate data frame for each block
    dflist_blocks <- split(dflist_unlist, f = dflist_unlist$block)
    
    # Block-specific change in bearing regs
    bearing_regs <- lapply(dflist_blocks, FUN = function(x) 
      return(lm(bearing_change ~ lat + long + bearing_prev + speed_prev
                + bearing_change_lag1, 
                data = x)))
    
    # Block-specific change in speed regs
    speed_regs <- lapply(dflist_blocks, FUN = function(x) 
      return(lm(speed_change ~ lat + long + bearing_prev + speed_prev
                + speed_change_lag1, 
                data = x)))
    
  } else {
    # Remove obs w/o change in bearing (or speed)
    dflist_unlist <- subset(dflist_unlist, !is.na(bearing_change))
    
    # Store a separate data frame for each block
    dflist_blocks <- split(dflist_unlist, f = dflist_unlist$block)
    
    # Block-specific change in bearing regs
    bearing_regs <- lapply(dflist_blocks, FUN = function(x) 
      return(lm(bearing_change ~ lat + long + bearing_prev + speed_prev, 
                data = x)))
    
    # Block-specific change in speed regs
    speed_regs <- lapply(dflist_blocks, FUN = function(x) 
      return(lm(speed_change ~ lat + long + bearing_prev + speed_prev, 
                data = x)))
  }
  
  return(list(bearing_regs, speed_regs))
}

#' Train models for TC simulations
#' 
#' @description Fit bearing, speed, and death models for TC simulations
#' 
#' @details Models fit on training TC data: 
#' \texttt{bearing_regs_auto}: block-specific AR models for bearing
#' \texttt{speed_regs_auto}: block-specific AR models for speed
#' \texttt{bearing_regs_nonauto}: block-specific non-AR models for bearing
#' \texttt{speed_regs_nonauto}: block-specific non-AR models for speed
#' \texttt{death_regs}: block-specific logistic reg models for TC death
#' \texttt{death_dens}: kernel density for TC death times
#' \texttt{death_rate}: 1/mean(train TC lengths). This is the MLE assuming an
#' exponential distribution on TC length. 
#' \texttt{max_length}: max length of training TCs
#' \texttt{bad_locations}: blocks with <= 1 death. Will use death_rate instead
#' of death_regs in these blocks.
#' 
#' @param train List of train TCs
#'
#' @return List containing the following trained models: bearing_regs_auto, 
#' speed_regs_auto, bearing_regs_nonauto, speed_regs_nonauto, death_regs, 
#' death_dens, death_rate, max_length, bad_locations. (See details section.)
#' @export
get_train_models <- function(train){
  
  # Append path regression variables to training data (auto = T for most general)
  train <- lapply(train, FUN = get_reg_df, auto = T)
  
  # Put training data in a single data frame
  train_unlist <- do.call("rbind", train) 
  
  # Fit block-specific bearing and speed regs for AR models
  bearing_speed_regs_auto <- get_bearing_speed_regs(train_unlist, auto = T)
  bearing_regs_auto <- bearing_speed_regs_auto[[1]]
  speed_regs_auto <- bearing_speed_regs_auto[[2]]
  
  # Fit block-specific bearing and speed regs for AR models
  bearing_speed_regs_nonauto <- get_bearing_speed_regs(train_unlist, auto = F)
  bearing_regs_nonauto <- bearing_speed_regs_nonauto[[1]]
  speed_regs_nonauto <- bearing_speed_regs_nonauto[[2]]
    
  # Get block specific lysis regressions 
  train_blocks <- split(train_unlist, f = train_unlist$block)
  death_regs <- lapply(train_blocks, 
                       FUN = function(x) return(glm(death ~ lat + long + 
                                                    bearing_prev + speed_prev + 
                                                    timestep,
                                                    family = binomial, data = x)))  
  
  # Fit kernel density to TC death times
  death_times <- sapply(train, FUN = function(x) nrow(x))
  death_dens <- density(death_times, bw = bw.nrd(death_times), kernel = "gaussian")
  
  # Get death rate and max observed tropical cyclone length
  lengths <- unlist(lapply(train, FUN = nrow))
  death_rate <- 1 / mean(lengths)
  max_length <- max(lengths)
    
  # Store blocks with <= 1 death. We will use overall death rate (instead of regs)
  # to model death in these blocks.
  bad_locations <- which(sapply(train_blocks, function(x) sum(x$death) <= 1))
  
  # Store train_models as a named list
  train_models <- list(bearing_regs_auto, speed_regs_auto, bearing_regs_nonauto,
                       speed_regs_nonauto, death_regs, death_dens, death_rate,
                       max_length, bad_locations)
  
  names(train_models) <- c("bearing_regs_auto", "speed_regs_auto", 
                           "bearing_regs_nonauto", "speed_regs_nonauto", 
                           "death_regs", "death_dens", "death_rate",
                           "max_length", "bad_locations")
    
  return(train_models)
}

#' Set up initial TC starting points
#'
#' @description Takes in a list of validation TC data frames. Transforms
#' data frames into starting observations for simulating new TCs. 
#' Saves starting 2/3 observations and appends regression columns.
#'
#' @param dfval List of validation TC data frames
#' @param auto Boolean indicating whether to set up data frames for 
#' autoregression framework
#'
#' @return List of data frames containing initial TC observations,
#' as well as regression variables
#' @export
get_starting_points <- function(dfval, auto = T){
  
  # Sanitize data frame into six-hour increment lat/long pairs
  dfval <- lapply(dfval, 
                  FUN = function(x) subset(x, time %in% c("0", "600", "1200", "1800")))

  # Save first 3 observations for AR, first 2 observations for non-AR
  if(auto){
    starts <- lapply(dfval, FUN = function(x) return(rbind(x[1, ], x[2, ], x[3, ])))
  } else {
    starts <- lapply(dfval, FUN = function(x) return(rbind(x[1, ], x[2, ])))
  }
  
  # Append bearing, speed, and death regression columns
  starts <- lapply(starts, FUN = get_reg_df, auto = auto)
  
  return(starts)
}

#' Round long/lat to one decimal point 
#' 
#' @description Rounds longitude and latitude degrees to closest tenth 
#' to match NOAA format
#'
#' @param curve_df Simulated TC data frame containing only a longitude column 
#' and a latitude column
#'
#' @return Simulated TC data frame, with rounded longitude/latitude columns 
#' @export
round_curve <- function(curve_df){
  return(cbind(round(curve_df[ ,1],1), round(curve_df[ ,2],1)))
}

