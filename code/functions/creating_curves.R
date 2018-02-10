#' Simulate whether to stop growing TC
#' 
#' @description For regression approach to TC deaths, this function simulates
#' whether to stop growing TC. 
#' 
#' @details If using block-specific death regressions to determine TC death, 
#' this function determines whether to stop growing TC. If block contains >= 2 
#' TC deaths, most recent observation characteristics are plugged into
#' block-specific regression to predict death probability. If block contains <= 1 
#' TC death, predicted death probability is 1 / mean length of training TCs.
#' (This is the MLE under an exponential distributional assumption on TC length.)
#' Decision of whether to stop TC depends on a draw from a Bernoulli distribution
#' with probability equal to the predicted death probability.
#'
#' @param curve_df TC data frame, containing obs thus far in simulation
#' @param death_regs List of block-specific death regressions
#' @param bad_locations Names of blocks containing <= 1 TC death
#' @param death_rate 1 / mean length of TCs in train data
#'
#' @return Boolean for whether to stop growing TC
is_dead_inner <- function(curve_df, death_regs, bad_locations, death_rate){
  
  # Number of TC observations at current point
  n_row <- nrow(curve_df)
  
  # Current block, longitude, and latitude
  block <- curve_df[n_row, ]$block
  current_long <- curve_df$long[n_row]
  current_lat <- curve_df$lat[n_row]

  # Predict probability of TC death at current point
  if(block %in% bad_locations){
  	prob <- death_rate
  } else {
  	prob <- predict(death_regs[[block]], type = "response",
  	                newdata = data.frame(curve_df[n_row, ]))
  }
  
  # Simulate whether TC dies at current observation
  is_dead <- rbinom(1,1,prob) == 1
  return(is_dead)
}

#' Simulate change in bearing and bearing
#' 
#' @description This function updates the bearing of the TC by predicting the 
#' change in bearing. 
#' 
#' @details At the current observation,  block-specific regression 
#' models predict change in bearing. Then normal random noise with mean 0 and
#' standard dev equal to the predicted standard error is added to the change in
#' bearing. The new bearing is determined by adding the old bearing and the
#' change in bearing.
#'
#' @param curve_df TC data frame, containing obs thus far in simulation
#' @param bearing_regs List of block-specific change in bearing regressions
#'
#' @return TC data frame, w/ simulated new bearing at current observation
bearing_inner <- function(curve_df, bearing_regs){
  
  # Number of TC observations at current point
  n_row <- nrow(curve_df)
  
  # Block of current observation
  block <- curve_df[n_row, ]$block

  # Use block-specific regression to predict change in bearing at current obs
  bearing_change <- predict(bearing_regs[[block]], 
                            newdata = data.frame(curve_df[n_row, ]),
                            se.fit = T)
  
  # Add normal randomness to change in bearing
  curve_df[n_row,'bearing_change'] <- bearing_change$fit +
  	 	                                rnorm(n = 1, sd = bearing_change$se.fit)
  
  # Determine new bearing, based on old bearing and change in bearing
  curve_df[n_row,'bearing'] <- curve_df[(n_row-1), 'bearing'] + 
  		                         curve_df[n_row, 'bearing_change']
  
  # Ensure 0 <= bearing < 360
  while(curve_df[n_row, 'bearing'] < 0){
    curve_df[n_row, 'bearing'] <- curve_df[n_row, 'bearing'] + 360
  }
  while(curve_df[n_row, 'bearing'] >= 360){
    curve_df[n_row, 'bearing'] <- curve_df[n_row, 'bearing'] - 360
  }
  
  # Ensure -180 < bearing_change <= 180
  while(curve_df[n_row, 'bearing_change'] <= -180){
    curve_df[n_row, 'bearing_change'] <- curve_df[n_row, 'bearing_change'] + 360
  }
  while(curve_df[n_row, ]['bearing_change'] > 180){
    curve_df[n_row, 'bearing_change'] <- curve_df[n_row, 'bearing_change'] - 360
  }
  return(curve_df)
}

#' Simulate change in speed and speed
#' 
#' @description This function updates the speed of the TC by predicting the 
#' change in speed. 
#' 
#' @details At the current observation, block-specific regression 
#' models predict change in speed. Then normal random noise with mean 0 and
#' standard dev equal to the predicted standard error is added to the change in
#' speed. The new speed is determined by adding the old speed and the
#' change in speed.
#'
#' @param curve_df TC data frame, containing obs thus far in simulation
#' @param speed_regs List of block-specific change in speed regressions
#'
#' @return TC data frame, w/ simulated new speed at current observation
speed_inner <- function(curve_df, speed_regs){
  
  # Number of TC observations at current point
  n_row <- nrow(curve_df)
  
  # Block of current observation
  block <- curve_df[n_row, ]$block

  # Use block-specific regression to predict change in speed at current obs
  speed_change <- predict(speed_regs[[block]], 
                          newdata = data.frame(curve_df[n_row, ]),
                          se.fit = T)
  
  # Add normal randomness to change in speed
  curve_df[n_row, 'speed_change'] <- speed_change$fit + 
  	                                 rnorm(n = 1, sd = speed_change$se.fit)
  
  # Determine new speed, based on old speed and change in speed
  curve_df[n_row, 'speed'] <- curve_df[(n_row-1), 'speed'] + 
  	                          curve_df[n_row, 'speed_change']
  
  return(curve_df)
}

#' Project forward to new point
#' 
#' @description After simulating a new bearing and new speed, this function
#' projects forward to the new point.
#'
#' @details This function takes curve_df with the simulated new bearing
#' and simulated new speed. It projects forward to the next point, assuming
#' that the TC maintains that bearing and speed for the next 6 hours.
#' Then it fills in the regression variables on the new point.
#'
#' @param curve_df TC data frame, containing obs thus far in simulation, 
#' as well as bearing and speed on current obs
#'
#' @return TC data frame, with simulated position of next observation
update_curve_inner <- function(curve_df){
  
  # Number of TC observations at current point
  n_row <- nrow(curve_df)
  
  # Append empty new row to curve_df
  curve_df <- rbind(curve_df, rep(NA, ncol(curve_df)))
  
  # Project forward to new point. Append to curve_df.
  new_point <- destPoint(p = c(curve_df$long[n_row], curve_df$lat[n_row]), 
                         b = curve_df$bearing[n_row], 
                         d = curve_df$speed[n_row]*6) # for 6 hours
  
  # Insert projected longitude/latitude as new point
  curve_df[n_row + 1, 'long'] <- new_point[1, 'lon']
  curve_df[n_row + 1, 'lat'] <- new_point[1, 'lat']
  
  # Append regression variables to new observation
  curve_df[n_row + 1, 'bearing_prev'] <- curve_df[n_row, 'bearing']
  curve_df[n_row + 1, 'speed_prev'] <- curve_df[n_row, 'speed']
  
  if(curve_df[n_row + 1, 'bearing_prev'] < 180){
    curve_df[n_row + 1, 'east_west_prev'] <- "E"
  } else {
    curve_df[n_row + 1, 'east_west_prev'] <- "W"
  }
  
  curve_df[n_row + 1, 'bearing_change_lag1'] <- curve_df[n_row, 'bearing_change']
  curve_df[n_row + 1, 'speed_change_lag1'] <- curve_df[n_row, 'speed_change']
  
  curve_df[n_row + 1, 'block'] <- get_block(curve_df[n_row + 1, 'long'], 
                                            curve_df[n_row + 1, 'lat'],
                                            curve_df[n_row + 1, 'east_west_prev'])
  curve_df[n_row + 1, 'timestep'] <- curve_df[n_row, 'timestep'] + 1

  return(curve_df)
}


#' Simulate TC path from initial observations
#' 
#' @description This function simulates a TC path sequentially, starting
#' with the initial observations. 
#' 
#' @details At each time step, block-specific regression functions determine 
#' change in TC bearing/speed. Using these values, the function projects forward 
#' to the next point. The user specifies whether the change in bearing/speed 
#' functions are autoregressive or not (auto_ind = T or F). The user also
#' specifies whether to use block-specific death regressions or draws from overall 
#' kernel density to model death of TC (death_regs_ind = T or F).
#'
#' @param curve_df Initial 2 (auto_ind = F) or 3 (auto_ind = T) rows of the TC path
#' @param bearing_regs List of block-specific change in bearing regressions
#' @param speed_regs List of block-specific change in speed regressions
#' @param death_regs List of block-specific death regressions
#' @param max_length Max length of TCs in training data
#' @param bad_locations Names of blocks containing <= 1 TC death
#' @param death_rate 1 / mean length of TCs in train data
#' @param death_dens Kernel density estimate of TC death times
#' @param death_regs_ind Indicator, whether to use death regressions
#' @param auto_ind Indicator, whether bearing/speed regs are autoregressive
#'
#' @return Full, simulated path of TC as a column of longitudinal
#' coordinates and a column of latitudinal coordinates.
#' @export
generate_curve <- function(curve_df, bearing_regs, speed_regs, death_regs,
                           max_length, bad_locations, death_rate, death_dens, 
                           death_regs_ind, auto_ind){

  # If death_regs_ind = T, is_dead_inner simulates if TC dies at current time.
  # If death_regs_ind = F, is_dead is always FALSE. Then the (upper bound on) 
  # number of TC timesteps is drawn from kernel density of length of training TCs.
  # This is an upper bound because TC immediately dies if it goes out of bounds.
  if(death_regs_ind){
    is_dead <- is_dead_inner(curve_df, death_regs, bad_locations, death_rate)
  } else {
    is_dead <- FALSE
    max_length <- round(sample(death_dens$x, size = 1, prob = death_dens$y))
  }
  
  # Determine current timestep
  timestep <- 2 + 1*auto_ind

  # Simulate new points until TC dies (goes out of bounds, timestep exceeds
  # max_length, or is_dead_inner returns TRUE for death_regs_ind = T).
	while(!is_dead & timestep <= max_length){
		
	  # Simulate new bearing and new speed. Project forward to new point.
	  curve_df <- bearing_inner(curve_df, bearing_regs)
		curve_df <- speed_inner(curve_df, speed_regs)
		curve_df <- update_curve_inner(curve_df)

		# For death_regs_ind = T, use is_dead_inner to simulate if TC dies at
		# current point.
		if(death_regs_ind){
		  is_dead <- is_dead_inner(curve_df, death_regs, bad_locations, death_rate)
		}
		
		# End TC if it goes into range w/o sufficient training data
		current_long <- curve_df[nrow(curve_df), ]$long
		current_lat <- curve_df[nrow(curve_df), ]$lat
		
		if(current_long < -107.3 | current_long > 10 | current_lat < 8 | 
		   current_lat > 60 | (current_long < -90 & current_lat > 40)){
		  is_dead <- TRUE 
		}

		# Update timestep
		timestep <- timestep + 1
	}

  # Save curve as a table with a longitude column and a latitude column
	sim_df <- cbind(curve_df$long, curve_df$lat) 
	 
	return(sim_df)
}
