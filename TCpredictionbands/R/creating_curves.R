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
#' @param path TC data frame, containing obs thus far in simulation
#' @param death_regs List of block-specific death regressions
#' @param bad_locations Names of blocks containing <= 1 TC death
#' @param death_rate 1 / mean length of TCs in train data
#'
#' @return Boolean for whether to stop growing TC
is_dead_inner <- function(path, death_regs, bad_locations, death_rate){
  
  # Number of TC observations at current point
  n_row <- nrow(path)
  
  # Current block, longitude, and latitude
  block <- path[n_row, ]$block
  current_long <- path$long[n_row]
  current_lat <- path$lat[n_row]

  # Predict probability of TC death at current point
  if (block %in% bad_locations) {
  	prob <- death_rate
  } else {
  	prob <- stats::predict(death_regs[[block]], type = "response",
  	                newdata = data.frame(path[n_row, ]))
  }
  
  # Simulate whether TC dies at current observation
  is_dead <- stats::rbinom(1,1,prob) == 1
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
#' @param path TC data frame, containing obs thus far in simulation
#' @param bearing_regs List of block-specific change in bearing regressions
#'
#' @return TC data frame, w/ simulated new bearing at current observation
bearing_inner <- function(path, bearing_regs){
  
  # Number of TC observations at current point
  n_row <- nrow(path)
  
  # Block of current observation
  block <- path[n_row, ]$block

  # Use block-specific regression to predict change in bearing at current obs
  bearing_change <- stats::predict(bearing_regs[[block]], 
                            newdata = data.frame(path[n_row, ]),
                            se.fit = T)
  
  # Add normal randomness to change in bearing
  path[n_row,'bearing_change'] <- bearing_change$fit +
  	 	                                stats::rnorm(n = 1, sd = bearing_change$se.fit)
  
  # Determine new bearing, based on old bearing and change in bearing
  path[n_row,'bearing'] <- path[(n_row - 1), 'bearing'] + 
  		                         path[n_row, 'bearing_change']
  
  # Ensure 0 <= bearing < 360
  while (path[n_row, 'bearing'] < 0) {
    path[n_row, 'bearing'] <- path[n_row, 'bearing'] + 360
  }
  while (path[n_row, 'bearing'] >= 360) {
    path[n_row, 'bearing'] <- path[n_row, 'bearing'] - 360
  }
  
  # Ensure -180 < bearing_change <= 180
  while (path[n_row, 'bearing_change'] <= -180) {
    path[n_row, 'bearing_change'] <- path[n_row, 'bearing_change'] + 360
  }
  while (path[n_row, ]['bearing_change'] > 180) {
    path[n_row, 'bearing_change'] <- path[n_row, 'bearing_change'] - 360
  }
  return(path)
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
#' @param path TC data frame, containing obs thus far in simulation
#' @param speed_regs List of block-specific change in speed regressions
#'
#' @return TC data frame, w/ simulated new speed at current observation
speed_inner <- function(path, speed_regs){
  
  # Number of TC observations at current point
  n_row <- nrow(path)
  
  # Block of current observation
  block <- path[n_row, ]$block

  # Use block-specific regression to predict change in speed at current obs
  speed_change <- stats::predict(speed_regs[[block]], 
                          newdata = data.frame(path[n_row, ]),
                          se.fit = T)
  
  # Add normal randomness to change in speed
  path[n_row, 'speed_change'] <- speed_change$fit + 
  	                                 stats::rnorm(n = 1, sd = speed_change$se.fit)
  
  # Determine new speed, based on old speed and change in speed
  path[n_row, 'speed'] <- path[(n_row - 1), 'speed'] + 
  	                          path[n_row, 'speed_change']
  
  return(path)
}

#' Project forward to new point
#' 
#' @description After simulating a new bearing and new speed, this function
#' projects forward to the new point.
#'
#' @details This function takes path with the simulated new bearing
#' and simulated new speed. It projects forward to the next point, assuming
#' that the TC maintains that bearing and speed for the next 6 hours.
#' Then it fills in the regression variables on the new point.
#'
#' @param path TC data frame, containing obs thus far in simulation, 
#' as well as bearing and speed on current obs
#'
#' @return TC data frame, with simulated position of next observation
update_curve_inner <- function(path){
  
  # Number of TC observations at current point
  n_row <- nrow(path)
  
  # Append empty new row to path
  path <- rbind(path, rep(NA, ncol(path)))
  
  # Project forward to new point. Append to path
  new_point <- geosphere::destPoint(p = c(path$long[n_row], path$lat[n_row]), 
                                    b = path$bearing[n_row], 
                                    d = path$speed[n_row]*6) # for 6 hours
  
  # Insert projected longitude/latitude as new point
  path[n_row + 1, 'long'] <- new_point[1, 'lon']
  path[n_row + 1, 'lat'] <- new_point[1, 'lat']
  
  # Append regression variables to new observation
  path[n_row + 1, 'bearing_prev'] <- path[n_row, 'bearing']
  path[n_row + 1, 'speed_prev'] <- path[n_row, 'speed']
  
  if (path[n_row + 1, 'bearing_prev'] < 180) {
    path[n_row + 1, 'east_west_prev'] <- "E"
  } else {
    path[n_row + 1, 'east_west_prev'] <- "W"
  }
  
  path[n_row + 1, 'bearing_change_lag1'] <- path[n_row, 'bearing_change']
  path[n_row + 1, 'speed_change_lag1'] <- path[n_row, 'speed_change']
  
  path[n_row + 1, 'block'] <- get_block(path[n_row + 1, 'long'], 
                                            path[n_row + 1, 'lat'],
                                            path[n_row + 1, 'east_west_prev'])
  path[n_row + 1, 'timestep'] <- path[n_row, 'timestep'] + 1

  return(path)
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
#' @param path Initial 2 or 3 (auto_ind = F or T, respectively) observations
#' of TC upon which to generate path
#' @param train_models List of trained models, generated from get_train_models
#' @param death_regs_ind Indicator, whether to use death regressions
#' @param auto_ind Indicator, whether bearing/speed regs are autoregressive
#'
#' @return Full, simulated path of TC as a column of longitudinal
#' coordinates and a column of latitudinal coordinates.
#' @export
generate_curve <- function(path, train_models, death_regs_ind, auto_ind){

  # Extract training model parameters
  if (auto_ind) {
    bearing_regs <- train_models$bearing_regs_auto
    speed_regs <- train_models$speed_regs_auto
  } else {
    bearing_regs <- train_models$bearing_regs_nonauto
    speed_regs <- train_models$speed_regs_nonauto
  }
  
  death_regs <- train_models$death_regs
  death_dens <- train_models$death_dens
  death_rate <- train_models$death_rate
  max_length <- train_models$max_length
  bad_locations <- train_models$bad_locations
  
  # If death_regs_ind = T, is_dead_inner simulates if TC dies at current time.
  # If death_regs_ind = F, is_dead is always FALSE. Then the (upper bound on) 
  # number of TC timesteps is drawn from kernel density of length of training 
  # TCs.
  # This is an upper bound because TC immediately dies if it goes out of bounds.
  if (death_regs_ind) {
    is_dead <- is_dead_inner(path, death_regs, bad_locations, death_rate)
  } else {
    is_dead <- FALSE
    max_length <- round(sample(death_dens$x, size = 1, prob = death_dens$y))
  }
  
  # Determine current timestep
  timestep <- 2 + 1*auto_ind

  # Simulate new points until TC dies (goes out of bounds, timestep exceeds
  # max_length, or is_dead_inner returns TRUE for death_regs_ind = T).
	while (!is_dead & timestep <= max_length) {
		
	  # Simulate new bearing and new speed. Project forward to new point.
	  path <- bearing_inner(path, bearing_regs)
		path <- speed_inner(path, speed_regs)
		path <- update_curve_inner(path)

		# For death_regs_ind = T, use is_dead_inner to simulate if TC dies at
		# current point.
		if (death_regs_ind) {
		  is_dead <- is_dead_inner(path, death_regs, bad_locations, death_rate)
		}
		
		# End TC if it goes into range w/o sufficient training data
		current_long <- path[nrow(path), ]$long
		current_lat <- path[nrow(path), ]$lat
		
		if (current_long < -107.3 | current_long > 10 | current_lat < 8 | 
		   current_lat > 60 | (current_long < -90 & current_lat > 40)) {
		  is_dead <- TRUE 
		}

		# Update timestep
		timestep <- timestep + 1
	}

  # Save curve as a table with a longitude column and a latitude column
	sim_df <- cbind(path$long, path$lat) 
	 
	return(sim_df)
}

#' Generate all simulated TC paths
#' 
#' @description The default settings of this function generate the simulated TCs 
#' used in the paper. Models are trained using the training TCs. Then TC paths
#' are simulated from the starting paths of the test TCs.
#'
#' @param train List of train TCs on which to train simulations
#' @param test List of test TCs from which to simulate new paths
#' @param remove_length_2 Boolean for whether to remove test TCs of length <= 2.
#' AR models need >= 3 observations to start simulation. (In train/test split
#' of the paper, the only TC of length 2 has been assigned to training.)
#' @param number_paths Number of paths to simulate for each test TC start
#' @param replicate Boolean for whether to replicate simulations of paper.
#' This reads in the train/test data from raw_data.Rdata, sets 
#' number_paths = 350, and sets a seed value.
#' @param verbose Boolean for whether to output progress
#'
#' @return List of simulated TC paths. The list length equals the number of test
#' TC, with one element named for each of the test TCs. Each of these elements 
#' is a list of length 4. These elements are a list of number_paths 
#' "Auto_DeathRegs" simulations, number_paths "Auto_NoDeathRegs" simulations, 
#' number_paths "NoAuto_DeathRegs" simulations, and number_paths 
#' "NoAuto_NoDeathRegs" simulations.
#' @export
generate_all <- function(train = NA, test = NA, remove_length_2 = T, 
                         number_paths = 350, replicate = T, verbose = T) {

  #Hack to set variables equal to NULL so that R CMD check does not flag them
  train_data <- test_data <- NULL
  
  # Read in data and set parameters if replicating paper
  if (replicate) {
    load("data/raw_data.Rdata")
    train <- train_data
    test <- test_data
    number_paths <- 350
    set.seed(1)
  }
  
  # if(remove_length_2), remove test TCs with length <= 2
  if (remove_length_2) {
    test <- test[unlist(lapply(test, nrow)) > 2]
  }
  
  # Train models on training data
  train_models <- get_train_models(train)
  
  # Create empty list of length 4 to store simulations for individual TC
  sim_setup <- vector("list", 4)
  names(sim_setup) <- c("Auto_DeathRegs", "Auto_NoDeathRegs", 
                        "NoAuto_DeathRegs", "NoAuto_NoDeathRegs")

  # Create list to store test simulations for all TCs
  test_sims <- vector("list", length(test))

  # Name test_sims after test TCs
  names(test_sims) <- names(test)
  
  # Set each element of test_sims equal to sim_setup
  test_sims <- lapply(test_sims, FUN = function(x) x = sim_setup)
  
  # MAIN LOOP for auto/non-auto and speed/non-speed combos
  tf <- c(T, F)

  # Loop over whether autoregressive terms are used in bearing/speed regs
  for (auto_ind in tf) {
    
    # Store starting points and set up regression variables
    # auto = F requires first two rows to get initial change in bearing/speed
    # auto = T requires first three rows to get initial change in lag 
    #        bearing/speed
    starts <- get_starting_points(test, auto = auto_ind)
    
    # Loop over whether logistic regressions are used to model lysis (death)
    for (death_regs_ind in tf) {
    
      # Set index for storage of simulations based on parameters.
      # Set up progress bar if verbose = T.
      if (auto_ind & death_regs_ind) {
        
        param_index <- 1
        if (verbose) {
          pb <- progress::progress_bar$new(
            format = "generating AR, death reg TCs [:bar] :current / :total", 
            total = length(test), clear = F, show_after = 0)
          invisible(pb$tick(0))
        }
        
      } else if (auto_ind & !death_regs_ind) {
       
        param_index <- 2 
        if (verbose) {
          pb <- progress::progress_bar$new(
            format = "generating AR, no death reg TCs [:bar] :current / :total", 
            total = length(test), clear = F, show_after = 0)
          invisible(pb$tick(0))
        }
        
      } else if (!auto_ind & death_regs_ind) {
        
        param_index <- 3
        if (verbose) {
          pb <- progress::progress_bar$new(
            format = paste0("generating non-AR, death reg TCs [:bar] ",
                            ":current / :total"), 
            total = length(test), clear = F, show_after = 0)
          invisible(pb$tick(0))
        }
        
      } else if (!auto_ind & !death_regs_ind) {
        
        param_index <- 4 
        if (verbose) {
          pb <- progress::progress_bar$new(
            format = paste0("generating non-AR, no death reg TCs [:bar] ",
                            ":current / :total"), 
            total = length(test), clear = F, show_after = 0)
          invisible(pb$tick(0))
        }
        
      }

      for (test_index in 1:length(test)) {
        
        # Update progress bar
        if (verbose) { pb$tick() } 
        
        # Empty list of length number_paths, to make number_paths curves
        paths <- vector("list", number_paths)

        # Get starting observations for all test TCs
        paths <- lapply(paths, FUN = function(x) return(starts[[test_index]]))
      
        # Generate number_paths curves from starting observations
        paths <- lapply(paths, 
                        FUN = function(path) 
                          generate_curve(path, train_models,
                                         death_regs_ind, auto_ind))
      
        # Round to closest tenth to match NOAA format
        paths <- lapply(paths, FUN = function(x) round_curve(x))
      
        # Set column names
        paths <- lapply(paths, FUN = function(x) {
                                          colnames(x) = c("long", "lat"); x})

        # Store simulations in test_sims
        test_sims[[test_index]][[param_index]] <- paths
      }
    }
  }
  return(test_sims)
}
