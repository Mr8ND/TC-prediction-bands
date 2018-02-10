library(geosphere)
library(plyr)
library(lubridate)

functions_loc = "code/functions/"
source(paste0(functions_loc, "Regression_functions.R")) 
source(paste0(functions_loc, "creating_curves.R"))
source(paste0(functions_loc, "read_data_source.R"))

#' Generate all simulated TC paths
#' 
#' @description The default settings of this function generate the simulated TCs 
#' used in the paper. Models are trained using the training TCs. Then TC paths
#' are simulated from the starting paths of the test TCs.
#'
#' @param number_paths Number of paths to simulate for each test TC start
#' @param seed Seed value for replicable results
#' @param verbose Boolean for whether to output progress
#'
#' @return List of simulated TC paths. The list length equals the number of test
#' TC, with one element named for each of the test TCs. Each of these elements is a 
#' list of length 4. These elements are a list of number_paths "Auto_DeathRegs" 
#' simulations, number_paths "Auto_NoDeathRegs" simulations, number_paths 
#' "NoAuto_DeathRegs" simulations, and number_paths "NoAuto_NoDeathRegs" simulations.
#' @export
generate_all <- function(number_paths = 1000, seed = 1, verbose = T){
  
  # Read in data --------------------------------------------
  
  # Pull data from HURDAT website
  tc_list <- pull_data()
  
  # Split data into train and test sets
  train_test <- split_train_test(tc_list, train_prop = 0.7, reproduce = T)
  train <- train_test[[1]]
  test <- train_test[[2]]
  
  # Create objects to store simulations on test TCs --------------------- 

  # Append path regression variables to training data (auto = T for most general)
  train <- lapply(train, FUN = get_reg_df, auto = T)

  # Create empty list of length 4 to store simulations for individual TC
  sim_setup <- vector("list", 4)
  names(sim_setup) <- c("Auto_DeathRegs", "Auto_NoDeathRegs", 
                        "NoAuto_DeathRegs", "NoAuto_NoDeathRegs")

  # Create list to store test simulations for all TCs
  test_sims <- vector("list", length(test))

  # Set each element of test_sims equal to sim_setup
  test_sims <- lapply(test_sims, FUN = function(x) x = sim_setup)

  # MAIN LOOP for auto/non-auto and speed/non-speed combos ----------------

  set.seed(seed)
  
  tf <- c(T, F)

  # Loop over whether autoregressive terms are used in bearing/speed regs
  for(auto_ind in tf){
      
    # Block-specific bearing/speed regressions ------------------
    
    train_unlist <- do.call("rbind", train) 
    bearing_speed_regs <- get_bearing_speed_regs(train_unlist, auto = auto_ind)
    bearing_regs <- bearing_speed_regs[[1]]
    speed_regs <- bearing_speed_regs[[2]]
    
    # Block-specific lysis (death) models -----------------------
    
    # Since death rates are block-specific, remove obs w/o block
    unlist_death <- subset(train_unlist, !is.na(block))
    
    # Get block specific lysis regressions 
    train_blocks <- split(unlist_death, f = unlist_death$block)
    death_regs <- lapply(train_blocks, 
                         FUN = function(x) return(glm(death ~ lat + long + 
                                                        bearing_prev + speed_prev + 
                                                        timestep,
                                                      family = binomial, data = x)))  
  
    # Fit kernel density to TC death times
    death_times <- sapply(train, FUN = function(x) nrow(x))
    death_dens <- density(death_times, bw = bw.nrd(death_times), kernel = "gaussian")
  
    # Get max observed tropical cyclone length
    lengths <- unlist(lapply(train, FUN = nrow))
    death_rate <- 1 / mean(lengths)
    max_length <- max(lengths)
    
    # Store blocks with <= 1 death. We will use overall death rate (instead of regs)
    # to model death in these blocks.
    bad_locations <- which(sapply(train_blocks,function(x) sum(x$death)<=1))
    
    # Generate curves -----------------------------------------
    
    # Store starting points and set up regression variables
    # auto = F requires first two rows to get initial change in bearing/speed
    # auto = T requires first three rows to get initial change in lag bearing/speed
    starts <- get_starting_points(test, auto = auto_ind)
    
    # Loop over whether logistic regressions are used to model lysis (death)
    for(death_regs_ind in tf){
    
      # Set index for storage of simulations based on parameters
      if(auto_ind & death_regs_ind){
        param_index <- 1
        if(verbose){ 
          cat("\nAR, death regs: generating paths for", length(test), "TCs\n") 
        }
      } else if(auto_ind & !death_regs_ind){
        param_index <- 2 
        if(verbose){ 
          cat("\nAR, no death regs: generating paths for", length(test), "TCs\n") 
        }
      } else if(!auto_ind & death_regs_ind){
        param_index <- 3
        if(verbose){ 
          cat("\nNon-AR, death regs: generating paths for", length(test), "TCs\n") 
        }
      } else if(!auto_ind & !death_regs_ind){
        param_index <- 4 
        if(verbose){ 
          cat("\nNon-AR, no death regs: generating paths for", length(test), "TCs\n") 
        }
      }

      for(test_index in 1:length(test)){
      
        paths <- vector("list", number_paths) # to make number_paths curves

        # Get starting observations for all test TCs
        paths <- lapply(paths, FUN = function(x) return(starts[[test_index]]))
      
        # Generate number_paths curves from starting observations
        paths <- lapply(paths, 
                        FUN = function(path) generate_curve(path, bearing_regs, 
                                                            speed_regs, death_regs, 
                                                            max_length, bad_locations, 
                                                            death_rate, death_dens, 
                                                            death_regs_ind, auto_ind))
      
        # Round to closest tenth to match NOAA format
        paths <- lapply(paths, FUN = function(x) round_curve(x))
      
        # Set column names
        paths <- lapply(paths, FUN = function(x) {colnames(x) = c("long", "lat"); x})

        # Store simulations in test_sims
        test_sims[[test_index]][[param_index]] <- paths
      
        if(verbose){ cat(test_index) } 
      }
    
    }
  
  }
  
  return(test_sims)

}

ptm <- proc.time()

test_sims <- generate_all()

proc.time() - ptm

# Save environment w/ one list per test TC.
# Each list has length 4 (one for each auto/non-auto death_reg/no_death_reg combo)
# Each of those 4 lists have length 1000 (all simulations from given starting observations)
test_env <- list2env(test_sims)
save(test_env, file = "data/generate/Test_Sims_1000.Rdata")
