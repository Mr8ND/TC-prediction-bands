library(geosphere)
library(plyr)

functions_loc = "code/functions/"
source(paste0(functions_loc,"Regression_functions.R")) 
source(paste0(functions_loc,"creating_curves.R"))

########################
##### Read in data #####
########################

# Read training data into list
train_names <- list.files(path = "./data/training/train", pattern=".txt")
train <- lapply(train_names, function(name) 
  read.table(paste0("data/training/train/",name)))

# Append path regression variables to training data (auto = T for most general)
train <- lapply(train, FUN = append.reg.vars, auto = T)

# Read in validation data 
val_names <- list.files(path = "data/training/validate", pattern=".txt")
val <- lapply(val_names, function(name.file) 
  read.table(paste0("data/training/validate/",name.file)))
  
#############################################################
##### Create validation TC objects to store simulations #####
#############################################################

# Create empty list of length 4 to store simulations for individual TC
sim_setup <- vector("list", 4)
names(sim_setup) <- c("Auto_DeathRegs", "Auto_NoDeathRegs", 
                      "NoAuto_DeathRegs", "NoAuto_NoDeathRegs")

# Create list to store validation simulations for all TCs
val_sims <- vector("list", length(val))

# Assign TC names to elements of val_sims 
names(val_sims) <- gsub(".txt", "", val_names)

# Set each element of val_sims equal to sim_setup
val_sims <- lapply(val_sims, FUN = function(x) x = sim_setup)

##################################################################
##### MAIN LOOP for auto/non-auto and speed/non-speed combos #####
##################################################################

set.seed(1)

ptm <- proc.time()

# Run time: to generate 2500 curves on each of 114 hurricane starts
# with 4 combos of auto / non-auto and death regressions / no death regressions.
# (slightly under 2 days)
#       user     system    elapsed 
# 159694.763    361.226 160229.562 

tf <- c(T, F)

# Loop over whether autoregressive terms are used in bearing/speed regs
for(auto.ind in tf){
    
  ####################################################
  ##### Block-specific bearing/speed regressions #####
  ####################################################
    
  train.unlist <- do.call("rbind", train) 
  bearing.speed.regs <- get.bearing.speed.regs(train.unlist, auto = auto.ind)
  bearing.regs <- bearing.speed.regs[[1]]
  speed.regs <- bearing.speed.regs[[2]]
    
  ###############################################
  ##### Block-specific lysis (death) models #####
  ###############################################
    
  # Since death rates are block-specific, remove obs w/o block
  unlist.death <- train.unlist[!(train.unlist$block == "NA"),]
    
  # Get block specific lysis regressions 
  train.blocks <- split(unlist.death, f = unlist.death$block)
  death.regs <- lapply(train.blocks,FUN = function(x) 
    return(glm(death ~ lat + long + bearing.prev + speed.prev + timestep,
               family = binomial, data = x)))  
  
  # Fit kernel density to TC death times
  death.times <- sapply(train, FUN = function(x) nrow(x))
  death.dens <- density(death.times, bw = bw.nrd(death.times), kernel = "gaussian")
  
  # Get max observed tropical cyclone length
  lengths <- unlist(lapply(train, FUN = nrow))
  death.rate <- 1 / mean(lengths)
  max.length <- max(lengths)
    
  # Store blocks with <= 1 death. We will use overall death rate (instead of regs)
  # to model death in these blocks.
  bad.locations <- which(sapply(train.blocks,function(x) sum(x$death)<=1))
    
  ###########################
  ##### Generate curves #####
  ###########################
    
  # Store starting points and set up regression variables
  # auto = F requires first two rows to get initial change in bearing/speed
  # auto = T requires first three rows to get initial change in lag bearing/speed
  starts <- get.starting.points(val, auto = auto.ind)
    
  # Loop over whether logistic regressions are used to model lysis (death)
  for(death.regs.ind in tf){
    
    # Set index for storage of simulations based on parameters
    if(auto.ind & death.regs.ind){
      param_index <- 1
    } else if(auto.ind & !death.regs.ind){
      param_index <- 2 
    } else if(!auto.ind & death.regs.ind){
      param_index <- 3
    } else if(!auto.ind & !death.regs.ind){
      param_index <- 4 
    }

    for(val_index in 1:length(val_names)){
      
      paths <- vector("list", 2500) # to make 2500 curves

      # Get starting observations for all validation TCs
      paths <- lapply(paths, FUN = function(x) return(starts[[val_index]]))
      
      # Generate 2500 curves from starting observations
      paths <- lapply(paths, FUN = function(path) generate.curve(path, 
                                             bearing.regs, speed.regs, death.regs, 
                                             max.length, bad.locations, death.rate,
                                             death.dens, death.regs.ind, auto.ind))
      
      # Round to closest tenth to match NOAA format
      paths <- lapply(paths, FUN = function(x) round.curve(x))
      
      # Set column names
      paths <- lapply(paths, FUN = function(x) {colnames(x) = c("long", "lat"); x})

      # Store simulations in val_sims
      val_sims[[val_index]][[param_index]] <- paths
      
      cat(val_index)
    }
    
  }
  
}

proc.time() - ptm

# Save environment w/ one list per validation TC.
# Each list has length 4 (one for each auto/non-auto death_reg/no_death_reg combo)
# Each of those 4 lists have length 2500 (all simulations from given starting observations)
val_env <- list2env(val_sims)
save(val_env, file = "data/generate/Val_Sims_2500.Rdata")
