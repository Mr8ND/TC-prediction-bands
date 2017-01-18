library(geosphere)
library(plyr)

functions_loc = "code/functions/"
source(paste0(functions_loc,"Regression_functions.R"))
source(paste0(functions_loc,"creating_curves.R"))

###############################
##### Read data into list #####
###############################

temp <- list.files(path = "./data/training/train", pattern=".txt")
dflist <- lapply(temp, function(name) 
  read.table(paste0("data/training/train/",name)))

##################################################################
##### MAIN LOOP for auto/non-auto and speed/non-speed combos #####
##################################################################

ptm <- proc.time()

# Run time: to generate 100 curves on each of 114 hurricane starts
# with 4 combos of auto/non-auto and speed/non-speed.
# > proc.time() - ptm
#    user   system  elapsed 
# 6957.826   79.612 7273.600 


tf <- c(T, F)

# Loop over whether autoregressive terms are used in bearing/speed regs
for(auto.ind in tf){
    
  ########################################################
  ##### Append regression variables to training data #####
  ########################################################
    
  dflist <- lapply(dflist, FUN = append.reg.vars, auto = auto.ind)
    
  ####################################################
  ##### Block-specific bearing/speed regressions #####
  ####################################################
    
  dflist.unlist <- do.call("rbind", dflist) 
  bearing.speed.regs <- get.bearing.speed.regs(dflist.unlist, auto = auto.ind)
  bearing.regs <- bearing.speed.regs[[1]]
  speed.regs <- bearing.speed.regs[[2]]
    
  ###############################################
  ##### Block-specific lysis (death) models #####
  ###############################################
    
  # Since death rates are block-specific, remove obs w/o block
  unlist.death <- dflist.unlist[!(dflist.unlist$block == "NA"),]
    
  # Get block specific lysis regressions 
  dflist.blocks <- split(unlist.death, f = unlist.death$block)
  death.regs <- lapply(dflist.blocks,FUN = function(x) 
    return(glm(death ~ lat +long + bearing.prev + speed.prev + timestep,
               family = binomial, data = x)))  
    
  # Get max observed tropical cyclone length
  lengths <- unlist(lapply(dflist, FUN = nrow))
  death.rate <- 1 / mean(lengths)
  max.length <- max(lengths)
    
  # Store blocks with <= 1 death. We will use overall death rate (instead of regs)
  # to model death in these blocks.
  bad.locations <- which(sapply(dflist.blocks,function(x) sum(x$death)<=1))
    
  ###########################
  ##### Generate curves #####
  ###########################
    
  # Read in validation data 
  names <- list.files(path = "data/training/validate" ,pattern=".txt")
  dfcv <- lapply(names, function(name.file) 
    read.table(paste0("data/training/validate/",name.file)))
    
  # Store starting points and set up regression variables
  # auto = F requires first two rows to get initial change in bearing/speed
  # auto = T requires first three rows to get initial change in lag bearing/speed
  starts <- get.starting.points(dfcv, auto = auto.ind)
    
  # Loop over whether logistic regressions are used to model lysis (death)
  for(death.regs.ind in tf){
    
    if(auto.ind & death.regs.ind){
      loc <- "data/generate/Val_Sims_Auto_DeathRegs/" 
    } else if(auto.ind & !death.regs.ind){
      loc <- "data/generate/Val_Sims_Auto_NoDeathRegs/" 
    } else if(!auto.ind & death.regs.ind){
      loc <- "data/generate/Val_Sims_NoAuto_DeathRegs/" 
    } else if(!auto.ind & !death.regs.ind){
      loc <- "data/generate/Val_Sims_NoAuto_NoDeathRegs/" 
    }

    set.seed(1)
    
    for(i in 1:length(names)){
      
      newdir <- paste0(loc, substr(names[i],1,nchar(names[i])-4),"/")
      ### to make new folders with bash:
      ### ls ../../training/validate/*.txt | xargs  basename | tr -d .txt | xargs -L 1 mkdir
      
      paths <- vector("list", 100) # to make 100 curves
      paths <- lapply(paths, FUN = function(x) return(starts[[i]]))
      
      paths <- lapply(paths, FUN = function(path) generate.curve(path, 
                                             bearing.regs, speed.regs, death.regs, 
                                             max.length, bad.locations, death.rate,
                                             death.regs.ind, auto.ind))
      
      # round to closest tenth to match NOAA format
      paths <- lapply(paths, FUN = function(x) round.curve(x))
      
      for(j in 1:length(paths)){
        write.table( paths[[j]], paste0(newdir,substr(names[i], 1, nchar(names[i])-4), "_sim_", j), 
                     append= F, sep=',', col.names = c("long", "lat"), row.names = F)
      }
      
      cat(i)
    }
    
  }
  
}

proc.time() - ptm

