
require(geosphere)
require(plyr)

functions_loc = "code/functions/"
source(paste0(functions_loc,"Regression_functions.R"))

###############################
##### Read data into list #####
###############################

setwd("data/training/train")
temp <- list.files(pattern=".txt")
dflist <- lapply(temp, read.table)

########################################################
##### Append latitude and longitude to data frames #####
########################################################

dflist <- lapply(dflist, FUN = append.lat.long)

#############################################
##### Append bearing and bearing.change #####
#############################################

dflist <- lapply(dflist, FUN = append.bearing)

####################################
##### Append east/west heading #####
####################################

# This is the direction that the hurricane traveled from the 
# previous position to the current position.
dflist <- lapply(dflist, FUN = append.eastwest)

#####################################
##### Append speed/speed.change #####
#####################################

dflist <- lapply(dflist, FUN = append.speed)

######################################
##### Append time step and death #####
######################################

dflist <- lapply(dflist, FUN = append.time.death)

################################################
##### Categorize long/lat/east-west blocks #####
################################################

dflist <- lapply(dflist, FUN = append.block)

############################
##### Lysis simulation #####
############################

lengths <- unlist(lapply(dflist, FUN = nrow))
death.rate <- 1 / mean(lengths)

#####################################
##### Bearing/speed regressions #####
#####################################

dflist.unlist <- do.call("rbind", dflist) 

# Remove final entry in each hurricane in order to model
# change in bearing and change in speed.
dflist.unlist <- dflist.unlist[!is.na(dflist.unlist$bearing.change),]

# Store a separate data frame for each block
dflist.blocks <- split(dflist.unlist, f = dflist.unlist$block)

bearing.regs <- lapply(dflist.blocks, FUN = function(x) 
  return(lm(bearing.change ~ lat + long 
            + bearing.prev + speed.prev, data = x)))

speed.regs <- lapply(dflist.blocks, FUN = function(x) 
  return(lm(speed.change ~ lat + long 
            + bearing.prev + speed.prev, data = x)))

###########################
##### Generate curves #####
###########################

# Read in validation data 
setwd("../../../data/training/validate")
temp <- list.files(pattern=".txt")
dfcv <- lapply(temp, read.table)

# Append latitude and longitude
dfcv <- lapply(dfcv, FUN = append.lat.long)

# Store starting points
starts <- lapply(dfcv, FUN = function(x) return(rbind(x[1,], x[2,])))

# Append bearing
starts <- lapply(starts, FUN = append.bearing)

# Append east/west
starts <- lapply(starts, FUN = append.eastwest)

# Append speed
starts <- lapply(starts, FUN = append.speed)

# Append time/death
starts <- lapply(starts, FUN = append.time.death)

# Append block
starts <- lapply(starts, FUN = append.block)



names <- list.files(pattern=".txt")
setwd("../../../")
loc <- "/data/generate/"

set.seed(1)

ptm <- proc.time()

# Run time: 3.5 hours to generate 100 curves on each of 333 hurricane starts.

for(i in 1:length(names)){
  
  newdir <- paste0(loc, substr(names[i],1,nchar(names[i])-4), "_non_ar_sims")
  dir.create(newdir)      # should test for error
  setwd(newdir)
  
  paths <- vector("list", 100)
  paths <- lapply(paths, FUN = function(x) return(starts[[i]]))
  
  paths <- lapply(paths, FUN = function(x) generate.curve.no.auto(x, death.rate))
  
  paths <- lapply(paths, FUN = function(x) round.curve(x))
  
  for(j in 1:length(paths)){
    write.table( paths[[j]], paste0(substr(names[i], 1, nchar(names[i])-4), "_sim_", j), 
                 append= F, sep=',', col.names = c("long", "lat"), row.names = F)
  }
  
  cat(i)
}


proc.time() - ptm
