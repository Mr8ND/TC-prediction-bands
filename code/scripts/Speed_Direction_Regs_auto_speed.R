
require(geosphere)
require(plyr)

functions_loc = "code/functions/"
source(paste0(functions_loc,"Regression_functions.R"))
source(paste0(functions_loc,"creating_curves.R"))
###############################
##### Read data into list #####
###############################

#setwd("")
temp <- list.files(path = "./data/training/train", pattern=".txt")
dflist <- lapply(temp, function(name) 
      read.table(paste0("data/training/train/",name)))

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

#################################################################
##### Append 1 previous bearing.change and speed.change lag #####
#################################################################

dflist <- lapply(dflist, FUN = lag.bearing.speed.change)

################################################
##### Categorize long/lat/east-west blocks #####
################################################

dflist <- lapply(dflist, FUN = append.block)

#####################################
##### Bearing/speed regressions #####
#####################################

dflist.unlist <- do.call("rbind", dflist) 
dflist.unlist.death <- dflist.unlist 
dflist.unlist.death <- dflist.unlist.death[!(dflist.unlist.death$block == "NA"),]
# Remove final entry in each hurricane in order to model
# change in bearing and change in speed.
# Also, remove entries without bearing.change.lag1 (or speed.change.lag1).
dflist.unlist <- dflist.unlist[!is.na(dflist.unlist$bearing.change),]
dflist.unlist <- dflist.unlist[!is.na(dflist.unlist$bearing.change.lag1),]

# Store a separate data frame for each block
dflist.blocks <- split(dflist.unlist, f = dflist.unlist$block)
dflist.blocks.death <- split(dflist.unlist.death, f = dflist.unlist.death$block)

bearing.regs.auto <- lapply(dflist.blocks, FUN = function(x) 
  return(lm(bearing.change ~ lat + long + bearing.prev + speed.prev
            + bearing.change.lag1, 
            data = x)))

speed.regs.auto <- lapply(dflist.blocks, FUN = function(x) 
  return(lm(speed.change ~ lat + long + bearing.prev + speed.prev
            + speed.change.lag1, 
            data = x)))

############################
##### Lysis simulation #####
############################

death.regs <- lapply(dflist.blocks.death,FUN = function(x) 
 return(glm(death~lat +long + bearing.prev + speed.prev + timestep,family = binomial,data = x)))  

#sum(sapply(dflist.blocks.death,function(x) sum(x$death)==0))

lengths <- unlist(lapply(dflist, FUN = nrow))
death.rate <- 1 / mean(lengths)
max.length <- max(lengths)

# for the blocks with no deaths, we'll just use the average across all observations

bad.locations <- which(sapply(dflist.blocks.death,function(x) sum(x$death)==0))

###########################
##### Generate curves #####
###########################

# Read in validation data 
temp <- list.files(path = "data/training/validate" ,pattern=".txt")
dfcv <- lapply(temp, function(name.file) 
  read.table(paste0("data/training/validate/",name.file)))

# Append latitude and longitude
dfcv <- lapply(dfcv, FUN = append.lat.long)

# Store starting points - need 3 rows to get lag on bearing.change/speed.change
# and speed.change. (Second row is first row where change is recorded, then 3 lags.)
starts <- lapply(dfcv, FUN = function(x) return(rbind(x[1,], x[2,], x[3,])))

# Append bearing
starts <- lapply(starts, FUN = append.bearing)

# Append east/west
starts <- lapply(starts, FUN = append.eastwest)

# Append speed
starts <- lapply(starts, FUN = append.speed)

# Append time/death
starts <- lapply(starts, FUN = append.time.death)

# Lag bearing.change and speed.change
starts <- lapply(starts, FUN = lag.bearing.speed.change)

# Append block
starts <- lapply(starts, FUN = append.block)



names <- temp
loc <- "data/generate/Validation_Sims_Auto_plus/"

set.seed(1)

ptm <- proc.time()

# Run time: to generate 100 curves on each of 114 hurricane starts.
# > proc.time() - ptm
#    user   system  elapsed 
#2401.436   20.147 2477.384

for(i in 1:length(names)){
  
  newdir <- paste0(loc, substr(names[i],1,nchar(names[i])-4),"/")
  ### to make new folders with bash:
  ### ls ../../training/validate/*.txt | xargs  basename | tr -d .txt | xargs -L 1 mkdir
  
  paths <- vector("list", 100) # to make 100 curves
  paths <- lapply(paths, FUN = function(x) return(starts[[i]]))
  
  #paths <- lapply(paths, FUN = function(x) generate.curve.auto(x, death.rate))
  paths <- lapply(paths,FUN = function(path) generate.curve.auto.ben(path,
      bearing.regs.auto,speed.regs.auto,death.regs,max.length,bad.locations,
      death.rate))
  paths <- lapply(paths, FUN = function(x) round.curve(x)) #not sure why we
  
  for(j in 1:length(paths)){
    write.table( paths[[j]], paste0(newdir,substr(names[i], 1, nchar(names[i])-4), "_sim_", j), 
                 append= F, sep=',', col.names = c("long", "lat"), row.names = F)
  }
  
  cat(i)
}


proc.time() - ptm