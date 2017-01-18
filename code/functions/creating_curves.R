is.dead.inner <- function(curve.df,death.regs,bad.locations,death.rate){
  n.row <- nrow(curve.df)
  block <- curve.df[n.row,]$block

  current.long <- curve.df$long[n.row]
  current.lat <- curve.df$lat[n.row]

  if(block %in% bad.locations){
  	prob = death.rate
  }else{
  	prob <- predict(death.regs[[block]],type= "response",
  	newdata = data.frame(curve.df[n.row,]))
  }
  #print(prob)
  return(rbinom(1,1,prob) == 1)
}

bearing.inner <- function(curve.df,bearing.regs){
  n.row <- nrow(curve.df)
  block <- curve.df[n.row,]$block

  bearing.change <- predict(bearing.regs[[block]], 
                            newdata=data.frame(curve.df[n.row,]),
                            se.fit = T)
  
  # updating "NA" values in old data

  curve.df[n.row,'bearing.change'] <- bearing.change$fit +
  	 	rnorm(n=1,sd=bearing.change$se.fit)
  
  curve.df[n.row,'bearing'] <- curve.df[(n.row-1),'bearing'] + 
  		curve.df[n.row,'bearing.change']
  # making sure size of bearing stays in range
  while(curve.df[n.row,'bearing'] < 0){
    curve.df[n.row,'bearing'] <- curve.df[n.row,'bearing'] + 360
  }
  while(curve.df[n.row,'bearing'] >= 360){
    curve.df[n.row,'bearing'] <- curve.df[n.row,'bearing'] - 360
  }
  
  while(curve.df[n.row,'bearing.change'] <= -180){
    curve.df[n.row,'bearing.change'] <- curve.df[n.row,'bearing.change'] + 360
  }
  while(curve.df[n.row,]['bearing.change'] > 180){
    curve.df[n.row,'bearing.change'] <- curve.df[n.row,'bearing.change'] - 360
  }
  return(curve.df)
}

speed.inner <- function(curve.df,speed.regs){
  n.row <- nrow(curve.df)
  block <- curve.df[n.row,]$block

  speed.change <- predict(speed.regs[[block]], 
                            newdata=data.frame(curve.df[n.row,]),
                            se.fit = T)
  curve.df[n.row,'speed.change'] <- speed.change$fit + 
  	rnorm(n=1, sd=speed.change$se.fit)
  
  curve.df[n.row,'speed'] <- curve.df[(n.row-1),'speed'] + 
  	curve.df[n.row,'speed.change']
  
  return(curve.df)
}

update.curve.inner <- function(curve.df){
  n.row <- nrow(curve.df)
  curve.df <- rbind(curve.df,rep(NA,ncol(curve.df)))
  # Append new point to curve.df
  new.point <- destPoint(p=c(curve.df$long[n.row], curve.df$lat[n.row]), 
                        b = curve.df$bearing[n.row], 
                        d = curve.df$speed[n.row]*6) # for 6 hours
  
  curve.df[n.row + 1,'long'] <- new.point[1,'lon']
  curve.df[n.row + 1,'lat'] <- new.point[1,'lat']
  
  curve.df[n.row + 1,'bearing.prev'] <- curve.df[n.row,"bearing"]
  curve.df[n.row + 1,'speed.prev'] <- curve.df[n.row,'speed']
  
  if(curve.df[n.row + 1,'bearing.prev'] < 180){
    curve.df[n.row + 1,'east.west.prev'] <- "E"
  } else {
    curve.df[n.row + 1,'east.west.prev'] <- "W"
  }
  
  curve.df[n.row + 1,'bearing.change.lag1'] <- curve.df[n.row,'bearing.change']
  curve.df[n.row + 1,'speed.change.lag1'] <- curve.df[n.row,'speed.change']
  
  curve.df[n.row + 1,'block'] <- get.block(curve.df[n.row + 1,'long'], 
                                           curve.df[n.row + 1,'lat'],
                                           curve.df[n.row + 1,'east.west.prev'])
  curve.df[n.row + 1,"timestep"] <- curve.df[n.row,"timestep"]+1

  return(curve.df)
}


generate.curve <- function(curve.df,bearing.regs,speed.regs,death.regs,
			max.length,bad.locations,death.rate,death.regs.ind,auto.ind){
	# this function should actually generate the rest of the curve 
	#
	# Inputs:
	# -------
	# curve.df       = the initial 3 rows of the curve 
	# bearing.regs   = bearing regressions (block dependent)
	# speed.regs     = speed regressions (block dependent)
	# death.regs     = death regressions (block dependent)
  # max.length     = max length of tropical cyclones in train data
  # bad.locations  = blocks with <= 1 death
  # death.rate     = 1 / mean length of TCs in train data
  # death.regs.ind = indicator, whether to use death regressions
  # auto.ind       = indicator, whether bearing/speed regs are autoregressive

	# Output:
	# -------
	# out            = updated curve.df when it finally died, long, lat

  if(death.regs.ind){
    is.dead.function <- is.dead.inner
  }else{
    is.dead.false.function <- function(a,b,c,d){
      return(FALSE)
    }
    is.dead.function <- is.dead.false.function
    
    max.length <- round(rexp(1, death.rate))
  }
  

	is.dead     <- is.dead.function(curve.df,death.regs,bad.locations,
									death.rate)
	life.length <- 2 + 1*auto.ind


	while(!is.dead & life.length <= max.length){
		curve.df <- bearing.inner(curve.df,bearing.regs)
		curve.df <- speed.inner(curve.df,speed.regs)
		curve.df <- update.curve.inner(curve.df)

		is.dead <- is.dead.function(curve.df,death.regs,bad.locations,
									death.rate)
		
		current.long <- curve.df[nrow(curve.df),]$long
		current.lat <- curve.df[nrow(curve.df),]$lat
		
		# kill tropical cyclone if it goes outside reasonable range
		if(current.long < -107.3 | current.long > 10 | current.lat < 8 | 
		   current.lat > 60 | (current.long < -90 & current.lat > 40)){
		  is.dead <- TRUE 
		}

		life.length <- life.length + 1
	}

	n.row <- nrow(curve.df)
	curve.df[n.row,"death"] <- 1

	out <- cbind(curve.df$long,curve.df$lat) 
	# ^saves each curve as a table with a longitude column and a latitude column 
	return(out)
}



