library(geosphere)

######################################################## 
##### Append latitude and longitude to data frames #####
########################################################

append.lat.long <- function(df){
  df$lat <- df$V5
  df$long <- df$V6
  return(df)
}

#############################################
##### Append bearing and bearing.change #####
#############################################

# Formula for available at http://www.movable-type.co.uk/scripts/latlong.html
# Append bearing and change in bearing columns to dataframes
# Bearing is calculated using geodesic path (shortest path on ellipsoid)
# See help(bearing)
append.bearing <- function(df){
  n.row <- nrow(df)
  bear <- bearing(cbind(df$long[1:(n.row-1)], df$lat[1:(n.row-1)]),
                  cbind(df$long[2:n.row], df$lat[2:n.row]))
  # Ensure 0 <= bearing < 360
  bear[bear < 0] <- bear[bear < 0] + 360
  bear <- c(bear, NA)
  df$bearing <- bear
  df$bearing.change <- c(NA, diff(bear))
  
  # Ensure -180 < bearing.change <= 180
  # If bearing.change <= -180, add 360
  df[df$bearing.change <= -180 & !is.na(df$bearing.change),]$bearing.change <- df[df$bearing.change <= -180 & !is.na(df$bearing.change),]$bearing.change + 360
  # If bearing.change > 180, subtract 360
  df[df$bearing.change > 180 & !is.na(df$bearing.change),]$bearing.change <- df[df$bearing.change > 180 & !is.na(df$bearing.change),]$bearing.change - 360
  
  df$bearing.prev <- c(NA, bear[1:(n.row-1)])
  return(df)
}

####################################
##### Append east/west heading #####
####################################

append.eastwest <- function(df){
  east <- df$bearing.prev < 180
  dir <- rep(NA, length(east))
  dir[east] <- "E"
  dir[!east] <- "W"
  
  df$east.west.prev <- dir
  return(df)
}

#####################################
##### Append speed/speed.change #####
#####################################

# Speed will be in meters per hour
# Distance between point 1 and point 2 divided by 6 is estimate of speed at pt 1
append.speed <- function(df){
  n.row <- nrow(df)
  dist <- distGeo(cbind(df$long[1:(n.row-1)], df$lat[1:(n.row-1)]),
                  cbind(df$long[2:n.row], df$lat[2:n.row]))
  dist <- c(dist, NA)
  df$speed <- dist / 6
  df$speed.change <- c(NA, diff(df$speed))
  df$speed.prev <- c(NA, df$speed[1:(n.row-1)])
  return(df)
}


######################################
##### Append time step and death #####
######################################

append.time.death <- function(df){
  n.row <- nrow(df)
  df$timestep <- 1:n.row
  dies <- rep(0, n.row)
  if(n.row > 2){
    dies[n.row] <- 1
  }
  
  df$death <- dies
  return(df)
}


##################################################################
##### Categorize long/lat blocks for non-autogressive models #####
##################################################################

get.block <- function(long, lat, east.west.prev){
  if(!is.na(east.west.prev))
  {
    ### WEST
    if(long < -90 && lat < 20 && east.west.prev == "W"){
      return("A1.west")
    } else if(-90 <= long && long < -80 && lat < 20 && east.west.prev == "W"){
      return("A2.west")
    } else if(-80 <= long && long < -70 && lat < 20 && east.west.prev == "W"){
      return("A3.west")
    } else if(-70 <= long && long < -60 && lat < 20 && east.west.prev == "W"){
      return("A4.west")
    } else if(-60 <= long && long < -50 && lat < 20 && east.west.prev == "W"){
      return("A5.west")
    } else if(-50 <= long && long < -40 && lat < 20 && east.west.prev == "W"){
      return("A6.west")
    } else if(-40 <= long && long < -30 && lat < 20 && east.west.prev == "W"){
      return("A7.west")
    } else if(-30 <= long && lat < 20 && east.west.prev == "W"){
      return("A8.west")
    } else if(long < -90 && 20 <= lat && lat < 30 && east.west.prev == "W"){
      return("B1.west")
    } else if(-90 <= long && long < -80 && 20 <= lat && lat < 30 && east.west.prev == "W"){
      return("B2.west")
    } else if(-80 <= long && long < -70 && 20 <= lat && lat < 30 && east.west.prev == "W"){
      return("B3.west")
    } else if(-70 <= long && long < -60 && 20 <= lat && lat < 30 && east.west.prev == "W"){
      return("B4.west")
    } else if(-60 <= long && long < -50 && 20 <= lat && lat < 30 && east.west.prev == "W"){
      return("B5.west")
    } else if(-50 <= long && long < -40 && 20 <= lat && lat < 30 && east.west.prev == "W"){
      return("B6.west")
    } else if(-40 <= long && 20 <= lat && lat < 30 && east.west.prev == "W"){
      return("B7.west")
    } else if(long < -90 && 30 <= lat && lat < 40 && east.west.prev == "W"){
      return("C1.west")
    } else if(-90 <= long && long < -80 && 30 <= lat && lat < 40 && east.west.prev == "W"){
      return("C2.west")
    } else if(-80 <= long && long < -70 && 30 <= lat && lat < 40 && east.west.prev == "W"){
      return("C3.west")
    } else if(-70 <= long && long < -60 && 30 <= lat && lat < 40 && east.west.prev == "W"){
      return("C4.west")
    } else if(-60 <= long && long < -50 && 30 <= lat && lat < 40 && east.west.prev == "W"){
      return("C5.west")
    } else if(-50 <= long && long < -40 && 30 <= lat && lat < 40 && east.west.prev == "W"){
      return("C6.west")
    } else if(long < -40 && 40 <= lat && east.west.prev == "W"){
      return("D1.west")
    } else if(-40 <= long && 30 <= lat && east.west.prev == "W"){
      return("D2.west")
    }
    
    ### EAST
    if(long < -80 && lat < 20 && east.west.prev == "E"){
      return("A1.east")
    } else if(-80 <= long && long < -70 && lat < 20 && east.west.prev == "E"){
      return("A2.east")
    } else if(-70 <= long && long < -60 && lat < 20 && east.west.prev == "E"){
      return("A3.east")
    } else if(long < -90 && 20 <= lat && lat < 30 && east.west.prev == "E"){
      return("B1.east")
    } else if(-90 <= long && long < -80 && 20 <= lat && lat < 30 && east.west.prev == "E"){
      return("B2.east")
    } else if(-80 <= long && long < -70 && 20 <= lat && lat < 30 && east.west.prev == "E"){
      return("B3.east")
    } else if(-70 <= long && long < -60 && 20 <= lat && lat < 30 && east.west.prev == "E"){
      return("B4.east")
    } else if(-60 <= long && long < -50 && lat < 30 && east.west.prev == "E"){
      return("B5.east")
    } else if(-50 <= long && long < -40 && lat < 30 && east.west.prev == "E"){
      return("B6.east")
    } else if(-40 <= long && lat < 30 && east.west.prev == "E"){
      return("B7.east")
    } else if(long < -90 && 30 <= lat && lat < 40 && east.west.prev == "E"){
      return("C1.east")
    } else if(-90 <= long && long < -80 && 30 <= lat && lat < 40 && east.west.prev == "E"){
      return("C2.east")
    } else if(-80 <= long && long < -70 && 30 <= lat && lat < 40 && east.west.prev == "E"){
      return("C3.east")
    } else if(-70 <= long && long < -60 && 30 <= lat && lat < 40 && east.west.prev == "E"){
      return("C4.east")
    } else if(-60 <= long && long < -50 && 30 <= lat && lat < 40 && east.west.prev == "E"){
      return("C5.east")
    } else if(-50 <= long && long < -40 && 30 <= lat && lat < 40 && east.west.prev == "E"){
      return("C6.east")
    } else if(-40 <= long && long < -30 && 30 <= lat && lat < 40 && east.west.prev == "E"){
      return("C7.east")
    } else if(-30 <= long && long < -20 && 30 <= lat && lat < 40 && east.west.prev == "E"){
      return("C8.east")
    } else if(-60 <= long && long < -50 && 40 <= lat && lat < 50 && east.west.prev == "E"){
      return("D1.east")
    } else if(-50 <= long && long < -40 && 40 <= lat && lat < 50 && east.west.prev == "E"){
      return("D2.east")
    } else if(-40 <= long && long < -30 && 40 <= lat && lat < 50 && east.west.prev == "E"){
      return("D3.east")
    } else if(-30 <= long && long < -20 && 40 <= lat && lat < 50 && east.west.prev == "E"){
      return("D4.east")
    } else if(-20 <= long && 30 <= lat && lat < 50 && east.west.prev == "E"){
      return("D5.east")
    } else if(long < -70 && 40 <= lat && east.west.prev == "E"){
      return("E1.east")
    } else if(-70 <= long && long < -60 && 40 <= lat && east.west.prev == "E"){
      return("E2.east")
    } else if(-60 <= long && long < -40 && 50 <= lat && east.west.prev == "E"){
      return("F1.east")
    } else if(-40 <= long && long < -20 && 50 <= lat && east.west.prev == "E"){
      return("F2.east")
    } else if(-20 <= long && 50 <= lat && east.west.prev == "E"){
      return("F3.east")
    }
  }
  
  if(is.na(east.west.prev)){
    return("NA")
  }
}

########################
##### Append block #####
########################

append.block <- function(df){
  blocks <- adply(df, 1, function(x) get.block(x$long, x$lat, x$east.west.prev))
  df$block <- blocks$V1
  return(df)
} 

###########################################################
##### Simulate new point for non-autoregressive model #####
###########################################################

new.point.no.auto <- function(curve.df, final.length){
  n.row <- nrow(curve.df)
  block <- curve.df[n.row,]$block
  
  current.long <- curve.df$long[n.row]
  current.lat <- curve.df$lat[n.row]
  
  # Final length of hurricane was determined by drawing from
  # Exp(death.rate) distribution.
  
  is.death <- (n.row >= final.length)
  if(current.long < -107.3 | current.long > 10 | current.lat < 8 | current.lat > 60 | 
     (current.long < -90 & current.lat > 40)){
    is.death <- 1
  }
  if(is.death){
    return(curve.df)
  }
  
  bearing.change <- predict(bearing.regs[block][[1]], 
                            newdata=data.frame(lat=curve.df[n.row,]$lat, 
                                               long=curve.df[n.row,]$long,
                                               bearing.prev=curve.df[n.row,]$bearing.prev,
                                               speed.prev=curve.df[n.row,]$speed.prev),
                            se.fit = T)
  
  curve.df[n.row,]['bearing.change'] <- as.numeric(bearing.change['fit']) + rnorm(n=1, mean=0, sd=bearing.change$"se.fit"[1])
  
  curve.df[n.row,]['bearing'] <- curve.df[(n.row-1),]['bearing'] + curve.df[n.row,]['bearing.change']
  
  while(curve.df[n.row,]['bearing'] < 0){
    curve.df[n.row,]['bearing'] <- curve.df[n.row,]['bearing'] + 360
  }
  while(curve.df[n.row,]['bearing'] >= 360){
    curve.df[n.row,]['bearing'] <- curve.df[n.row,]['bearing'] - 360
  }
  
  while(curve.df[n.row,]['bearing.change'] <= -180){
    curve.df[n.row,]['bearing.change'] <- curve.df[n.row,]['bearing.change'] + 360
  }
  while(curve.df[n.row,]['bearing.change'] > 180){
    curve.df[n.row,]['bearing.change'] <- curve.df[n.row,]['bearing.change'] - 360
  }
  
  speed.change <- predict(speed.regs[block][[1]], 
                          newdata=data.frame(lat=curve.df[n.row,]$lat, 
                                             long=curve.df[n.row,]$long,
                                             bearing.prev=curve.df[n.row,]$bearing.prev,
                                             speed.prev=curve.df[n.row,]$speed.prev),
                          se.fit = T)
  curve.df[n.row,]['speed.change'] <- as.numeric(speed.change['fit']) + rnorm(n=1, mean=0, sd=speed.change$"se.fit"[1])
  
  curve.df[n.row,]['speed'] <- curve.df[(n.row-1),]['speed'] + curve.df[n.row,]['speed.change']
  
  curve.df <- rbind(curve.df, rep(NA, ncol(curve.df)))
  
  # Append new point to curve.df
  newpoint <- destPoint(p=c(curve.df[n.row,]$long, curve.df[n.row,]$lat), 
                        b = curve.df[n.row,]$bearing, 
                        d = curve.df[n.row,]$speed*6)
  
  curve.df[n.row+1,]['long'] <- newpoint[1,]['lon']
  curve.df[n.row+1,]['lat'] <- newpoint[1,]['lat']
  
  curve.df[n.row+1,]['bearing.prev'] <- curve.df[n.row,]['bearing']
  curve.df[n.row+1,]['speed.prev'] <- curve.df[n.row,]['speed']
  
  if(curve.df[n.row+1,]['bearing.prev'] < 180){
    curve.df[n.row+1,]['east.west.prev'] <- "E"
  } else {
    curve.df[n.row+1,]['east.west.prev'] <- "W"
  }
  
  curve.df[n.row+1,]['block'] <- get.block(curve.df[n.row+1,]['long'], 
                                                   curve.df[n.row+1,]['lat'],
                                                   curve.df[n.row+1,]['east.west.prev'])
  
  return(curve.df)
}

#######################################################
##### Simulate new point for autoregressive model #####
#######################################################

new.point.auto <- function(curve.df, final.length){
  n.row <- nrow(curve.df)
  block <- curve.df[n.row,]$block
  
  current.long <- curve.df$long[n.row]
  current.lat <- curve.df$lat[n.row]
  
  # Final length of hurricane was determined by drawing from
  # Exp(death.rate) distribution.
  
  is.death <- (n.row >= final.length)
  if(current.long < -107.3 | current.long > 10 | current.lat < 8 | current.lat > 60 | 
     (current.long < -90 & current.lat > 40)){
    is.death <- 1
  }
  if(is.death){
    return(curve.df)
  }
  
  bearing.change <- predict(bearing.regs.auto[block][[1]], 
                            newdata=data.frame(lat=curve.df[n.row,]$lat, 
                                               long=curve.df[n.row,]$long,
                                               bearing.prev=curve.df[n.row,]$bearing.prev,
                                               speed.prev=curve.df[n.row,]$speed.prev,
                                               bearing.change.lag1=curve.df[n.row,]$bearing.change.lag1),
                            se.fit = T)
  
  curve.df[n.row,]['bearing.change'] <- as.numeric(bearing.change['fit']) + rnorm(n=1, mean=0, sd=bearing.change$"se.fit"[1])
  
  curve.df[n.row,]['bearing'] <- curve.df[(n.row-1),]['bearing'] + curve.df[n.row,]['bearing.change']
  
  while(curve.df[n.row,]['bearing'] < 0){
    curve.df[n.row,]['bearing'] <- curve.df[n.row,]['bearing'] + 360
  }
  while(curve.df[n.row,]['bearing'] >= 360){
    curve.df[n.row,]['bearing'] <- curve.df[n.row,]['bearing'] - 360
  }
  
  while(curve.df[n.row,]['bearing.change'] <= -180){
    curve.df[n.row,]['bearing.change'] <- curve.df[n.row,]['bearing.change'] + 360
  }
  while(curve.df[n.row,]['bearing.change'] > 180){
    curve.df[n.row,]['bearing.change'] <- curve.df[n.row,]['bearing.change'] - 360
  }
  
  speed.change <- predict(speed.regs.auto[block][[1]], 
                          newdata=data.frame(lat=curve.df[n.row,]$lat, 
                                             long=curve.df[n.row,]$long,
                                             bearing.prev=curve.df[n.row,]$bearing.prev,
                                             speed.prev=curve.df[n.row,]$speed.prev,
                                             speed.change.lag1=curve.df[n.row,]$speed.change.lag1),
                          se.fit = T)
  curve.df[n.row,]['speed.change'] <- as.numeric(speed.change['fit']) + rnorm(n=1, mean=0, sd=speed.change$"se.fit"[1])
  
  curve.df[n.row,]['speed'] <- curve.df[(n.row-1),]['speed'] + curve.df[n.row,]['speed.change']
  
  curve.df <- rbind(curve.df, rep(NA, ncol(curve.df)))
  
  # Append new point to curve.df
  newpoint <- destPoint(p=c(curve.df[n.row,]$long, curve.df[n.row,]$lat), 
                        b = curve.df[n.row,]$bearing, 
                        d = curve.df[n.row,]$speed*6)
  
  curve.df[n.row+1,]['long'] <- newpoint[1,]['lon']
  curve.df[n.row+1,]['lat'] <- newpoint[1,]['lat']
  
  curve.df[n.row+1,]['bearing.prev'] <- curve.df[n.row,]['bearing']
  curve.df[n.row+1,]['speed.prev'] <- curve.df[n.row,]['speed']
  
  if(curve.df[n.row+1,]['bearing.prev'] < 180){
    curve.df[n.row+1,]['east.west.prev'] <- "E"
  } else {
    curve.df[n.row+1,]['east.west.prev'] <- "W"
  }
  
  curve.df[n.row+1,]['bearing.change.lag1'] <- curve.df[n.row,]['bearing.change']
  curve.df[n.row+1,]['speed.change.lag1'] <- curve.df[n.row,]['speed.change']
  
  curve.df[n.row+1,]['block'] <- get.block(curve.df[n.row+1,]['long'], 
                                           curve.df[n.row+1,]['lat'],
                                           curve.df[n.row+1,]['east.west.prev'])
  
  return(curve.df)
}

###########################################################
##### Simulate new curve for non-autoregressive model #####
###########################################################

generate.curve.no.auto <- function(curve.df, death.rate){
  
  final.length <- round(rexp(1, rate = death.rate))
  current.size <- nrow(curve.df)
  new.size <- 0
  
  while(current.size != new.size){
    
    current.size <- nrow(curve.df)
    curve.df <- new.point.no.auto(curve.df, final.length)
    new.size <- nrow(curve.df)
  }
  
  curve.df <- cbind(curve.df$long, curve.df$lat)
  
  return(curve.df)
}

#######################################################
##### Simulate new curve for autoregressive model #####
#######################################################

generate.curve.auto <- function(curve.df, death.rate){
  
  final.length <- round(rexp(1, rate = death.rate))
  current.size <- nrow(curve.df)
  new.size <- 0
  
  while(current.size != new.size){
    
    current.size <- nrow(curve.df)
    curve.df <- new.point.auto(curve.df, final.length)
    new.size <- nrow(curve.df)
  }
  
  curve.df <- cbind(curve.df$long, curve.df$lat)
  
  return(curve.df)
}

###############################################
##### Round lat/long to one decimal point #####
###############################################

round.curve <- function(curve.df){
  return(cbind(round(curve.df[,1],1), round(curve.df[,2],1)))
}

############################################################
##### Lag bearing.change and speed.change with 3 terms #####
############################################################

lag.bearing.speed.change <- function(df){
  
  n.row <- nrow(df)
  
  bearing.change.minus1 <- rep(NA, n.row)
  bearing.change.minus1[2:n.row] <- df$bearing.change[1:(n.row-1)]
  df$bearing.change.lag1 <- bearing.change.minus1
  
  speed.change.minus1 <- rep(NA, n.row)
  speed.change.minus1[2:n.row] <- df$speed.change[1:(n.row-1)]
  df$speed.change.lag1 <- speed.change.minus1
  
  return(df)
}

