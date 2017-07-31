library(datamart)
require(geosphere)
require(plyr)

setwd("~/Google Drive/701 Final Project")
functions_loc = "code/functions/"
source(paste0(functions_loc,"Regression_functions.R"))

###############################
##### Read data into list #####
###############################

#setwd("~/Google Drive/701 Final Project/hurricane_data")
temp <- list.files(pattern="")
dflist <- lapply(temp, function(x) data.frame(read.table(x, header=TRUE, sep=",")))
#setwd("~/Google Drive/701 Final Project")

library(rworldmap)
#plotting_funct(true13 = dflist[[1]], list_estimate = dflist, weights = seq(0,1,length.out = 100))

dflist_bear <- lapply(dflist[c(1:75)], FUN = append.bearing)
dflist_dir <- lapply(dflist_bear, FUN = append.eastwest)
dflist_dist <- lapply(dflist_bear, FUN= function(x) cbind(x, "distance"=c(NA,distAlongP(x)[[1]])))

dflist_toy <- list()
index <- 1
for (i in c(1:length(dflist_dist))){
  x_df = dflist_dist[[i]]
  if (dim(x_df)[1]>=35){
    dflist_toy[[index]] <- x_df[1:35,]
    index = index + 1
  }
}

###############################################################
#dflist_toy is my list of hurricanes like I want them to be basically.
dflist_toy[[1]]
probability_vec = seq(0.00001, 0.1, length.out=length(dflist_toy))

dfpoints_toy = rearrangePathListNew(dflist_toy,probability_vec)

returnCenterPoint = function(data.frame, long=1, lat=2, bearing=3, weights=6){
  data.frame = data.frame[order(data.frame[, weights], decreasing = TRUE),]
  return(data.frame[1, c(long,lat,bearing)])
}

createConfintBandsPoints = function(dflist, level=.90, start.step=3){
  
  output=list()
  n = length(dflist)
  alpha.2 = (1-level)/2

  starting.point = c(mean(dflist[[start.step-1]][,1]), mean(dflist[[start.step-1]][,2]),
                     mean(dflist[[start.step-1]][,3]))
  index_out = 1
  confidence.points = list()
  confidence.points[[index_out]] = c(starting.point, starting.point)
  
  for (i in c(start.step:n)){
    deltabearing.distance <- returnDeltaBearingDistance(dflist[[i-1]], alpha.2)
    print(deltabearing.distance)
    
    newbear.left = confidence.points[[index_out]][3] + deltabearing.distance[1]
    newbear.right = confidence.points[[index_out]][6] + deltabearing.distance[2]
    
    dist.meters = uconv(deltabearing.distance[3], "nautical mile", "m", "Length")
    
    newpoint.left = destPoint(confidence.points[[index_out]][c(1,2)], newbear.left, dist.meters)
    newpoint.right = destPoint(confidence.points[[index_out]][c(4,5)], newbear.right, dist.meters)
    
    index_out = index_out + 1
    confidence.points[[index_out]] = c(newpoint.left, newbear.left, newpoint.right, newbear.right)
    print(confidence.points[[index_out]])
  }
  return(confidence.points)
}


createConfintBandsPointsNew = function(dflist, level=.90, start.step=3){
  
  output=list()
  n = length(dflist)
  alpha.2 = (1-level)/2
  
  starting.point = c(mean(dflist[[start.step-1]][,1]), mean(dflist[[start.step-1]][,2]),
                     mean(dflist[[start.step-1]][,3]))
  index_out = 1
  confidence.points = list()
  center.points = list()
  confidence.points[[index_out]] = c(starting.point, starting.point)
  center.points[[index_out]] = starting.point
  
  for (i in c(start.step:n)){
    deltabearing.distance <- returnDeltaBearingDistanceNew(dflist[[i-1]], alpha.2)
    print(deltabearing.distance)
    
    newbear.left = center.points[[index_out]][3] - abs(deltabearing.distance[1])
    newbear.right = center.points[[index_out]][3] + abs(deltabearing.distance[2])
    dist.meters.left = uconv(deltabearing.distance[3], "nautical mile", "m", "Length")
    dist.meters.right = uconv(deltabearing.distance[4], "nautical mile", "m", "Length")
    
    newpoint.left = destPoint(center.points[[index_out]][c(1,2)], newbear.left, dist.meters.left)
    newpoint.right = destPoint(center.points[[index_out]][c(1,2)], newbear.right, dist.meters.right)
    
    index_out = index_out + 1
    confidence.points[[index_out]] = c(newpoint.left, newbear.left, newpoint.right, newbear.right)
    #confidence.points[[index_out]] = deltabearing.distance
    print(confidence.points[[index_out]])
    
    center_point_vec = returnCenterPoint(dflist[[i]])
    #center_point_vec = c(mean(deltabearing.distance[1], deltabearing.distance[4]), 
    #                     mean(deltabearing.distance[2], deltabearing.distance[5]),
    #                    mean(deltabearing.distance[3], deltabearing.distance[6]))
    center.points[[index_out]] = as.numeric(center_point_vec)
  }
  return(list(confidence.points, center.points))
}

confpoints_centerpoints_toy = createConfintBandsPointsNew(dfpoints_toy, level=.99)
confpoints_toy = confpoints_centerpoints_toy[[1]]
centerpoints_toy = confpoints_centerpoints_toy[[2]]
for (i in c(1:length(confpoints_toy))){
  print(confpoints_toy[[i]])
  print(centerpoints_toy[[i]])
}


returnDeltaBearingDistance = function(data.frame, alpha.2, bearing=4, distance=5, weights=6){
  bearingweights.df = data.frame[,c(bearing, weights)]
  bearingweights.df = bearingweights.df[order(bearingweights.df[,1]),]
  bearingweight.ordered = bearingweights.df[,1]
  
  weight.vec = bearingweights.df[,2]
  weight.vec.reverse = rev(weight.vec)
  indsel = which(cumsum(weight.vec) > alpha.2)[1]
  indsel.reverse = which(cumsum(weight.vec.reverse) > alpha.2)[1]
  
  mean.distance = mean(data.frame[,distance])
  
  #Returns left quantile first and right quantile second.
  #Third element is the distance.
  
  return(c(bearingweight.ordered[indsel], selectLastNElement(bearingweight.ordered, indsel.reverse), mean.distance))
}

returnDeltaBearingDistanceNew = function(data.frame, alpha.2, bearing=4, distance=5, weights=6, lon=1, lat=2, bear.orig=3){
  
  bearingweights.df = data.frame[,c(bearing, distance, weights, lon, lat, bear.orig)]
  bearingweights.df = bearingweights.df[order(bearingweights.df[,3], decreasing = TRUE),]
  
  weight.vec = bearingweights.df[,3]/sum(bearingweights.df[,3])
  indsel = which(cumsum(weight.vec) > (1-alpha.2))[1]
  bearingweights.df = bearingweights.df[1:indsel,]
  
  left.bearing = min(bearingweights.df[,1])
  right.bearing = max(bearingweights.df[,1])
  
  left.distance = bearingweights.df[which(bearingweights.df[,1]==min(bearingweights.df[,1]))[1],2]
  right.distance = bearingweights.df[which(bearingweights.df[,1]==max(bearingweights.df[,1]))[1],2]
  
  #left.point = bearingweights.df[which(bearingweights.df[,1]==min(bearingweights.df[,1]))[1],c(4,5,6)]
  #right.point = bearingweights.df[which(bearingweights.df[,1]==max(bearingweights.df[,1]))[1],c(4,5,6)]
  
  return(c(left.bearing, right.bearing, left.distance, right.distance))
}

returnDeltaBearingDistance(dfpoints_toy[[2]], .25)


selectLastNElement = function(vec,pos){
  return(tail(vec,n=pos)[1])
}


quartz(width = 8,height = 6.5)
newmap = getMap(resolution = "low")
xlim = c(-105,0)
ylim = c(16, 36)
plot(newmap, ylim = ylim, xlim = xlim, asp = 1)


for(i in 1:length(dflist_toy)){
  lines(dflist_toy[[i]][,1],dflist_toy[[i]][,2],col="grey")
}

for (i in c(1:length(confpoints_toy))){
  points(confpoints_toy[[i]][1], confpoints_toy[[i]][2], col='black')
  points(confpoints_toy[[i]][4], confpoints_toy[[i]][5], col='red')
  points(centerpoints_toy[[i]][1], centerpoints_toy[[i]][2], col='blue')
}









