
require(geosphere)
require(plyr)

setwd("~/Google Drive/701 Final Project")
functions_loc = ""
source(paste0(functions_loc,"Regression_functions.R"))

###############################
##### Read data into list #####
###############################

setwd("~/Google Drive/701 Final Project/hurricane_data")
temp <- list.files(pattern="")
dflist <- lapply(temp, function(x) data.frame(read.table(x, header=TRUE, sep=",")))
setwd("~/Google Drive/701 Final Project")

#library(rworldmap)
#plotting_funct(true13 = dflist[[1]], list_estimate = dflist, weights = seq(0,1,length.out = 100))

dflist_bear <- lapply(dflist[c(1:75)], FUN = append.bearing)
dflist_dir <- lapply(dflist_bear, FUN = append.eastwest)
dflist_dist <- lapply(dflist_bear, FUN= function(x) cbind(x, "distance"=c(NA,distAlongP(x)[[1]])))

dflist_toy <- list()
index <- 1
for (i in c(1:length(dflist_dist))){
  x_df = dflist_dist[[i]]
  if (dim(x_df)[1]>=13){
    dflist_toy[[index]] <- x_df[1:13,]
    index = index + 1
  }
}

###############################################################
#dflist_toy is my list of hurricanes like I want them to be basically.
dflist_toy[[1]]
probability_vec = seq(0.00001, 0.1, length.out=length(dflist_toy))

dfpoints_toy = rearrangePathList(dflist_toy,probability_vec)

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

confpoints_toy = createConfintBandsPoints(dfpoints_toy, level=.5)
for (i in c(1:length(confpoints_toy))){
  print(confpoints_toy[[i]])
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

returnDeltaBearingDistance(dfpoints_toy[[2]], .25)


selectLastNElement = function(vec,pos){
  return(tail(vec,n=pos)[1])
}





