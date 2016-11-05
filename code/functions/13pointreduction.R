##### Libraries
library(geosphere)

#### Loading functions
source(file = "code/functions/Path_functions.R")

# making 13 point function

thirteen_points = function(df2,lonlat = TRUE){
  # Coverts list of point locations to 13 points equally spaced apart
  #
  # Inputs:
  #--------
  # df2               = 2 column data frame with all points lat/lon 
  # latlon            = if the order of the columns is latlon (will be reverse to function)
  
  # Outputs:
  #---------
  # new_13compression = data.frame (13 x 2) of points along the path equally spaced
  
  if(lonlat ==FALSE){ # reversing for geosphere functions
    df2 = df2[,c(2,1)]
  }
  
  dist_and_bearing = distAlongP(df2,longlat = TRUE)
  dist = dist_and_bearing[[1]]
  bearing = dist_and_bearing[[2]]
  
  total_dist = sum(dist)
  step13 = total_dist/12 # 12 equa-distance points along path
  cum_steps = step13*(1:11)
  cum_dist = cumsum(dist)[c(-length(dist))]
  
  new_13compression = data.frame(matrix(0,nrow=13,ncol=2))
  index = 2
  for(step in 1:length(cum_steps))
    {
    step_full_dist = cum_steps[step]
    start = sum(cum_dist<=step_full_dist) +1
    start_point = df2[start,]
    start_bearing = (bearing[start-1])
    step_dist = step_full_dist-cum_dist[start-1]
    
    new_point = destPoint(start_point,start_bearing,step_dist)
    new_13compression[index,] = new_point
    index = index + 1
  }
  new_13compression[1,] = df2[1,]
  new_13compression[13,] = df2[nrow(df2),]
  
  return(new_13compression)
  
}


### visual testing of function
## should be able to make df2 any lat long matrix from the training data:
## df_full = AL011955
## df2 =df_full[,5:6]
# df2 =data.frame(matrix(c( 
# 27.5, -87.5,
# 27.6, -87.9,
# 27.8, -88.2,
# 28.2, -88.4,
# 28.6, -88.6,
# 29.3, -89.0,
# 29.7, -89.4,
# 29.9, -89.5,
# 30.6, -90.4,
# 31.1, -91.6,
# 31.5, -92.8,
# 31.8, -93.8,
# 32.2, -94.7,
# 32.7, -95.0),ncol=2,byrow=T))
# 
# library(rworldmap)
# newmap = getMap(resolution = "low")
# plot(newmap, ylim = c(10, 47), xlim = c(-90, -10), asp = 1)
# 
# lines(df2[,2],df2[,1], col = "red")
# news_df2 =  thirteen_points(df2,lonlat = F)
# points(news_df2[,1],news_df2[,2], col = "red")
