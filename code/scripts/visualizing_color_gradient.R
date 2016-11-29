
#colfunc <- colorRampPalette(c("white", "black"))
#colfunc(10)


library(rworldmap)

index_funct = function(xx,levels = 3){
  up_low= range(xx)
  n = length(xx)
  cuts = 1/levels * 0:levels
  cuts2= (cuts * up_low[2]- up_low[1]) + up_low[1]
  
  index = rep(0,n)
  for(i in 1:levels){
    index[xx > cuts2[i]] = i
  }
  return(index)
}

# newmap = getMap(resolution = "low")

# plot(newmap, ylim = c(9, 40), xlim = c(-110, 2), asp = 1)

# validate_list =load_validate_data_list("")

# plot(newmap, ylim = c(9, 40), xlim = c(-110, 2), asp = 1)
# lines(validate_list[[60]][,6],validate_list[[60]][,5],col="black")
# num_paths = length(test_list)

# xx = p_estimate_test
# index = index_funct(xx,10)
# for(i in 1:150){
#     lines(test_list[[i]][,1],test_list[[i]][,2],col=colfunc(10)[index[i]])
# }

# lines(validate_list[[60]][,6],validate_list[[60]][,5],col="green",lwd=4)

# # vs

# for(i in 1:150){
#   lines(test_list[[i]][,1],test_list[[i]][,2],col="black")
# }



plotting_funct = function(true13,list_estimate,weights,lower="white",
  upper="black",levels =10,
  ylim = c(9,40),xlim =c(-110,2),main="Aiite"){
  # function will plot the true curve in green, rest of curves color gradiation
  # based on weights
  

  newmap = getMap(resolution = "low")
  plot(newmap, ylim = ylim, xlim = xlim, asp = 1,main=main)
  colfunc = colorRampPalette(c(lower, upper))
  cols = colfunc(levels)
  index = index_funct(weights,levels = levels)

  for(i in 1:length(list_estimate)){
    lines(list_estimate[[i]][,1],list_estimate[[i]][,2],col=cols[index[i]])
  }


  lines(true13[,1],true13[,2],col="green",lwd=4)




}