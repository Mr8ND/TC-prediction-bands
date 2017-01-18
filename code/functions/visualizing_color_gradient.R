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

col_convert_opacity = function(cols,alpha_level = 50){
  # alpha is percentage (rounded to 5%)
  if (alpha_level %%5 != 0){
    print("Retry with alpha level integer in range 0-100, and divisible by 5")
    return(NA)
  }

  name = paste0("",seq(100,0,by=-5))
  add_ons = c("FF","F2","E6","D9","CC","BF","B3","A6","99","8C","80","73",
    "66","59","4D","40","33","26","1A","0D","00")
  names(add_ons)= name
  cols_split = sapply(cols,function(color){strsplit(color,"#")[[1]][2]})

  return(paste0("#",add_ons[as.character(alpha_level)],cols_split))
}

plotting_funct = function(true13,list_estimate,weights,lower="white", upper="black",levels =10,
  ylim = c(9,40),xlim =c(-110,2),main="Aiite",alpha_level = 100,col_true = "green",lwd_true = 4){
  # function will plot the true curve in green, rest of curves color gradiation
  # based on weights
  

  newmap = getMap(resolution = "low")
  plot(newmap, ylim = ylim, xlim = xlim, asp = 1,main=main)
  colfunc = colorRampPalette(c(lower, upper))
  cols = colfunc(levels)
  #cols = col_convert_opacity(cols,alpha_level = alpha_level)
  index = index_funct(weights,levels = levels)
  
  ordering = order(index)
  for(i in ordering){
    lines(list_estimate[[i]][,1],list_estimate[[i]][,2],col=cols[index[i]])
  }


  lines(true13[,1],true13[,2],col=col_true,lwd=lwd_true)
}

plotting_funct_color_select = function(true13,list_estimate,cols,index,
  ylim = c(9,40),xlim =c(-110,2),main="Aiite",col_true = "green",lwd_true = 4){
  # function will plot the true curve in green, rest of curves color gradiation
  # based on weights
  

  newmap = getMap(resolution = "low")
  plot(newmap, ylim = ylim, xlim = xlim, asp = 1,main=main)

  ordering = order(index)
  for(i in ordering){
    lines(list_estimate[[i]][,1],list_estimate[[i]][,2],col=cols[index[i]])
  }


  lines(true13[,1],true13[,2],col=col_true,lwd=lwd_true)
}
