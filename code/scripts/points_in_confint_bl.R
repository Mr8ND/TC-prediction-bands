
#############
# Locations #
#############

project_location      = ""
functions_loc         = "code/functions/"
generate_loc          = "data/generate/"
train_f_loc           = "data/training/train/"

sub_directory_no_auto = paste0(generate_loc,"Validation_Sims_No_Auto/")
sub_directory_auto    = paste0(generate_loc,"Validation_Sims_Auto/")

desired_functions = c("Path_functions.R","13pointreduction.R",
                      "projection_map.R","visualizing_color_gradient.R","bubble_points_functions2.R")

#####################
# Loading functions #
#####################

for(f_name in desired_functions){
  source(paste0(functions_loc,f_name))
}

# Ben's way to load data in:
source(paste0(functions_loc,"load_training_first.R"))




checkPointsInConfint = function(df_list, center.radius.list){
  
  n=length(df_list)
  n1 = dim(df_list[[1]])[1]
  nc = length(center.radius.list)
  if (n1 != nc){
    print('Length of steps considered in the confidence interval is different from the one of
          the input paths')
  }
  
  points.list = numeric(nc)
  hist.data = c()
  
  for (h.df in df_list){
    counter = 0
    for (i in c(1:nc)){
      c.rad.vec = as.numeric(center.radius.list[[i]][c(1,2)])
      h.df.vec = as.numeric(h.df[i,c(1,2)])
      distance_measured = distGeo(c.rad.vec, h.df.vec) 
      if ((abs(distance_measured) <= abs(center.radius.list[[i]][3])) ||
          ((c.rad.vec[1]==h.df.vec[1]) && 
           (c.rad.vec[2]==h.df.vec[2]))){
        points.list[i] = points.list[i] + 1
        counter = counter + 1
      }
    }
    hist.data = c(hist.data, counter)
  }
  return(list(points.list/n, hist.data))
}

checkPointsInBands = function(df, center.radius.df){
  n.df = dim(df)[1]
  in.vec = numeric(n.df)
  
  for (i in c(1:n.df)){
    for (c in c(1:dim(center.radius.df)[1])){
      dist.measured = distGeo(as.numeric(center.radius.df[c,c(1,2)]),
                              as.numeric(df[i,c(1,2)]))
      if (abs(dist.measured) <= abs(center.radius.df[c,3])||
          ((df[i,1]==center.radius.df[c,1]) && 
           (df[i,2]==center.radius.df[c,2]))){
        in.vec[i] = 1
      }
    }
  }  
  return(in.vec)
}


ConfintPipeline = function(path.full.df, p_estimate.filename, truepath.df, range, auto_case=FALSE){
  
  load(paste0(generate_loc,p_estimate.filename))
  
  hist_data = c()
  radius_list =list()
  for (selected in range){
    
    print(c(selected, "SELECTED"))
    dflist_toy_bubble = path.full.df[[selected]]
    if (auto_case==TRUE){
      probability_vec = estimate_p_auto[[selected]]$p_estimate_test/sum(estimate_p_auto[[selected]]$p_estimate_test)
    }else{
      probability_vec = estimate_p_no_auto[[selected]]$p_estimate_test/sum(estimate_p_no_auto[[selected]]$p_estimate_test)
    }
    
    
    bubble_steps_CI <- bubbleCI(dflist_toy_bubble, probability_vec)
    NSWE.lists = calculateErrorBandsBubble(bubble_steps_CI, conversion=TRUE)
    error.NS = NSWE.lists[[1]]
    error.EW = NSWE.lists[[2]]
    center.radius = NSWE.lists[[3]]
    
    center.radius.df = c()
    for (i in c(1:length(center.radius))){
      center.radius.df = c(center.radius.df, center.radius[[i]][c(1,2,3)])
    }
    center.radius.df = data.frame(t(matrix(center.radius.df, nrow=3)))
    
    in.vec = checkPointsInBands(truepath.df[[selected]],center.radius.df)
    print(in.vec)
    hist_data = c(hist_data, sum(in.vec))
    
    radius_list[[selected]] = center.radius.df
  }
  
  return(list(hist_data = hist_data,center.radius.df = radius_list))
}

amount = 1:50
no_auto = load_sims(project_location,sub_directory_no_auto,group = amount)
auto = load_sims(project_location,sub_directory_auto,group = amount)

first_25_auto = ConfintPipeline(auto, "estimate_p_first25_28nov_auto.Rdata",
                                list_valid_subset13, c(1:25), auto_case=TRUE)
last_25_auto = ConfintPipeline(auto, "estimate_p_26_50_28nov_auto.Rdata",
                               list_valid_subset13, c(26:50), auto_case=TRUE)

first_25_no_auto = ConfintPipeline(no_auto, "estimate_p_first25_28nov_no_auto.Rdata",list_valid_subset13, c(1:25))
last_25_no_auto = ConfintPipeline(no_auto, "estimate_p_26_50_28nov_no_auto.Rdata",list_valid_subset13, c(26:50))


average_radius= function(list_df,range){
  out_a = c()
  out_m = c()
  for(i in range){
    out_a = c(out_a,mean(list_df[[i]][,3]))
    out_m = c(out_m,median(list_df[[i]][,3]))
    
  }
  return(list(average=out_a,median=out_m))
}


a = average_radius(first_25_auto$center.radius.df,1:25)
b = average_radius(last_25_auto$center.radius.df,26:50)
c = average_radius(first_25_no_auto$center.radius.df,1:25)
d = average_radius(last_25_no_auto$center.radius.df,26:50)
plot(a$average~a$median)

xx = c(a$median,b$median)
yy = c(first_25_auto$hist_data,last_25_auto$hist_data)


xx_no_auto =  c(c$median,d$median)
yy_no_auto = c(first_25_no_auto$hist_data,last_25_no_auto$hist_data)

plot(yy~(xx),pch=16,col=rgb(0,0,1,.5))#,xlim=c(0,1500000))
abline(lm(yy~(xx)),col=rgb(0,0,1,1))

#plot(jitter(1*(yy>6))~(xx),pch=16,col=rgb(0,0,1,.5))#,xlim=c(0,1500000))

points(yy_no_auto~(xx_no_auto),col=rgb(1,0,0,.5),pch =16)
#points(jitter(1*(yy_no_auto>6))~(xx_no_auto),col=rgb(1,0,0,.5),pch =16)
abline(lm(yy_no_auto~(xx_no_auto)),col="red")



hist(c(first_25_auto$hist_data,last_25_auto$hist_data),freq=F,col= rgb(0,0,1,.5),breaks = 1:13)
lines(density(c(first_25_auto$hist_data,last_25_auto$hist_data)),col="blue")
hist(c(first_25_no_auto$hist_data,last_25_no_auto$hist_data),add= T,freq = F,col = rgb(1,0,0,.5),breaks = 1:13)
lines(density(c(first_25_no_auto$hist_data,last_25_no_auto$hist_data)),col="red")



#-------------------------------------
quartz(width = 11.7,height = 6)
par(mfrow=c(1,2))

counts_auto = c(first_25_auto,last_25_auto)
counts_no_auto = c(first_25_no_auto,last_25_no_auto)


a = hist(counts_auto,breaks=0:13,plot=F)

b=barplot(a$counts,main = "Auto Regressive with Weights ",xlab ="Number of Points in the Confidence Interval" )
axis(side = 1,at = b, label = 1:13)

a = hist(counts_no_auto,breaks=0:13,plot=F)

b=barplot(a$counts,main = "Non-Auto Regressive with Weights ",xlab ="Number of Points in the Confidence Interval" )
axis(side = 1,at = b,label = 1:13)

quartz.save(paste0("images/histograms.pdf"))


quartz()
plot(counts_auto,(counts_no_auto),pch = 15, col = rgb(0,0,0,.15),cex = 5.3,
     )
abline(a=0,b=1)
abline(lm(counts_no_auto~counts_auto))
