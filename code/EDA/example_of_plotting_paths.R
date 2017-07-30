library(rworldmap)
newmap = getMap(resolution = "low")

functions_loc = "code/functions/"
source(paste0(functions_loc,"load_training_first.R"))

validate_list =load_validate_data_list("")

# 9/AL011978 is too long, but not bad.
# Same for 10/AL011983, AL031982/58, AL031985/60
# Check out 42/AL021987. :D 83,84
test_list = load_generated_curve_list(project_location = "",sub_directory = "AL011960_sims/")

plot(newmap, ylim = c(9, 40), xlim = c(-110, 2), asp = 1)

num_paths = length(test_list)

for(i in 1:100){
    lines(test_list[[i]][,1],test_list[[i]][,2],col="red")
}


lines(validate_list[[2]][,6],validate_list[[2]][,5],col="green")



######
# code for plot included in sharelatex file:
desired_functions = c("Path_functions.R","13pointreduction.R","projection_map.R")

# functions
for(f_name in desired_functions){
  source(paste0(functions_loc,f_name))
}

test_list = load_generated_curve_list("","AL021972/") 
test13 =thirteen_points_listable(list_df = test_list,c_position = 1:2,lonlat = TRUE)

library(rworldmap)
newmap = getMap(resolution = "low")

n1 = 9
n2 = 29

plot(newmap, ylim = c(9, 21), xlim = c(-95, -87), asp = 1)

# creating the paths (13 point and full number of point representation)
for(i in c(n1,n2)){
  points(test_list[[i]][,1],test_list[[i]][,2],col="black",pch=16,cex =1)
  lines(test_list[[i]][,1],test_list[[i]][,2])
  points(test13[[i]][,1],test13[[i]][,2],col="red",pch=15,cex =1)
  
}

# lines between the 2 paths
for( i in 1:13){
  lines(x = c(test13[[n1]][i,1],test13[[n2]][i,1]),
        y = c(test13[[n1]][i,2],test13[[n2]][i,2]),lty = 2,col="blue")
}
