library(geosphere)
library(rworldmap)
library(plot3D)

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
  "projection_map.R","visualizing_color_gradient.R")

#####################
# Loading functions #
#####################

for(f_name in desired_functions){
  source(paste0(functions_loc,f_name))
}

# Ben's way to load data in:
source(paste0(functions_loc,"load_training_first.R"))

######################################
#### Loading in Data (Training) ######
######################################

load(paste0(train_f_loc,"D_matrix.Rdata"))
source(paste0(functions_loc,"load_training_first.R"))

train_list =load_train_first_data_list("")
train13 =thirteen_points_listable(list_df = train_list,lonlat = FALSE)

Dmat = D_train
path_mat_list  = train13

load(file = paste0(train_f_loc,"capture.Rdata"))
load(paste0(train_f_loc,"training_structure.Rdata"))

##################################
#### Loading in Data (Test) ######
##################################
load(paste0(generate_loc,"estimate_p_first25_28nov_no_auto.Rdata"))

load(paste0(generate_loc,"test13_first25_28nov_no_auto.Rdata"))
load(paste0(generate_loc,"dmat_first25_28nov_no_auto.Rdata"))

load(paste0(generate_loc,"estimate_p_first25_28nov_auto.Rdata"))
load(paste0(generate_loc,"test13_first25_28nov_auto.Rdata"))
load(paste0(generate_loc,"dmat_first25_28nov_auto.Rdata"))



validation_data=load_validate_data_list_names("",names =T)


files_no_auto = list.files(paste0(project_location,sub_directory_no_auto))
file_names = unlist(strsplit(x = files_no_auto,split = "_non_ar_sims"))

list_valid_subset = list()
for(name in seq_along(file_names)){
  list_valid_subset[[name]] = validation_data$list[[which(validation_data$names == paste0(file_names[name],".txt"))]]
}
list_valid_subset13 = thirteen_points_listable(list_df = list_valid_subset,lonlat = FALSE)

#########
#########
#########
# Plots #
#########
#########
#########

  
newmap = getMap(resolution = "low")


#############################
### Showing Distance Calc ###
#############################
# the data we will be using is a from an inverse mapping (LOOCV step)
# -- mostly so we can talk about it if someone asks (hidden secrets :) )

i = 2
loocv_storage = capture[[1]][[1]]
inverse_path = loocv_storage$predicted[[i]]
original_path = train_list[[i]] 
original_path13 = train13[[i]]



quartz(width = 11,height = 4)
par(mfrow=c(1,3))
plot(newmap, ylim = c(10,50), xlim = c(-62,-60), asp = 1,main="Distance Between Curves")

lines(original_path[,6],original_path[,5],col="black")
points(original_path[,6],original_path[,5],col="black",pch = 16)
points(original_path13[,1],original_path13[,2],col = "red",pch = 15)
lines(inverse_path[,1],inverse_path[,2],col="black")
points(inverse_path[,1],inverse_path[,2],col = "red",pch = 15)

for(i in 1:13){
  lines(c(original_path13[i,1],inverse_path[i,1]),c(original_path13[i,2],inverse_path[i,2]),col="blue",lty="dotted")
}

#############################################################
### Showing generation vs training in the projected space ###
#############################################################
phi_map_out = training_structure$phi_map_out
psi_map_x = phi_map_out$psi_map_x
psi_map_x = psi_map_x[,-1]
lambda = phi_map_out$lambda

test_projected= estimate_p_no_auto[[i]]$test_projected

scatter3D(psi_map_x[,1],psi_map_x[,2],psi_map_x[,3],col=rgb(0,0,0,.1),
          xlab = "PC 1",ylab="PC 2",zlab="PC 3",
          main="Projected Space, Training and Generated Points",pch=16,
          phi = 10,theta=-20)
points3D(test_projected[,1],test_projected[,2],test_projected[,3],col=rgb(1,0,0,.1),add=T,pch=16)

####################################
### Showing Decay of Eigenvalues ###
####################################

barplot(lambda,main="Eigenvalues From Diffusion Decomposition")

quartz.save(paste0("images/",as.character(i),"_poster3plots.pdf"))


####################################################
#### Auto and Non Auto, with and without weights ###
####################################################

#ops <- par(mfrow = c(2,2),
#          oma = c(5,4,0,0) + 0.1,
#          mar = c(0,0,1,1) + 0.1)
i = 8
quartz(width = 8,height = 6.5)
par(mfrow=c(2,2))
#par(ops)

plotting_funct(list_valid_subset13[[i]],
	list_estimate = test13_no_auto_list[[i]],
	weights= estimate_p_no_auto[[i]]$p_estimate_test,main= paste("No Auto"))#,file_names[i]))

plotting_funct(list_valid_subset13[[i]],
	list_estimate = test13_auto_list[[i]],
	weights= estimate_p_auto[[i]]$p_estimate_test,main= paste("Auto"))#,file_names[i]))

weights_all = rep(1,200)

plotting_funct_color_select(list_valid_subset13[[i]],
	list_estimate = test13_no_auto_list[[i]],
	cols= c("black"),index =weights_all,
	main= paste("No Auto, No Weights"))#,file_names[i]))

plotting_funct_color_select(list_valid_subset13[[i]],
	list_estimate = test13_auto_list[[i]],
	cols= c("black"),index =weights_all,
	main= paste("Auto, No Weights"))#,file_names[i]))
 
quartz.save(paste0("images/",file_names[i],"_poster4plots.pdf"))



#####
# mixing auto and no-auto curves -> probably not for realz
combo_list = test13_no_auto_list[[i]]
start_length = length(combo_list)
for(j in 1:length(test13_auto_list[[i]])){
	combo_list[[j + start_length]] = test13_auto_list[[i]][[j]]
}
combo_weights = c(estimate_p_no_auto[[i]]$p_estimate_test,estimate_p_auto[[i]]$p_estimate_test)
combo_weights_all = rep(1,400)

quartz(width = 8,height = 6.5)

plotting_funct(list_valid_subset13[[i]],
	list_estimate = combo_list,
	weights= combo_weights,main= paste("Both"))


quartz(width = 8,height = 6.5)

plotting_funct_color_select(list_valid_subset13[[i]],
	list_estimate = combo_list,
	cols= c("black"),index =combo_weights_all,
	main= paste("Both, No weights"))#,file_names[i]))



