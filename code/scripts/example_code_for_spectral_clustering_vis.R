# example of spectral_clustering_visuals.R

# should restrict vis to max 200 paths.


project_location      = ""
functions_loc         = "code/functions/"
generate_loc          = "data/generate/"
train_f_loc           = "data/training/train/"

sub_dir_no_auto_nd = paste0(generate_loc,"Val_Sims_NoAuto_NoDeathRegs/")
sub_dir_auto_nd    = paste0(generate_loc,"Val_Sims_Auto_NoDeathRegs/")
sub_dir_no_auto_d  = paste0(generate_loc,"Val_Sims_NoAuto_DeathRegs/")
sub_dir_auto_d     = paste0(generate_loc,"Val_Sims_Auto_DeathRegs/")

desired_functions = c("Path_functions.R","13pointreduction.R",
  "projection_map.R","visualizing_color_gradient.R",
  "point_reduction_with_speed.R","estimating_p.R")

#####################
# Loading functions #
#####################

for(f_name in desired_functions){
  source(paste0(functions_loc,f_name))
}

# Ben's way to load data in:
source(paste0(functions_loc,"load_training_first.R"))

############################
# Loading in Training Data #
############################

load(paste0(train_f_loc,"D_matrix_squared_inner.Rdata"))
# ^D_train_squared
load(paste0(train_f_loc,"train_compression.Rdata"))
# ^train13
load(paste0(train_f_loc,"spectral_clustering_structure.Rdata"))
# ^train_info

K        <- train_info$structure$K
t        <- train_info$structure$t
dim      <- train_info$structure$dim
kdensity <- 10

tc_num = 18
simulation18 <- load_sims(project_location, sub_dir_auto_d,
						  group = tc_num)[[tc_num]]

scp_output <- spectral_cluster_process(simulation18, train13, D_train_squared,
						 K = K, t = t, dim = dim, kdensity = kdensity,
						 c_position = 1:2)


# save(scp_output, file = )
ggraphics <- ggvis_all(scp_output, simulation18,
		  c_position = 1:2,
		  zoom = 4,
		  train_alpha = .3, 
		  test_color_power = 1/3, 
		  test_color_low = "white",
		  test_color_high = "red")