#############
# Libraries #
#############

library(geosphere)

#############
# Locations #
#############

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

load(paste0(train_f_loc,"train_compression.Rdata"))

path_mat_list  = train13
speed_list = train12_speed

#############################
# Loading in Simulated data #
#############################

files_auto_d = list.files(paste0(project_location,sub_dir_auto_d))
file_names = as.vector(files_auto_d)[amount]


amount = 1:25
amount_string = "1_25"


names = c("no_auto_d","auto_d","no_auto_nd","auto_nd")
sims = list()
sims[["no_auto_d"]]  = load_sims(project_location,sub_dir_no_auto_d,
                                group = amount)
sims[["auto_d"]]     = load_sims(project_location,sub_dir_auto_d,
                                group = amount)
sims[["no_auto_nd"]] = load_sims(project_location,sub_dir_no_auto_nd,
                                group = amount)
sims[["auto_nd"]]    = load_sims(project_location,sub_dir_auto_nd,
                                group = amount)
sims$names = names


########################
########################
# Algorithmic approach #
########################
########################

#################################
# Loading in training structure #
#################################

load(paste0(train_f_loc,"D_matrix_squared_inner.Rdata"))
D_train = D_train_squared

###################################
# compression of generated curves #
###################################

gen_files = list.files(generate_loc)

if(paste0("test13_",amount_string,".Rdata") %in% gen_files){
load(file = paste0(generate_loc,"test13_",amount_string,".Rdata"))
  }else{
  test13 = list()
  test13$names = names

  for(j in 1:4){

    print(paste0(names[j],":"))

    for(i in amount){
      compression_both = compression_points_listable(
                                          list_df = sims[[names[j]]][[i]],
                                          c_position = 1:2,lonlat = TRUE)
      test13[[names[j]]][[i]] = lapply(compression_both,
                                          function(x) x$new_13compression)
      cat(paste0(i,","))
    }
    cat("\n")
  }
  save(test13,file = paste0(generate_loc,"test13_",amount_string,".Rdata"))
}


gen_files = list.files(generate_loc)

for(j in 1:4){
  if(paste0("D_test_",names[j],"_",amount_string,".Rdata") %in% gen_files){
    #pass
  }else{
    print(paste0(names[j],":"))
    D_test = list()
    for(i in amount){
      D_test[[i]] = distMatrixPath_t2t_path(
                                    path_mat_list_test = test13[[names[j]]][[i]],
                                    path_mat_list_train = path_mat_list)
      cat(paste0(i,","))
    }
    cat("\n")

    save(D_test,file= paste0(generate_loc,"D_test_",names[j],
                              "_",amount_string,".Rdata"))
  }
}

D_test_all = list()
D_test_all$names = names
for(j in 1:4){
  load(file= paste0(generate_loc,"D_test_",names[j],
                              "_",amount_string,".Rdata"))
  D_test_all[[names[j]]] = D_test
}


# #D_test_auto_list = list()
# D_test_no_auto_list = list()

# for(i in amount){
#   #D_test_auto_list[[i]] = 
#   #  distMatrixPath_train_test(path_mat_list_test = test13_auto_list[[i]],
#   #                            path_mat_list_train = train13)
  
#   D_test_no_auto_list[[i]] = 
#     distMatrixPath_train_test(path_mat_list_test = test13_no_auto_list[[i]],
#                               path_mat_list_train = train13)
#   print(i)
# }

# save(D_test_no_auto_list,file = paste0(generate_loc,"dmat_",amount_string,"_28nov_no_auto.Rdata"))
# # load(paste0(generate_loc,"dmat_first5_27nov.Rdata")) # D_test_auto_list,D_test_no_auto_list


K = 4
t = 1
dim = 5
kdensity = 10

training_structure=training_structure_estimating_p(D_train,K=4,t=1,dim=5,plot_n = 0)
save(training_structure,file = paste0(train_f_loc,"training_structure.Rdata"))

estimate_p = list()
estimate_p$names = names

for(j in 1:4){
  estimate_p[[names[j]]] = list()
  for(i in amount){
    estimate_p[[names[j]]][[i]] = 
      estimate_p_wrapper(training_structure_estimating_p = training_structure,
                         D_test = D_test_all[[names[j]]][[i]],
                        kdensity = kdensity)
  }
}
save(estimate_p,file=paste0(generate_loc,"estimate_p_",amount_string,".Rdata"))



#################
# Visualization #
################# 

# validation_data=load_validate_data_list_names("",names =T)

# list_valid_subset = list()
# for(name in seq_along(file_names)){
#   list_valid_subset[[name]] = validation_data$list[[which(validation_data$names == paste0(file_names[name],".txt"))]]
# }

# list_valid = compression_points_listable(list_df = list_valid_subset,lonlat = FALSE)
# list_valid_path13 = lapply(list_valid,function(x) x$new_13compression)



# #i = sample(seq_along(file_names),1)
# for(i in seq_along(file_names)){
#   print(i)
#   quartz(width = 15,height = 12)
#   par(mfrow=c(4,2))
#   for(j in 1:4){
#     plotting_funct(list_valid_path13[[i]],
#       list_estimate = test13[[ names[j] ]][[i]],
#       weights= estimate_p[[names[j]]][[i]]$p_estimate_test,
#       main= paste(names[j],file_names[i]))

#     weights_all = rep(1,length(estimate_p[[names[j]]][[i]]$p_estimate_test))

#     plotting_funct_color_select(list_valid_path13[[i]],
#       list_estimate = test13[[names[j]]][[i]],
#       cols= c("black"),index =weights_all,
#       main= paste("No Weights",names[j],file_names[i]))

#     }
# }





