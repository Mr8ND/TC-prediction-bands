# loading in data:

########################
##### Read in data #####
########################
load_train_first_data = function(project_location){  
  files_train <- list.files(path = paste0(project_location,"data/training/train/"),pattern = ".txt")
  #length(files_train)
  
  for (i in seq_along(files_train)){
    df.name <- nchar(files_train[i])-4
    assign(substr(files_train[i], 1, df.name), read.table(paste0(project_location,"data/training/train/",files_train[i])),envir=.GlobalEnv)
  }
  
}

load_train_first_data_list = function(project_location){  
  files_train <- list.files(path = paste0(project_location,"data/training/train/"),pattern = ".txt")
  #length(files_train)
  train_first_list = list()
  for (i in seq_along(files_train)){
    df.name <- nchar(files_train[i])-4
    train_first_list[[i]] = read.table(paste0(project_location,"data/training/train/",files_train[i]))
  }
  return(train_first_list)
}


load_validate_data = function(project_location){  
  files_train <- list.files(path = paste0(project_location,"data/training/validate/"),pattern = ".txt")
  #length(files_train)
  
  for (i in seq_along(files_train)){
    df.name <- nchar(files_train[i])-4
    assign(substr(files_train[i], 1, df.name), read.table(paste0(project_location,"data/training/validate/",files_train[i])),envir=.GlobalEnv)
  }
  
}


load_validate_data_list = function(project_location){  
  files_train <- list.files(path = paste0(project_location,"data/training/validate/"),pattern = ".txt")
  #length(files_train)
  validate_list = list()
  for (i in seq_along(files_train)){
    df.name <- nchar(files_train[i])-4
    validate_list[[i]] = read.table(paste0(project_location,"data/training/validate/",files_train[i]))
  }
  return(validate_list)
}


load_generated_curve_list = function(project_location,sub_directory){  
  files_train <- list.files(paste0(project_location,"data/generate/",sub_directory))
  #length(files_train)
  generated_list = list()
  for (i in seq_along(files_train)){
    generated_list[[i]] = read.table(paste0(project_location,"data/generate/",sub_directory,files_train[i]),header = T,sep=",")
  }
  return(generated_list)
}
