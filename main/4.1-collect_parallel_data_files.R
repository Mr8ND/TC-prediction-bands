# Input Argument --------------
# file pattern: file names that were produced by the pipeline that needs 
#   to be merged
# first string: files that have common string are mergered (put in grep)
# variable name: what we will have the list for storage
# second string: saved file is "first string" + "second_string" 
#   -> don't include ".Rdata"

# example: 
# Rscript main/collect_parallel_data_files.R output_pipeline_alphalevel0.1 output_pipeline _all

library(tidyverse)

###############################################################
# processing data produced from sca_process_pipeline.R --------

data_loc <- "main/data/"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  file_pattern <- args[1]  
  var_name <- args[2]
  end_string <- args[3]
}

list_of_files <- list.files(data_loc)[
            list.files(data_loc) %>% grep(pattern = file_pattern)
                                    ]
# initialize names (if need be) --------

a = load(paste0(data_loc,list_of_files[1])) # for order
all_projections <- list()

has_names_logic <- !is.null(eval(parse(text = paste0(a," %>% names"))))

if (has_names_logic) {
  load(paste0(data_loc,"Test_Sims_350.Rdata"))
  for (tc_name in names(test_env)) {
    all_projections[[tc_name]] <- 0
  }
}




# fill in all_projections ------

if (has_names_logic) { # has names 
  for (fname in list_of_files) {
    obj_name = load(paste0(data_loc,fname))
    for (tc_name in names(eval(parse(text = obj_name)))) {
      all_projections[[tc_name]] <- eval(parse(text = obj_name))[[tc_name]]
    }
  }
} else{ # doesn't have names
  for (fname in list_of_files) {
    obj_name = load(paste0(data_loc,fname))
    null_vec = eval(parse(text = paste0("sapply(",obj_name, ",is.null)")))
    for (tc_name in (1:length(null_vec))[!null_vec]) {
      all_projections[[tc_name]] <- eval(parse(text = obj_name))[[tc_name]]
    }
  }
}

# rename variable
eval(parse(text = paste0(var_name," <- all_projections")))

# save variable
eval(parse(text = paste0(
  "save(", var_name, 
  ", file = paste0(data_loc, file_pattern, end_string,",
  "'.Rdata'))")))



###############################################################