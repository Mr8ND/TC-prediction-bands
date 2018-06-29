# first string: files that have common string are mergered (put in grep)
# second string: saved file is "first string" + "second_string" - don't include ".Rdata"



library(tidyverse)

###############################################################
# processing data produced from sca_process_pipeline.R --------

data_loc <- "main/data/"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  file_pattern <- args[1] 
  end_string <- args[2]
}

list_of_files <- list.files(data_loc)[
            list.files(data_loc) %>% grep(pattern = file_pattern)
                                    ]
# initialize names --------
load(paste0(data_loc,'Test_Sims_350.Rdata')) # for order

all_projections <- list()
for (tc_name in names(test_env)) {
    all_projections[[tc_name]] <- 0
}

# fill in all_projections ------

for (fname in list_of_files) {
    load(paste0(data_loc,fname))
    for (tc_name in names(projection_out)) {
        all_projections[[tc_name]] <- projection_out[[tc_name]]
    }
}

if ((sapply(all_projections,class) %>%
    table %>%
    length) == 1 & 
      class(all_projections[[1]]) == "list") {
  save(all_projections, file = paste0(data_loc,
                                      file_pattern, 
                                      end_string,
                                      ".Rdata"))
}


###############################################################