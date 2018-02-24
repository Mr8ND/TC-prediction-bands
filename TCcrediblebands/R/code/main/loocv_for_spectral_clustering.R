## Bringing in Functions

library(progress)

functions_loc <- "code/functions/"
data_loc <- "data/"

desired_functions <- c("path_functions.R","thirteen_point_compression.R",
                    "projection_map.R","loocv_functions.R")

for (f_name in desired_functions) {
  source(paste0(functions_loc,f_name))
}

# did we already run this analysis? -------------------------

if ("loocv_optimal.Rdata" %in% list.files(data_loc)) {
	input <- ""
	while (!(input %in% c("y","n"))) {
		input <- readline(prompt = paste0("Optimal already obtained via LOOCV,",
		                                  " run again? [y/n]: "))
	}
}else{
	input <- "y"
}

if (input == "n") {
 	stop() # check if works - maybe do exit() instead?
}

# Load in training data -------------------------

load(paste0(data_loc,"raw_data.Rdata"))

# Create 13 point compression -------------------------

train_list_13_point <- thirteen_points_listable(train_data, 
											   c_position = c(6,5),
											   lonlat = TRUE, verbose = TRUE)

# Create Distance Matrix -------------------------

Dmat <- distMatrixPath_innersq(train_list_13_point) 

# Saving training_structure -------------------------

save(train_list_13_point, Dmat , file = paste0(data_loc,
                                               "sca_training_structure.Rdata"))

# Run LOOCV  -------------------------

K_range <- c(4,7,9,11)
t_range <- c(1,3,5,7)

n_K <- length(K_range)
n_t <- length(t_range)

error_mat <- matrix(0, 
					ncol = n_K,
					nrow = n_t)

pb <- progress_bar$new(
  format = "  Processing [:bar] :percent eta: :eta",
  total = n_K * n_t, clear = FALSE, width = 40)

for (i in 1:n_K) {
	for (j in 1:n_t) {
		k_select <- K_range[i]
		t_select <- t_range[i]

		loocv_out <- loocv_wrapper(path_mat_list = train_list_13_point,
								   Dmat = Dmat, K = k_select,
								   t = t_select,
								   longlat = TRUE, project_size = 4,
								   verbose = FALSE)

		error_mat[j,i] <- mean(loocv_out$diff_curves)

		pb$tick()
	}
}

# select which is optimal  -------------------------
# expected to be it is K = 4, t = 1

mat_idx <- which.min(error_mat)
col_idx <- ceiling(mat_idx/n_K)
row_idx <- mat_idx - (ceiling(mat_idx/n_K) - 1) * n_K

loocv_optimal <- list()
loocv_optimal[["K"]] <- K_range[row_idx]
loocv_optimal[["t"]] <- t_range[col_idx]

save(loocv_optimal, file = paste0(data_loc,"loocv_optimal.Rdata"))

print(paste0("Optimal values;",
	  "K:", as.character(loocv_optimal[["K"]]),
	  ", t:", as.character(loocv_optimal[["t"]])))
