library(tidyverse)
library(reshape2)
library(xtable)
library(forcats)
library(progress)
library(gridExtra)
library(TCpredictionbands)
library(ggmap)

# args -----------------

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  start_idx <- as.numeric(args[1])  #1
  end_idx <- as.numeric(args[2])	# 306
}

# parameters ----------------
train_curve_alpha <- .1
# train_curve_color = "black" (looks programmed in)
zoom <- 4
true_curve_color <- "green"
color_vec <- c("#00E5E5","#CB00C5" ,"#0300D8", "#BF0700")
image_loc <- "images_shiny/"

# Load Data ------------------

data_loc <- "main/data/"

latest_full_output_pipeline <- 'output_pipeline_all.Rdata'
a <- load(paste0(data_loc, latest_full_output_pipeline))
eval(parse(text = paste0("output_list_pipeline <- ",a)))


b <- load(paste0(data_loc,"raw_data.Rdata"))
c <- load(paste0(data_loc,"Test_Sims_350.Rdata"))
names(output_list_pipeline) <- names(test_env)


pb_options <- list(1,2,3,4,1:4)
pb_name_options <- c("kde", "bubble_ci", "delta_ball", "convex_hull", "all")
names(color_vec) <- pb_name_options[-5]

progressBar <- progress::progress_bar$new(
  format = "Processing [:bar] :percent eta: :eta",
  total = length(test_env[start_idx:end_idx]) , clear = FALSE, width = 51)


for (tc in names(test_env)[start_idx:end_idx]) {
	# true curve -----------
	true_tc <- test_data[[tc]]

	# training curve data frames ----------
	train_curves_auto_ker <- test_env[[tc]][["Auto_NoDeathRegs"]] %>% data_plot_paths_basic()
	train_curves_auto_bin <- test_env[[tc]][["Auto_DeathRegs"]] %>% data_plot_paths_basic()
	train_curves_nonauto_ker <- test_env[[tc]][["NoAuto_NoDeathRegs"]] %>% data_plot_paths_basic()
	train_curves_nonauto_bin <- test_env[[tc]][["NoAuto_DeathRegs"]] %>% data_plot_paths_basic()
	
	# base plot prep ---------------
	latrange <- range(c(true_tc$lat, 
					    train_curves_auto_ker$lat,
					    train_curves_auto_bin$lat,
					    train_curves_nonauto_ker$lat,
					    train_curves_nonauto_bin$lat))
	longrange <- range(c(true_tc$long, 
					    train_curves_auto_ker$long,
					    train_curves_auto_bin$long,
					    train_curves_nonauto_ker$long,
					    train_curves_nonauto_bin$long))

    ocean <- c(left = longrange[1], bottom = latrange[1],
               right = longrange[2], top = latrange[2])
    map   <- suppressWarnings(
    			ggmap::get_stamenmap(ocean, zoom = zoom, maptype = "toner-lite")
    			)
    
    # base map --------------
    base_map <- suppressWarnings(
    	ggmap::ggmap(map) +
	      theme_void()
	      )

    # adding simulations and true curve ------------
    base_auto_ker <- ggvis_paths(data_out = train_curves_auto_ker,
                         base_graph = base_map,
                         alpha = .1)  + 
      labs(x = "", y = "") +
      ggplot2::geom_path(data = true_tc,
                         ggplot2::aes_string(
                           x = 'long', y = 'lat'), color = true_curve_color,
                         size = 2)

    base_auto_bin <- ggvis_paths(data_out = train_curves_auto_bin,
                         base_graph = base_map,
                         alpha = .1) +
      labs(x = "", y = "") +
      ggplot2::geom_path(data = true_tc,
                         ggplot2::aes_string(
                           x = 'long', y = 'lat'), color = true_curve_color,
                         size = 2)

    base_nonauto_ker <- ggvis_paths(data_out = train_curves_nonauto_ker,
                         base_graph = base_map,
                         alpha = .1) +
      labs(x = "", y = "") +
      ggplot2::geom_path(data = true_tc,
                         ggplot2::aes_string(
                           x = 'long', y = 'lat'), color = true_curve_color,
                         size = 2)
      
    base_nonauto_bin <- ggvis_paths(data_out = train_curves_nonauto_bin,
                         base_graph = base_map,
                         alpha = .1) +
      labs(x = "", y = "") +
      ggplot2::geom_path(data = true_tc,
                         ggplot2::aes_string(
                           x = 'long', y = 'lat'), color = true_curve_color,
                         size = 2)

    # adding prediction bands --------
    for (pb_idx in 1:5) {
    	which_pb <- pb_name_options[pb_options[[pb_idx]]]

    	ggout_auto_ker <- base_auto_ker
    	ggout_auto_bin <- base_auto_bin
    	ggout_nonauto_ker <- base_nonauto_ker
    	ggout_nonauto_bin <- base_nonauto_bin

    	for (pb in which_pb) {
    		if(pb == "kde") {
    			kde_contour_dfs_auto_ker <- TCpredictionbands::contour_list_to_df(
        						    output_list_pipeline[[tc]][["Auto_NoDeathRegs"]][["kde"]][["contour"]])
      			ggout_auto_ker <- TCpredictionbands::ggvis_kde_contour(kde_contour_dfs_auto_ker,
      								base_graph = ggout_auto_ker,
                                 	color = color_vec[pb])

      			kde_contour_dfs_auto_bin <- TCpredictionbands::contour_list_to_df(
        						    output_list_pipeline[[tc]][["Auto_DeathRegs"]][["kde"]][["contour"]])
      			ggout_auto_bin <- TCpredictionbands::ggvis_kde_contour(kde_contour_dfs_auto_bin,
      								base_graph = ggout_auto_bin,
                                 	color = color_vec[pb])

      			kde_contour_dfs_nonauto_ker <- TCpredictionbands::contour_list_to_df(
        						    output_list_pipeline[[tc]][["NoAuto_NoDeathRegs"]][["kde"]][["contour"]])
      			ggout_nonauto_ker <- TCpredictionbands::ggvis_kde_contour(kde_contour_dfs_nonauto_ker,
      								base_graph = ggout_nonauto_ker,
                                 	color = color_vec[pb])

      			kde_contour_dfs_nonauto_bin <- TCpredictionbands::contour_list_to_df(
        						    output_list_pipeline[[tc]][["NoAuto_DeathRegs"]][["kde"]][["contour"]])
      			ggout_nonauto_bin <- TCpredictionbands::ggvis_kde_contour(kde_contour_dfs_nonauto_bin,
      								base_graph = ggout_nonauto_bin,
                                 	color = color_vec[pb])
	    		}
	    	if (pb == "bubble_ci") {
	    		bubble_plot_data_auto_ker <- output_list_pipeline[[tc]][["Auto_NoDeathRegs"]][["bubble_ci"]][["bubble_structure"]] %>%
        											sapply(function(x) if (class(x) == "matrix") {data.frame(x)}else{x})
		      	ggout_auto_ker <- TCpredictionbands::ggvis_bubble_data(
		        					bubble_plot_data = bubble_plot_data_auto_ker,
		        					base_graph = ggout_auto_ker, 
		        					color = color_vec[pb],
		        					connect = TRUE, 
		        					centers = FALSE) 

		      	bubble_plot_data_auto_bin <- output_list_pipeline[[tc]][["Auto_DeathRegs"]][["bubble_ci"]][["bubble_structure"]] %>%
        											sapply(function(x) if (class(x) == "matrix") {data.frame(x)}else{x})
		      	ggout_auto_bin <- TCpredictionbands::ggvis_bubble_data(
		        					bubble_plot_data = bubble_plot_data_auto_bin,
		        					base_graph = ggout_auto_bin, 
		        					color = color_vec[pb],
		        					connect = TRUE, 
		        					centers = FALSE) 

		      	bubble_plot_data_nonauto_ker <- output_list_pipeline[[tc]][["NoAuto_NoDeathRegs"]][["bubble_ci"]][["bubble_structure"]] %>%
        											sapply(function(x) if (class(x) == "matrix") {data.frame(x)}else{x})
		      	ggout_nonauto_ker <- TCpredictionbands::ggvis_bubble_data(
		        					bubble_plot_data = bubble_plot_data_nonauto_ker,
		        					base_graph = ggout_nonauto_ker, 
		        					color = color_vec[pb],
		        					connect = TRUE, 
		        					centers = FALSE) 

		      	bubble_plot_data_nonauto_bin <- output_list_pipeline[[tc]][["NoAuto_DeathRegs"]][["bubble_ci"]][["bubble_structure"]] %>%
        											sapply(function(x) if (class(x) == "matrix") {data.frame(x)}else{x})
		      	ggout_nonauto_bin <- TCpredictionbands::ggvis_bubble_data(
		        					bubble_plot_data = bubble_plot_data_nonauto_bin,
		        					base_graph = ggout_nonauto_bin, 
		        					color = color_vec[pb],
		        					connect = TRUE, 
		        					centers = FALSE) 
	    		}
    		if(pb == "delta_ball") {
	    		ggout_auto_ker <-  TCpredictionbands::ggvis_delta_ball_contour(
	    						   	output_lines = output_list_pipeline[[tc]][["Auto_NoDeathRegs"]][["delta_ball"]][["structure"]],
	                               	color = color_vec[pb],
	                               	base_graph = ggout_auto_ker)
	    		ggout_auto_bin <-  TCpredictionbands::ggvis_delta_ball_contour(
	    						   	output_lines = output_list_pipeline[[tc]][["Auto_DeathRegs"]][["delta_ball"]][["structure"]],
	                               	color = color_vec[pb],
	                               	base_graph = ggout_auto_bin)
	    		ggout_nonauto_ker <-  TCpredictionbands::ggvis_delta_ball_contour(
	    						   	output_lines = output_list_pipeline[[tc]][["NoAuto_NoDeathRegs"]][["delta_ball"]][["structure"]],
	                               	color = color_vec[pb],
	                               	base_graph = ggout_nonauto_ker)
	    		ggout_nonauto_bin <-  TCpredictionbands::ggvis_delta_ball_contour(
	    						   	output_lines = output_list_pipeline[[tc]][["NoAuto_DeathRegs"]][["delta_ball"]][["structure"]],
	                               	color = color_vec[pb],
	                               	base_graph = ggout_nonauto_bin)
	    		}
	    	if (pb == "convex_hull") {
	    		ggout_auto_ker  <- TCpredictionbands::ggvis_convex_hull(
	    							output_lines = output_list_pipeline[[tc]][["Auto_NoDeathRegs"]][["convex_hull"]][["structure"]],
                                    color = color_vec[pb],
                                    base_graph = ggout_auto_ker)
	    		ggout_auto_bin  <- TCpredictionbands::ggvis_convex_hull(
	    							output_lines = output_list_pipeline[[tc]][["Auto_DeathRegs"]][["convex_hull"]][["structure"]],
                                    color = color_vec[pb],
                                    base_graph = ggout_auto_bin)
	    		ggout_nonauto_ker  <- TCpredictionbands::ggvis_convex_hull(
	    							output_lines = output_list_pipeline[[tc]][["NoAuto_NoDeathRegs"]][["convex_hull"]][["structure"]],
                                    color = color_vec[pb],
                                    base_graph = ggout_nonauto_ker)
	    		ggout_nonauto_bin  <- TCpredictionbands::ggvis_convex_hull(
	    							output_lines = output_list_pipeline[[tc]][["NoAuto_DeathRegs"]][["convex_hull"]][["structure"]],
                                    color = color_vec[pb],
                                    base_graph = ggout_nonauto_bin)
	    		}
			}

		arrangement <- suppressWarnings(arrangeGrob(
						ggout_auto_ker + labs(title = "Auto & Kernel"),
				   		ggout_auto_bin + labs(title = "Auto & Logistic"),
				   		ggout_nonauto_ker + labs(title = "Non Auto & Kernel"),
				   		ggout_nonauto_bin + labs(title = "Non Auto & Logistic"),
				   		nrow = 2))

		suppressWarnings(
			ggsave(plot = arrangement,
			   file = paste0(image_loc,"pb_vis_",tc,"_",pb_name_options[pb_idx],".png"),
			   device = "png", width = 10, height = 6.5, units = "in")
			)
    }
	progressBar$tick()
}
