library(tidyverse)
library(reshape2)
library(xtable)
library(forcats)
library(progress)
library(gridExtra)
library(TCcrediblebands)
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
image_loc <- "report/images/sim_paths/"

# Load Data ------------------

data_loc <- "main/data/"

latest_full_output_pipeline <- 'output_pipeline_all_geo.Rdata'
a <- load(paste0(data_loc, latest_full_output_pipeline))
eval(parse(text = paste0("output_list_pipeline <- ",a)))


b <- load(paste0(data_loc,"raw_data.Rdata"))
c <- load(paste0(data_loc,"Test_Sims_350_geo.Rdata"))
names(output_list_pipeline) <- names(test_env)


progressBar <- progress::progress_bar$new(
  format = "Processing [:bar] :percent eta: :eta",
  total = length(start_idx:end_idx) , clear = FALSE, width = 51)


for (tc in names(test_env)[start_idx:end_idx]) {
	# true curve -----------
	true_tc <- test_data[[tc]]

	# training curve data frames ----------
	train_curves_auto_ker <- test_env[[tc]][["Auto_NoDeathRegs"]] %>% data_plot_paths_basic()
	train_curves_nonauto_ker <- test_env[[tc]][["NoAuto_NoDeathRegs"]] %>% data_plot_paths_basic()
	
	# base plot prep ---------------
	latrange <- range(c(true_tc$lat, 
					    train_curves_auto_ker$lat,
					    train_curves_nonauto_ker$lat))
	lonrange <- range(c(true_tc$long, 
					    train_curves_auto_ker$long,
					    train_curves_nonauto_ker$long))

    ocean <- c(left = lonrange[1], bottom = latrange[1],
               right = lonrange[2], top = latrange[2])
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

    base_nonauto_ker <- ggvis_paths(data_out = train_curves_nonauto_ker,
                         base_graph = base_map,
                         alpha = .1) +
      labs(x = "", y = "") +
      ggplot2::geom_path(data = true_tc,
                         ggplot2::aes_string(
                           x = 'long', y = 'lat'), color = true_curve_color,
                         size = 2)

    arrangement <- suppressWarnings(arrangeGrob(
      base_auto_ker + labs(title = "Auto & Kernel"),
      base_nonauto_ker + labs(title = "Non Auto & Kernel"), 
      nrow = 1))
    
		suppressWarnings(
			ggsave(plot = arrangement,
			   file = paste0(image_loc,"sim_vis_",tc,".png"),
			   device = "png", width = 10, height = 6.5, units = "in")
			)

	progressBar$tick()
}
