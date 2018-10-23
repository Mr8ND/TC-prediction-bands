library(tidyverse)
library(reshape2)
library(xtable)
library(forcats)
library(progress)
library(latex2exp)
library(gridExtra)
library(TCpredictionbands)
library(ggforce)

# theme -----------------
tc_theme <- theme_minimal() + 
  theme(strip.background = element_rect(fill = "grey90", color = NA),
        plot.title = element_text(hjust = 0.5, size = 18),
        strip.text.x = element_text(size = 13),
        strip.text.y = element_text(size = 13),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        plot.caption = element_text(size = 10))

# Color selection ------------

pb_color_vec <- c("#1b9e77","#d95f02","#7570b3", "#e7298a")
names(pb_color_vec) <- c("convex_hull", "bubble_ci", "delta_ball", "kde")

selected_color <- pb_color_vec["bubble_ci"]

# Load Data ------------------

data_loc <- "main/data/"
image_path <- "report/images/"
table_path <- "report/tables/"

# Changes for the "lon" to "long" have been tested on
# 'output_pipeline_alphalevel0.1_complete_2018-03-20.Rdata' by ND

latest_full_output_pipeline <- 'output_pipeline_alphalevel0.1_all.Rdata'
a <- load(paste0(data_loc, latest_full_output_pipeline))
eval(parse(text = paste0("output_list_pipeline <- ",a)))

##############################
# Spherical PB visualization #
##############################

# Processing Data  ----------------

b <- load(paste0(data_loc,"Test_Sims_350.Rdata"))
names(output_list_pipeline) <- names(test_env)

tc_name <- "AL162005"

spherical_ball <- output_list_pipeline[[tc_name]][["NoAuto_NoDeathRegs"]] %>%
  .[["bubble_ci"]] %>% .[["bubble_structure"]]

sim_curves <- test_env[[tc_name]] %>% .[["NoAuto_NoDeathRegs"]]
sim_curves_df <- sim_curves %>% data_plot_paths_basic

# simulated curve and most deep curve ----------------

vis_sim_curves <- ggvis_paths(data_out = sim_curves_df,
                              base_graph = ggplot(),
                              alpha = .1)

vis_sim_curves <- vis_sim_curves + 
  geom_line(data = spherical_ball[["centers"]],
            aes(x = long, y = lat),
            color = "red") +
  labs(x = "Latitude",
       y = "Longitude",
       title = "Simulated Curves")

# spheres -----------------

radius = (spherical_ball[["centers"]][-1,] - spherical_ball[["positive"]])^2 %>%
  apply(1, function(x) sqrt(sum(x)))

circle_df <- data.frame(spherical_ball[["centers"]],
                        radius = c(0,radius))

vis_spheres <- ggplot() + geom_circle(data = circle_df,
                       aes(y0 = lat, x0 = long,
                           r = radius),
                       color = "red") +
  geom_point(data = circle_df,
             aes(x = long, y = lat), color = selected_color) + 
  labs(x = "Latitude",
       y = "Longitude",
       title = "Union of Spheres") 

# contour ----------------

vis_contour <- TCpredictionbands::ggvis_bubble_data(
  bubble_plot_data = spherical_ball,
  base_graph = ggplot(), 
  color = 'red',
  connect = TRUE, 
  centers = FALSE,
  linewidth = .5) +
  labs(x = "Latitude",
       y = "Longitude",
       title = "Contour Visualization") +
  geom_point(data = circle_df[-nrow(circle_df),],
             aes(x = long, y = lat), color = selected_color) 

# full arrangment of graphics ---------------

arrange_vis <- arrangeGrob(vis_sim_curves + 
                             ylim(range(sim_curves_df$lat) + c(-20, 21)) + 
                             xlim(range(sim_curves_df$long)) +
                             tc_theme,
                        vis_spheres + 
                          ylim(range(sim_curves_df$lat) + c(-20, 21)) + 
                          xlim(range(sim_curves_df$long))  +
                          tc_theme,
                        vis_contour + 
                          ylim(range(sim_curves_df$lat) + c(-20, 21)) + 
                          xlim(range(sim_curves_df$long)) +
                          tc_theme, 
                        nrow = 1)

ggsave(plot = arrange_vis,
       filename = paste0(image_path,"spherical_ball_vis.png"),
       device = "png", width = 10, height = 3.5, units = "in")
