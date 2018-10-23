library(tidyverse)
library(reshape2)
library(xtable)
library(forcats)
library(progress)
library(gridExtra)
library(TCpredictionbands)
library(ggmap)
library(grid)

# parameters ----------------
train_curve_alpha <- .1
# train_curve_color = "black" (looks programmed in)
zoom <- 4
true_curve_color <- "#ffff00"
pb_color_vec <- c("#1b9e77","#d95f02","#7570b3", "#e7298a")
names(pb_color_vec) <- c("convex_hull", "bubble_ci", "delta_ball", "kde")
image_loc <- "images_shiny/"

# Load Data ------------------

data_loc <- "main/data/"
image_path <- "report/images/"

latest_full_output_pipeline <- 'output_pipeline_alphalevel0.1_all.Rdata'
a <- load(paste0(data_loc, latest_full_output_pipeline))
eval(parse(text = paste0("output_list_pipeline <- ",a)))


b <- load(paste0(data_loc,"raw_data.Rdata"))
c <- load(paste0(data_loc,"Test_Sims_350.Rdata"))
names(output_list_pipeline) <- names(test_env)

############
# graphics #
############

# theme
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

# KDE (non auto kernel) --------------------
tc <- "AL181984" # number 9
kde_true_tc <- test_data[[tc]]
kde_train_curves_nonauto_kernel <- test_env[[tc]][["NoAuto_NoDeathRegs"]] %>% 
  data_plot_paths_basic()

# base map <<<<<<<
latrange <- range(c(kde_true_tc$lat, 
                    kde_train_curves_nonauto_kernel$lat))
longrange <- range(c(kde_true_tc$long, 
                    kde_train_curves_nonauto_kernel$long))

ocean <- c(left = longrange[1], bottom = latrange[1],
           right = longrange[2], top = latrange[2])
map   <- suppressWarnings(
  ggmap::get_stamenmap(ocean, zoom = zoom, maptype = "toner-lite")
)

base_graph_kde <- ggmap::ggmap(map) 
# end of base map <<<<<<<<

graphic_kde <- ggvis_paths(data_out = kde_train_curves_nonauto_kernel,
                                alpha = .1,
                                base_graph = base_graph_kde)  + 
  labs(x = "Longitude", y = "Latitude") +
  ggplot2::geom_path(data = kde_true_tc,
                     ggplot2::aes_string(
                       x = 'long', y = 'lat'), color = true_curve_color,
                     size = 1) +
  labs(caption = "TC #AL181984: Non-Autoregressive Curves\n  with Kernel-based Lysis") 

kde_contour_dfs_nonauto_kernel <- TCpredictionbands::contour_list_to_df(
  output_list_pipeline[[tc]][["NoAuto_NoDeathRegs"]][["kde"]][["contour"]])

graphic_kde <- TCpredictionbands::ggvis_kde_contour(
  kde_contour_dfs_nonauto_kernel,
  base_graph = graphic_kde,
  color = pb_color_vec["kde"])


# Convex (auto logistic) --------------------
tc <- "AL071995" # number 29
convex_true_tc <- test_data[[tc]]
convex_train_curves_auto_logistic <- test_env[[tc]][["Auto_DeathRegs"]] %>% 
  data_plot_paths_basic()

# base map <<<<<<<
latrange <- range(c(convex_true_tc$lat, 
                    convex_train_curves_auto_logistic$lat,
                    output_list_pipeline %>% .[[tc]] %>% 
                      .[["Auto_DeathRegs"]] %>% .[["convex_hull"]] %>% 
                      .[["structure"]] %>% .[,"lat"]))
longrange <- range(c(convex_true_tc$long, 
                    convex_train_curves_auto_logistic$long,
                    output_list_pipeline %>% .[[tc]] %>% 
                      .[["Auto_DeathRegs"]] %>% .[["convex_hull"]] %>% 
                      .[["structure"]] %>% .[,"long"]))

ocean <- c(left = longrange[1], bottom = latrange[1],
           right = longrange[2], top = latrange[2])
map   <- suppressWarnings(
  ggmap::get_stamenmap(ocean, zoom = zoom, maptype = "toner-lite")
)

base_graph_convex <- ggmap::ggmap(map) 
# end of base map <<<<<<<<

graphic_convex <- ggvis_paths(data_out = convex_train_curves_auto_logistic,
                           alpha = .1,
                           base_graph = base_graph_convex)  + 
  labs(x = "Longitude", y = "Latitude") +
  ggplot2::geom_path(data = convex_true_tc,
                     ggplot2::aes_string(
                       x = 'long', y = 'lat'), color = true_curve_color,
                     size = 1) +
  labs(caption = "TC #AL071995: Autoregressive Curves\n  with Logistic-based Lysis") 

graphic_convex  <- TCpredictionbands::ggvis_convex_hull(
  output_lines = output_list_pipeline[[tc]][["Auto_DeathRegs"]][["convex_hull"]][["structure"]],
  color = pb_color_vec["convex_hull"],
  base_graph = graphic_convex)

# Delta ball (auto kernel) -----------------
tc <- "AL032009" # number 45
delta_true_tc <- test_data[[tc]]
delta_train_curves_auto_logistic <- test_env[[tc]][["Auto_NoDeathRegs"]] %>% 
  data_plot_paths_basic()

# base map <<<<<<<
latrange <- range(c(delta_true_tc$lat, 
                    delta_train_curves_auto_logistic$lat))
longrange <- range(c(delta_true_tc$long, 
                    delta_train_curves_auto_logistic$long))

ocean <- c(left = longrange[1], bottom = latrange[1],
           right = longrange[2], top = latrange[2])
map   <- suppressWarnings(
  ggmap::get_stamenmap(ocean, zoom = zoom, maptype = "toner-lite")
)

base_graph_delta <- ggmap::ggmap(map) 
# end of base map <<<<<<<<

graphic_delta <- ggvis_paths(data_out = delta_train_curves_auto_logistic,
                              alpha = .1,
                              base_graph = base_graph_delta)  + 
  labs(x = "Longitude", y = "Latitude") +
  ggplot2::geom_path(data = delta_true_tc,
                     ggplot2::aes_string(
                       x = 'long', y = 'lat'), color = true_curve_color,
                     size = 1) +
  labs(caption = "TC #AL032009: Autoregressive Curves\n  with Kernel-based Lysis") 

graphic_delta <-  TCpredictionbands::ggvis_delta_ball_contour(
  output_lines = output_list_pipeline[[tc]][["Auto_NoDeathRegs"]][["delta_ball"]][["structure"]],
  color = pb_color_vec["delta_ball"],
  base_graph = graphic_delta)

# Spherical (non auto logistic) ------------
tc <- "AL032016" # number 272
spherical_true_tc <- test_data[[tc]]
spherical_train_curves_noauto_logistic <- test_env[[tc]][["NoAuto_DeathRegs"]] %>% 
  data_plot_paths_basic()

# base map <<<<<<<
latrange <- range(c(spherical_true_tc$lat, 
                    spherical_train_curves_noauto_logistic$lat))
longrange <- range(c(spherical_true_tc$long, 
                    spherical_train_curves_noauto_logistic$long))

ocean <- c(left = longrange[1], bottom = latrange[1],
           right = longrange[2], top = latrange[2])
map   <- suppressWarnings(
  ggmap::get_stamenmap(ocean, zoom = zoom, maptype = "toner-lite")
)

base_graph_spherical <- ggmap::ggmap(map) 
# end of base map <<<<<<<<

graphic_spherical <- ggvis_paths(data_out = spherical_train_curves_noauto_logistic,
                                   alpha = .1,
                                   base_graph = base_graph_spherical
                                   )  + 
  labs(x = "Longitude", y = "Latitude") +
  ggplot2::geom_path(data = spherical_true_tc,
                     ggplot2::aes_string(
                       x = 'long', y = 'lat'), color = true_curve_color,
                     size = 1) +
  labs(caption = "TC #AL032016: Non-Autoregressive Curves\n  with Logistic-based Lysis") 

bubble_plot_data_nonauto_logistic <- output_list_pipeline[[tc]] %>%
  .[["NoAuto_DeathRegs"]] %>% .[["bubble_ci"]] %>%
  .[["bubble_structure"]] %>%
  sapply(function(x) if (class(x) == "matrix") {data.frame(x)}else{x})

graphic_spherical <- TCpredictionbands::ggvis_bubble_data(
  bubble_plot_data = bubble_plot_data_nonauto_logistic,
  base_graph = graphic_spherical, 
  color = pb_color_vec["bubble_ci"],
  connect = TRUE, 
  centers = FALSE,
  linewidth = .5) 

# legend ------

pre_leg_vis <- data.frame(PB.Method = rep(c("Kernel Density Estimate",
                           "Spherical",
                           "Convex Hull",
                           "Delta Ball"),2) %>%
                            factor(levels = c("Kernel Density Estimate",
                                              "Spherical",
                                              "Convex Hull",
                                              "Delta Ball")),
                       x =  rep(1:4,2),
                       y = c(rep(1,4), rep(2,4))) %>%
  rename(`PB Method` = PB.Method) %>%
ggplot() +
  geom_line(aes(x = x, y = y, color = `PB Method`)) +
  guides(color = guide_legend(nrow = 1, 
                              override.aes =
                                list(size = 2))) +
  tc_theme +
  theme(legend.position = "bottom") +
  scale_color_manual(values = as.vector(pb_color_vec[c("kde", "bubble_ci", 
                                                       "convex_hull", "delta_ball")])) +
  labs(color = "Prediction Band Type") 
leg_vis <- GGally::grab_legend(pre_leg_vis)

layout_matrix <- matrix(c(1,1,1,1, 2,2,2,2,
                          1,1,1,1, 2,2,2,2,
                          1,1,1,1, 2,2,2,2,
                          1,1,1,1, 2,2,2,2,
                          
                          3,3,3,3, 4,4,4,4,
                          3,3,3,3, 4,4,4,4,
                          3,3,3,3, 4,4,4,4,
                          3,3,3,3, 4,4,4,4,
                         
                          5,5,5,5, 5,5,5,5),
                       ncol = 8 , byrow = T)

grob_vis <- arrangeGrob(graphic_kde + tc_theme, 
                        graphic_spherical + tc_theme,
                        graphic_convex + tc_theme,
                        graphic_delta + tc_theme,
                        leg_vis,
                        layout_matrix = layout_matrix,
                        top = textGrob("Prediction Band Examples",
                                           gp = gpar(fontsize = 18)))

ggsave(plot = grob_vis,
       filename = paste0(image_path,
                         "pb_gallery.png"),
       device = "png", width = 10, height = 6/7*10, units = "in")

