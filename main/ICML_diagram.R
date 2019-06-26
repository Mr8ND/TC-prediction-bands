library(TCpredictionbands)
library(gridExtra)
library(tidyverse)

image_path <- "/Users/benjaminleroy/Documents/CMU/research/TC-prediction-bands/report/images/"

my_pipeline <- TCpredictionbands::sample_output_pipeline
my_sim <- TCpredictionbands::sample_sim
my_tc_length <- nrow(TCpredictionbands::sample_tc)
my_tc <- TCpredictionbands::sample_tc %>% 
  mutate(col = factor(c(rep(0,3), rep(1, my_tc_length - 3))))
my_tc_name <- TCpredictionbands::sample_tc_name

# base map 
sim_process <- TCpredictionbands::data_plot_paths_basic(test_list = my_sim)
latrange <- range(sim_process$lat, my_tc$lat) + c(-4,4)
longrange <- range(sim_process$long, my_tc$long) + c(-4,4)

longrange[2] <- -20
latrange[2] <- 65

ocean <- c(left = longrange[1], bottom = latrange[1],
           right = longrange[2], top = latrange[2])
map   <- ggmap::get_stamenmap(ocean, zoom = 4, maptype = "toner-lite")

base_graph <- ggmap::ggmap(map)



# zoom base

latrange_z <- range(my_tc$lat[1:3])  + c(-1.9,1.9)
longrange_z <- range(my_tc$long[1:3])  + c(-1.9,1.9)

ocean_z <- c(left = longrange_z[1], bottom = latrange_z[1],
           right = longrange_z[2], top = latrange_z[2])
map_z   <- ggmap::get_stamenmap(ocean_z, zoom = 8, maptype = "toner-lite")

base_graph_zoom <- ggmap::ggmap(map_z)



# visual true TC
first <- base_graph + 
  ggplot2::geom_path(data = my_tc,
                   ggplot2::aes_string(
                     x = 'long', y = 'lat', 
                     linetype = 'col', color = 'col')) +
  ggplot2::scale_color_manual(values = c("red", "black")) 


first_zoom <- base_graph_zoom + 
  ggplot2::geom_path(data = my_tc,
                     ggplot2::aes_string(
                       x = 'long', y = 'lat', 
                       linetype = 'col', color = 'col'),
                     size = 1.5) +
  ggplot2::scale_color_manual(values = c("red", "black")) 


# visual simulated curves
second <- ggvis_paths(TCpredictionbands::sample_sim,
                      base_graph = base_graph,
                      alpha = .05) 


db_pb <- TCpredictionbands::delta_structure(data_list = sample_sim, 
                                            alpha = .1,
                                            verbose = TRUE)

#names(db_pb$structure) <- c("long", "lat", "idx")


third <- ggvis_delta_ball_contour(db_pb$structure,base_graph = first,
                         color = "purple")

third <- third + aes(size = "a") + scale_size_manual(values = 1.5)

noleg <- theme(legend.position = "none")

image_path <- "report/images/"

ggsave(first_zoom + noleg + labs(x ="",y=""),
       filename = paste0(image_path,"pipeline1_bigger.png"),
       width = 6, height = 6)

ggsave(second + noleg + labs(x ="",y=""),
        filename = paste0(image_path,"pipeline2.png"),
        width = 6, height = 6)

ggsave(third + noleg + labs(x ="",y=""),
       filename = paste0(image_path,"pipeline3_bigger.png"),
       width = 6, height = 6)

# first 15 days of AL032009

