###############################
###############################
# discussion section graphics #
###############################
###############################

library(tidyverse)
library(reshape2)
library(xtable)
library(forcats)
library(progress)
library(latex2exp)
library(gridExtra)
library(TCpredictionbands)
library(GGally)
library(grid)

# Theme -----------------
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

# Load Data ------------------

data_loc <- "main/data/"
image_path <- "report/images/"
table_path <- "report/tables/"
zoom = 4

true_curve_color = "#ffff00"
smooth_color = "blue"
pb_color_vec <- c("#1b9e77","#d95f02","#7570b3", "#e7298a")
names(pb_color_vec) <- c("convex_hull", "bubble_ci", "delta_ball", "kde")

auto_vs_non_color_vec <- c("black", "grey70")
quantile5_colors <- colorRampPalette(c("red", "grey","blue"))(6)

sim_type_graphic_levels <- c("Autoregression & Logistic-based Lysis"   = "Auto_DeathRegs",
                             "Autoregression & Kernel-based Lysis"       = "Auto_NoDeathRegs",
                             "Non-Autoregression & Logistic-based Lysis" = "NoAuto_DeathRegs",
                             "Non-Autoregression & Kernel-based Lysis"   = "NoAuto_NoDeathRegs")
sim_type_graphic_labels <- names(sim_type_graphic_levels)


latest_full_output_pipeline <- 'output_pipeline_alphalevel0.1_all.Rdata'
a <- load(paste0(data_loc, latest_full_output_pipeline))
eval(parse(text = paste0("output_list_pipeline <- ",a)))

b <- load(paste0(data_loc,"Test_Sims_350.Rdata"))
c <- load(paste0(data_loc,"raw_data.Rdata"))

if (is.null(names(output_list_pipeline))) {
  warning(paste("output_list_pipeline list missing names.",
                "Renaming output_list_pipeline's entries with test env's names"))
  names(output_list_pipeline) <- names(test_env)
}

############
############
# Graphics #
############
############

###############################################
# simulation curves, extreme turning (plot 1) #
###############################################

# simulated curve, extreme turning: curvier true tc --------
tc <- "AL112004" # number 44
true_tc <- test_data[[tc]]
train_curves_auto_logistic <- test_env[[tc]][["Auto_DeathRegs"]] %>% data_plot_paths_basic()

xx1_crazy_curve <- ggvis_paths(data_out = train_curves_auto_logistic,
            alpha = .1)  + 
  labs(x = "Longitude", y = "Latitude") +
  ggplot2::geom_path(data = true_tc,
                     ggplot2::aes_string(
                       x = 'long', y = 'lat'), color = true_curve_color,
                     size = 1) +
  labs(caption = "TC #AL112004: Autoregressive Curves\n with Logistic-Based Lysis",
       title = "Example of a \"Wiggly\" TC") + tc_theme


# simulated curve, extreme turning: qqplot, change in bearing --------


# <<<<<<<<<<< data creation

train_models <- get_train_models(train_data)

bearing_regs_auto_df <- data.frame(.rownames = -1,
                                   bearing_change = -1,
                                   lat = -1,
                                   long = -1,
                                   bearing_prev = -1,
                                   speed_prev = -1,
                                   bearing_change_lag1 = -1,
                                   .fitted = -1,          
                                   .se.fit = -1,
                                   .resid = -1,
                                   .hat = -1,
                                   .sigma = -1,
                                   .cooksd = -1,
                                   .std.resid = -1,
                                   region = "ben") %>%
  mutate(region = as.character(region))

speed_regs_auto_df <- data.frame(.rownames = -1,
                                 speed_change = -1,
                                 lat = -1,
                                 long = -1,
                                 bearing_prev = -1,
                                 speed_prev = -1,
                                 speed_change_lag1 = -1,
                                 .fitted = -1,          
                                 .se.fit = -1,
                                 .resid = -1,
                                 .hat = -1,
                                 .sigma = -1,
                                 .cooksd = -1,
                                 .std.resid = -1,
                                 region = "ben") %>%
  mutate(region = as.character(region))

bearing_regs_nonauto_df <- data.frame(.rownames = -1,
                                      bearing_change = -1,
                                      lat = -1,
                                      long = -1,
                                      bearing_prev = -1,
                                      speed_prev = -1,
                                      .fitted = -1,          
                                      .se.fit = -1,
                                      .resid = -1,
                                      .hat = -1,
                                      .sigma = -1,
                                      .cooksd = -1,
                                      .std.resid = -1,
                                      region = "ben") %>%
  mutate(region = as.character(region))

speed_regs_nonauto_df <- data.frame(.rownames = -1,
                                    speed_change = -1,
                                    lat = -1,
                                    long = -1,
                                    bearing_prev = -1,
                                    speed_prev = -1,
                                    .fitted = -1,          
                                    .se.fit = -1,
                                    .resid = -1,
                                    .hat = -1,
                                    .sigma = -1,
                                    .cooksd = -1,
                                    .std.resid = -1,
                                    region = "ben") %>%
  mutate(region = as.character(region))


for (region in names(train_models[["bearing_regs_auto"]])) {
  new_df <- train_models[["bearing_regs_auto"]][[region]] %>% broom::augment() %>%
    mutate(region = region)
  
  bearing_regs_auto_df <- rbind(bearing_regs_auto_df,
                                new_df)
  
}
bearing_regs_auto_df <- bearing_regs_auto_df[-1,]


for (region in names(train_models[["speed_regs_auto"]])) {
  new_df <- train_models[["speed_regs_auto"]][[region]] %>% broom::augment() %>%
    mutate(region = region)
  
  speed_regs_auto_df <- rbind(speed_regs_auto_df,
                              new_df)
  
}

speed_regs_auto_df <- speed_regs_auto_df[-1,]

for (region in names(train_models[["bearing_regs_nonauto"]])) {
  new_df <- train_models[["bearing_regs_nonauto"]][[region]] %>% broom::augment() %>%
    mutate(region = region)
  
  bearing_regs_nonauto_df <- rbind(bearing_regs_nonauto_df,
                                   new_df)
  
}
bearing_regs_nonauto_df <- bearing_regs_nonauto_df[-1,]

for (region in names(train_models[["speed_regs_nonauto"]])) {
  new_df <- train_models[["speed_regs_nonauto"]][[region]] %>% broom::augment() %>%
    mutate(region = region)
  
  speed_regs_nonauto_df <- rbind(speed_regs_nonauto_df,
                                 new_df)
  
}

speed_regs_nonauto_df <- speed_regs_nonauto_df[-1,]

bearing_regs_all <- rbind(bearing_regs_auto_df[, ".resid", drop = F] %>%
                                 mutate(reg = "Autoregressive"),
                          bearing_regs_nonauto_df[, ".resid", drop = F] %>%
                                 mutate(reg = "Non-Autoregressive"))


# <<<<<<<<<< end data creation

xx1_qq_bearing <- ggplot(bearing_regs_all, 
                         aes(sample = .resid, color = reg)) +
  geom_qq(alpha = 1) +
  geom_qq_line() + 
  scale_color_manual(values = auto_vs_non_color_vec) +
  labs(x = "Theoretical Quantile Values",
       y = "Residual Quantile Values",
       color = "Model Type",
       title = "Change in Bearing \nQuantile-Quantile Plot") +
  tc_theme
  

# simulated curve, extreme turning: qqplot, change in speed -------------
  
speed_regs_all <- rbind(speed_regs_auto_df[, ".resid", drop = F] %>%
                          mutate(reg = "Autoregressive"),
                        speed_regs_nonauto_df[, ".resid", drop = F] %>%
                          mutate(reg = "Non-Autoregressive"))

xx1_qq_speed <- ggplot(speed_regs_all, 
                         aes(sample = .resid, color = reg)) +
  geom_qq(alpha = 1) +
  geom_qq_line() + 
  scale_color_manual(values = auto_vs_non_color_vec) +
  labs(x = "Theoretical Quantile Values",
       y = "Residual Quantile Values",
       color = "Model Type",
       title = "Change in Speed \nQuantile-Quantile Plot") +
  tc_theme

# simulated curve, extreme turning: final graphic (xx1) ---------  

no_leg <- theme(legend.position = "none")

xx1_layout_matrix <- matrix(c(1,1,1,1, 2,2,2,2, 3,3,3,3,
                              1,1,1,1, 2,2,2,2, 3,3,3,3,
                              1,1,1,1, 2,2,2,2, 3,3,3,3,
                              1,1,1,1, 2,2,2,2, 3,3,3,3,
                              
                              1,1,1,1, 4,4,4,4, 4,4,4,4,
                              5,5,5,5, 4,4,4,4, 4,4,4,4),
                        nrow = 6, byrow = T)

white_box <- ggplot() + theme( panel.background = element_rect(fill = "white"))

xx1 <- arrangeGrob(xx1_crazy_curve,
                   xx1_qq_bearing + no_leg,
                   xx1_qq_speed + no_leg, 
                   grab_legend(xx1_qq_bearing +
                                 guides(color = guide_legend(nrow = 1))),
                   white_box,
                   heights = c(1,1,1,1,.3,.7),
                   layout_matrix = xx1_layout_matrix
                   )

ggsave(plot = xx1,
       filename = paste0(image_path,"simulation_turning_discussion_graphic.png"),
       device = "png", width = 10, height = 4, units = "in")

#######################################
# simulation curves, lengths (plot 2) #
#######################################

# simulated curve, lengths: -----------

# <<<<<<<<<<< data creation 

sim_length_vs_length_df <- data.frame(tc = "ben",
                                      type = "ben",
                                      true_length = -1,
                                      sim_length = -1,
                                      quantile = "hi") %>%
  mutate(tc = as.character(tc),
         type = as.character(type),
         quantile = as.character(quantile))

for (tc in names(output_list_pipeline)) {
  specific_tc_info <- output_list_pipeline[[tc]]
  for (type in names(specific_tc_info)) {
    
    sim_tc_length <- test_env[[tc]][[type]] %>% sapply(function(x) dim(x)[1]) %>%
      quantile(p = 0:5/5)
    
    true_length <- test_data[[tc]] %>% nrow()
    
    sim_length_vs_length_df <- rbind(sim_length_vs_length_df,
                                     data.frame(tc = tc,
                                                type = type,
                                                true_length = true_length,
                                                sim_length = sim_tc_length,
                                                quantile = names(sim_tc_length)))
    
    
  }
}

sim_length_vs_length_df <- sim_length_vs_length_df[-1,]

sim_length_vs_length_df <- sim_length_vs_length_df %>% 
  mutate(type = factor(type, levels  = sim_type_graphic_levels,
                                          labels = sim_type_graphic_labels),
         quantile = fct_relevel(factor(quantile),
                                "100%","80%", "60%",
                                "40%", "20%", "0%"))

# <<<<<<<<<<< end of data creation 

xx2 <- sim_length_vs_length_df %>% ggplot() +
  geom_point(aes(x = true_length, y = sim_length,
                 color = factor(quantile)),
             alpha = .3) +
  facet_wrap(~factor(type)) +
  geom_smooth(aes(x = true_length, y = sim_length,
                  color = factor(quantile)),
              se = F) +
  scale_color_manual(values = quantile5_colors) +
  tc_theme +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1)) +
  labs(x = "Survival Time of True Curve",
       y = "Survival Time of Simulated Curves",
       title = "Comparing Survival Times between True and Simulated Curves",
       color = "Smoothing of Quantiles of Simulated Curves") 


ggsave(plot = xx2,
       filename = paste0(image_path,"simulation_length_discussion_graphic.png"),
       device = "png", width = 10, height = 6, units = "in")



#################################
# spherical PBs, depth (plot 3) #
#################################

# spherical PBs, depth: -------------

# <<<<<<<<< data creation

prop_vs_length_df <- data.frame(tc = "ben",
                                type = "ben",
                                prop_captured = -1,
                                length_deep = -1,
                                length_true = -1) %>%
  mutate(tc = as.character(tc),
         type = as.character(tc))

for (tc in names(output_list_pipeline)) {
  specific_tc_info <- output_list_pipeline[[tc]]
  for (type in names(specific_tc_info)) {
    
    prop_captured <- output_list_pipeline[[tc]][[type]] %>%
      .[["bubble_ci"]] %>% .[["in_vec"]] %>% mean
    
    length_deep <- output_list_pipeline[[tc]][[type]] %>%
      .[["bubble_ci"]] %>% .[["bubble_structure"]] %>% .[["centers"]] %>%
      dim %>% .[1]
    
    length_true <- test_data[[tc]] %>% nrow()
    
    prop_vs_length_df <- rbind(prop_vs_length_df,
                               data.frame(tc = tc,
                                          type = type,
                                          prop_captured = prop_captured,
                                          length_deep = length_deep,
                                          length_true = length_true))    
    
  }
}

prop_vs_length_df <- prop_vs_length_df[-1,]


prop_vs_length_df <- prop_vs_length_df %>% 
  mutate(type = factor(type, levels  = sim_type_graphic_levels,
                      labels = sim_type_graphic_labels))
# <<<<<<<<< end of data creation

xx3 <- prop_vs_length_df %>% ggplot() +
  geom_point(aes(x = length_true, y = length_deep),
             alpha = .3) +
  facet_wrap(~ type, nrow = 2) +
  geom_smooth(aes(x = length_true, y = length_deep),
              color = smooth_color) +
  labs(y = "Survival Time for Deepest Simulated Curve",
       x = "Survival Time of True Curve",
       title = "Survival Times of Deepest Spherical PB Curves versus True Curves") +
  tc_theme

ggsave(plot = xx3,
       filename = paste0(image_path,
                         "spherical_depth_length_discussion_graphic.png"),
       device = "png", width = 10, height = 6, units = "in")




#################################################
# spherical PBs, branching and curving (plot 4) #
#################################################

# spherical PBs, branching and curving: branching problem --------

tc <- "AL011965" # number 229
true_tc <- test_data[[tc]]
train_curves_auto_logistic <- test_env[[tc]][["Auto_DeathRegs"]] %>% data_plot_paths_basic()

xx4_branching_curve <- ggvis_paths(data_out = train_curves_auto_logistic,
                               alpha = .1)  + 
  labs(x = "Longitude", y = "Latitude") +
  ggplot2::geom_path(data = true_tc,
                     ggplot2::aes_string(
                       x = 'long', y = 'lat'), color = true_curve_color,
                     size = 1) +
  labs(caption = "TC #AL011965: Autoregressive Curves\n  with Logistic-based Lysis") 

bubble_plot_data_auto_logistic <- output_list_pipeline[[tc]] %>%
  .[["Auto_DeathRegs"]] %>% .[["bubble_ci"]] %>%
  .[["bubble_structure"]] %>%
  sapply(function(x) if (class(x) == "matrix") {data.frame(x)}else{x})

xx4_branching_curve <- TCpredictionbands::ggvis_bubble_data(
  bubble_plot_data = bubble_plot_data_auto_logistic,
  base_graph = xx4_branching_curve, 
  color = pb_color_vec["bubble_ci"],
  connect = TRUE, 
  centers = FALSE)

xx4_branching_curve <- xx4_branching_curve + tc_theme

# spherical PBs, branching and curving: curving problem --------

tc <- "AL072010" # number 54
true_tc <- test_data[[tc]]
train_curves_auto_logistic <- test_env[[tc]][["Auto_DeathRegs"]] %>% data_plot_paths_basic()

# base map <<<<<<<
latrange <- range(c(true_tc$lat, 
                      train_curves_auto_logistic$lat))
longrange <- range(c(true_tc$long, 
                    train_curves_auto_logistic$long))

ocean <- c(left = longrange[1], bottom = latrange[1],
           right = longrange[2], top = latrange[2])
map   <- suppressWarnings(
  ggmap::get_stamenmap(ocean, zoom = zoom, maptype = "toner-lite")
)

base_graph_xx4_curve <- ggmap::ggmap(map) 
# end of base map <<<<<<<<

xx4_curving_curve <- ggvis_paths(data_out = train_curves_auto_logistic,
                                 alpha = .1,
                                 base_graph = base_graph_xx4_curve)  + 
  labs(x = "Longitude", y = "Latitude") +
  ggplot2::geom_path(data = true_tc,
                     ggplot2::aes_string(
                       x = 'long', y = 'lat'), color = true_curve_color,
                     size = 1) +
  labs(caption = "TC #AL072010: Autoregressive Curves\n  with Logistic-based Lysis") 

bubble_plot_data_auto_logistic <- output_list_pipeline[[tc]] %>%
  .[["Auto_DeathRegs"]] %>% .[["bubble_ci"]] %>%
  .[["bubble_structure"]] %>%
  sapply(function(x) if (class(x) == "matrix") {data.frame(x)}else{x})

xx4_curving_curve <- TCpredictionbands::ggvis_bubble_data(
  bubble_plot_data = bubble_plot_data_auto_logistic,
  base_graph = xx4_curving_curve, 
  color = pb_color_vec["bubble_ci"],
  connect = TRUE, 
  centers = FALSE) 

xx4_curving_curve <- xx4_curving_curve + tc_theme

xx4_layout_matrix <- matrix(c(1,1,1,1, 3,3,3,3,
                              1,1,1,1, 2,2,2,2,
                              1,1,1,1, 2,2,2,2, 
                              1,1,1,1, 2,2,2,2, 
                              
                              1,1,1,1, 4,4,4,4),
                            nrow = 5, byrow = T)

white_box <- ggplot() + theme( panel.background = element_rect(fill = "white"))

xx4 <- arrangeGrob(xx4_branching_curve, xx4_curving_curve,
             white_box, white_box,
             layout_matrix = xx4_layout_matrix,
             heights = c(.5,1,1,1,.5),
             top = textGrob(paste("Visualization of Drawbacks of",
                                  "Spherical Prediction Bands"),
                            gp = gpar(fontsize = 18)))

ggsave(plot = xx4,
       filename = paste0(image_path,
                         "spherical_branching_curving_discussion_graphic.png"),
       device = "png", width = 10, height = 5, units = "in")



#########################################
# KDE PBs, patchy and outliers (plot 5) #
#########################################

# KDE PBs, patchy and outliers: patchy -----------

tc <- "AL162005" # number 5
true_tc <- test_data[[tc]]
train_curves_auto_kernel <- test_env[[tc]][["Auto_NoDeathRegs"]] %>% data_plot_paths_basic()

# base map <<<<<<<
latrange <- range(c(true_tc$lat, 
                    train_curves_auto_kernel$lat))
longrange <- range(c(true_tc$long, 
                    train_curves_auto_kernel$long))

ocean <- c(left = longrange[1], bottom = latrange[1],
           right = longrange[2], top = latrange[2])
map   <- suppressWarnings(
  ggmap::get_stamenmap(ocean, zoom = zoom, maptype = "toner-lite")
)

base_graph_xx5_patchy <- ggmap::ggmap(map) 
# end of base map <<<<<<<<

xx5_patchy_curve <- ggvis_paths(data_out = train_curves_auto_kernel,
                                alpha = .1,
                                base_graph = base_graph_xx5_patchy)  + 
  labs(x = "Longitude", y = "Latitude") +
  ggplot2::geom_path(data = true_tc,
                     ggplot2::aes_string(
                       x = 'long', y = 'lat'), color = true_curve_color,
                     size = 1) +
  labs(caption = "TC #AL162005: Autoregressive Curves\n  with Kernel-based Lysis") 

kde_contour_dfs_auto_kernel <- TCpredictionbands::contour_list_to_df(
  output_list_pipeline[[tc]][["Auto_NoDeathRegs"]][["kde"]][["contour"]])

xx5_patchy_curve <- TCpredictionbands::ggvis_kde_contour(
                                                  kde_contour_dfs_auto_kernel,
                                                  base_graph = xx5_patchy_curve,
                                                  color = pb_color_vec["kde"])

xx5_patchy_curve <- xx5_patchy_curve  + tc_theme

# KDE PBs, patchy and outliers: outlier -----------

tc <- "AL192012" # number 149
true_tc <- test_data[[tc]]
train_curves_nonauto_kernel <- test_env[[tc]][["NoAuto_NoDeathRegs"]] %>% data_plot_paths_basic()

# base map <<<<<<<
latrange <- range(c(true_tc$lat, 
                    train_curves_nonauto_kernel$lat))
longrange <- range(c(true_tc$long, 
                    train_curves_nonauto_kernel$long))

ocean <- c(left = longrange[1], bottom = latrange[1],
           right = longrange[2], top = latrange[2])
map   <- suppressWarnings(
  ggmap::get_stamenmap(ocean, zoom = zoom, maptype = "toner-lite")
)

base_graph_xx5_outlier <- ggmap::ggmap(map) 
# end of base map <<<<<<<<

xx5_outlier_curve <- ggvis_paths(data_out = train_curves_nonauto_kernel,
                                alpha = .1,
                                base_graph = base_graph_xx5_outlier)  + 
  labs(x = "Longitude", y = "Latitude") +
  ggplot2::geom_path(data = true_tc,
                     ggplot2::aes_string(
                       x = 'long', y = 'lat'), color = true_curve_color,
                     size = 1) +
  labs(caption = "TC #AL192012: Autoregressive Curves\n  with Kernel-based Lysis") 

kde_contour_dfs_auto_kernel <- TCpredictionbands::contour_list_to_df(
  output_list_pipeline[[tc]][["NoAuto_NoDeathRegs"]][["kde"]][["contour"]])

xx5_outlier_curve <- TCpredictionbands::ggvis_kde_contour(
  kde_contour_dfs_auto_kernel,
  base_graph = xx5_outlier_curve,
  color = pb_color_vec["kde"])

xx5_outlier_curve <- xx5_outlier_curve + tc_theme

xx5 <- arrangeGrob(xx5_patchy_curve, xx5_outlier_curve,
                   nrow = 1,
                   top = textGrob(paste("Visualization of Drawbacks of",
                                        "KDE Prediction Bands"),
                                  gp = gpar(fontsize = 18)))

ggsave(plot = xx5,
       filename = paste0(image_path,
                         "kde_patchy_outlier_discussion_graphic.png"),
       device = "png", width = 10, height = 5, units = "in")


