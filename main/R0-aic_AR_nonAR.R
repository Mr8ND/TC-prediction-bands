# This script determines the percent decrease in AIC for block-specific 
# AR vs non-AR bearing and speed regressions.
# It plots geographical maps with change (increase/decrease) in AIC.

library(tidyverse)
library(TCpredictionbands)
library(maps)
library(maptools)
library(scales)

# Load Data ------------------

data_loc <- "main/data/"
image_path <- "report/images/"

load(paste0(data_loc, "raw_data.Rdata"))
train <- train_data

# Block-specific bearing/speed regressions ------------------

# Append path regression variables to training data (auto=T for most general)
train <- lapply(train, FUN = TCpredictionbands::get_reg_df, auto = T)

# Put training data in a single data frame
train_unlist <- do.call("rbind", train) 

# Remove obs w/o lag change in bearing (or speed). This gives the data frames
# for AIC. To compare AIC with ANOVA, we need to use the same data frames for
# AR and non-AR regressions, and fewer data points are eligible for AR regressions. 
train_unlist <- train_unlist %>% filter(!is.na(bearing_change_lag1))

# Get block-specific bearing/speed regressions, AR
bearing_speed_regs_auto <- get_bearing_speed_regs(train_unlist, auto = T)
bearing_regs_auto <- bearing_speed_regs_auto[[1]]
speed_regs_auto <- bearing_speed_regs_auto[[2]]

# Get block-specific bearing/speed regressions, non-AR  
bearing_speed_regs_nonauto <- get_bearing_speed_regs(train_unlist, auto = F)
bearing_regs_nonauto <- bearing_speed_regs_nonauto[[1]]
speed_regs_nonauto <- bearing_speed_regs_nonauto[[2]]

# AIC comparison on block-specific bearing regressions ------------------

# Create empty list to store percent change in AIC from including lag term
AIC_chg_bearing <- vector("list", length(bearing_regs_nonauto))
names(AIC_chg_bearing) <- names(bearing_regs_nonauto)

# Percent change in AIC from including lag term
for(i in 1:length(AIC_chg_bearing)){
  
  block_name <- names(AIC_chg_bearing)[i]
  
  # Make sure we are using same data (same number of obs) from AR and non-AR       
  stopifnot(nobs(bearing_regs_nonauto[[block_name]]) ==
              nobs(bearing_regs_auto[[block_name]]))
  
  AIC_AR <- AIC(bearing_regs_auto[[block_name]])
  AIC_nonAR <- AIC(bearing_regs_nonauto[[block_name]])
  
  AIC_chg_bearing[[block_name]] <- AIC_AR - AIC_nonAR
}

# Convert list to vector
AIC_chg_bearing <- unlist(AIC_chg_bearing)

# AIC comparison on block-specific speed regressions ------------------

# Create empty list to store percent change in AIC from including lag term
AIC_chg_speed <- vector("list", length(speed_regs_nonauto))
names(AIC_chg_speed) <- names(speed_regs_nonauto)

# Percent change in AIC from including lag term
for(i in 1:length(AIC_chg_speed)){
  
  block_name <- names(AIC_chg_speed)[i]
  
  # Make sure we are using same data (same number of obs) from AR and non-AR       
  stopifnot(nobs(speed_regs_nonauto[[block_name]]) ==
              nobs(speed_regs_auto[[block_name]]))
  
  AIC_AR <- AIC(speed_regs_auto[[block_name]])
  AIC_nonAR <- AIC(speed_regs_nonauto[[block_name]])
  
  AIC_chg_speed[[block_name]] <- AIC_AR - AIC_nonAR
}

# Convert list to vector
AIC_chg_speed <- unlist(AIC_chg_speed)

# Get world map data ------------------

map_world <- maps::map(database = "world", plot = F, fill = T, 
                       col = "transparent", xlim = c(-140, 12), ylim = c(0, 60))

IDs <- sapply(strsplit(map_world$names, ":"), function(x) x[1])

map_world_sp <- maptools::map2SpatialPolygons(map_world, IDs = IDs,
                                              proj4string=CRS("+proj=longlat +datum=WGS84"))

# Data frame with blocks and block-specific p-vals ------------------

# Rectangle bounds for blocks
west_bounds <- data.frame(
  xmin = c(-120, -90, -80, -70, -60, -50, -40, -30, -120, -90, -80, -70, 
           -60, -50, -40, -120, -90, -80, -70, -60, -50, -120, -40), 
  xmax = c(-90, -80, -70, -60, -50, -40, -30, 10, -90, -80, -70, -60, 
           -50, -40, 10, -90, -80, -70, -60, -50, -40, -40, 10),
  ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 20, 20, 20, 20, 20, 20, 20,
           30, 30, 30, 30, 30, 30, 40, 30),
  ymax = c(20, 20, 20, 20, 20, 20, 20, 20, 30, 30, 30, 30, 30, 30, 30,
           40, 40, 40, 40, 40, 40, 65, 65))

east_bounds <- data.frame(
  xmin = c(-120, -80, -70, -60, -50, -40, -120, -90, -80, -70,
           -120, -90, -80, -70, -60, -50, -40, -30, -20, 
           -120, -70, -60, -50, -40, -30, -60, -40, -20), 
  xmax = c(-80, -70, -60, -50, -40, 10, -90, -80, -70, -60,
           -90, -80, -70, -60, -50, -40, -30, -20, 10,
           -70, -60, -50, -40, -30, -20, -40, -20, 10),
  ymin = c(0, 0, 0, 0, 0, 0, 20, 20, 20, 20, 30, 30, 30, 30, 30, 
           30, 30, 30, 30, 40, 40, 40, 40, 40, 40, 50, 50, 50),
  ymax = c(20, 20, 20, 30, 30, 30, 30, 30, 30, 30, 40, 40, 40, 40, 
           40, 40, 40, 40, 50, 65, 65, 50, 50, 50, 50, 65, 65, 65))

# Determine bearing/speed p-values for each west-bound block
get.block <- Vectorize(TCpredictionbands::get_block)

west_bounds <- west_bounds %>% mutate(
  xmidpt = (xmin + xmax) / 2, ymidpt = (ymin + ymax) / 2,
  block = get.block(long = xmidpt, lat = ymidpt, east_west_prev = "W"),
  AIC_chg_bearing = AIC_chg_bearing[block],
  AIC_chg_speed = AIC_chg_speed[block])

# Determine bearing/speed p-values for each east-bound block
east_bounds <- east_bounds %>% mutate(
  xmidpt = (xmin + xmax) / 2, ymidpt = (ymin + ymax) / 2,
  block = get.block(long = xmidpt, lat = ymidpt, east_west_prev = "E"),
  AIC_chg_bearing = AIC_chg_bearing[block],
  AIC_chg_speed = AIC_chg_speed[block])

# Combine east and west bounds into one data frame
all_bounds <- rbind(east_bounds, west_bounds) %>%
  mutate(direction_bearing = 
           c(rep("Bearing Regressions for TCs Moving East", nrow(east_bounds)),
             rep("Bearing Regressions for TCs Moving West", nrow(west_bounds))),
         direction_speed = 
           c(rep("Speed Regressions for TCs Moving East", nrow(east_bounds)),
             rep("Speed Regressions for TCs Moving West", nrow(west_bounds))),
         AIC_dec_bearing = ifelse(AIC_chg_bearing < 0, "Decrease", "Increase"),
         AIC_dec_speed = ifelse(AIC_chg_speed < 0, "Decrease", "Increase"))


# Set TC ggplot2 theme ------------------
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

# Plot chg in AIC of block-specific bearing regressions on map -----------------

bearing_map <- ggplot(all_bounds) +
  labs(x = "Longitude", y = "Latitude", fill = "Change in AIC",
       title = paste("Change in AIC from using Lag Term in Block-Specific", 
                     "Bearing Models")) +
  coord_cartesian(xlim = c(-110, 2), ylim = c(9, 60)) +
  geom_polygon(data = map_world_sp, aes(long, lat, group = group),
               fill = "white") +
  geom_path(data = map_world_sp, aes(long, lat, group = group),
            color = "black") +
  tc_theme +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.spacing = unit(2, "lines"),
        legend.title = element_text(hjust = 0.5)) +
  geom_rect(data = all_bounds, 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                fill = AIC_dec_bearing), 
            colour = "black", alpha = 0.8,
            size = 0.35, inherit.aes = T) +
  facet_grid(. ~ direction_bearing) +
  scale_fill_manual(values = c("pink", "white"))

ggsave(bearing_map, 
       filename = paste0(image_path, "aic_bearing.png"),
       width = 10, height = 4)

# Plot chg in AIC of block-specific speed regressions on map ------------------

speed_map <- ggplot(all_bounds) +
  labs(x = "Longitude", y = "Latitude", fill = "Change in AIC",
       title = paste("Change in AIC from using Lag Term in Block-Specific", 
                     "Speed Models")) +
  coord_cartesian(xlim = c(-110, 2), ylim = c(9, 60)) +
  geom_polygon(data = map_world_sp, aes(long, lat, group = group),
               fill = "white") +
  geom_path(data = map_world_sp, aes(long, lat, group = group),
            color = "black") +
  tc_theme +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.spacing = unit(2, "lines"),
        legend.title = element_text(hjust = 0.5)) +
  geom_rect(data = all_bounds,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                fill = AIC_dec_speed),
            colour = "black", alpha = 0.8,
            size = 0.35, inherit.aes = T) +
  facet_grid(. ~ direction_speed) +
  scale_fill_manual(values = c("pink", "white"))

ggsave(speed_map, 
       filename = paste0(image_path, "aic_speed.png"),
       width = 10, height = 4)
