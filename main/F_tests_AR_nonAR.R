# This script determines the p-values for block-specific F tests of AR vs non-AR
# change in bearing and speed regressions. It plots geographical maps with 
# regions shaded by p-value.

library(tidyverse)
library(TCcrediblebands)
library(rworldmap)
library(calibrate)

# Load Data ------------------

data_loc <- "main/data/"
image_path <- "report/images/"

load(paste0(data_loc, "raw_data.Rdata"))
train <- train_data

# Block-specific bearing/speed regressions ------------------

# Append path regression variables to training data (auto=T for most general)
train <- lapply(train, FUN = TCcrediblebands::get_reg_df, auto = T)

# Put training data in a single data frame
train_unlist <- do.call("rbind", train) 

# Remove obs w/o lag change in bearing (or speed). This gives the data frames
# for AR regressions. To perform the F tests, we need to use the same data frames
# for AR and non-AR regressions, and fewer data points are eligible for AR regressions. 
train_unlist <- train_unlist %>% filter(!is.na(bearing_change_lag1))

# Get block-specific bearing/speed regressions, AR
bearing_speed_regs_auto <- get_bearing_speed_regs(train_unlist, auto = T)
bearing_regs_auto <- bearing_speed_regs_auto[[1]]
speed_regs_auto <- bearing_speed_regs_auto[[2]]

# Get block-specific bearing/speed regressions, non-AR  
bearing_speed_regs_nonauto <- get_bearing_speed_regs(train_unlist, auto = F)
bearing_regs_nonauto <- bearing_speed_regs_nonauto[[1]]
speed_regs_nonauto <- bearing_speed_regs_nonauto[[2]]

# ANOVA on block-specific bearing regressions ------------------

# Create empty list to store p-values
bearing_pvals <- vector("list", length(bearing_regs_nonauto))
names(bearing_pvals) <- names(bearing_regs_nonauto)

# P-values for F test of H0: non-auto vs. H1: auto
for(i in 1:length(bearing_pvals)){
  
  tc_name <- names(bearing_pvals)[i]
  
  bearing_pvals[[tc_name]] <- anova(bearing_regs_nonauto[[tc_name]], 
                                    bearing_regs_auto[[tc_name]])$Pr[2]
}

# Convert list to vector
bearing_pvals <- unlist(bearing_pvals)

# ANOVA on block-specific speed regressions ------------------

# Create empty list to store p-values
speed_pvals <- vector("list", length(speed_regs_nonauto))
names(speed_pvals) <- names(speed_regs_nonauto)

# P-values for F test of H0: non-auto vs. H1: auto
for(i in 1:length(speed_pvals)){
  
  tc_name <- names(speed_pvals)[i]
  
  speed_pvals[[tc_name]] <- anova(speed_regs_nonauto[[tc_name]], 
                                  speed_regs_auto[[tc_name]])$Pr[2]
}

# Convert list to vector
speed_pvals <- unlist(speed_pvals)

# Benjamini-Yekutieli adjustment for FDR ------------------

# Sort p values into increasing order
bearing_pvals <- sort(bearing_pvals)
speed_pvals <- sort(speed_pvals)

# Number of regions
m <- length(bearing_pvals)

# Indices
k <- 1:m

# Dependence correction
c_m <- sum(1 / (1:m))

# False discovery rate
alpha <- 0.05

# Sequence of thresholds against which to compare p-values
s <- (k * alpha) / (c_m * m)

# Get maximum p-value such that pval[i] < s[i]
cutoff_bearing <- max(bearing_pvals[bearing_pvals < s])
cut_bear_ind <- which(bearing_pvals == cutoff_bearing)

cutoff_speed <- max(speed_pvals[speed_pvals < s])
cut_speed_ind <- which(speed_pvals == cutoff_speed)

# Plot p-values of west block-specific bearing regressions on map ------------------

# Get world map data
map.world <- map_data(map = "world")
map.world <- map.world %>% filter(long >= -140, long <= 12, lat >= 0, lat <= 60)

# Rectangle bounds for blocks of TCs moving west
west_bounds <- data.frame(
  xmin = c(-120, -90, -80, -70, -60, -50, -40, -30, -120, -90, -80, -70, -60, -50, -40,
           -120, -90, -80, -70, -60, -50, -120, -40), 
  xmax = c(-90, -80, -70, -60, -50, -40, -30, 10, -90, -80, -70, -60, -50, -40, 10,
           -90, -80, -70, -60, -50, -40, -40, 10),
  ymin = c(0, 0, 0, 0, 0, 0, 0, 0, 20, 20, 20, 20, 20, 20, 20,
           30, 30, 30, 30, 30, 30, 40, 30),
  ymax = c(20, 20, 20, 20, 20, 20, 20, 20, 30, 30, 30, 30, 30, 30, 30,
           40, 40, 40, 40, 40, 40, 65, 65))

# Determine p-value for each block
get.block <- Vectorize(TCcrediblebands::get_block)

west_bounds <- west_bounds %>% mutate(
  xmidpt = (xmin + xmax) / 2, ymidpt = (ymin + ymax) / 2,
  block = get.block(long = xmidpt, lat = ymidpt, east_west_prev = "W"),
  pvals = bearing_pvals[blocks])

# World map w/ regions shaded by p-value
bearing_west <- ggplot(map.world, aes(x = long, y = lat, group = group)) +
  labs(title = "P-values of block-specific F tests for non-AR vs AR models", 
       subtitle = "Bearing regressions for TCs moving west",
       x = "Longitude", y = "Latitude") +
  coord_cartesian(xlim=c(-110, 2), ylim=c(9, 60)) +
  geom_path(col = "darkgrey") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  geom_rect(data = west_bounds, 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = pvals), 
            colour = "black", alpha = 0.5,
            size = 0.35, inherit.aes = F) +
  scale_fill_gradient2(low = "red", high = "#0000ff", mid = "white",
      midpoint = log10(0.5*(bearing_pvals[cut_bear_ind] +
                              bearing_pvals[cut_bear_ind - 1])),
      guide = "colourbar", trans = "log10",
      breaks = c(min(west_bounds$pvals), bearing_pvals[cut_bear_ind], 10^{-6}, 
                 10^{-9}, max(west_bounds$pvals)),
      labels = c(signif(min(west_bounds$pvals), 3),
                 signif(bearing_pvals[cut_bear_ind], 3),
                 10^{-6}, 10^{-9}, signif(max(west_bounds$pvals), 3))) +
  labs(fill = "p-values") +
  ggsave(filename = paste0(image_path, "F_tests_bearing_west_color.png"),
         width = 8, height = 5)

# Plot p-values of east block-specific bearing regressions on map ------------------

# Rectangle bounds for blocks of TCs moving east
east_bounds <- data.frame(
  xmin = c(-120, -80, -70, -60, -50, -40, -120, -90, -80, -70,
           -120, -90, -80, -70, -60, -50, -40, -30, -20, 
           -120, -70, -60, -50, -40, -30, -60, -40, -20), 
  xmax = c(-80, -70, -60, -50, -40, 10, -90, -80, -70, -60,
           -90, -80, -70, -60, -50, -40, -30, -20, 10,
           -70, -60, -50, -40, -30, -20, -40, -20, 10),
  ymin = c(0, 0, 0, 0, 0, 0, 20, 20, 20, 20,
           30, 30, 30, 30, 30, 30, 30, 30, 30, 40, 40, 40, 40, 40, 40, 50, 50, 50),
  ymax = c(20, 20, 20, 30, 30, 30, 30, 30, 30, 30,
           40, 40, 40, 40, 40, 40, 40, 40, 50, 65, 65, 50, 50, 50, 50, 65, 65, 65))

# Determine p-value for each block
east_bounds <- east_bounds %>% mutate(
  xmidpt = (xmin + xmax) / 2, ymidpt = (ymin + ymax) / 2,
  block = get.block(long = xmidpt, lat = ymidpt, east_west_prev = "E"),
  pvals = bearing_pvals[block])

# World map w/ regions shaded by p-value
bearing_east <- ggplot(map.world, aes(x = long, y = lat, group = group)) +
  labs(title = "P-values of block-specific F tests for non-AR vs AR models", 
       subtitle = "Bearing regressions for TCs moving east",
       x = "Longitude", y = "Latitude") +
  coord_cartesian(xlim=c(-110, 2), ylim=c(9, 60)) +
  geom_path(col = "darkgrey") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  geom_rect(data = east_bounds, 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = pvals), 
            colour = "black", alpha = 0.5,
            size = 0.35, inherit.aes = F) +
  scale_fill_gradient2(low = "red", high = "#0000ff", mid = "white",
      midpoint = log10(0.5*(bearing_pvals[cut_bear_ind] +
                              bearing_pvals[cut_bear_ind - 1])),
      guide = "colourbar", trans = "log10",
      breaks = c(min(east_bounds$pvals), bearing_pvals[cut_bear_ind], 10^{-6}, 
                 10^{-9}, 10^{-12}, max(east_bounds$pvals)),
      labels = c(signif(min(east_bounds$pvals), 3), 
                 signif(bearing_pvals[cut_bear_ind], 3),
                 10^{-6}, 10^{-9}, 10^{-12}, signif(max(east_bounds$pvals), 3))) +
  labs(fill = "p-values") +
  ggsave(filename = paste0(image_path, "F_tests_bearing_east_color.png"),
         width = 8, height = 5)

# Plot p-values of west block-specific speed regressions on map ------------------

# Determine p-value for each block
west_bounds <- west_bounds %>% mutate(
  xmidpt = (xmin + xmax) / 2, ymidpt = (ymin + ymax) / 2,
  block = get.block(long = xmidpt, lat = ymidpt, east_west_prev = "W"),
  pvals = speed_pvals[blocks])

# World map w/ regions shaded by p-value
speed_west <- ggplot(map.world, aes(x = long, y = lat, group = group)) +
  labs(title = "P-values of block-specific F tests for non-AR vs AR models", 
       subtitle = "Speed regressions for TCs moving west",
       x = "Longitude", y = "Latitude") +
  coord_cartesian(xlim=c(-110, 2), ylim=c(9, 60)) +
  geom_path(col = "darkgrey") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  geom_rect(data = west_bounds, 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = pvals), 
            colour = "black", alpha = 0.5,
            size = 0.35, inherit.aes = F) +
  scale_fill_gradient2(low = "red", high = "#0000ff", mid = "white",
      midpoint = log10(0.5*(speed_pvals[cut_speed_ind] +
                              speed_pvals[cut_speed_ind - 1])),
      guide = "colourbar", trans = "log10",
      breaks = c(min(west_bounds$pvals), speed_pvals[cut_speed_ind], 10^{-6}, 
                 10^{-9}, max(west_bounds$pvals)),
      labels = c(signif(min(west_bounds$pvals), 3),
                 signif(speed_pvals[cut_speed_ind], 3),
                 10^{-6}, 10^{-9}, signif(max(west_bounds$pvals), 3))) +
  labs(fill = "p-values") +
  ggsave(filename = paste0(image_path, "F_tests_speed_west_color.png"),
         width = 8, height = 5)

# Plot p-values of east block-specific speed regressions on map ------------------

# Determine p-value for each block
east_bounds <- east_bounds %>% mutate(
  xmidpt = (xmin + xmax) / 2, ymidpt = (ymin + ymax) / 2,
  block = get.block(long = xmidpt, lat = ymidpt, east_west_prev = "E"),
  pvals = speed_pvals[block])

# World map w/ regions shaded by p-value
speed_east <- ggplot(map.world, aes(x = long, y = lat, group = group)) +
  labs(title = "P-values of block-specific F tests for non-AR vs AR models", 
       subtitle = "Speed regressions for TCs moving east",
       x = "Longitude", y = "Latitude") +
  coord_cartesian(xlim=c(-110, 2), ylim=c(9, 60)) +
  geom_path(col = "darkgrey") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  geom_rect(data = east_bounds, 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = pvals), 
            colour = "black", alpha = 0.5,
            size = 0.35, inherit.aes = F) +
  scale_fill_gradient2(low = "red", high = "#0000ff", mid = "white",
      midpoint = log10(0.5*(speed_pvals[cut_speed_ind] +
                              speed_pvals[cut_speed_ind - 1])),
      guide = "colourbar", trans = "log10",
      breaks = c(min(east_bounds$pvals), speed_pvals[cut_speed_ind], 10^{-6}, 
                 10^{-9}, 10^{-12}, max(east_bounds$pvals)),
      labels = c(signif(min(east_bounds$pvals), 3), 
                 signif(speed_pvals[cut_speed_ind], 3),
                 10^{-6}, 10^{-9}, 10^{-12}, signif(max(east_bounds$pvals), 3))) +
  labs(fill = "p-values") +
  ggsave(filename = paste0(image_path, "F_tests_speed_east_color.png"),
         width = 8, height = 5)
