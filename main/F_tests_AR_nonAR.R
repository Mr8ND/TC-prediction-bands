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
cutoff_speed <- max(speed_pvals[speed_pvals < s])

# Plot p-values of west block-specific bearing regressions on map ------------------

# Create coordinates of one point per block
long.w <- c(-95, -85, -75, -65, -55, -45, -35, -25,
            -95, -85, -75, -65, -55, -45, -35,
            -95, -85, -75, -65, -55, -45, -45, -30) 
lat.w <- c(15, 15, 15, 15, 15, 15, 15, 15,
           25, 25, 25, 25, 25, 25, 25,
           35, 35, 35, 35, 35, 35, 45, 45)
get.block <- Vectorize(TCcrediblebands::get_block)

# Create labels with block-specific p-values
blocks <- get.block(long.w, lat.w, "W")
pval.labels <- round(bearing_pvals[blocks], 3) %>% as.character
pval.labels[pval.labels == "0"] <- "< 0.001"
pval.labels[pval.labels < cutoff_bearing] <- 
  paste(pval.labels[pval.labels < cutoff_bearing], "(*)")

# Get world map data
map.world <- map_data(map = "world")
map.world <- map.world %>% filter(long >= -140, long <= 12, lat >= 0, lat <= 70)

# Plot map and block-specific p-values
bearing_west <- ggplot(map.world, aes(x = long, y = lat, group = group)) +
  labs(title = "P-values of block-specific F tests for non-AR vs AR models", 
       subtitle = "Bearing regressions for TCs moving west",
       caption = paste("(*) indicates rejection of H0 (non-AR) in favor of H1 (AR)",
          "using \nBenjamini-Yekutieli (dependence-corrected) FDR adjustment"),
       x = "Longitude", y = "Latitude") +
  coord_cartesian(xlim=c(-110, 2), ylim=c(9, 60)) +
  geom_path(col = "grey") + 
  theme(legend.position="none", panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5, size = 14), 
        plot.caption = element_text(hjust = 0.5, size = 12),
        axis.title = element_text(size = 14)) +
  geom_hline(yintercept = 20) +
  geom_hline(yintercept = 30) +
  geom_segment(aes(x = -90, y = 9, xend = -90, yend = 20)) + #A1.west
  geom_segment(aes(x = -80, y = 9, xend = -80, yend = 20)) + #A2.west
  geom_segment(aes(x = -70, y = 9, xend = -70, yend = 20)) + #A3.west
  geom_segment(aes(x = -60, y = 9, xend = -60, yend = 20)) + #A4.west
  geom_segment(aes(x = -50, y = 9, xend = -50, yend = 20)) + #A5.west
  geom_segment(aes(x = -40, y = 9, xend = -40, yend = 20)) + #A6.west
  geom_segment(aes(x = -30, y = 9, xend = -30, yend = 20)) + #A7.west
  # + A8.west is to right +
  geom_segment(aes(x = -90, y = 20, xend = -90, yend = 30)) + #B1.west
  geom_segment(aes(x = -80, y = 20, xend = -80, yend = 30)) + #B2.west
  geom_segment(aes(x = -70, y = 20, xend = -70, yend = 30)) + #B3.west
  geom_segment(aes(x = -60, y = 20, xend = -60, yend = 30)) + #B4.west
  geom_segment(aes(x = -50, y = 20, xend = -50, yend = 30)) + #B5.west
  geom_segment(aes(x = -40, y = 20, xend = -40, yend = 30)) + #B6.west
  # + B7.west is to right +
  geom_segment(aes(x = -120, y = 40, xend = -40, yend = 40)) +
  geom_segment(aes(x = -90, y = 30, xend = -90, yend = 40)) + #C1.west
  geom_segment(aes(x = -80, y = 30, xend = -80, yend = 40)) + #C2.west
  geom_segment(aes(x = -70, y = 30, xend = -70, yend = 40)) + #C3.west
  geom_segment(aes(x = -60, y = 30, xend = -60, yend = 40)) + #C4.west
  geom_segment(aes(x = -50, y = 30, xend = -50, yend = 40)) + #C5.west
  geom_segment(aes(x = -40, y = 30, xend = -40, yend = 40)) + #C6.west
  geom_segment(aes(x = -40, y = 40, xend = -40, yend = 60)) + #D1.west
  annotate("text", x = long.w, y = lat.w, label = pval.labels, col = "red")

# Save plot
ggsave(filename = paste0(image_path, "F_tests_bearing_west.png"),
       plot = bearing_west, width = 10, height = 7)

# Plot p-values of east block-specific bearing regressions on map ------------------

# Create coordinates of one point per block
long.e <- c(-85, -75, -65, -95, -85, -75, -65, -55, -45, -35,
            -95, -85, -75, -65, -55, -45, -35, -25,
            -55, -45, -35, -25, -15, -75, -65, -50, -30, -15) 
lat.e <- c(15, 15, 15, 25, 25, 25, 25, 25, 25, 25,
           35, 35, 35, 35, 35, 35, 35, 35,
           45, 45, 45, 45, 40, 52, 52, 52, 52, 52)
get.block <- Vectorize(TCcrediblebands::get_block)

# Create labels with block-specific p-values
blocks <- get.block(long.e, lat.e, "E")
pval.labels <- round(bearing_pvals[blocks], 3) %>% as.character
pval.labels[pval.labels == "0"] <- "< 0.001"
pval.labels[pval.labels < cutoff_bearing] <- 
  paste(pval.labels[pval.labels < cutoff_bearing], "(*)")

# Get world map data
map.world <- map_data(map = "world")
map.world <- map.world %>% filter(long >= -140, long <= 12, lat >= 0, lat <= 70)

# Plot map and block-specific p-values
bearing_east <- ggplot(map.world, aes(x = long, y = lat, group = group)) +
  labs(title = "P-values of block-specific F tests for non-AR vs AR models", 
       subtitle = "Bearing regressions for TCs moving east",
       caption = paste("(*) indicates rejection of H0 (non-AR) in favor of H1 (AR)",
          "using \nBenjamini-Yekutieli (dependence-corrected) FDR adjustment"),
       x = "Longitude", y = "Latitude") +
  coord_cartesian(xlim=c(-110, 2), ylim=c(9, 60)) +
  geom_path(col = "grey") + 
  theme(legend.position="none", panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5, size = 14), 
        plot.caption = element_text(hjust = 0.5, size = 12),
        axis.title = element_text(size = 14)) +
  geom_hline(yintercept = 30) +
  geom_segment(aes(x = -120, y = 20, xend = -60, yend = 20)) +
  geom_segment(aes(x = -120, y = 30, xend = -60, yend = 30)) +
  geom_segment(aes(x = -80, y = 9, xend = -80, yend = 20)) +  #A1.east
  geom_segment(aes(x = -70, y = 9, xend = -70, yend = 20)) +  #A2.east
  geom_segment(aes(x = -60, y = 9, xend = -60, yend = 20)) +  #A3.east
  geom_segment(aes(x = -90, y = 20, xend = -90, yend = 30)) + #B1.east
  geom_segment(aes(x = -80, y = 20, xend = -80, yend = 30)) + #B2.east
  geom_segment(aes(x = -70, y = 20, xend = -70, yend = 30)) + #B3.east
  geom_segment(aes(x = -60, y = 20, xend = -60, yend = 30)) + #B4.east
  geom_segment(aes(x = -60, y = 30, xend = -50, yend = 30)) + #B5.east
  geom_segment(aes(x = -50, y = 9, xend = -50, yend = 30)) +  #B5.east
  geom_segment(aes(x = -50, y = 30, xend = -40, yend = 30)) + #B6.east
  geom_segment(aes(x = -40, y = 9, xend = -40, yend = 30)) +  #B6.east
  # B7.east is to right +
  geom_segment(aes(x = -120, y = 40, xend = -20, yend = 40)) +
  geom_segment(aes(x = -90, y = 30, xend = -90, yend = 40)) + #C1.east
  geom_segment(aes(x = -80, y = 30, xend = -80, yend = 40)) + #C2.east
  geom_segment(aes(x = -70, y = 30, xend = -70, yend = 40)) + #C3.east
  geom_segment(aes(x = -60, y = 30, xend = -60, yend = 40)) + #C4.east
  geom_segment(aes(x = -50, y = 30, xend = -50, yend = 40)) + #C5.east
  geom_segment(aes(x = -40, y = 30, xend = -40, yend = 40)) + #C6.east
  geom_segment(aes(x = -30, y = 30, xend = -30, yend = 40)) + #C7.east
  geom_segment(aes(x = -20, y = 30, xend = -20, yend = 40)) + #C8.east
  geom_segment(aes(x = -60, y = 50, xend = -20, yend = 50)) + 
  geom_segment(aes(x = -60, y = 40, xend = -60, yend = 50)) + #D1.east
  geom_segment(aes(x = -50, y = 40, xend = -50, yend = 50)) + #D1.east
  geom_segment(aes(x = -40, y = 40, xend = -40, yend = 50)) + #D2.east
  geom_segment(aes(x = -30, y = 40, xend = -30, yend = 50)) + #D3.east
  geom_segment(aes(x = -20, y = 30, xend = -20, yend = 50)) + #D4.east
  geom_segment(aes(x = -20, y = 10, xend = -20, yend = 50)) + #D5.east
  geom_segment(aes(x = -70, y = 40, xend = -70, yend = 60)) + #E1.east
  geom_segment(aes(x = -60, y = 40, xend = -60, yend = 60)) + #E2.east
  geom_segment(aes(x = -40, y = 50, xend = -40, yend = 60)) + #F1.east
  geom_segment(aes(x = -20, y = 50, xend = -20, yend = 60)) + #F2.east
  geom_segment(aes(x = -20, y = 50, xend = 10, yend = 50)) + #F3.east
  annotate("text", x = long.e, y = lat.e, label = pval.labels, col = "red")

# Save plot
ggsave(filename = paste0(image_path, "F_tests_bearing_east.png"),
       plot = bearing_east, width = 10, height = 7)

# Plot p-values of west block-specific speed regressions on map ------------------

# Create coordinates of one point per block
long.w <- c(-95, -85, -75, -65, -55, -45, -35, -25,
            -95, -85, -75, -65, -55, -45, -35,
            -95, -85, -75, -65, -55, -45, -45, -30) 
lat.w <- c(15, 15, 15, 15, 15, 15, 15, 15,
           25, 25, 25, 25, 25, 25, 25,
           35, 35, 35, 35, 35, 35, 45, 45)
get.block <- Vectorize(TCcrediblebands::get_block)

# Create labels with block-specific p-values
blocks <- get.block(long.w, lat.w, "W")
pval.labels <- round(speed_pvals[blocks], 3) %>% as.character
pval.labels[pval.labels == "0"] <- "< 0.001"
pval.labels[pval.labels < cutoff_speed] <- 
  paste(pval.labels[pval.labels < cutoff_speed], "(*)")

# Get world map data
map.world <- map_data(map = "world")
map.world <- map.world %>% filter(long >= -140, long <= 12, lat >= 0, lat <= 70)

# Plot map and block-specific p-values
speed_west <- ggplot(map.world, aes(x = long, y = lat, group = group)) +
  labs(title = "P-values of block-specific F tests for non-AR vs AR models", 
       subtitle = "Speed regressions for TCs moving west",
       caption = paste("(*) indicates rejection of H0 (non-AR) in favor of H1 (AR)",
          "using \nBenjamini-Yekutieli (dependence-corrected) FDR adjustment"),
       x = "Longitude", y = "Latitude") +
  coord_cartesian(xlim=c(-110, 2), ylim=c(9, 60)) +
  geom_path(col = "grey") + 
  theme(legend.position="none", panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5, size = 14), 
        plot.caption = element_text(hjust = 0.5, size = 12),
        axis.title = element_text(size = 14)) +
  geom_hline(yintercept = 20) +
  geom_hline(yintercept = 30) +
  geom_segment(aes(x = -90, y = 9, xend = -90, yend = 20)) + #A1.west
  geom_segment(aes(x = -80, y = 9, xend = -80, yend = 20)) + #A2.west
  geom_segment(aes(x = -70, y = 9, xend = -70, yend = 20)) + #A3.west
  geom_segment(aes(x = -60, y = 9, xend = -60, yend = 20)) + #A4.west
  geom_segment(aes(x = -50, y = 9, xend = -50, yend = 20)) + #A5.west
  geom_segment(aes(x = -40, y = 9, xend = -40, yend = 20)) + #A6.west
  geom_segment(aes(x = -30, y = 9, xend = -30, yend = 20)) + #A7.west
  # + A8.west is to right +
  geom_segment(aes(x = -90, y = 20, xend = -90, yend = 30)) + #B1.west
  geom_segment(aes(x = -80, y = 20, xend = -80, yend = 30)) + #B2.west
  geom_segment(aes(x = -70, y = 20, xend = -70, yend = 30)) + #B3.west
  geom_segment(aes(x = -60, y = 20, xend = -60, yend = 30)) + #B4.west
  geom_segment(aes(x = -50, y = 20, xend = -50, yend = 30)) + #B5.west
  geom_segment(aes(x = -40, y = 20, xend = -40, yend = 30)) + #B6.west
  # + B7.west is to right +
  geom_segment(aes(x = -120, y = 40, xend = -40, yend = 40)) +
  geom_segment(aes(x = -90, y = 30, xend = -90, yend = 40)) + #C1.west
  geom_segment(aes(x = -80, y = 30, xend = -80, yend = 40)) + #C2.west
  geom_segment(aes(x = -70, y = 30, xend = -70, yend = 40)) + #C3.west
  geom_segment(aes(x = -60, y = 30, xend = -60, yend = 40)) + #C4.west
  geom_segment(aes(x = -50, y = 30, xend = -50, yend = 40)) + #C5.west
  geom_segment(aes(x = -40, y = 30, xend = -40, yend = 40)) + #C6.west
  geom_segment(aes(x = -40, y = 40, xend = -40, yend = 60)) + #D1.west
  annotate("text", x = long.w, y = lat.w, label = pval.labels, col = "red")

# Save plot
ggsave(filename = paste0(image_path, "F_tests_speed_west.png"),
       plot = speed_west, width = 10, height = 7)

# Plot p-values of east block-specific speed regressions on map ------------------

# Create coordinates of one point per block
long.e <- c(-85, -75, -65, -95, -85, -75, -65, -55, -45, -35,
            -95, -85, -75, -65, -55, -45, -35, -25,
            -55, -45, -35, -25, -15, -75, -65, -50, -30, -15) 
lat.e <- c(15, 15, 15, 25, 25, 25, 25, 25, 25, 25,
           35, 35, 35, 35, 35, 35, 35, 35,
           45, 45, 45, 45, 40, 52, 52, 52, 52, 52)
get.block <- Vectorize(TCcrediblebands::get_block)

# Create labels with block-specific p-values
blocks <- get.block(long.e, lat.e, "E")
pval.labels <- round(speed_pvals[blocks], 3) %>% as.character
pval.labels[pval.labels == "0"] <- "< 0.001"
pval.labels[pval.labels < cutoff_speed] <- 
  paste(pval.labels[pval.labels < cutoff_speed], "(*)")

# Get world map data
map.world <- map_data(map = "world")
map.world <- map.world %>% filter(long >= -140, long <= 12, lat >= 0, lat <= 70)

# Plot map and block-specific p-values
speed_east <- ggplot(map.world, aes(x = long, y = lat, group = group)) +
  labs(title = "P-values of block-specific F tests for non-AR vs AR models", 
       subtitle = "Speed regressions for TCs moving east",
       caption = paste("(*) indicates rejection of H0 (non-AR) in favor of H1 (AR)",
          "using \nBenjamini-Yekutieli (dependence-corrected) FDR adjustment"),
       x = "Longitude", y = "Latitude") +
  coord_cartesian(xlim=c(-110, 2), ylim=c(9, 60)) +
  geom_path(col = "grey") + 
  theme(legend.position="none", panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5, size = 14), 
        plot.caption = element_text(hjust = 0.5, size = 12),
        axis.title = element_text(size = 14)) +
  geom_hline(yintercept = 30) +
  geom_segment(aes(x = -120, y = 20, xend = -60, yend = 20)) +
  geom_segment(aes(x = -120, y = 30, xend = -60, yend = 30)) +
  geom_segment(aes(x = -80, y = 9, xend = -80, yend = 20)) +  #A1.east
  geom_segment(aes(x = -70, y = 9, xend = -70, yend = 20)) +  #A2.east
  geom_segment(aes(x = -60, y = 9, xend = -60, yend = 20)) +  #A3.east
  geom_segment(aes(x = -90, y = 20, xend = -90, yend = 30)) + #B1.east
  geom_segment(aes(x = -80, y = 20, xend = -80, yend = 30)) + #B2.east
  geom_segment(aes(x = -70, y = 20, xend = -70, yend = 30)) + #B3.east
  geom_segment(aes(x = -60, y = 20, xend = -60, yend = 30)) + #B4.east
  geom_segment(aes(x = -60, y = 30, xend = -50, yend = 30)) + #B5.east
  geom_segment(aes(x = -50, y = 9, xend = -50, yend = 30)) +  #B5.east
  geom_segment(aes(x = -50, y = 30, xend = -40, yend = 30)) + #B6.east
  geom_segment(aes(x = -40, y = 9, xend = -40, yend = 30)) +  #B6.east
  # B7.east is to right +
  geom_segment(aes(x = -120, y = 40, xend = -20, yend = 40)) +
  geom_segment(aes(x = -90, y = 30, xend = -90, yend = 40)) + #C1.east
  geom_segment(aes(x = -80, y = 30, xend = -80, yend = 40)) + #C2.east
  geom_segment(aes(x = -70, y = 30, xend = -70, yend = 40)) + #C3.east
  geom_segment(aes(x = -60, y = 30, xend = -60, yend = 40)) + #C4.east
  geom_segment(aes(x = -50, y = 30, xend = -50, yend = 40)) + #C5.east
  geom_segment(aes(x = -40, y = 30, xend = -40, yend = 40)) + #C6.east
  geom_segment(aes(x = -30, y = 30, xend = -30, yend = 40)) + #C7.east
  geom_segment(aes(x = -20, y = 30, xend = -20, yend = 40)) + #C8.east
  geom_segment(aes(x = -60, y = 50, xend = -20, yend = 50)) + 
  geom_segment(aes(x = -60, y = 40, xend = -60, yend = 50)) + #D1.east
  geom_segment(aes(x = -50, y = 40, xend = -50, yend = 50)) + #D1.east
  geom_segment(aes(x = -40, y = 40, xend = -40, yend = 50)) + #D2.east
  geom_segment(aes(x = -30, y = 40, xend = -30, yend = 50)) + #D3.east
  geom_segment(aes(x = -20, y = 30, xend = -20, yend = 50)) + #D4.east
  geom_segment(aes(x = -20, y = 10, xend = -20, yend = 50)) + #D5.east
  geom_segment(aes(x = -70, y = 40, xend = -70, yend = 60)) + #E1.east
  geom_segment(aes(x = -60, y = 40, xend = -60, yend = 60)) + #E2.east
  geom_segment(aes(x = -40, y = 50, xend = -40, yend = 60)) + #F1.east
  geom_segment(aes(x = -20, y = 50, xend = -20, yend = 60)) + #F2.east
  geom_segment(aes(x = -20, y = 50, xend = 10, yend = 50)) + #F3.east
  annotate("text", x = long.e, y = lat.e, label = pval.labels, col = "red")

# Save plot
ggsave(filename = paste0(image_path, "F_tests_speed_east.png"),
       plot = speed_east, width = 10, height = 7)

