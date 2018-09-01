# This script plots residuals versus fitted values for the change in bearing
# and change in speed regressions. 
# Saves images of pooled (across blocks) residuals vs fits for bearing/AR, 
# bearing/non-AR, speed/AR, speed/non-AR.
# Creates, but does not save, images of residuals vs fits for bearing/AR, 
# bearing/non-AR, speed/AR, speed/non-AR on individual blocks.

library(tidyverse)
library(TCcrediblebands)

# Load Data ------------------
data_loc <- "main/data/"
image_path <- "report/images/"

load(paste0(data_loc, "raw_data.Rdata"))
train <- train_data

# Train models on training data ------------------
train_models <- get_train_models(train)

# Set TC ggplot2 theme ------------------
tc_theme <- theme_minimal() + 
  theme(strip.background = element_rect(fill = "grey90", color = NA),
        plot.title = element_text(hjust = 0.5, size = 18),
        strip.text.x = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12), 
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

# Resids vs fits, bearing AR and non-AR ------------------

# Pool all bearing AR/non-AR residuals
bearing_resids_AR <- lapply(train_models$bearing_regs_auto, 
                      FUN = function(x) resid(x, na.action = na.exclude)) %>% 
                              unlist %>% unname

bearing_resids_nonAR <- lapply(train_models$bearing_regs_nonauto, 
                      FUN = function(x) resid(x, na.action = na.exclude)) %>%
                              unlist %>% unname

# Pool all bearing AR/non-AR fits
bearing_fits_AR <- lapply(train_models$bearing_regs_auto, 
                      FUN = function(x) fitted(x, na.action = na.exclude)) %>%
                              unlist %>% unname

bearing_fits_nonAR <- lapply(train_models$bearing_regs_nonauto, 
                      FUN = function(x) fitted(x, na.action = na.exclude)) %>%
                              unlist %>% unname

# Data frames of bearing AR/non-AR residuals and fits
bearing_AR_df <- data.frame(resids = bearing_resids_AR,
                            fits = bearing_fits_AR)

bearing_nonAR_df <- data.frame(resids = bearing_resids_nonAR,
                               fits = bearing_fits_nonAR)

# Combine data frames of bearing AR/non-AR residuals and fits
bearing_df <- rbind(bearing_AR_df, bearing_nonAR_df) %>%
  mutate(model = factor(c(
    rep("Autoregressive Bearing Models", nrow(bearing_AR_df)),
    rep("Non-Autoregressive Bearing Models", nrow(bearing_nonAR_df)))))

# Plot of pooled bearing AR and non-AR residuals versus fits
resids_fits_bear <- bearing_df %>% 
  ggplot(aes(x = fits, y = resids)) +
  geom_point(alpha = 0.1) +
  facet_wrap(~ model) +
  labs(x = "Fitted Values", y = "Residuals", 
       title = "Residuals Versus Fitted Values of Change-in-Bearing Models") +
  tc_theme

ggsave(resids_fits_bear, 
       filename = paste0(image_path, "resids_fit_bear.png"),
       width = 10, height = 4)

# Resids vs fits, speed AR and non-AR ------------------

# Pool all speed AR/non-AR residuals
speed_resids_AR <- lapply(train_models$speed_regs_auto, 
                      FUN = function(x) resid(x, na.action = na.exclude)) %>% 
                              unlist %>% unname

speed_resids_nonAR <- lapply(train_models$speed_regs_nonauto, 
                      FUN = function(x) resid(x, na.action = na.exclude)) %>%
                              unlist %>% unname

# Pool all speed AR/non-AR fits
speed_fits_AR <- lapply(train_models$speed_regs_auto, 
                      FUN = function(x) fitted(x, na.action = na.exclude)) %>%
                              unlist %>% unname

speed_fits_nonAR <- lapply(train_models$speed_regs_nonauto, 
                      FUN = function(x) fitted(x, na.action = na.exclude)) %>%
                              unlist %>% unname

# Data frames of speed AR/non-AR residuals and fits
speed_AR_df <- data.frame(resids = speed_resids_AR,
                          fits = speed_fits_AR)

speed_nonAR_df <- data.frame(resids = speed_resids_nonAR,
                             fits = speed_fits_nonAR)

# Combine data frames of bearing AR/non-AR residuals and fits
speed_df <- rbind(speed_AR_df, speed_nonAR_df) %>%
  mutate(model = factor(c(
    rep("Autoregressive Speed Models", nrow(speed_AR_df)),
    rep("Non-Autoregressive Speed Models", nrow(speed_nonAR_df)))))

# Plot of pooled speed AR and non-AR residuals versus fits
resids_fit_speed <- speed_df %>% 
  ggplot(aes(x = fits, y = resids)) +
  geom_point(alpha = 0.1) +
  facet_wrap(~ model) +
  labs(x = "Fitted Values", y = "Residuals", 
       title = "Residuals Versus Fitted Values of Change-in-Speed Models") +
  tc_theme

ggsave(resids_fit_speed,
       filename = paste0(image_path, "resids_fit_speed.png"),
       width = 10, height = 4)

# Resids versus fits on individual blocks ------------------
block_names <- train_models$bearing_regs_auto %>% names

# AR, bearing
for(name in block_names){
  plot(fitted(train_models$bearing_regs_auto[[name]], na.action = na.exclude),
       resid(train_models$bearing_regs_auto[[name]], na.action = na.exclude),
       main = paste("AR bearing", name))
}

# Non-AR, bearing
for(name in block_names){
  plot(fitted(train_models$bearing_regs_nonauto[[name]], na.action = na.exclude),
       resid(train_models$bearing_regs_nonauto[[name]], na.action = na.exclude),
       main = paste("Non-AR bearing", name))
}

# AR, speed
for(name in block_names){
  plot(fitted(train_models$speed_regs_auto[[name]], na.action = na.exclude),
       resid(train_models$speed_regs_auto[[name]], na.action = na.exclude),
       main = paste("AR speed", name))
}

# Non-AR, speed
for(name in block_names){
  plot(fitted(train_models$speed_regs_nonauto[[name]], na.action = na.exclude),
       resid(train_models$speed_regs_nonauto[[name]], na.action = na.exclude),
       main = paste("Non-AR speed", name), 
       xlab = "Fitted Values", ylab = "Residuals")
}
