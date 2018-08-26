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

# Resids vs fits, bearing AR ------------------

# Pool all bearing AR residuals
bearing_resids_AR <- lapply(train_models$bearing_regs_auto, 
                            FUN = function(x) x$residuals) %>% unlist %>% unname

# Pool all bearing AR fits
bearing_fits_AR <- lapply(train_models$bearing_regs_auto, 
                            FUN = function(x) x$fitted.values) %>% unlist %>% unname

# Data frame of bearing AR residuals and fits
bearing_AR_df <- data.frame(resids = bearing_resids_AR,
                            fits = bearing_fits_AR)

# Plot of pooled bearing AR residuals versus fits
bearing_AR_df %>% ggplot(aes(x = fits, y = resids)) +
  geom_point(alpha = 0.2) +
  labs(x = "Fitted Values", y = "Residuals", 
       title = "Residuals versus Fitted Values",
       subtitle = "AR Bearing Regressions") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  ggsave(filename = paste0(image_path, "resids_fit_bear_AR.png"),
         width = 8, height = 5)

# Resids vs fits, bearing non-AR ------------------

# Pool all bearing non-AR residuals
bearing_resids_nonAR <- lapply(train_models$bearing_regs_nonauto, 
                            FUN = function(x) x$residuals) %>% unlist %>% unname

# Pool all bearing non-AR fits
bearing_fits_nonAR <- lapply(train_models$bearing_regs_nonauto, 
                            FUN = function(x) x$fitted.values) %>% unlist %>% unname

# Data frame of bearing non-AR residuals and fits
bearing_nonAR_df <- data.frame(resids = bearing_resids_nonAR,
                               fits = bearing_fits_nonAR)

# Plot of pooled bearing non-AR residuals versus fits
bearing_nonAR_df %>% ggplot(aes(x = fits, y = resids)) +
  geom_point(alpha = 0.2) +
  labs(x = "Fitted Values", y = "Residuals", 
       title = "Residuals versus Fitted Values",
       subtitle = "Non-AR Bearing Regressions") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  ggsave(filename = paste0(image_path, "resids_fit_bear_nonAR.png"),
         width = 8, height = 5)

# Resids vs fits, speed AR ------------------

# Pool all speed AR residuals
speed_resids_AR <- lapply(train_models$speed_regs_auto, 
                            FUN = function(x) x$residuals) %>% unlist %>% unname

# Pool all speed AR fits
speed_fits_AR <- lapply(train_models$speed_regs_auto, 
                            FUN = function(x) x$fitted.values) %>% unlist %>% unname

# Data frame of speed AR residuals and fits
speed_AR_df <- data.frame(resids = speed_resids_AR,
                            fits = speed_fits_AR)

# Plot of pooled speed AR residuals versus fits
speed_AR_df %>% ggplot(aes(x = fits, y = resids)) +
  geom_point(alpha = 0.2) +
  labs(x = "Fitted Values", y = "Residuals", 
       title = "Residuals versus Fitted Values",
       subtitle = "AR Speed Regressions") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  ggsave(filename = paste0(image_path, "resids_fit_speed_AR.png"),
         width = 8, height = 5)

# Resids vs fits, speed non-AR ------------------

# Pool all speed non-AR residuals
speed_resids_nonAR <- lapply(train_models$speed_regs_nonauto, 
                            FUN = function(x) x$residuals) %>% unlist %>% unname

# Pool all speed non-AR fits
speed_fits_nonAR <- lapply(train_models$speed_regs_nonauto, 
                            FUN = function(x) x$fitted.values) %>% unlist %>% unname

# Data frame of speed non-AR residuals and fits
speed_nonAR_df <- data.frame(resids = speed_resids_nonAR,
                               fits = speed_fits_nonAR)

# Plot of pooled speed non-AR residuals versus fits
speed_nonAR_df %>% ggplot(aes(x = fits, y = resids)) +
  geom_point(alpha = 0.2) +
  labs(x = "Fitted Values", y = "Residuals", 
       title = "Residuals versus Fitted Values",
       subtitle = "Non-AR Speed Regressions") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  ggsave(filename = paste0(image_path, "resids_fit_speed_nonAR.png"),
         width = 8, height = 5)

# Resids versus fits on individual blocks ------------------
block_names <- train_models$bearing_regs_auto %>% names

# AR, bearing
for(name in block_names){
  plot(train_models$bearing_regs_auto[[name]]$fitted.values,
       train_models$bearing_regs_auto[[name]]$residuals,
       main = paste("AR bearing", name))
}

# Non-AR, bearing
for(name in block_names){
  plot(train_models$bearing_regs_nonauto[[name]]$fitted.values,
       train_models$bearing_regs_nonauto[[name]]$residuals,
       main = paste("Non-AR bearing", name))
}

# AR, speed
for(name in block_names){
  plot(train_models$speed_regs_auto[[name]]$fitted.values,
       train_models$speed_regs_auto[[name]]$residuals,
       main = paste("AR speed", name))
}

# Non-AR, speed
for(name in block_names){
  plot(train_models$speed_regs_nonauto[[name]]$fitted.values,
       train_models$speed_regs_nonauto[[name]]$residuals,
       main = paste("Non-AR speed", name), 
       xlab = "Fitted Values", ylab = "Residuals")
}
