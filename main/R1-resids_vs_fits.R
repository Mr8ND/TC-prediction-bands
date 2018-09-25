# This script plots residuals versus fitted values for the change in bearing
# and change in speed regressions. 
# Saves images of pooled (across blocks) residuals vs fits for bearing/AR, 
# bearing/non-AR, speed/AR, speed/non-AR. 
# Overlays resids vs fits for a randomly selected block.
# Creates, but does not save, images of residuals vs fits for bearing/AR, 
# bearing/non-AR, speed/AR, speed/non-AR on individual blocks.

library(tidyverse)
library(TCpredictionbands)
library(grid)
library(gridExtra)

# Load Data ------------------
data_loc <- "main/data/"
image_path <- "report/images/"

load(paste0(data_loc, "raw_data.Rdata"))
train <- train_data

# Train models on training data ------------------
train_models <- get_train_models(train)

# theme ------------------
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

# Pool all bearing AR/non-AR block names
bearing_names_AR <- lapply(train_models$bearing_regs_auto, 
                      FUN = function(x) fitted(x, na.action = na.exclude)) %>%
  unlist %>% names %>% substr(1, 7)

bearing_names_nonAR <- lapply(train_models$bearing_regs_nonauto, 
                        FUN = function(x) fitted(x, na.action = na.exclude)) %>%
  unlist %>% names %>% substr(1, 7)

# Randomly select block for bearing AR/non-AR resids vs fits
set.seed(27)
bearing_AR_block <- sample(names(train_models$bearing_regs_auto), 1)
bearing_nonAR_block <- sample(names(train_models$bearing_regs_nonauto), 1)

# Data frames of bearing AR/non-AR residuals and fits
bearing_AR_df <- data.frame(resids = bearing_resids_AR,
                            fits = bearing_fits_AR,
                            blocks = bearing_names_AR) %>%
  mutate(random_block = factor(blocks == bearing_AR_block, 
                               levels = c(T, F))) %>%
  mutate(random_block = fct_recode(random_block,
                                   "Yes" = "TRUE", "No" = "FALSE"))

bearing_nonAR_df <- data.frame(resids = bearing_resids_nonAR,
                               fits = bearing_fits_nonAR,
                               blocks = bearing_names_nonAR) %>%
  mutate(random_block = factor(blocks == bearing_nonAR_block, 
                               levels = c(T, F)))  %>%
  mutate(random_block = fct_recode(random_block,
                                   "Yes" = "TRUE", "No" = "FALSE"))

# Plots of pooled bearing AR and non-AR residuals versus fits
resids_fits_bear_1 <- bearing_AR_df %>%
  ggplot(aes(x = fits, y = resids, color = random_block)) +
  geom_point(data = bearing_AR_df %>% filter(random_block == "No"),
             aes(color = random_block), alpha = 0.05) +
  geom_point(data = bearing_AR_df %>% filter(random_block == "Yes"),
             aes(color = random_block), alpha = 0.9, size = 0.5) +
  scale_color_manual(values = c("black", "red")) +
  labs(x = "Fitted Values", y = "Residuals", 
       title = "Autoregressive Bearing Models",
       caption = paste0("Random block: Heading east,\n",
                        "-40\u00b0 \u2264 lon < -20\u00b0, lat \u2265 50\u00b0")) +
  tc_theme +
  theme(legend.position = "none")

resids_fits_bear_2 <- bearing_nonAR_df %>%
  ggplot(aes(x = fits, y = resids, color = random_block)) +
  geom_point(data = bearing_nonAR_df %>% filter(random_block == "No"),
             aes(color = random_block), alpha = 0.05) +
  geom_point(data = bearing_nonAR_df %>% filter(random_block == "Yes"),
             aes(color = random_block), alpha = 0.9, size = 0.5) +
  scale_color_manual(values = c("black", "red")) +
  labs(x = "Fitted Values", y = "Residuals", 
       title = "Non-Autoregressive Bearing Models",
       caption = paste0("Random block: Heading east,\n",
                        "-70\u00b0 \u2264 lon < -60\u00b0, lat < 20\u00b0")) +
  tc_theme +
  theme(legend.position = "none")

layout_matrix <- matrix(c(1, 2), ncol = 2, byrow = T)

resids_fits_bear <- arrangeGrob(resids_fits_bear_1, 
            resids_fits_bear_2,
            layout_matrix = layout_matrix,
            top = textGrob(paste("Residuals Versus Fitted Values", 
                                 "of Change in Bearing Models"), 
                           gp = gpar(fontsize = 18)))

ggsave(resids_fits_bear, 
       filename = paste0(image_path, "resids_fits_bear.png"),
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

# Pool all speed AR/non-AR block names
speed_names_AR <- lapply(train_models$speed_regs_auto, 
                         FUN = function(x) fitted(x, na.action = na.exclude)) %>%
  unlist %>% names %>% substr(1, 7)

speed_names_nonAR <- lapply(train_models$speed_regs_nonauto, 
                            FUN = function(x) fitted(x, na.action = na.exclude)) %>%
  unlist %>% names %>% substr(1, 7)

# Randomly select block for speed AR/non-AR resids vs fits
set.seed(88)
speed_AR_block <- sample(names(train_models$speed_regs_auto), 1)
speed_nonAR_block <- sample(names(train_models$speed_regs_nonauto), 1)

# Data frames of speed AR/non-AR residuals and fits
speed_AR_df <- data.frame(resids = speed_resids_AR,
                          fits = speed_fits_AR,
                          blocks = speed_names_AR) %>%
  mutate(random_block = factor(blocks == speed_AR_block, 
                               levels = c(T, F))) %>%
  mutate(random_block = fct_recode(random_block,
                                   "Yes" = "TRUE", "No" = "FALSE"))

speed_nonAR_df <- data.frame(resids = speed_resids_nonAR,
                             fits = speed_fits_nonAR,
                             blocks = speed_names_nonAR) %>%
  mutate(random_block = factor(blocks == speed_nonAR_block, 
                               levels = c(T, F)))  %>%
  mutate(random_block = fct_recode(random_block,
                                   "Yes" = "TRUE", "No" = "FALSE"))

# Plots of pooled speed AR and non-AR residuals versus fits
resids_fits_speed_1 <- speed_AR_df %>%
  ggplot(aes(x = fits, y = resids, color = random_block)) +
  geom_point(data = speed_AR_df %>% filter(random_block == "No"),
             aes(color = random_block), alpha = 0.05) +
  geom_point(data = speed_AR_df %>% filter(random_block == "Yes"),
             aes(color = random_block), alpha = 0.9, size = 0.5) +
  scale_color_manual(values = c("black", "red")) +
  labs(x = "Fitted Values", y = "Residuals", 
       title = "Autoregressive Speed Models",
       caption = paste0("Random block: Heading west,\n",
          "-60\u00b0 \u2264 lon < -50\u00b0, 20\u00b0 \u2264 lat < 30\u00b0")) +
  tc_theme +
  theme(legend.position = "none")

resids_fits_speed_2 <- speed_nonAR_df %>%
  ggplot(aes(x = fits, y = resids, color = random_block)) +
  geom_point(data = speed_nonAR_df %>% filter(random_block == "No"),
             aes(color = random_block), alpha = 0.05) +
  geom_point(data = speed_nonAR_df %>% filter(random_block == "Yes"),
             aes(color = random_block), alpha = 0.9, size = 0.5) +
  scale_color_manual(values = c("black", "red")) +
  labs(x = "Fitted Values", y = "Residuals", 
       title = "Non-Autoregressive Speed Models",
       caption = paste0("Random block: Heading west,\n",
                        "-80\u00b0 \u2264 lon < -70\u00b0, lat < 20\u00b0")) +
  tc_theme +
  theme(legend.position = "none")

layout_matrix <- matrix(c(1, 2), ncol = 2, byrow = T)

resids_fits_speed <- arrangeGrob(resids_fits_speed_1, 
                                 resids_fits_speed_2,
                                 layout_matrix = layout_matrix,
                          top = textGrob(paste("Residuals Versus Fitted Values", 
                                               "of Change in Speed Models"),
                                         gp = gpar(fontsize = 18)))

ggsave(resids_fits_speed,
       filename = paste0(image_path, "resids_fits_speed.png"),
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
