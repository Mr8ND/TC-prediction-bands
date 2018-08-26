# Explore the densities of simulated TC survival times 
library(tidyverse)

# data processing of survival times

data_loc <- "main/data/"
image_loc <- "report/images/"

# Read in simulated curves
a <- load(paste0(data_loc, "Test_Sims_350.Rdata"))

# Store lengths of simulated TCs of each type
all_sims <- as.list.environment(test_env)

auto_logreg_all <- lapply(all_sims, FUN = function(tc) tc$Auto_DeathRegs)
auto_logreg_lengths <- unlist(lapply(auto_logreg_all, 
                                     FUN = function(tc) sapply(tc, nrow)))

auto_kde_all <- lapply(all_sims, FUN = function(tc) tc$Auto_NoDeathRegs)
auto_kde_lengths <- unlist(lapply(auto_kde_all, 
                                  FUN = function(tc) sapply(tc, nrow)))

nonauto_logreg_all <- lapply(all_sims, FUN = function(tc) tc$NoAuto_DeathRegs)
nonauto_logreg_lengths <- unlist(lapply(nonauto_logreg_all, 
                                        FUN = function(tc) sapply(tc, nrow)))

nonauto_kde_all <- lapply(all_sims, FUN = function(tc) tc$NoAuto_NoDeathRegs)
nonauto_kde_lengths <- unlist(lapply(nonauto_kde_all, 
                                     FUN = function(tc) sapply(tc, nrow)))

##############################
##### Visual of 4 curves ##### 
##############################

# set theme ---------------
theme_set(theme_minimal() +
            theme(strip.background = element_rect(fill = "grey90", color = NA))
)

# compile data frames ------------
four_hist_data <- data.frame(
  Times = c(auto_logreg_lengths, auto_kde_lengths, 
            nonauto_logreg_lengths, nonauto_kde_lengths),
  Auto = c(rep(1, length(auto_logreg_lengths) + length(auto_kde_lengths)),
           rep(0, length(nonauto_logreg_lengths) + length(nonauto_kde_lengths))),
  DeathReg = c(rep(1, length(auto_logreg_lengths)), 
               rep(0,length(auto_kde_lengths)),
               rep(1, length(nonauto_logreg_lengths)), 
               rep(0, length(nonauto_kde_lengths))))

# graphic ---------------

survival_times_dens <- ggplot(four_hist_data) + 
  geom_density(aes(color = 
                     factor(DeathReg, levels = 0:1,
                            labels = c("Kernel","Logistic")), 
                   x = Times),
                 alpha = .5, position = "identity") + 
  facet_grid(factor(Auto, levels = 0:1,labels = c("Non-Auto","Auto"))~.)  +
  labs(x = "Survival Time of TC (6-hour intervals)",
       y = "Density", 
       color = "Lysis Model",
       title = "Distribution of TC Survival Times") + 
  scale_color_manual(values = c("red","blue")) 

ggsave(plot = survival_times_dens,
       filename = paste0(image_loc, "Four_DeathTimePlots.pdf"), 
         width = 6, height = 4)

