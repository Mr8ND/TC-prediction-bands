library(tidyverse)
library(gridExtra)
library(TCcrediblebands)
library(progress)
library(forcats)

# data loading -------------------

data_loc <- "main/data/"

a <- load(paste0(data_loc,"sim_validation_results100_2018-07-07.Rdata"))
simulation_validation_pipeline100 <- simulation_validation_pipeline


# summary functions ------------------

average_n_length_mat <- function(list_in_vec, sim_number = NULL){

  out_mat <- sapply(list_in_vec, function(x){
    c(prop = mean(x),length = length(x))
  }) %>% t %>% data.frame
  
  if (!is.null(sim_number)) {
    idx <- sample(x = nrow(out_mat), size = min(sim_number,nrow(out_mat)))
    out_mat <- out_mat[idx, ]
  }
  
  out_mat$index <- rownames(out_mat)
  
  out_mat <- out_mat[order(out_mat$prop),]
  
  n <- nrow(out_mat)
  
  out_mat$quantile <- 1 - c(1:n)/n
  
  unique_quant <- out_mat %>%
    group_by(prop) %>%
    summarize(max_quant = max(quantile))
  
  out_mat <- out_mat %>% left_join(unique_quant,
                        by = c("prop" = "prop"))
  
  # adding 1.0, 0 point
  out_mat <- rbind(
    data.frame(prop = 0,
               length = -1,
               index = -1,
               quantile = 1,
               max_quant = 1),
    out_mat
  )
  
  return(out_mat)
}

# makes a list of vector that can provide "emprirical cdf" of proportion captured

average_n_length_mat_wrapper <- function(tc_list,
                                         sim_number = NULL,
                                         verbose = TRUE){
  
  if (verbose) {
    n_desired = length(tc_list)
    pb <- progress::progress_bar$new(
      format = "Convert In Vec to Vis DF [:bar] :percent eta: :eta",
      total = n_desired, clear = FALSE, width = 52)
  }
  
  
  output_list <- list()
  for (tc_name in names(tc_list)) {
    output_list[[tc_name]] <- list()
    for (sim_type in names(tc_list[[tc_name]])) {
      output_list[[tc_name]][[sim_type]] <- list()
      for (cb_type in names(tc_list[[tc_name]][[sim_type]])) {
        df_compression <- tc_list[[tc_name]][[sim_type]][[cb_type]] %>%
            average_n_length_mat(sim_number = sim_number)  
        output_list[[tc_name]][[sim_type]][[cb_type]] <- df_compression
      }
    }
    if (verbose) {
      pb$tick()
    }
  }
  return(output_list)
}

all_sim_df_together <- function(df_list_tc, verbose = TRUE){
  if (verbose) {
    n_desired = length(df_list_tc)
    pb <- progress::progress_bar$new(
      format = "Convert Vis DF to Single DF [:bar] :percent eta: :eta",
      total = n_desired, clear = FALSE, width = 54)
  }
  combo_df <- data.frame(prop = -1, 
                         length = -1, 
                         index = -1,
                         quantile = -1,
                         max_quant = -1,
                         tc_name = "Ben",
                         sim_type = "Auto_DeathRegs",
                         cb_type = "delta_ball")
  
  for (tc_name in names(df_list_tc)) {
    for (sim_type in names(df_list_tc[[tc_name]])) {
      for (cb_type in names(df_list_tc[[tc_name]][[sim_type]])) {
        inner_df <- df_list_tc[[tc_name]][[sim_type]][[cb_type]] %>%
          mutate(tc_name = tc_name, 
                 sim_type = sim_type,
                 cb_type = cb_type)
        combo_df <- rbind(combo_df, inner_df)
      }
    }
    if (verbose) {
      pb$tick()
    }
  }
  
  combo_df <- combo_df[-1,]
  return(combo_df)
}

make_mat_cleaner <- function(single_mat){
  single_mat_clean <- single_mat %>%
    mutate(sim_type = factor(sim_type, 
                             levels = c("Auto_DeathRegs", "Auto_NoDeathRegs",
                                        "NoAuto_DeathRegs", "NoAuto_NoDeathRegs"),
                             labels = c("Auto Regression, Kernel Death",
                                        "Auto Regression, Bernoulli Death",
                                        "Non Auto Regression, Kernel Death",
                                        "Non Auto Regression, Bernoulli Death")),
           cb_type = factor(cb_type,
                            levels = c("kde", "bubble_ci",
                                       "delta_ball",  "convex_hull"),
                            labels = c("Kernel Density Estimate",
                                       "Pointwise Bubble Estimate",
                                       "Delta Ball Covering",
                                       "Convex Hull"))
    )
  
  return(single_mat_clean)
}

grab_subset_sim <- function(sim_validation_pipeline, lower, upper){
  out_list <- list()
  for (tc in names(sim_validation_pipeline)) {
    out_list[[tc]] <- list()
    for (sim in names(sim_validation_pipeline[[tc]])) {
      out_list[[tc]][[sim]] <- list()
      for (cb in names(sim_validation_pipeline[[tc]][[sim]])) {
        out_list[[tc]][[sim]][[cb]] <- list()
        idx = 1
        for (idv in lower:upper) {
          out_list[[tc]][[sim]][[cb]][[idx]] <- sim_validation_pipeline[[
                                                        tc]][[sim]][[cb]][[idv]]
          idx = idx + 1
        }
      }
    }
  }
  
  return(out_list)
}


b <- load(paste0(data_loc,"sim_validation_results75_2018-06-29.Rdata"))
simulation_validation_pipeline50 <- grab_subset_sim(
                                      simulation_validation_pipeline, 1, 50)
simulation_validation_pipeline25 <- grab_subset_sim(
                                      simulation_validation_pipeline, 51, 75)

# processing data for ggplot visuals  ------------------

df_list_tc <- average_n_length_mat_wrapper(simulation_validation_pipeline100)
df_list_tc50 <- average_n_length_mat_wrapper(simulation_validation_pipeline50)
df_list_tc25 <- average_n_length_mat_wrapper(simulation_validation_pipeline25)

single_mat <- all_sim_df_together(df_list_tc)
single_mat50 <- all_sim_df_together(df_list_tc50)
single_mat25 <- all_sim_df_together(df_list_tc25)

single_mat <- make_mat_cleaner(single_mat)
single_mat50 <- make_mat_cleaner(single_mat50)
single_mat25 <- make_mat_cleaner(single_mat25)

# saving data for ggplot visuals  ------------------

save(df_list_tc, df_list_tc50, df_list_tc25,
     single_mat, single_mat50, single_mat25,
     file = paste0("main/data/sim_validation_results_summary_data",
                   Sys.Date(),
                   ".Rdata"))


# loading data
#b <- load("main/data/sim_validation_results_summary_data2018-07-07.Rdata")

# Visualizations -----------------

image_path <- "mini_report/images/cb_strength/"

  
# data creation for visualization -----------------

all_data_three <- rbind(single_mat %>% mutate(num_curves = 100),
                        single_mat50 %>% mutate(num_curves = 50),
                        single_mat25 %>% mutate(num_curves = 25)) %>% 
  filter(length != -1)

all_data_pointwise <- all_data_three %>%
  group_by(tc_name, sim_type, cb_type, num_curves) %>%
  summarize(num_captured_points = sum(round(length*prop)),
            total_num_points = sum(length)) %>%
  mutate(prop_captured = num_captured_points/total_num_points)

all_data_uniform <- all_data_three %>% 
  group_by(tc_name, sim_type, cb_type, num_curves) %>%
  summarize(num_captured_curves = sum(prop == 1)) %>%
  mutate(prop_captured_curves = num_captured_curves/num_curves)

all_data_pointwise %>% ggplot() + 
  geom_boxplot(aes(y = prop_captured, 
                   fill = cb_type, 
                   x = factor(num_curves))) + 
  facet_grid(~sim_type) + geom_hline(yintercept = .9)


# Actual visualization and saving -----------------------


# uniform PB
all_data_uniform %>% ggplot() + 
  geom_boxplot(aes(y = prop_captured_curves, 
                   fill = forcats::fct_relevel(cb_type,
                                      "Pointwise Bubble Estimate",
                                      "Kernel Density Estimate",
                                      "Convex Hull",
                                      "Delta Ball Covering"),
                   color = forcats::fct_relevel(cb_type,
                                                "Pointwise Bubble Estimate",
                                                "Kernel Density Estimate",
                                                "Convex Hull",
                                                "Delta Ball Covering"),
                   x = factor(num_curves)), alpha = .5) + 
  facet_grid(~sim_type, labeller = label_wrap_gen(width = 18)) + 
  geom_hline(yintercept = .9, linetype = "dashed") +
  guides(fill = guide_legend(reverse = TRUE),
         color = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  scale_y_continuous(breaks = c(0,.25,.5,.75,.9,1)) +
  theme(strip.background = element_rect(fill = "grey90", color = NA)) +
  labs(x = "Number of Curves Examined",
       y = "Proportion of Curves Captured",
       fill = "Prediction Band Type",
       color = "Prediction Band Type")

ggsave(paste0(image_path,"unif_cb_boxplot.pdf"), device = "pdf",
       width = 10, height = 6.5, units = "in")

# pointwise PB
all_data_pointwise %>% ggplot() + 
  geom_boxplot(aes(y = prop_captured, 
                   fill = forcats::fct_relevel(cb_type,
                                               "Pointwise Bubble Estimate",
                                               "Kernel Density Estimate",
                                               "Convex Hull",
                                               "Delta Ball Covering"),
                   color = forcats::fct_relevel(cb_type,
                                                "Pointwise Bubble Estimate",
                                                "Kernel Density Estimate",
                                                "Convex Hull",
                                                "Delta Ball Covering"),
                   x = factor(num_curves)), alpha = .5) + 
  facet_grid(~sim_type, labeller = label_wrap_gen(width = 18)) + 
  geom_hline(yintercept = .9, linetype = "dashed") +
  guides(fill = guide_legend(reverse = TRUE),
         color = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  scale_y_continuous(breaks = c(0,.25,.5,.75,.9,1)) +
  theme(strip.background = element_rect(fill = "grey90", color = NA)) +
  labs(x = "Number of Curves Examined",
       y = "Proportion of Points of Curves Captured",
       fill = "Prediction Band Type",
       color = "Prediction Band Type")

ggsave(paste0(image_path,"pw_cb_boxplot.pdf"), device = "pdf",
       width = 10, height = 6.5, units = "in")





### area:

latest_full_output_pipeline <- 'output_pipeline_alphalevel0.1_complete_2018-07-02.Rdata'

data_loc <- "main/data/"
a <- load(paste0(data_loc, latest_full_output_pipeline)) #output_list_pipeline
eval(parse(text = paste0("output_list_pipeline <- ",a)))


df_area <- data.frame(area = -99, cb = "ben", tc = "ben", sim = "ben") %>%
              mutate(cb = as.character(cb),
                     tc = as.character(tc),
                     sim = as.character(sim))

for (tc in 1:length(output_list_pipeline)) {
  for (sim in names(output_list_pipeline[[tc]])) {
    for (cb in names(output_list_pipeline[[tc]][[sim]])[-5]) {
      df_area <- rbind(df_area, 
                       data.frame(
                        area = output_list_pipeline[[tc]][[sim]][[cb]]$area,
                        cb = cb, tc = tc, sim = sim))
    }
  }
}

df_area <- df_area[-1,]

library(reshape2)

df_area <- df_area %>%
    mutate(sim = factor(sim, 
                             levels = c("Auto_DeathRegs", "Auto_NoDeathRegs",
                                        "NoAuto_DeathRegs", "NoAuto_NoDeathRegs"),
                             labels = c("Auto Regression, Kernel Death",
                                        "Auto Regression, Bernoulli Death",
                                        "Non Auto Regression, Kernel Death",
                                        "Non Auto Regression, Bernoulli Death")),
           cb = factor(cb,
                            levels = c("kde", "bubble_ci",
                                       "delta_ball",  "convex_hull"),
                            labels = c("Kernel Density Estimate",
                                       "Pointwise Bubble Estimate",
                                       "Delta Ball Covering",
                                       "Convex Hull")))

area_table <- df_area %>% 
  group_by(sim, cb) %>%
  summarize(mean = mean(area),
            median = median(area),
            q25 = quantile(area,.25),
            q75 = quantile(area,.75),
            sd = sd(area)) %>% 
  melt(id.vars = c("sim", "cb")) %>%
  dcast( variable ~ sim + cb) %>% 
  mutate(variable = as.character(variable))

area_table[,-1] <- round(area_table[,-1])


area_table <- rbind(area_table,
                    c("Auto/Non Auto:",
                      sapply(names(area_table), 
                              function(x) strsplit(x = x, split = ",")[[1]][1])[-1]
                      ),
                    c("Death:",
                      sapply(names(area_table),
                              function(x) strsplit(x = x, split = ", |_")[[1]][2])[-1]),
                    c("Prediction Band:",
                      sapply(names(area_table),
                              function(x) strsplit(x = x, split = "_")[[1]][2])[-1]))

# names(area_table) <- gsub(x = names(area_table), 
#                           pattern = "_", 
#                           replacement = " & ")
# names(area_table)[1] <- " "

columns <- c(1,14:17) # 2:5, 6:9, 10:13, 14:17

area_table %>% 
  filter(variable %in% c("Auto/Non Auto:", "Death:", "Prediction Band:",
                         "q25", "median","q75" )) %>%
  .[c(4:6,2,1,3),columns] %>% xtable