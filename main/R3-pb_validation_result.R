library(tidyverse)
library(gridExtra)
library(TCpredictionbands)
library(progress)
library(forcats)
library(reshape2)
library(xtable)

# data loading -------------------

data_loc <- "main/data/"
image_path <- "report/images/"
table_path <- "report/tables/"

a <- load(paste0(data_loc,"sim_validation_results100.Rdata"))
simulation_validation_pipeline100 <- simulation_validation_pipeline

# renaming vectors ------------------

cb_type_table_levels <- c("Kernel Density Estimate" = "kde",
                          "Spherical" = "bubble_ci",
                          "Delta Ball"     = "delta_ball",
                          "Convex Hull"             = "convex_hull")
cb_type_table_labels <- names(cb_type_table_levels)

cb_type_graphic_levels <- c("Kernel Density Estimate" = "kde",
                            "Spherical" = "bubble_ci",
                            "Delta Ball"     = "delta_ball",
                            "Convex Hull"             = "convex_hull")
cb_type_graphic_labels <- names(cb_type_graphic_levels)

sim_type_table_levels <- c("AR \\& Logistic"     = "Auto_DeathRegs",
                           "AR \\& Kernel"       = "Auto_NoDeathRegs",
                           "Non-AR \\& Logistic" = "NoAuto_DeathRegs",
                           "Non-AR \\& Kernel"   = "NoAuto_NoDeathRegs")
sim_type_table_labels <- names(sim_type_table_levels)

sim_type_graphic_levels <- 
  c("Autoregression & Logistic-based Lysis"     = "Auto_DeathRegs",
    "Autoregression & Kernel-based Lysis"       = "Auto_NoDeathRegs",
    "Non-Autoregression & Logistic-based Lysis" = "NoAuto_DeathRegs",
    "Non-Autoregression & Kernel-based Lysis"   = "NoAuto_NoDeathRegs")

sim_type_graphic_labels <- names(sim_type_graphic_levels)

# color selection --------------------

# old colors (prettier)
pb_color_vec_val_old <- c("#0300D8", "#BF0700", "#00E5E5", "#CB00C5")
names(pb_color_vec_val_old) <- c("bubble_ci","kde", 
                                 "convex_hull", "delta_ball")
pb_color_vec_pb_pres_old <- c("#00E5E5","#CB00C5" ,"#0300D8", "#BF0700")
names(pb_color_vec_pb_pres_old) <- c("kde", "bubble_ci", 
                                     "delta_ball", "convex_hull")

# current colors
pb_color_vec <- c("#1b9e77","#d95f02","#7570b3", "#e7298a")
names(pb_color_vec) <- c("convex_hull", "bubble_ci", "delta_ball", "kde")

# define theme -----------------------
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

# summary functions ------------------

#' Create empirical cdf style data frame (with potentially a random subset of
#' the data)
#'
#' @param list_in_vec list of "in_vec"s, vectors with binary values expressing
#' if point of TC/ simulated TC is captured in prediction band
#' @param sim_number integer, if non-NULL we sample a subset of size sim_number
#' of "in_vecs" in use them.
#'
#' @return empirical cdf style data frame
#' @export
#'
#' @examples
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

#' create a list of data frames that contain information to visualize
#' empirical cdf of proportion captured
#'
#' @param tc_list list of tcs (list of list of lists - see details)
#' @param sim_number integer, if not null, you only want a random subset of the
#' simulated in_vecs
#' @param verbose logical for if you want a progress bar
#'
#' @return list of data frames of style average_n_length_mat
#' @export
#'
#' @details
#' tc_list is expected to be a the form:
#'   - list where each element is associated with 1 TC, with this list we expect
#'      - 4 lists pertaining to each simulation time, each of which contains:
#'        - 4 elements pertaining to each type of prediction interval
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
      for (cb_type in names(tc_list[[tc_name]][[sim_type]])[-5]) {
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

#' Converts list of data frames for visual to a single data frame
#'
#' @param df_list_tc list of data frames (per true tc)
#' @param verbose logic to decide if progress bar is provided
#'
#' @return single data frame compression of df_list_tc
#' @export
#'
#' @examples
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
      for (cb_type in names(df_list_tc[[tc_name]][[sim_type]])[-5]) {
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

#' Cleans up matrix by converting factor names to be more readable
#'
#' @param single_mat data frame with sim_type and cb_type
#'
#' @return updated data frame
#' @export
#'
#' @examples
make_mat_cleaner <- function(single_mat){
  single_mat_clean <- single_mat %>%
    mutate(sim_type_graph = factor(sim_type,
                             levels = sim_type_graphic_levels,
                             labels = sim_type_graphic_labels),
           cb_type_graph = factor(cb_type,
                            levels = cb_type_graphic_levels,
                            labels = cb_type_graphic_labels),
           sim_type_table = factor(sim_type,
                                   levels = sim_type_table_levels,
                                   labels = sim_type_table_labels),
           cb_type_table = factor(cb_type,
                                  levels = cb_type_table_levels,
                                  labels = cb_type_table_labels))
  return(single_mat_clean)
}

#' Selects a subset of the simulation validation pipeline object based on a
#' range of indices for the simualted TCs.
#'
#' @param sim_validation_pipeline list object
#' @param lower integer value for the lower end of the range of simulated TC
#' indices
#' @param upper integer value for the upper end of the range of simulated TC
#' indices
#'
#' @return smaller version of sim_validation_pipeline
#' @export
#'
#' @examples
grab_subset_sim <- function(sim_validation_pipeline, lower, upper){
  out_list <- list()
  for (tc in names(sim_validation_pipeline)) {
    out_list[[tc]] <- list()
    for (sim in names(sim_validation_pipeline[[tc]])) {
      out_list[[tc]][[sim]] <- list()
      for (cb in names(sim_validation_pipeline[[tc]][[sim]])[-5]) {
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

#' Selects a subset of the time storage matrix based on a range of indices for
#' the simualted TCs.
#'
#' @param time_storage list object
#' @param lower integer value for the lower end of the range of simulated TC
#' indices
#' @param upper integer value for the upper end of the range of simulated TC
#' indices
#'
#' @return smaller version of time_storage
#' @export
#'
#' @examples
grab_subset_time <- function(time_storage, lower, upper){
  out_list <- list()
  for (sim in names(time_storage)) {
    out_list[[sim]] <- time_storage[[sim]][lower:upper]
  }
  return(out_list)
}


# grabbing last bit of data ------------------

b <- load(paste0(data_loc,"sim_validation_results75.Rdata"))
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
     file = paste0("main/data/sim_validation_results_summary_data.Rdata"))

# loading data -----------
# (if you've already run the above)
### b <- load("main/data/sim_validation_results_summary_data.Rdata")



# Visualizations -----------------

# data creation for visualization -----------------

all_data_three <- rbind(single_mat %>% mutate(num_curves = 100),
                        single_mat50 %>% mutate(num_curves = 50),
                        single_mat25 %>% mutate(num_curves = 25)) %>%
  filter(length != -1)

all_data_pointwise_graph <- all_data_three %>%
  group_by(tc_name, sim_type_graph, cb_type_graph, num_curves) %>%
  summarize(num_captured_points = sum(round(length*prop)),
            total_num_points = sum(length)) %>%
  mutate(prop_captured = num_captured_points/total_num_points)

all_data_uniform_graph <- all_data_three %>%
  group_by(tc_name, sim_type_graph, cb_type_graph, num_curves) %>%
  summarize(num_captured_curves = sum(prop == 1)) %>%
  mutate(prop_captured_curves = num_captured_curves/num_curves)


# Actual visualization and saving -------------------

# proportion captured, uniform vs pointwise ------------------

# uniform PB
uniform_pb_assessment <- all_data_uniform_graph %>% ggplot() +
  geom_boxplot(aes(y = prop_captured_curves,
                   fill = forcats::fct_relevel(cb_type_graph,
                                      "Spherical",
                                      "Kernel Density Estimate",
                                      "Convex Hull",
                                      "Delta Ball"),
                   color = forcats::fct_relevel(cb_type_graph,
                                      "Spherical",
                                      "Kernel Density Estimate",
                                      "Convex Hull",
                                      "Delta Ball"),
                   x = factor(num_curves)), alpha = .5) +
  facet_grid(~sim_type_graph, labeller = label_wrap_gen(width = 18)) +
  geom_hline(yintercept = .9, linetype = "dashed") +
  guides(fill = guide_legend(reverse = TRUE),
         color = guide_legend(reverse = TRUE)) +
  scale_y_continuous(breaks = c(0,.25,.5,.75,.9,1)) + 
  tc_theme +
  labs(x = "Number of Curves Examined",
       y = "Proportion of Curves Captured",
       fill = "Prediction Band Type",
       color = "Prediction Band Type",
       title = "Average Uniform Containment per Curve") +
  scale_color_manual(values = as.vector(pb_color_vec[c("bubble_ci", "kde",
                                             "convex_hull", "delta_ball")])) +
  scale_fill_manual(values = as.vector(pb_color_vec[c("bubble_ci", "kde",
                                            "convex_hull", "delta_ball")]))

ggsave(plot = uniform_pb_assessment,
       file = paste0(image_path,"sim_unif_cb_boxplot.png"), device = "png",
       width = 10, height = 6.5, units = "in")

# pointwise PB

pointwise_pb_assessment <- all_data_pointwise_graph %>% ggplot() +
  geom_boxplot(aes(y = prop_captured,
                   fill = forcats::fct_relevel(cb_type_graph,
                                      "Spherical",
                                      "Kernel Density Estimate",
                                      "Convex Hull",
                                      "Delta Ball"),
                   color = forcats::fct_relevel(cb_type_graph,
                                      "Spherical",
                                      "Kernel Density Estimate",
                                      "Convex Hull",
                                      "Delta Ball"),
                   x = factor(num_curves)), alpha = .5) +
  facet_grid(~ sim_type_graph, labeller = label_wrap_gen(width = 18)) +
  geom_hline(yintercept = .9, linetype = "dashed") +
  guides(fill = guide_legend(reverse = TRUE),
         color = guide_legend(reverse = TRUE)) +
  scale_y_continuous(breaks = c(0,.25,.5,.75,.9,1)) +
  tc_theme +
  labs(x = "Number of Curves Examined",
       y = "Proportion of Points of Curves Captured",
       fill = "Prediction Band Type",
       color = "Prediction Band Type",
       title = "Average Pointwise Containment per Curve") +
  scale_color_manual(values = as.vector(
                                pb_color_vec[c("bubble_ci","kde", 
                                               "convex_hull", "delta_ball")])) +
  scale_fill_manual(values = as.vector(
    pb_color_vec[c("bubble_ci","kde", 
                   "convex_hull", "delta_ball")]))

ggsave(plot = pointwise_pb_assessment,
       file = paste0(image_path,"sim_pw_cb_boxplot.png"), device = "png",
       width = 10, height = 6.5, units = "in")


# visual for area distributions -----------------

latest_full_output_pipeline <- 'output_pipeline_alphalevel0.1_all.Rdata'

data_loc <- "main/data/"
a <- load(paste0(data_loc, latest_full_output_pipeline)) #output_list_pipeline
eval(parse(text = paste0("output_list_pipeline <- ",a)))


df_area <- data.frame(area = -99, tc = "ben", cb_type = "ben",
                      sim_type = "ben") %>%
              mutate(tc = as.character(tc),
                     cb_type = as.character(cb_type),
                     sim_type = as.character(sim_type))

for (tc in 1:length(output_list_pipeline)) {
  for (sim in names(output_list_pipeline[[tc]])) {
    for (cb in names(output_list_pipeline[[tc]][[sim]])[
          !(names(output_list_pipeline[[tc]][[sim]]) %in% 
                                          c("depth_vector","time"))]) {
      df_area <- rbind(df_area,
                       data.frame(
                        area = output_list_pipeline[[tc]][[sim]][[cb]]$area,
                        tc = tc, cb_type = cb, sim_type = sim))
    }
  }
}

df_area <- df_area[-1,]

df_area2 <- make_mat_cleaner(df_area)

df_area2 %>% ggplot() +
  geom_density(aes(x = area, color = forcats::fct_relevel(cb_type_graph,
                                        "Spherical",
                                        "Kernel Density Estimate",
                                        "Delta Ball",
                                        "Convex Hull"))) +
  facet_wrap( ~ sim_type_graph, labeller = label_wrap_gen(width = 25)) +
  scale_color_manual(values = as.vector(pb_color_vec[c("bubble_ci", "kde",
                                             "delta_ball", "convex_hull")])) +
  tc_theme + 
  theme(legend.position = "bottom") +
  labs(color = "Prediction Band Type",
       x = "Area of Prediction Band",
       y = "Density",
       title = "Distribution of Area of Prediction Bands") +
  guides(color =  guide_legend(override.aes =
                                list(size = 3)))

ggsave(paste0(image_path,"sim_area_density.png"), device = "png",
       width = 10, height = 6.5, units = "in")


# time processing visuals --------------


latest_full_output_pipeline <- 'output_pipeline_alphalevel0.1_all.Rdata'

data_loc <- "main/data/"
a <- load(paste0(data_loc, latest_full_output_pipeline)) #output_list_pipeline
eval(parse(text = paste0("output_list_pipeline <- ",a)))

df_time <- data.frame(depth = -1, data_deep = -1, 
                      dist = -1, inner_kde = -1,
                      inner_bubble = -1, inner_delta = -1, 
                      inner_convex = -1,
                      tc = "ben", sim_type = "ben") %>%
              mutate(tc = as.character(tc),
                     sim_type = as.character(sim_type))

for (tc in 1:length(output_list_pipeline)) {
  for (sim in names(output_list_pipeline[[tc]])) {
    time_out <- output_list_pipeline[[tc]][[sim]][["time"]]
    df_time <- rbind(df_time,
                     data.frame(
                      depth = time_out$depth,
                      data_deep = time_out$data_deep,
                      dist = time_out$dist,
                      inner_kde = time_out$kde,
                      inner_bubble = time_out$bubble,
                      inner_delta = time_out$delta,
                      inner_convex = time_out$convex,
                      tc = tc, sim_type = sim))
  }
}

df_time <- df_time[-1,] %>% mutate(kde = inner_kde,
                              bubble_ci = dist + depth + inner_bubble,
                              delta_ball = dist + depth + data_deep + 
                                            inner_delta,
                              convex_hull = dist + depth + data_deep + 
                                            inner_convex)

df_time2 <- df_time %>%
  reshape2::melt(id.vars = c("tc", "sim_type"),
                 measure.vars = c("kde", "bubble_ci",
                                  "delta_ball", "convex_hull")) %>%
  rename(cb_type = variable)

df_time3 <- df_time2 %>% make_mat_cleaner

df_time4 <- df_time3 %>% group_by(sim_type_table, cb_type_table) %>%
  summarize(time = paste0(sprintf("%.2f",round(mean(value),2)),
                          " \\(\\pm\\) ", 
                          sprintf("%.2f",round(sd(value),2)))) %>%
  reshape2::dcast(sim_type_table ~ cb_type_table) %>% 
  rename("Simulation Curve Type" = "sim_type_table")

### xtable -----------------

bold_somerows <- 
        function(x) gsub('BOLD(.*)',paste0('\\\\textbf{\\1','}'),x)
        # function used in print statement below - not really used

xtable_time <- df_time4 %>% xtable(
                align = c("rR{1.4in}L{1.1in}L{.95in}L{1in}L{.95in}"),
                 caption = paste("Average time (in seconds) to",
                                 "fit one Prediction Band with 350",
                                 "simulated curves, \\\\ \\(\\pm\\) 1 standard",
                                 "deviation."),
                 label = "tab:time_fitting")

addtorow <- list()
addtorow$pos <- list(4)
addtorow$command <- paste("\\hline \\multicolumn{5}{L{6in}}{\\footnotesize", 
                          "NOTE: Based on simulated curves created with", 
                          "either autoregressive (AR) or non-autoregressive", 
                          "(Non-AR) models for changes in bearing and", 
                          "speed and with either a kernel-based lysis", 
                          "model (Kernel) or a logistic-based lysis",
                          "model (Logistic).} \\\\")

print(xtable_time, 
      table.placement = "h!",
      include.rownames = FALSE,
      sanitize.text.function = identity, 
      caption.placement = "top",
      hline.after = c(-1, -1, 0),
      add.to.row = addtorow,
      file = paste0(table_path,"sim_time_fitting.tex"))

# time for prediction -------------

# note these times are associated with 100 fits

df_time_p <- data.frame(kde = -1,
                        bubble_ci = -1,
                        delta_ball = -1,
                        convex_hull = -1,
                        tc = "ben", sim_type = "ben") %>%
              mutate(tc = as.character(tc),
                     sim_type = as.character(sim_type))

for (tc in 1:length(simulation_validation_pipeline100)) {
  for (sim in names(simulation_validation_pipeline100[[tc]])) {
    time_out <- simulation_validation_pipeline100[[tc]][[sim]][["time"]]
    df_time_p <- rbind(df_time_p,
                     data.frame(
                      kde = time_out["kde"],
                      bubble_ci = time_out["bubble"],
                      delta_ball = time_out["delta"],
                      convex_hull = time_out["convex"],
                      tc = tc, sim_type = sim))
  }
}

df_time_p2 <- df_time_p[-1,] %>%
  reshape2::melt(id.vars = c("tc", "sim_type"),
                 measure.vars = c("kde", "bubble_ci",
                                  "delta_ball", "convex_hull")) %>%
  rename("cb_type" = "variable")

df_time_p3 <- df_time_p2 %>% make_mat_cleaner

### quick check for symmetry-ish --------------
# df_time_p3 %>% ggplot() +
#   geom_histogram(aes(x = value)) +
#   facet_grid(sim_type_graph ~ cb_type_graph)
# ---------------------------------------------

df_time_p4 <- df_time_p3 %>% group_by(sim_type_table, cb_type_table) %>%
  summarize(time = paste0(sprintf("%.2f",round(mean(value),2)),
                          " \\(\\pm\\) ", 
                          sprintf("%.2f",round(sd(value),2)))) %>%
            reshape2::dcast(sim_type_table ~ cb_type_table) %>%
            rename("Simulation Curve Type" = "sim_type_table")

### xtable -----------------

xtable_time_p <- df_time_p4 %>% xtable(
                 align = c("rR{1.4in}L{1.1in}L{.95in}L{.95in}L{.95in}"),
                 caption = paste("Average time (in seconds) to assess the",
                                 "containment of 100 curves inside a PB,",
                                 "\\\\ \\(\\pm\\) 1 standard deviation."),
                 label = "tab:time_prediction100")


print(xtable_time_p, 
      table.placement = "h!",
      include.rownames = FALSE,
      sanitize.text.function = identity, 
      caption.placement = "top",
      hline.after = c(-1, -1, 0, 4),
      file = paste0(table_path,"sim_time_prediction100.tex"))


#####################
# Compressed Tables #
#####################

# summary table for time

create_pb <- df_time3 %>% group_by(cb_type_table) %>%
  summarize(create_pb_mean = round(mean(value),2)) %>% 
  data.frame %>% t

evaluate_coverage <- df_time_p3 %>% group_by(cb_type_table) %>%
  summarize(evaluate_coverage = round(mean(value),2)) %>% 
  data.frame %>% t



df_table_compressed <- rbind(create_pb[2,],
                             evaluate_coverage[2,]) %>%
  data.frame %>% mutate(`Average Time to ...` = c("Create PB (sec)", "Evaluate Coverage (sec)")) %>%
  dplyr::select(`Average Time to ...`, X1, X2, X3, X4)

names(df_table_compressed)[-1] <- create_pb[1,]

df_table_compressed <- df_table_compressed[,c(1,3,2,5,4)]

xtable_compress <- df_table_compressed %>% xtable(
  align = c("rR{2in}L{.75in}L{1.1in}L{.9in}L{.75in}"),
  caption = paste("Average time (in seconds) to create a PB out of 350",
                  "simulated curves or evaluate the coverage of",
                  "100 simulated curves."),
  label = "tab:comp_time_summary")


print(xtable_compress, 
      table.placement = "h!",
      include.rownames = FALSE,
      hline.after = c(-1, -1, 0, 2),
      sanitize.text.function = identity, 
      caption.placement = "top",
      file = paste0(table_path,"sim_time_compressed.tex"))



# summary table for accuracy and precision

all_data_pointwise_table <- all_data_three %>%
  group_by(tc_name, sim_type_table, cb_type_table, num_curves) %>%
  summarize(num_captured_points = sum(round(length*prop)),
            total_num_points = sum(length)) %>%
  mutate(prop_captured = num_captured_points/total_num_points)

all_data_uniform_table <- all_data_three %>%
  group_by(tc_name, sim_type_table, cb_type_table, num_curves) %>%
  summarize(num_captured_curves = sum(prop == 1)) %>%
  mutate(prop_captured_curves = num_captured_curves/num_curves)

table_acc_perc_pw <- all_data_pointwise_table %>% 
  filter(num_curves == 100) %>% # just the set of 100 averaged
  group_by(cb_type_table) %>%
  dplyr::summarize(
    pointwise_median = round(median(prop_captured),2))

table_acc_perc_un <- all_data_uniform_table %>% 
  filter(num_curves == 100) %>% # just the set of 100 averaged
  group_by(cb_type_table) %>%
  dplyr::summarize(
    uniform_median = round(median(prop_captured_curves),2))

area_mat <- data.frame(tc_name = "ben", 
                       sim_type = "ben",
                       cb_type = "ben",
                       area = -1) %>%
  mutate(tc_name = as.character(tc_name),
         sim_type = as.character(sim_type),
         cb_type = as.character(cb_type))

for (tc in names(output_list_pipeline)) {
  specific_tc_info <- output_list_pipeline[[tc]]
  
  for (type in names(specific_tc_info)) {
    
    for (pb_type in c("kde","bubble_ci", "delta_ball", "convex_hull")){
      area_val <- specific_tc_info[[type]][[pb_type]][["area"]]
      area_mat <- rbind(area_mat,
                        data.frame(tc_name = tc,
                                   sim_type = type,
                                   cb_type = pb_type,
                                   area = area_val))
      
    }
  }
}

area_mat <- area_mat[-1,]
area_mat <- make_mat_cleaner(area_mat)


table_acc_perc_area <- area_mat %>% 
  group_by(cb_type_table) %>%
  dplyr::summarize(
    area_median = format(round(median(area * 60 * 50),-2),
                         big.mark=",",
                         scientific=FALSE) )

# using https://www.nhc.noaa.gov/gccalc.shtml
# and checking the distance between 31N 37W and 32N 37W and 32N 37W and 32N 38W
# to get 60 * 50 nm^2 for 1 lat/long square (around the area we are looking at)


table_final_ap <- table_acc_perc_pw %>% 
  left_join(table_acc_perc_un,
            by = c("cb_type_table" = "cb_type_table")) %>%
  left_join(table_acc_perc_area,
            by = c("cb_type_table" = "cb_type_table")) %>% t

table_final_ap_colnames <- table_final_ap[1,]
table_final_ap_rownames <- rownames(table_final_ap)

table_final_ap <- table_final_ap[-1,]
colnames(table_final_ap) <- table_final_ap_colnames
table_final_ap <- cbind(table_final_ap_rownames[-1], table_final_ap)
colnames(table_final_ap)[1] <- table_final_ap_rownames[1]
rownames(table_final_ap) = NULL
table_final_ap[,1] <- c("Median \\textit{Pointwise} Accuracy",
                "Median \\textit{Uniform} Accuracy",
                "Median Area ($nm^2$)")
table_final_ap <- table_final_ap %>% data.frame() %>% rename(" " = "cb_type_table",
                                             "Kernel Density Estimate"= "Kernel.Density.Estimate",
                                             "Delta Ball" = "Delta.Ball",
                                             "Convex Hull" = "Convex.Hull")

table_final_ap <- table_final_ap[,c(1,3,2,5,4)]


xtable_ap <- table_final_ap %>% 
  xtable(align = c("rR{2in}L{.75in}L{1.1in}L{.9in}L{.75in}"),
         digits = 2,
         caption = paste("Accuracy and precision of PBs created from 309",
                         "different starting points, with PBs analyzed across",
                         "all 4 types of simulated curves. Median pointwise",
                         "and uniform accuracy as defined in Appendix",
                         "\\ref{sec:validity_and_effiency} obtained for",
                         "each PB attempting to capture 100 newly simulated",
                         "curves from the same distribution that defined",
                         "the PB. Median area recorded in",
                         "square nautical miles."),
         label = "tab:median_prop_captured_and_size_summary")

print(xtable_ap, 
      table.placement = "h!",
      include.rownames = FALSE,
      hline.after = c(-1, -1, 0, 3),
      caption.placement = "top",
      sanitize.text.function = bold_somerows, 
      #^for some reason we need this - even though not used
      file = paste0(table_path,"sim_accuracy_compressed.tex"))






