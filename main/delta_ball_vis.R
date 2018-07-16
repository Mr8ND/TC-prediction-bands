# creation of example of what delta ball does visualzation
library(tidyverse)
library(TCcrediblebands)
library(clusterSim)
library(sp)
library(gridExtra)
library(ggforce)

# data generation -----------------

set.seed(1991)
data_moon <- shapes.worms(500)
data_raw <- data_moon[[1]][data_moon[[2]] == 2,] %>% data.frame

data_scaled <- data_raw %>% scale()
data_raw <- data_scaled %>% data.frame()


# raw data visual ----------------------
a <- ggplot(data_raw, aes(x = X1, y = X2)) +
	geom_point(size = .1) 


# delta processing and creation of sp object -------------
X1 <- X2 <- id <- idx <- idx_tri <- n <- NULL
data <- data_raw[,1:2]
sp::coordinates(data) <- names(data_raw)[1:2]

d <- get_delta(data_raw, dist_mat = NULL)

delta <- d$mm_delta/2

# visual of delta balls -----------------
b <- ggplot(data_raw, aes(x0 = X1, y0 = X2, r = delta)) + 
	geom_circle(fill = "black", color = "white")


# Delaunay triangulation creation --------------
dtri_data_edges <- rgeos::gDelaunayTriangulation(data, onlyEdges = T, tolerance = 0)

all_lines <- dtri_data_edges@lines[[1]]@Lines 
all_lines_df <- data.frame(x = -1, y = -1, idx = -1)

for (idx in 1:length(all_lines)) {
	dat <- all_lines[[idx]]@coords %>% data.frame %>% mutate(idx = idx) 
	all_lines_df <- rbind(all_lines_df, dat)
}
all_lines_df <- all_lines_df[-1,]

# visualization of Delaunay triangulation -------------
c <- ggplot(all_lines_df, aes(x = x, y = y, group = idx)) +
	geom_path()


# removal of lines outside delta ball covering ------------
lines_info <- get_lines(dtri_data_edges, data_raw, delta, n_steps = 100)

desired_lines <- lines_info$lines_mat

desired_lines_plot <- desired_lines

# visualization of lines inside delta ball covering --------------
d <- ggplot(desired_lines_plot) + 
	geom_path(aes(x = x, y = y, group = idx))

# removal of lines in the interior -----------
keep <- desired_lines %>% apply(MARGIN = 1, 
                              function(row) sum(is.na(row)) == 0) 
desired_lines <- desired_lines[keep,]

removed_mat <- lines_info$removed_mat

# string representation of nodes and edges 
nodes <- paste0("(",desired_lines$x, ",", desired_lines$y, ")")
edge_mat <- matrix(c(nodes[seq(from = 1,to = length(nodes),by = 2)],
                   nodes[seq(from = 2,to = length(nodes),by = 2)]),
                 ncol = 2) %>% 
data.frame() %>% 
dplyr::mutate(X1 = as.character(X1),
       X2 = as.character(X2),
       id = desired_lines$idx[seq(from = 1,to = length(nodes),by = 2)])

# get DT triangles
dtri_data_tri <- rgeos::gDelaunayTriangulation(data,tolerance = 0)
tri_matrix <- get_tri_matrix(dtri_data_tri)

tuples_of_tri <- data.frame(rbind(tri_matrix[,c(1,2)],
                                tri_matrix[,c(1,3)],
                                tri_matrix[,c(2,3)],
                                # both directions
                                tri_matrix[,c(2,1)],
                                tri_matrix[,c(3,1)],
                                tri_matrix[,c(3,2)]),
                          stringsAsFactors = F 
) %>%
dplyr::mutate(idx_tri = rep(1:nrow(tri_matrix),times = 6))


# hidden function in TCcrediblebands -------------
remove_lines_from_tri <- function(tuples_of_tri, removed_mat){

  # Hack to make R CMD check not fail
  x <- y <- first <- second <- idx <- NULL

  # removes triangles for tuples data frame that have an edge removed 
  removed_mat <- removed_mat[apply(removed_mat, 1, 
                                   function(row) sum(is.na(row)) == 0), ]
  
  tuples_of_tri$combo <- apply(tuples_of_tri,1, 
                               function(row) paste0(row[1],"~",row[2]))
  
  removed_values_dat <- removed_mat %>%
    dplyr::group_by(idx) %>% dplyr::summarize(first = paste0("(",x[1],",", y[1],")"),
                                second = paste0("(",x[2],",", y[2],")"),
                                combo = paste0(first,"~",second),
                                combo2 = paste0(second,"~",first))
  
  removed_values_single <- c(removed_values_dat$combo, 
                             removed_values_dat$combo2)
  
  remove_tri <- tuples_of_tri$idx_tri[(
    tuples_of_tri$combo %in% removed_values_single
  )]
  
  out_tuples <- tuples_of_tri[!(tuples_of_tri$idx_tri %in% remove_tri),]
  
  return(out_tuples)
}

tuples_of_tri <- remove_lines_from_tri(tuples_of_tri = tuples_of_tri,
                                     removed_mat = removed_mat)

num_tri <- edge_mat %>% dplyr::left_join(tuples_of_tri,
                                by = c("X1" = "X1", "X2" = "X2"))  %>%
dplyr::group_by(id) %>% dplyr::summarise(idx_tri = paste0(idx_tri,collapse = ","),
                           X1 = unique(X1),
                           X2 = unique(X2),
                           count = n())

index_mapping <- data.frame(dl = sort(unique(desired_lines$idx)),
                          nt = sort(unique(num_tri$id)))

select_lines <- (num_tri[num_tri$count == 1, c("id")] %>% 
                 dplyr::left_join(index_mapping, by = c("id" = "nt")))$dl

output_lines <- desired_lines %>% dplyr::filter(idx %in% select_lines)

# visualizing edge lines ---------------
e <- output_lines %>% ggplot(aes(x = x, y = y, group = idx)) +
	geom_path()


# final graphics ---------------------
theme_set(theme_minimal() + theme(plot.title = element_text(hjust = 0.5)))
arrange_vis <- arrangeGrob(
			 a + labs(title = "Raw data\n",
					  x = "x", y = "y"),
			 b + labs(title = "Delta Ball around each point\n",
					  x = "x", y = "y"),
			 c + labs(title = "Delaunay Triangulation\n",
					  x = "x", y = "y"),
			 d + labs(title = "Removal of Lines not covered\nby Delta Ball",
					  x = "x", y = "y"),
			 e + labs(title = "Removal of Inner Lines\n",
					  x = "x", y = "y"),nrow = 2)

image_path <- "report/images/"
ggsave(plot = arrange_vis,
	   filename = paste0(image_path,"delta_ball_vis.pdf"),
	   device = "pdf", width = 10, height = 6.5, units = "in")
