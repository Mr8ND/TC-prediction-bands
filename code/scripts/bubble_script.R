#############
# Libraries #
#############


library(datamart)
library(geosphere)


#############
# Locations #
#############

project_location      = ""
functions_loc         = "code/functions/"
generate_loc          = "data/generate/"
train_f_loc           = "data/training/train/"

sub_directory_no_auto = paste0(generate_loc,"Validation_Sims_No_Auto/")
sub_directory_auto    = paste0(generate_loc,"Validation_Sims_Auto/")

desired_functions = c("Path_functions.R","13pointreduction.R",
                      "projection_map.R","visualizing_color_gradient.R","bubble_points_functions2.R")

#####################
# Loading functions #
#####################

for(f_name in desired_functions){
  source(paste0(functions_loc,f_name))
}

# Ben's way to load data in:
source(paste0(functions_loc,"load_training_first.R"))


# bubble_points_scripts (don't save)


##########################
# Loading simulated data #
##########################

amount = 1:50

no_auto = load_sims(project_location,sub_directory_no_auto,group = amount)
auto = load_sims(project_location,sub_directory_auto,group = amount)

load(paste0(generate_loc,"estimate_p_first25_28nov_auto.Rdata"))


validation_data=load_validate_data_list_names("",names =T)
source(paste0(functions_loc,"load_training_first.R"))


files_no_auto = list.files(paste0(project_location,sub_directory_no_auto))
file_names = unlist(strsplit(x = files_no_auto,split = "_non_ar_sims"))

validation_data=load_validate_data_list_names("",names =T)


list_valid_subset = list()
for(name in seq_along(file_names)){
  list_valid_subset[[name]] = validation_data$list[[which(validation_data$names == paste0(file_names[name],".txt"))]]
}
list_valid_subset13 = thirteen_points_listable(list_df = list_valid_subset,lonlat = FALSE)








selected_ones = 8
dflist_toy_bubble = auto[[selected_ones]]
probability_vec = estimate_p_auto[[selected_ones]]$p_estimate_test/sum(estimate_p_auto[[selected_ones]]$p_estimate_test)

bubble_steps_CI <- bubbleCI(dflist_toy_bubble, probability_vec)
list_path = dflist_toy_bubble
prob_vec_path = probability_vec









NSWE.lists = calculateErrorBandsBubble(bubble_steps_CI, conversion=TRUE)
error.NS = NSWE.lists[[1]]
error.EW = NSWE.lists[[2]]
center.radius = NSWE.lists[[3]]

lower_bound_errors = c()
upper_bound_errors = c()
for (i in c(1:length(error.EW))){
  lower_bound_errors = c(lower_bound_errors,error.EW[[i]][c(1,2)])
  upper_bound_errors = c(upper_bound_errors,error.EW[[i]][c(3,4)])
}
lower_bound_errors.df = data.frame(t(matrix(lower_bound_errors, nrow=2)))
upper_bound_errors.df= data.frame(t(matrix(upper_bound_errors, nrow=2)))

print(lower_bound_errors.df)
print(upper_bound_errors.df)


#quartz(width = 8,height = 6.5)
newmap = getMap(resolution = "low")
ylim = c(9,40)
xlim =c(-110,2)
plot(newmap, ylim = ylim, xlim = xlim, asp = 1)

for(i in 1:length(dflist_toy_bubble)){
  lines(dflist_toy_bubble[[i]][,1],dflist_toy_bubble[[i]][,2],col="grey")
}

center.radius.df = c()
for (i in c(1:length(center.radius))){
  center.radius.df = c(center.radius.df, center.radius[[i]][c(1,2)])
}
center.radius.df = data.frame(t(matrix(center.radius.df, nrow=2)))
#points(center.radius.df[,1], center.radius.df[,2], col="black")


lines(lower_bound_errors.df[,1], lower_bound_errors.df[,2], col="black",lwd = 2)
lines(upper_bound_errors.df[,1], upper_bound_errors.df[,2], col="black",lwd = 2)

lines(list_valid_subset13[[selected_ones]][,1], list_valid_subset13[[selected_ones]][,2],col="red",lwd = 4)

#lines(lowerbound.df[,1], lowerbound.df[,2], col="blue")
#lines(upperbound.df[,1], upperbound.df[,2], col="blue")







####################
# Taking into account the ending point


max_prob_index= which.max(prob_vec_path)
num_steps=nrow(dflist_toy_bubble[[max_prob_index]])




quartz(width = 8,height = 6.5)
newmap = getMap(resolution = "low")
ylim = c(9,40)
xlim =c(-110,2)
plot(newmap, ylim = ylim, xlim = xlim, asp = 1)

for(i in 1:length(dflist_toy_bubble)){
  lines(dflist_toy_bubble[[i]][,1],dflist_toy_bubble[[i]][,2],col="grey")
}

center.radius.df = c()
for (i in c(1:length(center.radius))){
  center.radius.df = c(center.radius.df, center.radius[[i]][c(1,2)])
}
center.radius.df = data.frame(t(matrix(center.radius.df, nrow=2)))
points(center.radius.df[1:num_steps,1], center.radius.df[1:num_steps,2], col="black")
points(center.radius.df[(num_steps+1):nrow(center.radius.df),1], center.radius.df[(num_steps+1):nrow(center.radius.df),2], col="pink",pch=16)


lines(lower_bound_errors.df[1:num_steps,1], lower_bound_errors.df[1:num_steps,2], col="black",lwd = 2)
lines(upper_bound_errors.df[1:num_steps,1], upper_bound_errors.df[1:num_steps,2], col="black",lwd = 2)
lines(x =c(upper_bound_errors.df[num_steps,1],lower_bound_errors.df[num_steps,1] ),
      y =c(upper_bound_errors.df[num_steps,2],lower_bound_errors.df[num_steps,2]) ,col="black",lwd=2,lty = "dashed")

lines(list_valid_subset13[[selected_ones]][,1], list_valid_subset13[[selected_ones]][,2],col="red",lwd = 4)





##### 
# Ben's Orthogonal Exploration
####

source("code/functions/Path_functions.R")

desired_path = center.radius.df
out = distAlongP(data.df = desired_path)
bearing = out[[2]]

bearing_orthog = bearing + 90


positive = matrix(0,nrow =length(center.radius),ncol =2 )
negative = matrix(0,nrow =length(center.radius),ncol =2 )


for(i in seq_along(center.radius)){
  radius = center.radius[[i]][3]
  direction = bearing_orthog[i]
  positive[i,] = destPoint(center.radius.df[i,],direction,radius)#uconv(radius, "nautical mile","m", "Length"))
  negative[i,] = destPoint(center.radius.df[i,],direction,-radius)#uconv(radius, "nautical mile","m", "Length"))
  
}

positive= positive[-nrow(positive),]
negative= negative[-nrow(negative),]



max_prob_index= which.max(prob_vec_path)
num_steps=nrow(dflist_toy_bubble[[max_prob_index]])




quartz(width = 8,height = 6.5)
newmap = getMap(resolution = "low")
ylim = c(9,40)
xlim =c(-110,2)
plot(newmap, ylim = ylim, xlim = xlim, asp = 1,main ="Confidence Band, on Auto-Regression Curves")

for(i in 1:length(dflist_toy_bubble)){
  lines(dflist_toy_bubble[[i]][,1],dflist_toy_bubble[[i]][,2],col="grey")
}

center.radius.df = c()
for (i in c(1:length(center.radius))){
  center.radius.df = c(center.radius.df, center.radius[[i]][c(1,2)])
}
center.radius.df = data.frame(t(matrix(center.radius.df, nrow=2)))
points(center.radius.df[1:(num_steps-1),1], center.radius.df[1:(num_steps-1),2], col="pink",pch=16)
points(center.radius.df[1:(num_steps-1),1], center.radius.df[1:(num_steps-1),2], col="black",pch=1)

#points(center.radius.df[(num_steps+1):nrow(center.radius.df),1], center.radius.df[(num_steps+1):nrow(center.radius.df),2], col="pink",pch=16)


lines(positive[1:(num_steps-1),1],positive[1:(num_steps-1),2],col="pink",lwd=3)
lines(negative[1:(num_steps-1),1],negative[1:(num_steps-1),2],col="pink",lwd=3)

for( i in 1:(num_steps-1)){
  lines(c(positive[i,1],center.radius.df[i,1] ),
        c(positive[i,2],center.radius.df[i,2]),col="pink",lty = "dashed")
  lines(c(negative[i,1],center.radius.df[i,1] ),
        c(negative[i,2],center.radius.df[i,2]),col="pink",lty = "dashed")
}

lines(list_valid_subset13[[selected_ones]][,1], list_valid_subset13[[selected_ones]][,2],col="red",lwd = 3)

legend("bottomleft", lwd = c(3,3,1,1), lty = c("solid","solid","dashed","solid"), border = F,bty = "n",
       col =c("red","pink","pink","grey"),legend = c("True Curve","Confidence Band","Confidence Radius","Generated Curves"),
       text.width = .5,cex = .9)



quartz.save(paste0("images/ci_paper.pdf"))
#points(list_valid_subset13[[selected_ones]][,1], list_valid_subset13[[selected_ones]][,2],col="black",pch = 16,cex = .5)

##########
##########
##########
# NIC - Here's were you need to drop the vector:
##########
##########
##########

#counts = c(rep(2:13,each=4),rep(13,10))

# 

quartz(width = 11.7,height = 6)
par(mfrow=c(1,2))

counts_auto = counts
counts_non_auto = counts


a = hist(counts_auto,breaks=0:13,plot=F)

b=barplot(a$counts,main = "Auto Regressive with Weights ",xlab ="Number of Points in the Confidence Interval" )
axis(side = 1,at = b,label = 1:13,)

a = hist(counts_no_auto,breaks=1:13,plot=F)

b=barplot(a$counts,main = "Non-Auto Regressive with Weights ",xlab ="Number of Points in the Confidence Interval" )
axis(side = 1,at = b,label = 2:13,)

quartz.save(paste0("images/histograms.pdf"))




