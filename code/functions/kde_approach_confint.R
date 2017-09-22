require(datamart)
require(geosphere)
require(plyr)
require(rworldmap)
require(caret)
require(ks)
require(gtools)


#############
# Locations #
#############

project_location      = ""
functions_loc         = "code/functions/"
weights_loc           = "data/"
true_curve_loc        = "data/training/validate/"
sim_curve_loc         = "data/"

findFilesInFolder = function(subfolder, name.pattern, path.out=FALSE){
  temp = list.files(path=subfolder, pattern=name.pattern)
  if (path.out==TRUE){
    for (i in c(1:length(temp))){
      temp[i] = paste0(subfolder,temp[i])
    }
  }
  return(temp)
}

weight.files = mixedsort(findFilesInFolder(weights_loc, "estimate_p", path.out=TRUE))
sim.curve.folders = findFilesInFolder(sim_curve_loc, "Val_Sims", path.out=TRUE)
tc.code.list = findFilesInFolder(sim.curve.folders[1], "AL")

# Limit the tc.code.list to the first 100
limit.tc = 114
tc.code.list = tc.code.list[c(1:limit.tc)]

# True Curve file list
true.curve.file.vec = rep_len("", length.out=limit.tc)
for (j in c(1:limit.tc)){
  true.curve.file.vec[j] = paste0(true_curve_loc, findFilesInFolder(true_curve_loc, tc.code.list[j]))
}

# Creating a list with all the location files according to the sim.curve.folders.
# Every list is going to have a list in which to look for the simulated TC of the TC with that
# specific code.

sim.curve.folders.list = list()
for (j in c(1:4)){
  path.vec = rep_len("", length.out=length(tc.code.list))
  for (i in c(1:length(tc.code.list))){
    path.vec[i] = findFilesInFolder(paste0(sim.curve.folders[j],'/'), tc.code.list[i] , path.out=TRUE)
    path.vec[i] = paste0(path.vec[i], '/')
  }
  sim.curve.folders.list[[j]] = path.vec
}
names(sim.curve.folders.list) = c('auto_d', 'auto_nd', 'no_auto_d', 'no_auto_nd')


#######################

idx_chosen = 18
curve.type = 'auto_d'
sim.curve.folders = sim.curve.folders.list[[curve.type]]
temp = findFilesInFolder(sim.curve.folders[idx_chosen], "_sim_", path.out=TRUE)
dflist = lapply(temp, function(x) data.frame(read.table(x, header=TRUE, sep=",")))


quartz(width = 8,height = 6.5)
newmap = getMap(resolution = "low")
xlim = c(-105,0)
ylim = c(16, 36)
plot(newmap, ylim = ylim, xlim = xlim, asp = 1)

plot(kde.obj, cont = c(50,75,90,95,99), add=TRUE, col.pt="blue", display="filled.contour2")

for (i in c(1:length(dflist))){
  lines(dflist[[i]][,1], dflist[[i]][,2], col='black')
}


##########################################
### KDE APPROACH
###########################################

flattenTCListWeight = function(dflist, weight.vec){
  # Dflist in this case is a list with n different hurricanes. Usually we have n=100.
  # Each element of such list a dataframe with at least longitude and latitude.
  # This function append by row all dataframes in the list and adds a column, which has the same
  # value for each TC, indicating the weight assigned to such TC.
  # The output is a dataframe will all the dataframes appended on the list and the a column
  # with the weights of the corresponding TC appended.
  
  # Initializing the first step of the process, by creating a vector with the weight of the
  # first TC and appending it to the dataframe in the first position of the list.
  weight.spec.mat = rep(weight.vec[1], nrow(dflist[[1]]))
  dfmat = cbind(dflist[[1]], weight.spec.mat)
  
  # Iterating the same process for all the dataframe in the list
  for (i in c(2:length(dflist))){
    weight.spec.mat = rep(weight.vec[i], nrow(dflist[[i]]))
    dfmat = rbind(dfmat, cbind(dflist[[i]], weight.spec.mat))
  }
  
  return(dfmat)
}

fitKDEObject = function(dfmat, h.band=NULL, long=1, lat=2, weight=3, grid.size=1000){
  
  # This function simply the KDE object given a matrix which has the dataframes TC for training
  # appended to it. By default it considers longitude as first column, latitude as second and
  # weight as third. The default grid size for fitting the KDE is 1000 - it can be reduced
  # to speed up the computation times.
  
  if (!is.null(h.band)){
    h.mat = diag(2)*h.band
    kde.obj = kde(dfmat[,c(long,lat)], w=dfmat[,weight], gridsize=c(grid.size), H=h.mat)
  } else {
    kde.obj = kde(dfmat[,c(long,lat)], w=dfmat[,weight], gridsize=c(grid.size))
  }
  return(kde.obj)
}

predictKDEObject = function(kde.obj, predict.mat, alpha.level=NULL, long=1, lat=2){
  
  # This function, given a KDE object and a matrix to be predicted - ideally a test matrix
  # predicts the density value for the values that needed to be predicted.
  # The values are appended to the matrix and returned.
  # If an alpha.level is entered, then an extra column will be added, in which 1 means that
  # the value is above the alpha.level contour - i.e. within that probability contour - else 0
  # is returned.
  
  # The prediction mat is formatted and the prediction is performed
  predict.mat.kdefit = predict.mat[,c(long,lat)]
  predict.vec = predict(kde.obj, x = predict.mat.kdefit, zero.flag = TRUE)
  out.mat = cbind(predict.mat, predict.vec)
  
  # If the alpha level is selected, then the function will select the right level from the
  # kde.obj$cont vector and store it for comparison.
  if (!is.null(alpha.level)){
    contour.alpha.level = as.numeric(kde.obj$cont[paste(as.character((1-alpha.level)*100), "%", sep="")])
    in.alpha.vec = as.numeric(predict.vec>=contour.alpha.level)
    out.mat = cbind(out.mat, in.alpha.vec)
  }
  
  return(out.mat)
}

evaluatePredictedMatrix = function(predicted.mat, weight=3, in.alpha.level=5){
  # This function evaluates the matrix with prediction and whether the value is in the
  # alpha level or not and reports a weighted average of the points that were not in
  # the countour level using their weights.
  
  sel.vec = predicted.mat[, in.alpha.level]==0
  calc.mat = predicted.mat[sel.vec,weight]
  return(sum(calc.mat)/sum(predicted.mat[,weight]))
}

kcvValidationSingleTC = function(dflist, weight.vec, alpha.levels, bandwith.levels, k=5, seed=7){
  
  # This function provides k fold cross validation given a list of n hurricanes, usually 100.
  # It divides them in different folds, performs division, kde fitting, prediction and
  # metric evaluation, which are output as a list.
  
  # Initializing the values and creates the fold list.
  set.seed(seed)
  fold.list = createFolds(dflist, k = 5, list = TRUE, returnTrain = FALSE)
  k.vec = c(1:k)
  results.list = list()
  
  for (k.value in k.vec){
    
    # Initializes the matrix for results
    result.mat = matrix(, nrow = length(bandwith.levels), ncol = length(bandwith.levels))
    
    # Creates the training matrix
    train.idx.vec = as.numeric(unlist(fold.list[k.vec[-k.value]]))
    train.list = dflist[train.idx.vec]
    train.weight.vec = weight.vec[train.idx.vec]
    train.mat = flattenTCListWeight(train.list, train.weight.vec)
    
    # Creates the test matrix
    test.idx.vec = as.numeric(unlist(fold.list[k.vec[k.value]]))
    test.list = dflist[test.idx.vec]
    test.weight.vec =  weight.vec[test.idx.vec]
    test.mat = flattenTCListWeight(test.list, test.weight.vec)
    
    # Create the KDE objects - one for bandwith
    kde.obj.list = lapply(bandwith.levels, function(x) fitKDEObject(dfmat = train.mat, h.band = x))
    
    # For each of the KDE objects, it populates one row of the result matrix by evaluating
    # the metric function for all the alpha levels considered
    for (i in c(1:length(kde.obj.list))){
      result.mat[i,] = sapply(alpha.levels, function(x) evaluatePredictedMatrix(predictKDEObject(kde.obj.list[[i]], test.mat, alpha.level = x)))
    }
    results.list[[k.value]] = result.mat
  }
  
  final.result.mat = Reduce("+", results.list) / length(results.list)
  return(final.result.mat)
}

percDecrFormatting = function(result.mat){
  perc.final.result.mat = matrix(, nrow=nrow(result.mat), ncol=ncol(result.mat)-1)
  for (j in c(1:ncol(perc.final.result.mat))){
    perc.final.result.mat[,j] = 100*(result.mat[,j+1]-result.mat[,j])/result.mat[,j]
  }
  return(perc.final.result.mat)
}

absDecrFormatting = function(result.mat){
  perc.final.result.mat = matrix(, nrow=nrow(result.mat), ncol=ncol(result.mat)-1)
  for (j in c(1:ncol(perc.final.result.mat))){
    perc.final.result.mat[,j] = (result.mat[,j+1]-result.mat[,j])
  }
  return(perc.final.result.mat)
}




# Loading in the true curve. We automatic assume that longitude is in the 6th curve for true
# curves and latitude is in 5
true.curve = data.frame(read.table(true.curve.file.vec[idx_chosen], header=FALSE, sep=" "))[,c(6,5)]
names(true.curve) = c("long", "lat")

# Loading in the right weight and normalizing them
idx.weights = (idx_chosen-1) %/% 25
load(weight.files[idx.weights+1])
tc.weight.vec = estimate_p[[curve.type]][[idx_chosen]]$p_estimate_test
tc.weight.vec = tc.weight.vec/sum(tc.weight.vec)



#probability.vec = seq(0.001, 0.1, length.out = length(dflist))
#probability.vec = estimate_p$auto_d[[1]]$p_estimate_test
#probability.vec = probability.vec/sum(probability.vec)

#dfmat = flattenTCListWeight(dflist, probability.vec)
#kde.obj = fitKDEObject(dfmat)
#predict.mat = predictKDEObject(kde.obj, dfmat[c(1:100),], alpha.level = .9)
#metric.value = evaluatePredictedMatrix(predict.mat)

#### CROSS-VALIDATION

bandwith.levels = c(0.01, 0.03, 0.05, 0.07, 0.09)
alpha.levels = c(0.75, 0.80, 0.85, 0.90, 0.95)

ptm <- proc.time()
final.result.mat = kcvValidationSingleTC(dflist = dflist, weight.vec = tc.weight.vec, 
                                        alpha.levels = alpha.levels, bandwith.levels = bandwith.levels)
proc.time() - ptm

abs.result.mat = absDecrFormatting(final.result.mat)
which(abs.result.mat==min(abs.result.mat), arr.ind=TRUE)
abs.result.mat
final.result.mat

#true.curve = data.frame(read.table("data/training/validate/AL011969.txt", header=FALSE, sep=" "))[,c(6,5)]
#names(true.curve) = c("long", "lat")

predictKDEObject(kde.obj, true.curve, alpha.level = .9)


####################
## VISUALIZATION
####################

ptm <- proc.time()
dfmat = flattenTCListWeight(dflist, tc.weight.vec)
kde.obj = fitKDEObject(dfmat)

quartz(width = 8,height = 6.5)
newmap = getMap(resolution = "low")
xlim = c(-105,0)
ylim = c(16, 36)
plot(newmap, ylim = ylim, xlim = xlim, asp = 1)

for (i in c(1:length(dflist))){
  lines(dflist[[i]][,1], dflist[[i]][,2], col=rgb(0,0,0,0.25), lwd=0.2)
}
plot(kde.obj, cont = 85, add=TRUE, col='pink', drawlabels=FALSE, lwd=4)
lines(true.curve[,1], true.curve[,2], col='red', lwd=4)
proc.time() - ptm


############################
## VISUALIZATION PIPELINE
############################


kdeListFunction <- function(curve.type, sim.curve.folders.list, true.curve.file.vec, weight.files, length.hurricane=114){
  
  print(paste0('Working on ', curve.type,' Curves.'))
  curve.type.index = which(c('auto_d', 'auto_nd', 'no_auto_d', 'no_auto_nd')==curve.type)
  sim.curve.folders = sim.curve.folders.list[[curve.type.index]]
  kde.list = list()
  
  for (index_chosen in c(1:length.hurricane)){
    true.curve = data.frame(read.table(true.curve.file.vec[index_chosen], header=FALSE, sep=" "))[,c(6,5)]
    names(true.curve) = c("long", "lat")
    
    temp = findFilesInFolder(sim.curve.folders[index_chosen], "_sim_", path.out=TRUE)
    dflist = lapply(temp, function(x) data.frame(read.table(x, header=TRUE, sep=",")))
    
    idx.weights = (index_chosen-1) %/% 25
    load(weight.files[idx.weights+1])
    tc.weight.vec = estimate_p[[curve.type]][[index_chosen]]$p_estimate_test
    tc.weight.vec = tc.weight.vec/sum(tc.weight.vec)
    dfmat = flattenTCListWeight(dflist, tc.weight.vec)
    kde.obj = fitKDEObject(dfmat)
    
    kde.list[[index_chosen]] = kde.obj
  }
  print(paste0('kde_obj_list', curve.type, '.Rdata'))
  save(kde.list, file = paste0('kde_obj_list', curve.type, '.Rdata'))
}

## Run everything in location before running this part

for (ct in c('auto_d', 'auto_nd', 'no_auto_d', 'no_auto_nd')){
  kdeListFunction(curve.type = ct, 
                  sim.curve.folders.list = sim.curve.folders.list, 
                  true.curve.file.vec = true.curve.file.vec,
                  weight.files = weight.files)
}


