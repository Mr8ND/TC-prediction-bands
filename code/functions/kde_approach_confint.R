require(datamart)
require(geosphere)
require(plyr)
require(rworldmap)
require(caret)
require(ks)

# Set the right working directory before running the code
temp <- list.files(pattern="")
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







#probability.vec = seq(0.001, 0.1, length.out = length(dflist))
probability.vec = estimate_p$auto_d[[1]]$p_estimate_test
probability.vec = probability.vec/sum(probability.vec)

dfmat = flattenTCListWeight(dflist, probability.vec)
kde.obj = fitKDEObject(dfmat)
predict.mat = predictKDEObject(kde.obj, dfmat[c(1:100),], alpha.level = .9)
metric.value = evaluatePredictedMatrix(predict.mat)



bandwith.levels = c(0.01, 0.03, 0.05, 0.07, 0.09)
alpha.levels = c(0.75, 0.80, 0.85, 0.90, 0.95)

ptm <- proc.time()
final.result.mat = kcvValidationSingleTC(dflist = dflist, weight.vec = probability.vec, 
                                        alpha.levels = alpha.levels, bandwith.levels = bandwith.levels)
proc.time() - ptm

abs.result.mat = absDecrFormatting(final.result.mat)
which(abs.result.mat==min(abs.result.mat), arr.ind=TRUE)
abs.result.mat
final.result.mat

true.curve = data.frame(read.table("data/training/validate/AL011969.txt", header=FALSE, sep=" "))[,c(6,5)]
names(true.curve) = c("long", "lat")

predictKDEObject(kde.obj, true.curve, alpha.level = .9)




quartz(width = 8,height = 6.5)
newmap = getMap(resolution = "low")
xlim = c(-105,0)
ylim = c(16, 36)
plot(newmap, ylim = ylim, xlim = xlim, asp = 1)

plot(kde.obj, cont = c(50,75,90,95,99), add=TRUE, col.pt="blue", display="filled.contour2")

for (i in c(1:length(dflist))){
  lines(dflist[[i]][,1], dflist[[i]][,2], col='black')
}
lines(true.curve[,1], true.curve[,2], col='blue', lwd=3)
