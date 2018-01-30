#load the randomForest library. if you havent installed it, run the next line
#install.packages("randomForest")
library(randomForest)
library(MLmetrics)
library(rpart)

source('x264-util.R')

X264_N_FEATURES = 16 # 10 + 6: we remove the root feature (the mandatory ASM is incl; actually looking at ASE they consider N=16 I guess because of possible values of ref)
N_REPEAT = 1




# splitdf function will return a list of training and testing sets
splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  #trainindex <- sample(index, trunc(length(index)/2))
  trainindex <- sample(index, trunc(X264_N_FEATURES * 4))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

# predDimension: time or size? 
predComputation <- function(iris, predDimension) {
  
#apply the function
splits <- splitdf(iris)

# save the training and testing sets as data frames
training <- splits$trainset
testing <- splits$testset

if (predDimension == "time") {
  model <- mkRandomForestByTime(training) 
      # rpart(elapsedtime~.-systemtime-usertime-size-configurationID-H264-no_asm, data=training,
      #   method = "anova",
      #   parms = list(split = "information"),
      #   control = rpart.control(minsplit = 2,
      #                           minbucket = 8,
      #                           #maxdepth = maxDepth,
      #                           #cp = complexity,
      #                           usesurrogate = 0,
      #                           maxsurrogate = 0))
}
else if (predDimension == "size") {
  model <- mkRandomForestBySize(training) 
      # rpart(size~.-systemtime-usertime-elapsedtime-configurationID-H264-no_asm, data=training)
}
else {
  print("Error: size or time!")
}
# print(model)

#what are the important variables (via permutation)
#varImpPlot(model, type=1)

#predict the outcome of the testing data
predicted <- predict(model, newdata=testing)
#predicted <- predict(model, data=testing) # for CART 

# what is the proportion variation explained in the outcome of the testing data?
# i.e., what is 1-(SSerror/SStotal)
################ FIXME 
actual <- testing$elapsedtime # SIZE ????
################ FIXME 
rsq <- 1-sum((actual-predicted)^2)/sum((actual-mean(actual))^2)
#rsq <- sum((actual-predicted)^2)/sum((actual-mean(actual))^2)
list(act=actual,prd=predicted,rs=rsq)
}

computeRegAccuracy <- function(data, dim) {
  
  aracc <- data.frame(0.0)
  for(i in 1:N_REPEAT) {
    racc <- predComputation(data, dim)
    racc$d <- abs((racc$act - racc$prd) / racc$act) 
    mae <- ((100 / length(racc$d)) * sum(racc$d))
    #if (dim == "size") {
    #  mae <- mae / 10^7
    #}
    aracc <- data.frame(aracc, mae)
  }
  
  aracc$X0 <- NULL
  # return(racc$rs * 100)
  return(aracc)
  #return(MAE(y_pred = racc$prd, y_true = racc$act))
  # if (dim == "size") {
  #   return ((racc$d * 100) / 1000000)
  # }
  # else {
  #   return (racc$d * 100)
  # }
  # return(MAPE(y_pred = racc$prd, y_true = racc$act))
  #return(racc$rs)
}

plotAllRegAccuracies <- function(dim) { # time or size?
  
  mcolours <- colours()[1:length(dirs)] # sample(colours(), 
  # Multiple histograms
  #par(mfrow=c(7, 7))
  #par(mfrow=c(3, 4))
  accs <- data.frame(0.0)
  for (i in 1:length(dirs)) {
    accu <- computeRegAccuracy(loadVideoMeasurements(i), dim)
    accs <- data.frame(accs, transpose(accu)[,1])
  }  
  
  par(mfrow=c(1, 1))
  accs$X0 <- NULL
  #barplot(transpose(accs)[,1],horiz=FALSE, cex.names=0.8, col = mcolours)
  boxplot(accs)
}


computeLinearRelVideo <- function(vid1, vid2,nsample) {
    v1 <- loadVideoMeasurements(vid1)
    v3 <- loadVideoMeasurements(vid2)
    print(paste("correlation between",vid1,"and",vid2,"is",cor(v1$elapsedtime, v3$elapsedtime)))
    v13 <- data.frame(0.0)

    v13 <- data.frame(v13, v3$elapsedtime) 
    v13 <- data.frame(v13, v1$elapsedtime)
    v13$X0 <- NULL
    
    lm.fit <- lm(data = v13[1:nsample,], v1.elapsedtime~v3.elapsedtime)
    print(summary(lm.fit))
    v13r <- predict(lm.fit,v13[nsample+1:nrow(v13),],interval="prediction")
    
    par(mfrow=c(1, 1))
    plot(v3$elapsedtime, v1$elapsedtime)
    abline(lm.fit,col="red")
    return (abs(v13r[,1] - v13$v1.elapsedtime[nsample+1:nrow(v13)]) / v13r[,1])
}

computeLinearRelVideoBySize <- function(vid1, vid2,nsample) {
  v1 <- loadVideoMeasurements(vid1)
  v3 <- loadVideoMeasurements(vid2)
  print(paste("correlation between",vid1,"and",vid2,"is",cor(v1$size, v3$size)))
  v13 <- data.frame(0.0)
  v13 <- data.frame(v13, v3$size) 
  v13 <- data.frame(v13, v1$size)
  v13$X0 <- NULL
  
  lm.fit <- lm(data = v13[1:nsample,], v1.size~v3.size)
  print(summary(lm.fit))
  v13r <- predict(lm.fit,v13[nsample+1:nrow(v13),],interval="prediction")
  
  par(mfrow=c(1, 1))
  plot(v3$size, v1$size)
  abline(lm.fit,col="red")
  return (abs(v13r[,1] - v13$v1.size[nsample+1:nrow(v13)]) / v13r[,1])
}

