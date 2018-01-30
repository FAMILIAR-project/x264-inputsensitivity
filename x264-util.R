library(randomForest)
library(gbm)

NTREE = 100

mkRandomForestByTime <- function(dat) {
  mtry <- (ncol(dat) - 7) # 7: because we excluse non predictor variables! => BAGGING (m=p)
  return(randomForest
         (elapsedtime~.-systemtime-usertime-size-configurationID-H264-no_asm, 
           data=dat,importance=TRUE,ntree=NTREE,keep.forest=TRUE,mtry=mtry)
         )
}

mkRandomForestBySize <- function(dat) {
  mtry <- (ncol(dat) - 7) # 7: because we excluse non predictor variables!
  return(randomForest
         (size~.-systemtime-usertime-elapsedtime-configurationID-H264-no_asm, data=dat,importance=TRUE,ntree=NTREE,keep.forest=TRUE,mtry=mtry)
         )
}


mkBoostingByTime <- function(dat) {
  return(
    gbm(elapsedtime~.-systemtime-usertime-size-configurationID-H264-no_asm, 
        data=dat,distribution="gaussian",n.tree=10000)
  )
}

mkBoostingBySize <- function(dat) {
  return(
    gbm(size~.-systemtime-usertime-elapsedtime-configurationID-H264-no_asm, 
        data=dat,distribution="gaussian",n.tree=10000)
  )
}
