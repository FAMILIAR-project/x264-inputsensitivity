library(readr)
library(randomForest)
library(reshape2)
# library(dplyr)
library(data.table)
library(ggplot2)

source('x264-util.R')

# seems working only with R studio 
script.dir <- dirname(sys.frame(1)$ofile)
mainDirectory <- paste(script.dir, "/data", sep="") 
# mainDirectory <- "/Users/macher1/Documents/SANDBOX/x264-sensitivity/x264-inputsensitivity/data"

print(mainDirectory)


  
  #"/Users/macher1/Documents/SANDBOX/x264-backup/x264-benchmarks/wasm/tmp"

loadVideoMeasurements <- function (vidNumber) {
  dir <- dirs[vidNumber]
  return(loadVideoMeasurementsByDirectory(dir))
}


# in each directory we assume there is a x264-results1.csv (by convention it is the case)
loadVideoMeasurementsByDirectory <- function(dir) {
  filename <- paste(dir, "/", "x264-results1.csv", sep = "")
  csvR <- read_csv(filename, 
                   col_types = cols(H264 = col_logical(),
                                    configurationID = col_character(),
                                    no_8x8dct = col_factor(levels = c("true", "false")), no_asm = col_factor(levels = c("true", "false")),
                                    no_cabac = col_factor(levels = c("true", "false")), no_deblock = col_factor(levels = c("true", "false")),
                                    no_fast_pskip = col_factor(levels = c("true", "false")), no_mbtree = col_factor(levels = c("true", "false")),
                                    no_mixed_refs = col_factor(levels = c("true", "false")), no_weightb = col_factor(levels = c("true", "false")))); 
                                    # no_8x8dct = col_logical(), no_asm = col_logical(),
                                    # no_cabac = col_logical(), no_deblock = col_logical(),
                                    # no_fast_pskip = col_logical(), no_mbtree = col_logical(),
                                    # no_mixed_refs = col_logical(), no_weightb = col_logical())); 
  ###
  # get the size of the original video
  # TODO
  #filename <- paste(dir, "/", "SIZE", sep="")
  #if(file.exists(filename)) {
  #  res <- readLines(filename)
  #  fsize <- as.numeric(res[1]) #100000
  #  osize <- seq.int(fsize, fsize, length.out = length(csvR$size))
  #  print(paste("SIZE found!",fsize))
  #  csvR$size <- csvR$size / osize
  #}
  ###
  return(csvR);
}

computeImportanceWBoostingBySize <- function(x264_results) { # 'IncNodePurity' or '%IncMSE'
  bag <- mkBoostingBySize(x264_results)
  return(as.data.frame(summary(bag))$rel.inf)
}

computeImportanceBySize <- function(x264_results, inc) { # 'IncNodePurity' or '%IncMSE'
  bag <- mkRandomForestBySize(x264_results)
  return(as.data.frame(importance(bag))[inc])
}

computeImportanceByTime <- function(x264_results, inc) { # 'IncNodePurity' or '%IncMSE'
  bag <- mkRandomForestByTime(x264_results)
  return(as.data.frame(importance(bag))[inc])
}




reorder_cormat <- function(cormat) {
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

plotCorrelationsAndDensity <- function(data) {
  cormat <- round(cor(data, method ="spearman"), 2)
  
  # Reordered correlation data visualization :
  
  # Reorder the correlation matrix
  cormat <- reorder_cormat(cormat)
  upper_tri <- get_upper_tri(cormat)
  
  # Melt the correlation matrix
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  
  # par(mfrow=c(1, 1))
  
  # Create a ggheatmap
  ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-1,1), space = "Lab",
                         name="Pearson\nCorrelation") +
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                     size = 8, hjust = 1))+
    coord_fixed()
  
  # Print the heatmap
  print(ggheatmap)
  
 
  
  ggheatmap +
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 1) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))
  
  
  #print(ggheatmap)

  mcolours <- sample(colours(), length(data))
  # Multiple histograms
  par(mfrow=c(3, 4))
  # par(mfrow=c(1, 1))
  colnames <- dimnames(data)[[2]]
  #plot(ecdf(data[,1]))
  for (i in 1:length(data)) {
    #hist(xlab = "", data[,i], main=paste("video", i), probability=TRUE, col="gray", border="white")
    # colnames[i]
    de <- density(data[,i])
    plot(de, type="n", main=colnames[i])
    polygon(de, col="red", border="gray")
    #plot(ecdf(data[,i]), add=TRUE, col=mcolours[i])
  }
  
  
  #ggplot(data = melted_cormat, aes(x=X1, y=X2, fill=value)) + 
   # geom_tile()
  
}

computeRankDifferencesByTime <- function (vidNumber1, vidNumber2) {
  x264_res1 <- loadVideoMeasurements(vidNumber1)
  x264_res2 <- loadVideoMeasurements(vidNumber2)
  
  x264_res3 <- data.frame(x264_res1) # including time of res1
  
  v1 <- rank(x264_res1$elapsedtime, ties.method = "min")
  v2 <- rank(x264_res2$elapsedtime, ties.method = "min")
  
  x264_res3$elapsedtime2 <- x264_res2$elapsedtime
  x264_res3$ranktime <- v1
  x264_res3$ranktime2 <- v2
  
  
  x264_res3$drank <- x264_res3$ranktime2 - x264_res3$ranktime
  return(x264_res3)
  
}

computeRankDifferencesBySize <- function (vidNumber1, vidNumber2) {
  x264_res1 <- loadVideoMeasurements(vidNumber1)
  x264_res2 <- loadVideoMeasurements(vidNumber2)
  
  x264_res3 <- data.frame(x264_res1) # including size of res1
  
  v1 <- rank(x264_res1$size, ties.method = "min")
  v2 <- rank(x264_res2$size, ties.method = "min")
  
  x264_res3$size2 <- x264_res2$size
  x264_res3$ranksize <- v1
  x264_res3$ranksize2 <- v2
  
  
  x264_res3$drank <- x264_res3$ranksize2 - x264_res3$ranksize
  return(x264_res3)
  
}



plotAllImportances <- function(impl) {
  
  mcolours <- sample(colours(), length(impl))
  # Multiple histograms
  # par(mfrow=c(5, 7))
  par(mfrow=c(3, 4))
  colnames <- dimnames(impl)[[2]]

  for (i in 1:length(impl)) {
    barplot(impl[[i]], horiz=FALSE, cex.names=0.8,names.arg = c(paste("feature", colnames[i])), col = mcolours[i])
  }  
  
  
}


# load datas and launch analysis 
dirs <- list.dirs(mainDirectory, recursive = FALSE)
i <- 0
etimes <- data.frame(0.0) # raw times 
esizes <- data.frame(0) # raw sizes
rsizes <- data.frame(0) # r for rank
rtimes <- data.frame(0)
  
impSizes <- data.frame(0.0) # imp for "importance"
impTimes <- data.frame(0.0)


for(dir in dirs) { 
  csvR <- loadVideoMeasurementsByDirectory(dir)
  i <- i + 1
  etimes <- data.frame (etimes, csvR$elapsedtime)
  colnames(etimes)[length(etimes)] <- paste("video", i, sep="")
  esizes <- data.frame (esizes, csvR$size)
  colnames(esizes)[length(esizes)] <- paste("video", i, sep="")
  
  #v1 <- order(csvR$size)
  #ranksizes <- data.frame(csvR[v1, ], ranksize = as.numeric(factor(csvR$size[v1])))
  #rsizes <- data.frame (rsizes, ranksizes$ranksize)
  
  v1 <- rank(csvR$size, ties.method = "min")
  rsizes <- data.frame (rsizes, v1) 
  
  v2 <- rank(csvR$elapsedtime, ties.method = "min")
  rtimes <- data.frame (rtimes, v2) 
  
  
  # 'IncNodePurity' or '%IncMSE'
  impSizes <- data.frame(impSizes, computeImportanceBySize(csvR, '%IncMSE')) #data.frame(impSizes, computeImportanceWBoostingBySize(csvR)) #
  impTimes <- data.frame(impTimes, computeImportanceByTime(csvR, '%IncMSE'))
  
  print(paste("video", i, " => ", dir, sep=""))
}
  
  
# post-process
rtimes$X0 <- NULL
rsizes$X0 <- NULL
esizes$X0 <- NULL
etimes$X0 <- NULL
impSizes$X0 <- NULL
impTimes$X0 <- NULL

impSizes2 <- transpose(impSizes)
impTimes2 <- transpose(impTimes)
colnames(impSizes2) <- rownames(impSizes)
colnames(impTimes2) <- rownames(impTimes)

# end post-process

####### USAGE 

### RQ1 
# TODO: spearman for the correlation but one can use pearson (btw does it lead to different results?)
# TODO: kernel density is naive; I've tried with histograms also. 
# eg Kullback-Leibler (KL) divergence is used to compare the similarity between the performance distributions
# in the paper "Transfer Learning for Performance Modeling of Configurable Systems: An Exploratory Analysis"
#plotCorrelationsAndDensity(etimes) 
#plotCorrelationsAndDensity(esizes)

# functions to look at ranking difference between 2 videos 
# typically for showhing a strong/spectacular change in the ranking between 2 videos
# TODO: what are the two videos that lead to higher raking differences? 
# TODO: what are the two videos that lead to the highest change for the 1st top configuration? 
# (I imagine the following fictive situation: configurationX is rank number 1 for video1, but the same configurationX is rank number 899 for video 23) ) 
# TODO: what are the two videos that lead to the highest change for the top 10 configuration? top 100 configuration? 
# View(computeRankDifferencesByTime(6, 7))
# View(computeRankDifferencesBySize(3, 31)) 

# RQ1 but this specific question: "Do inputs change the influential options?"
# plotAllImportances(impTimes2)
# plotAllImportances(impSizes2)
# for computing feature importances we rely on computeImportanceByTime ou computeImportanceBySize
#  the idea is to a machine learning algorithm (eg random forest, cf x264-util.R) and then rely on the related importance() function of packages 
# TODO: for computing feature importances, there is certainly a better / dedicated techniques ... 
# TODO: big question is to determine whetherthe same options impact execution time or size... or whether it depends on input videos


### RQ2 
# Can we identify "cheaper" inputs? (same distribution, but less costly to measure) Can we group together inputs? (same distribution, so no need to transfer)
# it seems we can certainly exploit the correlation matrix 
# example: cor(loadVideoMeasurements(1)$elapsedtime, loadVideoMeasurements(27)$elapsedtime)
# [1] 0.9618621
# we can say we can group 1 et 27 given the high correlation 
#> mean(loadVideoMeasurements(1)$elapsedtime)
#[1] 23.08805
#> mean(loadVideoMeasurements(27)$elapsedtime)
#[1] 2.402001
#video1 takes 23 seconds on average per config to process/measure
#video27 takes only 2.5 seconds
# in the future one can take video27 instead of video1 because it is cheaper 
# cheap means here: time needed to measure a sample of configurations
# TODO: automatically identify clusters of videos (cluster is not necessarily limited to 2 videos, can be 3, 4, ...)
# TODO: associate the cheapest video for a given cluster
# TODO: what's unclear is the criterion to group videos (threshold of correlations? unsupervised learning?)
# TODO: even more difficult: we can create clusters such that in each cluster the correlation is "maximised" (sounds a good greedy strategy); 
# a more sophisticated strategy is to create clusters such that we minimize the cost of cheapest videos of each cluster (sum of execution time of cheap videos)

### RQ3 

## preliminary remarks
# a correlation coefficient is good at first sight 
# but what we really want is to transfer prediction from one video to another
# TODO: can we imagine that even with a close to 0 correlation coefficient we find very effective "function transfers"? 
# it is possible see the literature on multi-target learning, transfer learning, adaptive learning, etc. 

# anyway: for evaluating the error rate of a learning algorithm over one video
# plotAllRegAccuracies("time") 
# plotAllRegAccuracies("size")
#or for a specific video: computeRegAccuracy(loadVideoMeasurements(2), “time”)
#mae
# 1 2.959217
# interesting to note that there are some videos are more challenging in terms of predicition erros 
## end of preliminary remarks

# what is really interesting for RQ3 is to transfer effectively a prediction model for one video to another 
# computeLinearRelVideo(1, 2, 1000)
# linear regression is used right now
# here between video1 and video2 
# idea is as follows:
  # * we are able to predit performances for video 1 (over a sample s1)
  # * we use another sample of configs (s2) but this time  with video 2. We hope s2 will be small (ie smaller than s1, a subset of s1)
  # * are we able to transfer predictions from video 1 to video 2? stated differently: 
  # can we save the re-learning over the whole sample of s1 on video 2 ?
# actual implementation is naive for 3 reasons:
#  * we rely on all raw measurements of video 1 (and not on a performance perdiction model over a sample) => zero error, which is a strong assumption of course
#  * we learn over 1000 configurations (for video 2, cf 3rd parameter) => it is a lot of course, but we can play on this parameter
#  * I am using a very basic/simple linear regression but we use more sophisticated techniques  (reg linear polynomial) 
# 3 points are TODO (we need to extend/improve the current implementation)
# challenging example 
#  > cor(loadVideoMeasurements(27)$size, loadVideoMeasurements(4)$size)
# [1] 0.5517978
# sounds hard to find a transfer function

