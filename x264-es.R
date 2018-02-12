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
#plotCorrelationsAndDensity(etimes) 
#plotCorrelationsAndDensity(esizes)

# ce sont des fonctions pour regarder la différence de classement entre deux vidéos en particulier
# c’est typiquement pour montrer qu’on peut avoir un bouleversement fort sur le classement entre deux vidéos
# TODO: what are the two videos that lead to higher raking differences? 
# TODO: what are the two videos that lead to the highest change for the 1st top configuration? 
# (I imagine the following fictive situation: configurationX is rank number 1 for video1, but the same configurationX is rank number 899 for video 23) ) 
# TODO: what are the two videos that lead to the highest change for the top 10 configuration? top 100 configuration? 
# View(computeRankDifferencesByTime(6, 7))
# View(computeRankDifferencesBySize(3, 31)) 

