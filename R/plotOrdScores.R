#' Plotting indicator time series
#'
#' This function carries out ordination analysis and plots the ordination scores in two dimensions.
#' The input file is a data frame with a series of regular time steps and data values at those time steps.
#'
#' @param dataset A \link{data.frame} in which the first column is numerical values representing time steps 
#' and subsequent columns are data values at those time steps.
#' @param columns an integer or vector defining the column numbers of data to be included.  Defaults to the entire data set.
#' @param mintime a value representing the earliest time step to be included in the analysis.  Defaults to first time step in data set.
#' @param maxtime a value representing the last time step to be included in the analysis.  Defaults to last time step in data set.
#' @param noNAs a value between 0 and 1 representing the proportion of allowable NAs for any given time series.  
#' NA values are imputed with the mean across the time series for the ordination.  Time series exceeding the proportion of NAs
#' specified are removed from the analysis.  
#' @param color color options for the segments connecting time steps. Either "rainbow" (default) or "gray" are available.  
#' @param lab.cols color specification for the time step labels.  
#' @param tim.cex a value indicating scaling for the size of the time step labels.
#' @param add.pts if TRUE, adds the PCA loadings or NMDS species on the ordination plot.  Defaults to FALSE.  
#' @param indatformat a format in which the time steps are input, as defined in \link{strptime}.  Defaults to year with century ("%Y").  
#' @param outdatformat a format as defined in \link{strptime} which represents the desired format for labeling the plot.  Defaults to year without century ("%y"). 
#' @param method the ordination method to be used in the analysis.  Defaults to principal components analysis ("pca") using \link{prcomp}.  Other option 
#' is non-metric multidimensional scaling ("nmds") using \link{metaMDS}.
#' @param nmdsk an integer representing the number of dimensions to be used in \link{metaMDS}, if the NMDS method is specified.  
#' @param return
#'
#' @note
#' Data must be input in a data frame where rows represent time steps and columns represent data series.   
#' \itemize{
#' \item The first columns contains a string of time units in any specified format.  
#' \item Columns 2+ contain data values for the respective time steps.
#' }
#'
#' @references
#' Figure 1 in https://onlinelibrary.wiley.com/doi/full/10.1111/gcb.12894
#' 
#' @examples
#' ## make traffic plot of Gulf of Mexico fishery landings
#'  
#'  plotOrdScores(GOMlandings)
#'
plotOrdScores <- function(dataset, columns = NA, mintime = NA, maxtime = NA, noNAs = 0, 
                          color = "rainbow", lab.cols = 1, tim.cex = 1, add.pts = FALSE, 
                          indatformat = "%Y", outdatformat = "%y", main = "title",  
                          method="pca", nmdsk = 2, return = "scores")  {     

# dependencies ------------------------------------------
  if ("vegan" %in% installed.packages() == FALSE) { install.packages("vegan") }
  library(vegan)
  
# define default settings -------------------------------
  times <- as.Date(as.character(dataset[, 1]), indatformat)
  if (is.na(mintime))  {  timmin <- min(times) } else { timmin <- as.Date(as.character(mintime), indatformat) }
  if (is.na(maxtime))  {  timmax <- max(times) } else { timmin <- as.Date(as.character(mintime), indatformat) }
  if (length(columns) == 1)  { columns <- 2:ncol(dataset) }
  
#  subset data for defined years -------------------------
  dataset <- dataset[order(times), ]
  times   <- times[order(times)]
  mdat <-  dataset[which(times == timmin):which(times == timmax), ]
  times2 <-  times[which(times == timmin):which(times == timmax)]
  mdat <- mdat[,columns]                                                     #  subset defined indicator columns
  mat <- mdat
  
# subset columns that contain less than minimum proportion of NAs
  mat <-  mat[, which(colSums(is.na(mat)) / ncol(mat) <= noNAs)]

# replace NAs with means ---------------------------------
    for (i in 1:(ncol(mat))) {                                              #  replace NAs with mean of indicator
      mat[which(is.na(mat[, i])), i] <- mean(mat[, i], na.rm = T) }    
   
# carry out ordination ----------------------------------
  if (method=="pca")  {  
    pc <- prcomp(mat, scale=T)                                                      #  carry out PCA
    x <- pc$x[, 1]
    y <- pc$x[, 2] 
    rx <- pc$rotation[, 1]
    ry <- pc$rotation[, 2]
    tab <- summary(pc)$imp 
    alab <- "PC" 
    }
  if (method=="nmds")  {  
    pc <- metaMDS(mat, distance = "bray", k=nmdsk)                                                   #  carry out PCA
    x <- pc$points[, 1]
    y <- pc$points[, 2]
    rx <- pc$species[, 1]
    ry <- pc$species[, 2]
    tab <- pc$stress
    alab <- "NMDS axis " 
    }                                                 
# calculate segment lengths --------------
  seg <- seq(length(x)-1)                                                  

# make plot ------------------------------
  if(method=="nmds")  {
    plot(x, y, type="n", xlab = paste0(alab, "1"),   
                         ylab = paste0(alab, "2"), 
                         col=2, main = main, font.main = 1, axes = F)
                         box()  }
  if(method=="pca")  {  
    plot(x, y, type="n", xlab = paste0(alab, " 1 (", round(tab[2, 1], 3)*100, "%)"),  
                         ylab = paste0(alab, " 2 (", round(tab[2, 2], 3)*100, "%)"), 
                         col = 2, tcl = -0.025, main = main, font.main = 1)   
    }                                                                     # x and y labels have % variance explained
  if (add.pts == TRUE) { points(rx, ry, cex = 0.9, pch = 19) }            # add species points if specified
  
  abline(h = 0, lty = 2, col = gray(0.7))
  abline(v = 0, lty = 2, col = gray(0.7))

  cols <-  rep(8, length(seg))                                            # gray lines
  if (color == "rainbow") { 
    cols <- rainbow(length(times2), start = 0.01, end = 0.75)  } else {
    cols <- color }                                                       # specify pretty rainbow colors
  segments(x[seg], y[seg], x[(seg + 1)], y[(seg + 1)], col = cols)        # plot pretty rainbow segments - O'Hare airport tunnel style
  
# plot time steps --------------------
  text(x, y, format(times2, "%y"), cex = tim.cex, col = lab.cols)          # overlay year labels
  box()

# return either scores only or entire ordination output ----------------------------  
  if (return == "scores")  { return(cbind(x,y)) }                                     #  return yearly scores on first two axes
  if (return == "all")     { return(pc)         }
}                                                                               
