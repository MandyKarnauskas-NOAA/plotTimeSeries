#' Traffic light plots for time series data
#'
#' This function produces a traffic light plot for visualizing trends in multiple time series.
#' The input file is a data frame with a series of time steps and data values at those time steps.
#' Time series data are automatically sorted within the plot based on an ordination analysis, such that those displaying similar
#' trends appear more closely together.
#'
#' @param dataset A \link{data.frame} in which the first column is numerical values representing time steps and subsequent columsn are data values at those time steps.
#' @param columns an integer or integer list defining the column numbers of data to be included.  Defaults to the entire data set.
#' @param mintime a value representing the earliest time step to be included in the analysis.  Defaults to first time step in data set.
#' @param maxtime a value representing the last time step to be included in the analysis.  Defaults to last time step in data set.
#' @param noNAs a value between 0 and 1 representing the proportion of allowable NAs for any given time series.
#' NA values are imputed with the mean across the time series for the ordination.  Time series exceeding the proportion of NAs
#' specified are removed from the analysis.
#' @param splits number of color breaks for the plot.  Options are 4 (quartiles with red and green) or 5 (quintiles with red, yellow, green).
#' @param cexlabs a value indicating scaling for the size of the labels.
#' @param indatformat a format in which the time steps are input, as defined in \link{strptime}.  Defaults to year with century ("%Y").
#' @param outdatformat a format as defined in \link{strptime} which represents the desired format for labeling the plot.  Defaults to year with century ("%Y").
#' @param graycol a boolean indicating whether or not grayscale colors should be used.  Defaults to FALSE.
#' @param method the ordination method for sorting the time series data. Defaults to principal components analysis ("pca") using \link{prcomp}.  Other option
#' is non-metric multidimensional scaling ("nmds") using \link{metaMDS}.
#' @param PC an integer indicating which ordination axis the time series data should be sorted by on the plot.  Specify either 1 (for first axis) or 2 (for second axis).
#'
#' @note
#' Data must be input in a data frame where rows represent time steps and columns represent data series.
#' \itemize{
#' \item The first columns contains a string of time units in any specified format.
#' \item Columns 2+ contain data values for the respective time steps.
#' }
#'
#' @references
#' <https://www.aoml.noaa.gov/ocd/ocdweb/ESR_GOMIEA/report/GoM_EcosystemStatusReport2017.pdf>
#'
#' @examples
#' ## make traffic plot of Gulf of Mexico fishery landings
#'  par(mar = c(3, 10, 1, 1))
#'  trafficLightPlot(GOMlandings)
#'
#'  trafficLightPlot(GOMlandings, mintime = 1980, maxtime = 2000, splits = 4, method = "nmds")
#'
#' @export
trafficLightPlot <- function(dataset, columns = NA, mintime = NA, maxtime = NA,
                    noNAs = 0, splits = 5, cexlabs = 0.9, indatformat = "%Y", outdatformat = "%Y",
                    graycol = FALSE, method = "pca", PC = 1)   {

# dependencies ------------------------------------------
  if ("vegan" %in% installed.packages() == FALSE) { install.packages("vegan") }
  library(vegan)

# define default settings -------------------------------
  times <- strptime(as.character(dataset[, 1]), indatformat)
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
  mdat <- mdat[, which(colSums(is.na(mdat)) / ncol(mdat) <= noNAs)]

  for (i in 1:(ncol(mat))) {                                                 #  replace NAs with mean of indicator
    mat[which(is.na(mat[,i])), i] <- mean(mat[, i], na.rm = T) }                 #  warning messages OK !!!!

  mdat <- t(mdat)

# ordination analysis and sorting of time series
  if (method=="pca")  {  pc <- prcomp(mat, scale=T)                          #  carry out PCA                                                              # transpose for sorting
                         m2 <- mdat[order(pc$rot[, PC]), ]  }                # order species by their loadings on the 1st or 2nd PC

  if (method=="nmds")  {  pc <- metaMDS(mat, distance = "bray", k=2)         #  carry out NMDS
                          m2 <- mdat[order(pc$species[, PC]), ]  }
    m2 <- t(m2)                                                                       # transpose back
    m2 <- scale(m2)                                                                   # scale matrix

# split by quintiles or quantiles ----------------
    if (splits == 5)  {
      levs <- quantile(m2, na.rm = T, probs = seq(0, 1, 0.2))
      cols = c("#FF000039", "#FF900049", "#CCFF0069" , "#0090FF89", "#0000FF99")
        if (graycol==TRUE)  { cols = gray(seq(0.2, 1, 0.2))  }
    }
    if (splits == 4)  {
      levs <- quantile(m2, na.rm = T, probs = seq(0, 1, 0.25))
      cols = c("#FF000099", "#FF000030", "#0000FF50", "#0000FF99")
        if (graycol==TRUE)  { cols = gray(seq(0.25, 1, 0.25))  }
    }
  m3 <- cut(m2, breaks = levs, incl=T)
  m3 <- as.numeric(m3)
  m4 <- matrix(m3, ncol=ncol(m2))

# plot traffic plot and axes ---------------------------------
  image(1:length(times2), (1:ncol(m2)), m4, col=cols, axes=F, xlab="", ylab="", las=2, useRaster=T)

  axis(1, lwd = 1, at = 1:length(times2), lab = format(times2, outdatformat))
  #axis(1, at = 1:length(times2), lab = rep("", length(times2)), las = 1, tck = -0.008, col = 1)
  axis(2, at = 1:ncol(m2), lab = sub(".", " ", sub(".", " ", colnames(m2), fixed = TRUE), fixed = TRUE),
       cex.axis=cexlabs, las = 1, tcl=0)
  box()
return(m2)

}
