
#' Plotting indicator time series
#'
#' This function is for standardized plotitng of indicator time series, such as those found in NOAA's Ecosystem Status Reports.
#' The function imports a data frame of indicator values and dates and plots the time series, denoting mean and values above and below one standard deviation from the mean.
#' An optional trend analysis highlights changes in the mean and slope of the time series in the last 5 years of data.
#'
#' @param filename an object of class 'indicatordata' or a .csv file in standardized format.  See details below.
#' @param coltoplot an integer or integer list defining the column numbers of indicator file to plot.  Defaults to a single column of data in column 2.
#' @param plotrownum an integer defining the number of rows of plots in a multi-panel plot.
#' @param plotcolnum an integer defining the number of columns of plots in multi-panel plot.
#' @param sublabel a logical value indicating whether optional descriptive information should appear within main label.
#' @param sameYscale a logical value indicating whether a consistent y-axis scale is desired across multiple panels.
#' @param yposadj a number specifying manual adjustment of position of y-axis label; values >1 move text further from the axis.
#' @param widadj expansion factor to adjust the total width of plot.
#' @param hgtadj expansion factor to adjust the total height of plot.
#' @param type a character string indicating which type of plot is desired. Defaults to points, with lines for consecutive years only.
#'         "ptsOnly" and "allLines" can be specified.
#' @param trendAnalysis a logical value indicating whether to highlight the trend in mean and slope over last 5 years; defaults to TRUE unless fewer than 5 years of data.
#' @param propNAallow if fraction denoting the allowable proportion of missing values in last 5 years; when the proportion of NAs exceeds this value, trend analysis will not appear (defaults to 0.5)
#' @param redgreen a logical value indicating whether to remove red/green shading of anomalies from plot.
#' @param anom a character string indicating whether to convert indicator to monthly anomalies.  One of "none", "mon" (monthly anomalies) or "stmon" (standardized monthly anomalies) can be used.
#' @param outname a character string specifying alternate output filename, if file input name is not desired.
#' @param outtype a character string specifying format for output, if manual saving is not desired.  Options are "png" or "pdf".
#'
#' @note
#' Data can be input directly as a .csv file in the format below, or as an object of class 'indicatordata' which contains two data frames.
#' \itemize{
#' \item The first \link{data.frame} contains 3 rows with the indicator name, unit, and sublabel.  Number of columns is equal to the number of indicators.
#' \item The second \link{data.frame} contains the data; the first column contains the dates and columns 2+ contain the indicator values.
#' }
#' A .csv file should be formatted as follows:
#' \itemize{
#'  \item **Row 1** contains a character string of the indicator name (main title of plot).
#'  \item **Row 2** contains a character string of the indicator units (y-axis label).
#'  \item **Row 3** contains an optional character string with information on the sublabel (when multiple panels are used).
#'  \item **Column 1** is time values, **columns 2** and on are indicator data.  The first three values of column 1 are NA.
#'  \item Time can be in year (with century), or monthly time step in a variety of formats (e.g, Jan1986, Jan-86, 1986jan), including or excluding day of month.
#'  }
#'
#' @references
#' <https://www.aoml.noaa.gov/ocd/ocdweb/ESR_GOMIEA/report/GoM_EcosystemStatusReport2017.pdf>
#'
#' @examples
#' ## plot a single indicator
#'  plotIndicatorTimeSeries("indicator.csv")
#'
#'  ## plot an indicator with plot adjustments
#'  plotIndicatorTimeSeries("menhaden_abundance_index.csv", sublabel=F, yposadj=0.8, widadj=0.8)
#'
#'  ## plot a six-panel plot of indicator values reported at different locations
#'  plotIndicatorTimeSeries("seagrass_acreage.csv", coltoplot = 2:7, plotrownum = 3, plotcolnum = 2, widadj = 0.6, sameYaxis = T)

#' function()

plotIndicatorTimeSeries <-  function(filename)  {

# set default setting for simple 1-panel plot with trend analysis -----
if (!exists("coltoplot"))   { coltoplot <- 2 }
if (!exists("plotrownum"))  { plotrownum <- 1 }
if (!exists("plotcolnum"))  { plotcolnum <- 1 }
if (!exists("sublabel"))    { sublabel <- FALSE }
if (!exists("sameYscale"))  { sameYscale <- FALSE }
if (!exists("yposadj"))     { yposadj <- 1 }
if (!exists("widadj"))      { widadj <- 1 }
if (!exists("hgtadj"))      { hgtadj <- 1 }
if (!exists("type"))        { type <- "" }
if (!exists("trendAnalysis"))  { trendAnalysis <- TRUE }
if (!exists("propNAallow")) { propNAallow <- 0.5 }
if (!exists("redgreen"))    { redgreen <- TRUE }
if (!exists("anom"))        { anom <- "none" }
if (!exists("outname"))     { outname <- NA }
if (!exists("outtype"))     { outtype <- "" }

# read in file --------------------------------------------------------

if (grep(".csv", filename) == 1)  {                       # if old formula, convert to class indicatordata
indnames <- read.table(filename, header=F, sep=",", skip=0, quote="", stringsAsFactors = FALSE)[1:3,]   # load data file
inddata <- read.table(filename, header=F, sep=",", skip=3, quote="", stringsAsFactors = FALSE)   # load data file labels
s <- list(labels = indnames, dat = inddata)
class(s) <- "indicatordata"
  } else {
    s <- filename
    s$labels <- cbind(c(NA, NA, NA), s$labels)
    }

d1 <- s$labels            # use former naming conventions
d <- s$dat
# d <- d[rowSums(d[2:ncol(d)], na.rm=T) != 0,]                       # remove rows with no data

# convert dates to standardized format --------------------
formatlis <- c("%d%b%Y", "%d%b%y", "%d-%b-%y", "%d-%b-%Y", "%d%y%b", "%d%Y%b", "%d-%y%b", "%d%Y%b")  # list of formats

if (class(d$V1[1]) == "integer" & nchar(d$V1[1]) <= 4) {              # is time column values of years?
  monthly <- FALSE                                                     # if so, monthly F and set time to year
  tim_all <- d$V1
  }  else  {                                                          # else need to find and extract month format
  monthly <- TRUE
  if (is.na(as.Date(d$V1[1], tryFormats = formatlis, optional = TRUE))) {   # if no day available, add it manually
    d$V1 <- paste0("1-", d$V1)                                              # adding a day to date string
    datelis <- as.Date(d$V1, tryFormats = formatlis)                        # convert date
      } else {
    datelis <- as.Date(d$V1, tryFormats = formatlis)                        # if day is available then convert date
      }
  }

if (monthly==TRUE) {                                                        # if monthly, convert to decimal date
  tim_all <- as.numeric(substr(datelis, 1, 4)) + ((as.numeric(strftime(datelis, format = "%j")) - 1) / 365)
}

# adjustment for width ---------------------------------------------------
  if (monthly==F) { wid <- length(tim_all)*4 }  else  { wid <- length(tim_all) / 2 }
  if (length(tim_all) <= 10 & length(tim_all) > 5) {  wid <- wid*2  }
  if (length(tim_all) <= 5)  {  wid <- wid*3  }
  wid <- wid * widadj     #  set adjusted width if specified

# set graphics specifications based on number of panels ------------------
  if (plotcolnum + plotrownum > 2)  { plotcolnum2 <- plotcolnum*0.65; plotrownum2 <- plotrownum*0.65 }  else
                                    { plotcolnum2 <- plotcolnum; plotrownum2 <- plotrownum }

# adjust name for output graphic, if specified ------------------------------
  if (is.na(outname))  {
    if (grep(".csv", filename) == 1)  {
      filnam <- paste0(strsplit(filename, ".csv"), ".", outtype)
        }   else   {
        filnam <- paste0(filename, ".", outtype)
        }  }  else   {
            filnam <- outname
            }

# adjust plot size for extra long labels ------------------------------------
  if (sublabel==T) { mm <- paste(as.character(d1[1,max(coltoplot)]), "\n", as.character(d1[3,max(coltoplot)]), sep="")
            } else { mm <- d1[1,max(coltoplot)] }
  longlabs <- max(nchar(as.character(mm)), nchar(as.character(d1[2,max(coltoplot)])))
  if ( longlabs > 30  )  {
    wid <- longlabs/30 * wid
    hgtadj <- longlabs/30 * hgtadj
    }

# open plot window if png is selected format (default) ----------------------
if (outtype=="png")  {
  png(filename = filnam, units = "in", width = ((wid+10)/7)*plotcolnum2/1.3, height = hgtadj * (3.5*plotrownum2)/1.3, pointsize = 12, res = 72*4) }

# layout for single or multi-panel plots ------------------------------------
  nf <- layout(matrix(c(1:(plotrownum*plotcolnum*2)), plotrownum, plotcolnum*2, byrow = TRUE), rep(c(wid/5, 1), plotcolnum), rep(4, plotrownum))
#  layout.show(nf)

# layout for single plots with fewer than 5 data points or no trend analysis ---
  if (length(coltoplot)==1 & length(tim_all) <= 5 | trendAnalysis==F)  {
  nf <- layout(matrix(c(1:(plotrownum*plotcolnum)), plotrownum, plotcolnum, byrow = TRUE), rep(c(wid/5), plotcolnum), rep(3, plotrownum))
          }
#  layout.show(nf)

# get common yscale -------------------------------------------------------------
  ymin <- min(d[,coltoplot], na.rm=T) * 0.99
  ymax <- max(d[,coltoplot], na.rm=T) * 1.01

# loop through indicator columns ----------------------------------------
  for (i in coltoplot)  {

  co_all <- d[,i]                                                 # data

# calculate monthly anomalies if specified ------------------------------
    if (anom=="mon")  {
      moref <- match(strftime(datelis, format="%b"), month.abb)
      moav  <- tapply(co_all, moref, mean, na.rm=T)
        for (m in 1:12) {
          co_all[which(moref==m)] <- co_all[which(moref==m)] - moav[m]    }
                      }

# calculate standardized monthly anomalies if specified ------------------
    if (anom=="stmon")  {
      moref <- match(strftime(datelis, format="%b"), month.abb)
      moav  <- tapply(co_all, moref, mean, na.rm=T)
      most  <- tapply(co_all, moref, sd, na.rm=T)
        for (m in 1:12) {
          co_all[which(moref==m)] <- (co_all[which(moref==m)] - moav[m])/most[m]   }
                    }
# create sublabel ---------------------------------------------------------
  if (sublabel==T) { mm <- paste(as.character(d1[1,i]), "\n", as.character(d1[3,i]), sep="") } else {
                     mm <- d1[1,i] }                               # create y-axis label

yl <- d1[2,i]
  if (anom=="mon")   { yl <- paste(yl, "\n", "monthly anomaly", sep="") }     # adjust label if monthly anomaly
  if (anom=="stmon") { yl <- paste(yl, "\n", "standardized monthly anomaly", sep="") }

colind <- c("#FF000080", "#00FF0080")             # shading of anomalies +/- 1 S.D.

# in case of missing values in column -------------------------------------
  if (sum(!is.na(co_all)) == 0) {  plot.new(); plot.new()  }  else  {

  tim <- tim_all[!is.na(co_all)]        # for dealing with missing values
  co <- co_all[!is.na(co_all)]

# start data plot -----------------------------------------------------------
if (length(tim) > 5) {                  # plotting if more than 5 data points

  if (trendAnalysis==T)  {  par(mar=c(2.5,5,3,0), xpd=F)  }  else  {    # set margins based on trend analysis T or F
                            par(mar=c(2.5,5,3,1), xpd=F)  }
  par(mgp=c(3*yposadj,1,0))

  # blank plot with specified y limits
  if (sameYscale==T)  {   plot(tim_all, co_all, col = 0, axes = F, xlab = "", ylab = yl, main = mm, ylim = c(ymin, ymax))    }
  if (sameYscale==F)  {   plot(tim_all, co_all, col = 0, axes = F, xlab = "", ylab = yl, main = mm)                        }

  if (length(tim) >= 5 & redgreen==T) {

      # make red and green polygons --------------
      for (j in 2:length(tim))  {  polygon(c(tim[j-1], tim[j], tim[j], tim[j-1]),
                                           y=c(mean(co, na.rm=T), mean(co, na.rm=T), co[j], co[j-1]),
                                           col=colind[as.numeric(mean(co[(j-1):j], na.rm=T) > mean(co, na.rm=T))+1],
                                           border=F) }
                                         }

    # make white square polygon across years ---------
    polygon(c(min(tim_all, na.rm=T)-5, max(tim_all, na.rm=T)+5,
              max(tim_all, na.rm=T)+5, min(tim_all, na.rm=T)-5),
              c(mean(co_all, na.rm=T)-sd(co_all, na.rm=T),
                mean(co_all, na.rm=T)-sd(co_all, na.rm=T),
                mean(co_all, na.rm=T)+sd(co_all, na.rm=T),
                mean(co_all, na.rm=T)+sd(co_all, na.rm=T)), col="white", border=T)

    # make blue window in last 5 years ----------
    if (trendAnalysis==T)  {
      polygon(c(max(tim_all, na.rm=T)-4.5-as.numeric(monthly)/2.1,
                max(tim_all, na.rm=T)+0.5-as.numeric(monthly)/2.4,
                max(tim_all, na.rm=T)+0.5-as.numeric(monthly)/2.4,
                max(tim_all, na.rm=T)-4.5-as.numeric(monthly)/2.1),
                c((mean(co_all, na.rm=T)-sd(co_all, na.rm=T)),
                  (mean(co_all, na.rm=T)-sd(co_all, na.rm=T)),
                  (mean(co_all, na.rm=T)+sd(co_all, na.rm=T)),
                  (mean(co_all, na.rm=T)+sd(co_all, na.rm=T))), col="#0000FF20", border=F)
                  }

    # plot the points or the lines -----------------------------------------
  tstep <- round(mean(diff(tim_all)), 3)   # determine time step
    if (type == "ptsOnly")  {
        points(tim_all, co_all, pch=20, cex=1.5)
        }            # plot time series - points
    if (type == "")  {
      if (round(mean(diff(tim)), 3) > tstep)  {
        points(tim_all, co_all, pch=20, cex=0.75)   # if gaps between time steps, plot small points because lines may not appear
        }
      if (round(mean(diff(tim)), 3) == mean(diff(tim_all)))  {
        points(tim_all, co_all, pch=20, cex=1.5)    # if no gaps between time steps, plot larger pts because lines will connect all
        }
        inc <- which(round(diff(tim), 3) == tstep)  # which time steps are equal?
      for (k in inc)  {
        lines(tim_all[k:(k+1)], co_all[k:(k+1)], lwd=2)  # plot time series - lines for yearly steps only
        }  }
      if (type == "allLines")  {
        lines(tim, co, lwd=2)               # plot time series - lines for all years
        points(tim_all, co_all, pch=20, cex=0.75)
       }

    # add parallel lines for mean and sd ---------------------------
    abline(h = mean(co, na.rm=T), lty=8)
    abline(h = mean(co, na.rm=T) + sd(co, na.rm=T), lty=1)
    abline(h = mean(co, na.rm=T) - sd(co, na.rm=T), lty=1)

    # add axes and tick marks ------------------------
    if (length(tim) > 10)  { axis(1, at=seq(1900, 2050, 5)) } else {
                             axis(1, at=seq(1900, 2050, 2)) }           # add axis 1
  axis(1, at=seq(1900, 2050, 1), tck=-0.015, lab=rep("", 151))                                                                  # add axis 1 small ticks
  axis(2, las=2); box()                                                                                                         # add axis 2
# end data plot -------------------------------------------------------------

# start trend plot ----------------------------------------------------------
  if (trendAnalysis==T)  {
  par(mar=c(2.5,0,3,0))                                                         #  second panel on mean and trend of last 5 years

  last5 <-     co_all[which(tim_all > max(tim_all)-5)]
  last5tim <- tim_all[which(tim_all > max(tim_all)-5)]

  plot(1, xlim=c(0.94,1.06), ylim=c(0.6, 1.6), col=0, axes=F, xlab="", ylab="")  # create empty plot

  # analyze mean and slope of last 5 years ----------------------------------------------
  if (sum(is.na(last5)) / length(last5) < propNAallow)  {            # if proportion of NAs does not exceed limit
  points(1, 1.225, pch=20, cex=5)                                    # plot point for trend analysis
  if (mean(last5, na.rm=T) > (mean(co, na.rm=T)+sd(co, na.rm=T)))  {
    text(1, 1.2, col="white", "+", cex=2.6, font=2) }                # above mean +1se last 5 years
  if (mean(last5, na.rm=T) < (mean(co, na.rm=T)-sd(co, na.rm=T)))  {
    text(1, 1.2, col="white", "-", cex=2.6, font=2) }                # below mean -1se last 5 years

    res <- summary(lm(last5 ~ last5tim))     # calculate linear trend last 5 years
    slope <- coef(res)[2,1] * 5              # slope in per year unit * 5 years (this is total rise over 5-yr run)
    slopelim <- sd(co, na.rm=T)              # is linear trend > 1 se?

    # Note!!  The specific comparison coded here references the linear regression rate of change
    # calculated over the 5-year period, versus the standard deviation of entire time series.
      if (slope >  slopelim)  {
        arrows(0.98, 0.89, x1 = 1.02, y1 = 1.01, length = 0.08, angle = 45, code = 2, lwd = 3)  }   # add up arrow for positive trend
      if (slope <  -slopelim) {
        arrows(0.98, 1.01, x1 = 1.02, y1 = 0.89, length = 0.08, angle = 45, code = 2, lwd = 3) }    # add down arrow for negative trend
      if (slope <= slopelim & slope >= -slopelim) {
        arrows(0.97, 0.95, x1 = 1.03, y1 = 0.95, length = 0.08, angle = 45, code = 3, lwd = 3) }    # double arrow if no trend
                                                  }                  # end analysis of mean and slope
                    }                                                # end trend plot
  }                                                                  # end looping through indicator columns
# end trend plot for long time series -----------------------------------------

# start plot for short time series <= 5 ---------------------------------------
if (length(tim) <= 5) {

  par(mar=c(2.5,5,3,1), xpd=F)

  par(mgp=c(3*yposadj,1,0))

  # plot time series - blank plot to fill in -------------------------------------
  if (sameYscale==T)  {   plot(tim_all, co_all, col = 0, axes = F, xlab = "", ylab = yl, main = mm, ylim = c(ymin, ymax))    }
  if (sameYscale==F)  {   plot(tim_all, co_all, col = 0, axes = F, xlab = "", ylab = yl, main = mm)                          }

  # make red and green polygons --------------------------------------------------
  if (redgreen==T) {
    for (j in 2:length(tim))  {
      polygon(c(tim[j-1], tim[j], tim[j], tim[j-1]),
           y=c(mean(co, na.rm=T), mean(co, na.rm=T), co[j], co[j-1]),
           col=colind[as.numeric(mean(co[(j-1):j], na.rm=T) > mean(co, na.rm=T))+1], border=F) }
    }

# make white square polygon across years ---------------------------------------
  polygon(c(min(tim_all)-5, max(tim_all)+5,
            max(tim_all)+5, min(tim_all)-5),
          c(mean(co_all, na.rm=T)-sd(co_all, na.rm=T),
            mean(co_all, na.rm=T)-sd(co_all, na.rm=T),
            mean(co_all, na.rm=T)+sd(co_all, na.rm=T),
            mean(co_all, na.rm=T)+sd(co_all, na.rm=T)), col="white", border=T)

# plot the points or the lines -----------------------------------------
  tstep <- round(mean(diff(tim_all)), 3)   # determine time step
  if (type == "ptsOnly")  {
    points(tim_all, co_all, pch=20, cex=1.5)
  }            # plot time series - points
  if (type == "")  {
    if (round(mean(diff(tim)), 3) > tstep)  {
      points(tim_all, co_all, pch=20, cex=0.75)   # if gaps between time steps, plot small points because lines may not appear
    }
    if (round(mean(diff(tim)), 3) == mean(diff(tim_all)))  {
      points(tim_all, co_all, pch=20, cex=1.5)    # if no gaps between time steps, plot larger pts because lines will connect all
    }
    inc <- which(round(diff(tim), 3) == tstep)  # which time steps are equal?
    for (k in inc)  {
      lines(tim_all[k:(k+1)], co_all[k:(k+1)], lwd=2)  # plot time series - lines for yearly steps only
    }  }
  if (type == "allLines")  {
    lines(tim, co, lwd=2)               # plot time series - lines for all years
    points(tim_all, co_all, pch=20, cex=0.75)
  }

# add mean and SE parallel lines -----------------------------------------------
  abline(h=mean(co, na.rm=T), lty=8)
  abline(h=mean(co, na.rm=T)+sd(co, na.rm=T), lty=1)
  abline(h=mean(co, na.rm=T)-sd(co, na.rm=T), lty=1)

  axis(1, at=tim)
  axis(1, at=seq(1900, 2050, 1), tck=-0.015, lab=rep("", 151))                                                 # add axes
  axis(2, las=2); box()

# reset plotting params if additional panels to be plotted and trend analysis set to TRUE
  if (length(coltoplot) > 1 & trendAnalysis==T)  {
    par(mar=c(0,0,0,0), xpd=F)
    plot(1, col="white", axes=F, xlab="", ylab="")     }
    }        # end plot for short time series <= 5
   }         # end missing values check
  }          # end looping through indicator columns

# end plot ----------------------

# for pdf output ---------------------------------------------------------------
if (outtype=="pdf")  {
  dev.copy(pdf, filnam, width=((wid+10)/7)*plotcolnum2/1.3, height=hgtadj*(3.5*plotrownum2)/1.3)  #, pointsize=12, res=72*4)
  dev.off()
  }                                                                # close graphics device if pdf

  if (outtype != "") {  dev.off() }   # close graphics device if png
}

knitr::opts_chunk$set(echo = F, warning = F, message = F)                       # fix suggested by B. Best

# end of function

