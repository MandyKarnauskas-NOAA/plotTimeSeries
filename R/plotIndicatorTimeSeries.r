#' Plotting indicator time series
#'
#' This function is for standardized plotitng of indicator time series, such as those found in NOAA's Ecosystem Status Reports.
#' The function imports a data frame of indicator values and dates and plots the time series, denoting mean and values above and below one standard deviation from the mean.
#' An optional trend analysis highlights changes in the mean and slope of the time series in the last 5 years of data (or other specified window).
#'
#' @param indobject an object of class `indicatordata`. See details below. For deprecated .csv format, see \link{conv2indicatordata}.
#' @param coltoplot an integer or integer list defining the column numbers of indicator file to plot.  Defaults to a single column of data in column 1.
#' @param plotrownum an integer defining the number of rows of plots in a multi-panel plot.
#' @param plotcolnum an integer defining the number of columns of plots in multi-panel plot.
#' @param sublabel a logical value indicating whether optional descriptive information should appear within main label.
#' @param sameYscale a logical value indicating whether a consistent y-axis scale is desired across multiple panels.
#' @param yposadj a number specifying manual adjustment of position of y-axis label; values > 1 move text further from the axis.
#' @param widadj expansion factor to adjust the total width of plot.
#' @param hgtadj expansion factor to adjust the total height of plot.
#' @param type a character string indicating which type of plot is desired. Defaults to points, with lines for consecutive time steps only.
#'         `ptsOnly` and `allLines` can be specified for only points or only lines, respectively.
#' @param CItype is a character string indictating which type of confidence intervals are desired.
#'          Defaults to shaded bands filling in the area between the upper and lower intervals, or use `pts` for an interval plot.
#' @param trendAnalysis a logical value indicating whether to highlight the trend in mean and slope over last 5 years; defaults to TRUE unless fewer than 5 years of data.
#' @param tWindow an integer defining the number of years over which the recent trend analysis should be calculated; defaults to last 5 years.
#' @param propNAallow if fraction denoting the allowable proportion of missing values in last 5 years; when the proportion of NAs exceeds this value, trend analysis will not appear (defaults to 0.5)
#' @param redgreen a logical value indicating whether to remove red/green shading of anomalies from plot.
#' @param anom a character string indicating whether to convert indicator to monthly anomalies.  One of `none`, `mon` (monthly anomalies) or `stmon` (standardized monthly anomalies) can be used.
#' @param dateformat a format as defined in \link{strptime} which is used for monthly time steps only.  Can be full date or month/year combination only.
#' @param outname a character string specifying alternate output filename; defaults to using the object name.
#' @param outtype a character string specifying format for output, if manual saving is not desired.  Options are `bmp`, `jpeg`, `png`, `tiff`, or `eps`.
#' @param ... Arguments to be passed to methods such as specifications for \link{plot} or \link{axis}, particularly `cex.axis`, `cex.main`, and `cex.lab` for sizing labels.
#'
#' @note
#' A deprecated version of this code allowed for .csv format to be input.  These can now be converted using \link{conv2indicatordata}.
#'
#' Data must be input as a list object of class `indicatordata` which contains at least three attributes: `labels`, `indicators`, and `datelist`.
#' \itemize{
#' \item `labels` is a \link{data.frame} containing up to 3 rows and the number of columns equal to the number of indicators.
#'        **Row 1** specifies the indicator name, **row 2** specifies the measurement unit, and **row 3** specifies a sublabel.
#' \item `indicators` is a \link{data.frame} containing the indicator data, with each column containing values for a given indicator and time step.
#' \item `datelist` is a \link{vector} containing the time steps at which the indicators were measured, in chronological order.
#' The length of `datelist`` must be equal to the number of rows in `indicators`.
#'  \item Time can be in year (with century), or monthly time step in a variety of formats (e.g, Jan1986, Jan-86, 1986jan), including or excluding day of month.
#'  \item Optional attributes: `ulim` and `llim` are represent, respectively, the upper and lower confidence intervals.
#'  They must be in the format of a \link{data.frame}, with equal dimensions to `indicators`.  The first column of `ulim` corresponds to the first column of `indicators`, and so on.
#'  }
#'
#' @references
#' <https://www.aoml.noaa.gov/ocd/ocdweb/ESR_GOMIEA/report/GoM_EcosystemStatusReport2017.pdf>
#'
#' @examples
#' ## plot a single indicator
#'  head(menhaden)
#'  plotIndicatorTimeSeries(menhaden)
#'
#'  ## plot a four-panel plot of indicator values reported at different locations
#'  plotIndicatorTimeSeries(bottomDO, coltoplot = 1:4, sublabel = T, sameYscale = T)
#'
#'  ## plot an indicator, compare with plot of standardized anomalies
#'  par(mfrow = c(2, 1))
#'  plotIndicatorTimeSeries(NPP)
#'  plotIndicatorTimeSeries(NPP, anom = "stmon")
#'
#'@export
plotIndicatorTimeSeries <-  function(indobject, coltoplot = 1, plotrownum = 1, plotcolnum = 1,
                                     sublabel = F, sameYscale = F, yposadj = 1, widadj = 1, hgtadj = 1, type = "default", CItype = "band",
                                     trendAnalysis = T, tWindow = 5, propNAallow = 0.60, redgreen = T, anom = "none",
                                     dateformat = "%b%Y", outname = NA, outtype = "", ...)  {

# dependencies ------------------------------------------

  if ("fields" %in% installed.packages() == FALSE) { install.packages("fields") }
  library(fields)

# read in file --------------------------------------------------------

if (class(indobject) != "indicatordata")  {  print("Need to use indicatordata object")  }

indobject$labels <- data.frame(indobject$labels, stringsAsFactors = F)

d1 <- indobject$labels            # use former naming conventions
d <-  indobject$indicators
dd <- indobject$datelist

# convert dates to standardized format --------------------
formatlis <- c(dateformat)  # list of formats

if (class(dd[1]) == "integer" & nchar(dd[1]) <= 4) {              # is time column values of years?
  monthly <- FALSE                                                     # if so, monthly F and set time to year
  tim_all <- dd
  }  else  {                                                          # else need to find and extract month format
  monthly <- TRUE
  if (is.na(as.Date(dd[1], tryFormats = formatlis, optional = TRUE))) {   # if no day available, add it manually
    dd <- paste0("1-", dd)                                              # adding a day to date string
    datelis <- as.Date(dd, tryFormats =  paste0("%d-", formatlis))                        # convert date
      } else {
    datelis <- as.Date(dd, tryFormats =  paste0("%d-", formatlis))                        # if day is available then convert date
      }
  }

if (monthly==TRUE) {                                                        # if monthly, convert to decimal date
  tim_all <- as.numeric(substr(datelis, 1, 4)) + ((as.numeric(strftime(datelis, format = "%j")) - 1) / 365)
}

# adjustment for width ---------------------------------------------------
  if (monthly == F) { wid <- length(tim_all) * 1.5 }  else  { wid <- length(tim_all) / 6 }
  if (length(tim_all) <= 10 & length(tim_all) > 5) {  wid <- wid*2  }
  if (length(tim_all) <= 5)  {  wid <- wid*3  }
  wid <- wid * widadj     #  set adjusted width if specified

# set graphics specifications based on number of panels ------------------

  if (length(coltoplot) > (plotrownum * plotcolnum)) {  plotcolnum <- 2; plotrownum <- ceiling(length(coltoplot) / 2)  }
  #  if (length(coltoplot) < (plotrownum * plotcolnum)) {  plotrownum <- length(coltoplot)  }

  if (plotcolnum + plotrownum > 2)  { plotcolnum2 <- plotcolnum*0.65; plotrownum2 <- plotrownum*0.65 }  else
                                    { plotcolnum2 <- plotcolnum; plotrownum2 <- plotrownum }

# adjust name for output graphic, if specified ------------------------------
  if (is.na(outname))  {
        filnam <- paste0(indobject$labels[1,1], ".", outtype)
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
if (outtype == "png")  {
  png(filename = filnam, units = "in", pointsize = 12, res = 72*10,
      width = ((wid+10)/7)*plotcolnum2/1.3, height = hgtadj * (3.5*plotrownum2)/1.3) }

if (outtype == "bmp")  {
    bmp(filename = filnam, units = "in", pointsize = 12, res = 72*5,
        width = ((wid+10)/7)*plotcolnum2/1.3, height = hgtadj * (3.5*plotrownum2)/1.3) }

if (outtype == "jpeg")  {
    jpeg(filename = filnam, units = "in", pointsize = 12, quality = 100, res = 72*10,
        width = ((wid+10)/7)*plotcolnum2/1.3, height = hgtadj * (3.5*plotrownum2)/1.3) }

if (outtype == "tiff")  {
    tiff(filename = filnam, units = "in", pointsize = 12, compression = "none", res = 72*5,
        width = ((wid+10)/7)*plotcolnum2/1.3, height = hgtadj * (3.5*plotrownum2)/1.3) }

if (outtype == "ps")  {
  postscript(file = filnam,
             width=((wid+10)/7)*plotcolnum2/1.3, height=hgtadj*(3.5*plotrownum2)/1.3) }  #, pointsize=12, res=72*4)

# layout for single or multi-panel plots ------------------------------------
  nf <- layout(matrix(c(1:(plotrownum*plotcolnum*2)), plotrownum, plotcolnum*2, byrow = TRUE), rep(c(wid/5, 1), plotcolnum), rep(4, plotrownum))
#  layout.show(nf)

# layout for single plots with fewer than 5 data points or no trend analysis ---
  if (length(coltoplot)==1 & length(tim_all) <= 5 | trendAnalysis==F)  {
  nf <- layout(matrix(c(1:(plotrownum*plotcolnum)), plotrownum, plotcolnum, byrow = TRUE), rep(c(wid/5), plotcolnum), rep(3, plotrownum))
          }
#  layout.show(nf)

# get common yscale -------------------------------------------------------------
  if (length(indobject$llim) > 0) {
    ymin_st <- min(cbind(d[, coltoplot], indobject$llim[, coltoplot]), na.rm=T) * 0.99
    ymax_st <- max(cbind(d[, coltoplot], indobject$ulim[, coltoplot]), na.rm=T) * 1.01
  } else {
    ymin_st <- min(d[, coltoplot], na.rm=T) * 0.99
    ymax_st <- max(d[, coltoplot], na.rm=T) * 1.01
  }

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

# insert subscript for m2 units --------------------------------------------

expflag <- 0

if (grepl("^", yl) == TRUE)  {
  yl1 <- str_replace_all(yl, " ", "~")
  yl <- yl1
  expflag <- 1
  }

colind <- c("#FF000080", "#00FF0080")             # shading of anomalies +/- 1 S.D.

# in case of missing values in column -------------------------------------
  if (sum(!is.na(co_all)) == 0) {  plot.new(); plot.new()  }  else  {

  tim <- tim_all[!is.na(co_all)]        # for dealing with missing values
  co <- co_all[!is.na(co_all)]

  if (length(indobject$llim) > 0) {
    ymin <- min(c(co_all, indobject$llim[, i]), na.rm=T) * 0.99
    ymax <- max(c(co_all, indobject$ulim[, i]), na.rm=T) * 1.01
  } else {
    ymin <- min(cbind(co_all), na.rm=T) * 0.99
    ymax <- max(cbind(co_all), na.rm=T) * 1.01
  }

# start data plot -----------------------------------------------------------
if (length(tim) > 5) {                  # plotting if more than 5 data points

  if (trendAnalysis==T)  {  par(mar=c(2.5,5,3,0), xpd=F)  }  else  {    # set margins based on trend analysis T or F
                            par(mar=c(2.5,5,3,1), xpd=F)  }
  par(mgp=c(3*yposadj,1,0))

  # blank plot with specified y limits
  if (expflag == 1) {
    if (sameYscale == T)  {   plot(tim_all, co_all, col = 0, axes = F, xlab = "", ylab = parse(text = yl), main = mm, ylim = c(ymin_st, ymax_st), ...)    }
    if (sameYscale == F)  {   plot(tim_all, co_all, col = 0, axes = F, xlab = "", ylab = parse(text = yl), main = mm, ylim = c(ymin, ymax), ...)                        }
  }  else {
    if (sameYscale == T)  {   plot(tim_all, co_all, col = 0, axes = F, xlab = "", ylab = yl, main = mm, ylim = c(ymin_st, ymax_st), ...)    }
    if (sameYscale == F)  {   plot(tim_all, co_all, col = 0, axes = F, xlab = "", ylab = yl, main = mm, ylim = c(ymin, ymax), ...)                        }
  }

  if (redgreen == T) {

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
      polygon(c(max(tim_all, na.rm=T)-tWindow+0.5-as.numeric(monthly)/2.1,
                max(tim_all, na.rm=T)+0.5-as.numeric(monthly)/2.4,
                max(tim_all, na.rm=T)+0.5-as.numeric(monthly)/2.4,
                max(tim_all, na.rm=T)-tWindow+0.5-as.numeric(monthly)/2.1),
                c((mean(co_all, na.rm=T)-sd(co_all, na.rm=T)),
                  (mean(co_all, na.rm=T)-sd(co_all, na.rm=T)),
                  (mean(co_all, na.rm=T)+sd(co_all, na.rm=T)),
                  (mean(co_all, na.rm=T)+sd(co_all, na.rm=T))), col="#0000FF30", border=F)
                  }

  # plot the confidence intervals --------------------------

  if (length(indobject$llim) > 0) {
  ulim_all <- indobject$ulim[, i]
  llim_all <- indobject$llim[, i]
  ulim <-  ulim_all[!is.na(co_all)]
  llim <-  llim_all[!is.na(co_all)]

  if (CItype == "pts")  {
    for (m in 1:length(tim_all))  {
      arrows(tim_all[m], ulim_all[m], x1 = tim_all[m], y1 = llim_all[m], length = 1/wid, angle = 90, code = 3, col = gray(0.4))
    }
  }            # plot time series - points
  if (CItype == "band")  {
    polygon(x = c(tim, tim[length(tim): 1]), y = c(ulim, llim[length(tim): 1]), border = NA, col = "#00000020")   }
  }

  # plot the points or the lines -----------------------------------------
  tstep <- round(mean(diff(tim_all)), 2)   # determine time step
    if (type == "ptsOnly")  {
        points(tim_all, co_all, pch=20, cex=1.5)
        }            # plot time series - points
    if (type == "default")  {
      if (round(mean(diff(tim)), 2) >= tstep)  {
        points(tim_all, co_all, pch=20, cex=0.75)   # if gaps between time steps, plot small points because lines may not appear
        }
      if (round(mean(diff(tim)), 2) == tstep)  {
        points(tim_all, co_all, pch=20, cex=1.5)    # if no gaps between time steps, plot larger pts because lines will connect all
        }
        inc <- tim[which(round(diff(tim), 2) <= tstep)]  # which time steps are equal?
      for (n in inc)  {
        k <- which(tim_all == n)
        lines(tim_all[k:(k+1)], co_all[k:(k+1)], lwd=2)  # plot time series - lines for yearly steps only
      }
        }
      if (type == "allLines")  {
        lines(tim, co, lwd=2)               # plot time series - lines for all years
        points(tim_all, co_all, pch=20, cex=0.75)
      }

    # add parallel lines for mean and sd ---------------------------
    abline(h = mean(co, na.rm=T), lty=8)
    abline(h = mean(co, na.rm=T) + sd(co, na.rm=T), lty=1)
    abline(h = mean(co, na.rm=T) - sd(co, na.rm=T), lty=1)

    # add axes and tick marks ------------------------
    if ((max(tim_all) - min(tim_all)) > 10)  { axis(1, at=seq(1900, 2050, 5), ...) } else {
                                               axis(1, at=seq(1900, 2050, 2), ...) }           # add axis 1
  axis(1, at=seq(1900, 2050, 1), tck=-0.015, lab=rep("", 151), ...)                                                                  # add axis 1 small ticks
  axis(2, las=2, ...); box()                                                                                                         # add axis 2
# end data plot -------------------------------------------------------------

# start trend plot ----------------------------------------------------------
  if (trendAnalysis == T)  {
  par(mar=c(2.5,0,3,0))                                                         #  second panel on mean and trend of last 5 years

  last5 <-     co_all[which(tim_all > max(tim_all)-tWindow)]
  last5tim <- tim_all[which(tim_all > max(tim_all)-tWindow)]

  plot(1, xlim=c(0.5,1.5), ylim=c(0.5, 1.9), col=0, axes=F, xlab="", ylab="")  # create empty plot

  # analyze mean and slope of last 5 years ----------------------------------------------
  ptsiz <- 0.15 / hgtadj
  if (outtype == "") { ptsiz <- ptsiz / 2}
  if (sum(is.na(last5)) / length(last5) < propNAallow)  {            # if proportion of NAs does not exceed limit
    add.image(1, 1.2 + 0.2/hgtadj, ptSolid, col = grey(0:1), image.width = ptsiz, image.height = ptsiz)    # plot point for trend analysis
    if (mean(last5, na.rm=T) > (mean(co, na.rm=T)+sd(co, na.rm=T)))  {
      add.image(1, 1.2 + 0.2/hgtadj, ptPlus, col = grey(0:1), image.width = ptsiz, image.height = ptsiz)        }      # above mean +1se last 5 years
    if (mean(last5, na.rm=T) < (mean(co, na.rm=T)-sd(co, na.rm=T)))  {
      add.image(1, 1.2 + 0.2/hgtadj, ptMinus, col = grey(0:1), image.width = ptsiz, image.height = ptsiz)       }      # below mean -1se last 5 years

    res <- summary(lm(last5 ~ last5tim))     # calculate linear trend last 5 years
    slope <- coef(res)[2,1] * tWindow              # slope in per year unit * 5 years (this is total rise over 5-yr run)
    slopelim <- sd(co, na.rm=T)              # is linear trend > 1 se?

    # Note!!  The specific comparison coded here references the linear regression rate of change
    # calculated over the specified window in years, versus the standard deviation of entire time series.
    if (slope >  slopelim)  {
      add.image(1, 1.2 - 0.2/hgtadj, arrowUp, col = grey(0:1), image.width = ptsiz, image.height = ptsiz)      }   # add up arrow for positive trend
    if (slope <  -slopelim) {
      add.image(1, 1.2 - 0.2/hgtadj, arrowDown, col = grey(0:1), image.width = ptsiz, image.height = ptsiz)    }    # add down arrow for negative trend
    if (slope <= slopelim & slope >= -slopelim) {
      add.image(1, 1.2 - 0.2/hgtadj, arrowSide, col = grey(0:1), image.width = ptsiz, image.height = ptsiz)    }    # double arrow if no trend
                            }                  # end analysis of mean and slope
                    }                                                # end trend plot
  }                                                                  # end looping through indicator columns
# end trend plot for long time series -----------------------------------------

# start plot for short time series <= 5 ---------------------------------------
if (length(tim) <= 5) {

  par(mar=c(2.5,5,3,1), xpd=F)

  par(mgp=c(3*yposadj,1,0))

  # plot time series - blank plot to fill in -------------------------------------
  if (expflag == 1) {
    if (sameYscale == T)  {   plot(tim_all, co_all, col = 0, axes = F, xlab = "", ylab = parse(text = yl), main = mm, ylim = c(ymin_st, ymax_st), ...)    }
    if (sameYscale == F)  {   plot(tim_all, co_all, col = 0, axes = F, xlab = "", ylab = parse(text = yl), main = mm, ylim = c(ymin, ymax), ...)                        }
  }  else {
    if (sameYscale == T)  {   plot(tim_all, co_all, col = 0, axes = F, xlab = "", ylab = yl, main = mm, ylim = c(ymin_st, ymax_st), ...)    }
    if (sameYscale == F)  {   plot(tim_all, co_all, col = 0, axes = F, xlab = "", ylab = yl, main = mm, ylim = c(ymin, ymax), ...)                        }
  }

  # plot the confidence intervals --------------------------

  if (length(indobject$llim) > 0) {
    ulim_all <- indobject$ulim[, i]
    llim_all <- indobject$llim[, i]
    ulim <-  ulim_all[!is.na(co_all)]
    llim <-  llim_all[!is.na(co_all)]

    if (CItype == "pts")  {
      for (m in 1:length(tim_all))  {
        arrows(tim_all[m], ulim_all[m], x1 = tim_all[m], y1 = llim_all[m], length = 0.05, angle = 90, code = 3, col = gray(0.4))
      }
    }            # plot time series - points
    if (CItype == "band")  {
      polygon(x = c(tim, tim[length(tim): 1]), y = c(ulim, llim[length(tim): 1]), border = NA, col = "#00000020")   }
  }

  # plot the points or the lines -----------------------------------------
  tstep <- round(mean(diff(tim_all)), 2)   # determine time step
  if (type == "ptsOnly")  {
    points(tim_all, co_all, pch=20, cex=1.5)
  }            # plot time series - points
  if (type == "default")  {
    if (round(mean(diff(tim)), 2) > tstep)  {
      points(tim_all, co_all, pch=20, cex=0.75)   # if gaps between time steps, plot small points because lines may not appear
    }
    if (round(mean(diff(tim)), 2) == tstep)  {
      points(tim_all, co_all, pch=20, cex=1.5)    # if no gaps between time steps, plot larger pts because lines will connect all
    }
    inc <- tim[which(round(diff(tim), 2) <= tstep)]  # which time steps are equal?
    for (n in inc)  {
      k <- which(tim_all == n)
      lines(tim_all[k:(k+1)], co_all[k:(k+1)], lwd=2)  # plot time series - lines for yearly steps only
      }
    }
  if (type == "allLines")  {
    lines(tim, co, lwd=2)               # plot time series - lines for all years
    points(tim_all, co_all, pch=20, cex=0.75)
  }

# add mean and SE parallel lines -----------------------------------------------
  abline(h=mean(co, na.rm=T), lty=8)
#  abline(h=mean(co, na.rm=T)+sd(co, na.rm=T), lty=1)
#  abline(h=mean(co, na.rm=T)-sd(co, na.rm=T), lty=1)

  axis(1, at=tim, ...)
  axis(1, at=seq(1900, 2050, 1), tck=-0.015, lab=rep("", 151), ...)                                                 # add axes
  axis(2, las=2, ...); box()

# reset plotting params if additional panels to be plotted and trend analysis set to TRUE
  if (length(coltoplot) > 1 & trendAnalysis==T)  {
    par(mar=c(0,0,0,0), xpd=F)
    plot(1, col="white", axes=F, xlab="", ylab="")     }
    }        # end plot for short time series <= 5
   }         # end missing values check
  }          # end looping through indicator columns

# end plot ----------------------

  if (outtype != "") {  dev.off()  }   # close graphics device if png or eps
}



