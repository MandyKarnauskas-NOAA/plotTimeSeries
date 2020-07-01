
#' Plotting indicator time series
#'
#' This function is for standardized plotitng of indicator time series for Ecosystem Status Reports.
#' Typical formatting used by Alaska Report Card and CC IEA
#'   Plots time series, values above and below 1 S.D., and mean
#'   Highlights last 5 years of data and shows pattern in mean and trend
#'
#' @param filename file in standardized indicator reporting csv format.
#'   **Column 1** is time values, **columns 2** and on are indicator data.
#'   **Row 1** must be indicator name (main title of plot).
#'   **Row 2** must be units (y-axis label).
#'   **Row 3** is optional information for sublabel (when multiple panels are used).
#'   Time can be in year (with century), or monthly time step in a variety of formats (e.g, Jan1986, Jan-86, 1986jan)
#'
#' @param coltoplot column numbers of indicator file to plot (defaults to only column 2)
#' @param plotrownum number of rows of plots in multi-panel plot
#' @param plotcolnum number of columns of plots in multi-panel plot
#'        plotrownum and plotcolnum must be used if length(coltoplot) > 1 to specify layout (e.g., 4 panels could be plotrownum = 2, plotcolnum = 2)
#' @param sublabel whether optional descriptive information should appear within main label
#' @param sameYscale set to TRUE for multi-panel plots if consistent y-axis scale is desired.
#' @param yposadj manual adjustment of position of y-axis label
#' @param widadj adjust total width of plot
#' @param hgtadj adjust total height of plot
#' @param type plot type of lines or points
#'         defaults to points, with lines for consecutive years only
#'         for points only use type = "ptsOnly"
#'         for all lines use type="allLines"
#' @param trendAnalysis if TRUE, highlight trend in mean and SD over last 5 years; defaults to TRUE unless fewer than 5 years of data
#' @param propNAallow if proportion of missing values in last 5 years exceeds this value, overrides trendAnalysis=T and will not plot trend (defaults to 0.4)         -
#' @param redgreen set to FALSE to remove red/green shading of anomalies from plot
#' @param anom calculate and plot monthly anomalies; for monthly anomalies use anom = "mon", for standardized monthly anomalies use anom ="stmon"
#' @param outname specify alternate output filename if input name is not desired
#' @param outtype format for output ("png", "pdf")
#'
#' @keywords
#'
#' @export
#' @examples
#'  plotIndicatorTimeSeries("indicator.csv", coltoplot=2:4, plotrownum=3)
#'  plotIndicatorTimeSeries("amo.csv", coltoplot=2, plotrownum=1, plotcolnum=1, adjlabmain=1, sublabel=F)
#'  plotIndicatorTimeSeries("aveSocialConnectedness.csv", coltoplot=2, plotrownum=1, plotcolnum=1, sublabel=F)
#'  plotIndicatorTimeSeries("menhaden_abundance_index.csv", coltoplot=2, plotrownum=1, plotcolnum=1, sublabel=F, yposadj=0.8, widadj=0.8)
#'  plotIndicatorTimeSeries("seagrass_acreage.csv", coltoplot=2:7, plotrownum=3, plotcolnum=2, adjlabmain=1, sublabel=T, widadj=0.6, trendAnalysis=F)

#' function()

plotIndicatorTimeSeries <-  function(filename, coltoplot=2, plotrownum = 1, plotcolnum = 1,
                                        sublabel=F, sameYscale=F, yposadj=1, widadj=1, hgtadj=1, type="",
                                            trendAnalysis=T, propNAallow= 0.60, redgreen=T, anom="none",
                                                outname=NA, outtype="png")  {

# read in file --------------------------------------------------------
d1 <- read.table(filename, header=F, sep=",", skip=0, quote="", stringsAsFactors = FALSE)   # load data file
d <-  read.table(filename, header=F, sep=",", skip=3, quote="", stringsAsFactors = FALSE)   # load data file labels
d <-  d[rowSums(d[2:ncol(d)], na.rm=T) != 0,]                                   # remove rows with no data

# convert dates to standardized format --------------------
formatlis <- c("%d%b%Y", "%d%b%y", "%d-%b-%y", "%d-%b-%Y", "%d%y%b")  # list of formats

if (class(d$V1[1]) == "integer" & nchar(d$V1[1]) <= 4) {              # is time column values of years?
  monthly <- FALSE                                                     # if so, monthly F and set time to year
  tim_all <- d$V1
  }  else  {                                                          # else need to find and extract month format
  monthly <- TRUE
  if (is.na(as.Date(d$V1[1], tryFormats = formatlis, optional = TRUE))) {   # if no day available, add it manually
    d$V1 <- paste0("1-", d$V1)                                              # adding a day to date string
    datelis <- as.Date(d$V1, tryFormats = formatlis)                        # convert date
      } else {
  datelis <- as.Date(d$V1, tryFormats = formatlis)                          # if day is available then convert date
      }
  }

if (monthly==TRUE) {                                                        # if monthly, convert to decimal date
  tim_all <- as.numeric(substr(datelis, 1, 4)) + ((as.numeric(strftime(datelis, format = "%j")) - 1) / 365)
}

# adjustment for width ---------------------------------------------------
  if (monthly==F) { wid <- length(tim_all)*2 }  else  { wid <- length(tim_all)/6 }
  if (length(tim_all) <= 10 & length(tim_all) > 5) {  wid <- wid*2  }
  if (length(tim_all) <= 5)  {  wid <- wid*3  }
  wid <- wid * widadj     #  set adjusted width if specified

# set graphics specifications based on number of panels ------------------
  if (plotcolnum + plotrownum > 2)  { plotcolnum2 <- plotcolnum*0.65; plotrownum2 <- plotrownum*0.65 }  else
                                    { plotcolnum2 <- plotcolnum; plotrownum2 <- plotrownum }

# adjust name for output graphic, if specified ------------------------------
  if (is.na(outname))  {  filnam <- paste(c(unlist(strsplit(filename, ".csv"))), ".", outtype, sep="") }   else   {
                          filnam <- outname }

# adjust plot size for extra long labels ------------------------------------
  if (sublabel==T) { mm <- paste(as.character(d1[1,max(coltoplot)]), "\n", as.character(d1[3,max(coltoplot)]), sep="") } else {
                     mm <- d1[1,max(coltoplot)] }
    longlabs <- max(nchar(as.character(mm)), nchar(as.character(d1[2,max(coltoplot)])))
  if ( longlabs > 30  )  {
    wid <- longlabs/30 * wid
    hgtadj <- longlabs/30 * hgtadj   }

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
      moref <- (match(tolower(molis), tolower(month.abb)))
      moav  <- tapply(co_all, moref, mean, na.rm=T)
        for (m in 1:12) {
          co_all[which(moref==m)] <- co_all[which(moref==m)] - moav[m]    }
                      }

  # calculate standardized monthly anomalies if specified ------------------
    if (anom=="stmon")  {
      moref <- (match(tolower(molis), tolower(month.abb)))
      moav  <- tapply(co_all, moref, mean, na.rm=T)
      most  <- tapply(co_all, moref, sd, na.rm=T)
        for (m in 1:12) {
          co_all[which(moref==m)] <- (co_all[which(moref==m)] - moav[m])/most[m]   }
                    }

  # in case of missing values in column -------------------------------------
  if (sum(!is.na(co_all)) == 0) {  plot.new(); plot.new()  }  else  {

  tim <- tim_all[!is.na(co_all)]        # for dealing with missing values
  co <- co_all[!is.na(co_all)]

# start data plot -----------------------------------------------------------
if (length(tim) > 5) {                  # plotting if more than 5 data points

  if (trendAnalysis==T)  {  par(mar=c(2.5,5,3,0), xpd=F)  }  else  {  par(mar=c(2.5,5,3,1), xpd=F)  }

  par(mgp=c(3*yposadj,1,0))

  if (sublabel==T) { mm <- paste(as.character(d1[1,i]), "\n", as.character(d1[3,i]), sep="") } else {
                     mm <- d1[1,i] }                               # create y-axis label
  yl <- d1[2,i]
    if (anom=="mon")   { yl <- paste(yl, "\n", "monthly anomaly", sep="") }     # adjust label if monthly anomaly
    if (anom=="stmon") { yl <- paste(yl, "\n", "standardized monthly anomaly", sep="") }

  # blank plot with specified y limits
  if (sameYscale==T)  {   plot(tim_all, co_all, col = 0, axes = F, xlab = "", ylab = yl, main = mm, ylim = c(ymin, ymax))    }
  if (sameYscale==F)  {   plot(tim_all, co_all, col = 0, axes = F, xlab = "", ylab = yl, main = mm)                        }

  colind <- c("#FF000080", "#00FF0080")             # shading of anomalies +/- 1 S.D.

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
                  (mean(co_all, na.rm=T)+sd(co_all, na.rm=T))), col="#0000FF20", border=F)       }

    # plot the points or the lines -----------------------------------------
    if (type == "ptsOnly")  {
        points(tim_all, co_all, pch=20, cex=1.5)  }                                                  # plot time series - points
    if (type == "")  {
      if (mean(diff(tim_all)) <= 1)  {
        points(tim_all, co_all, pch=20, cex=0.75) }
      if (mean(diff(tim_all)) >  1)  {
        points(tim_all, co_all, pch=20, cex=1.5) }
        inc <- which(diff(tim_all)==1)
      for (k in inc)  {
        lines(tim_all[k:(k+1)], co_all[k:(k+1)], lwd=2)  }  }                           # plot time series - lines for yearly steps only
      if (type == "allLines")  {
        lines(tim_all, co_all, lwd=2)
       points(tim_all, co_all, pch=20, cex=0.75)   }    # plot time series - lines for all years

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

  if (sublabel==T) { mm <- paste(as.character(d1[1,i]), "\n", as.character(d1[3,i])) } else {
                     mm <- d1[1,i] }
  if (sameYscale==T)  {   plot(tim_all, co_all, col=0, axes=F, xlab="", ylab=d1[2,i], main=mm, ylim=c(ymin, ymax))    }                   # plot time series
  if (sameYscale==F)  {   plot(tim_all, co_all, col=0, axes=F, xlab="", ylab=d1[2,i], main=mm)                        }                   # plot time series

  # make red and green polygons --------------------------------------------------

  colind <- c("#FF000080", "#00FF0080")           # shading of anomalies +/- 1 S.D.

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

# plot the points or the lines -------------------------------------------------
  if (type == "ptsOnly")  {
      points(tim_all, co_all, pch=20, cex=1.5)  }                                                  # plot time series - points
  if (type == "")  {
    if (mean(diff(tim_all)) <= 1)  {
      points(tim_all, co_all, pch=20, cex=0.75) }
    if (mean(diff(tim_all)) >  1)  {
      points(tim_all, co_all, pch=20, cex=1.5) }
      inc <- which(diff(tim_all)==1)
      for (k in inc)  {
        lines(tim_all[k:(k+1)], co_all[k:(k+1)], lwd=2)  }  }                                           # plot time series - lines for yearly steps only
  if (type == "allLines")  {
        lines(tim_all, co_all, lwd=2)
        points(tim_all, co_all, pch=20, cex=0.75)   }              # plot time series - lines for all years

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
  }          # end missing values check
  }          # end looping through indicator columns

# end plot ----------------------

# for pdf output ---------------------------------------------------------------
if (outtype=="pdf")  {
  dev.copy(pdf, filnam, width=((wid+10)/7)*plotcolnum2/1.3, height=hgtadj*(3.5*plotrownum2)/1.3)  #, pointsize=12, res=72*4)
  dev.off()    }                                                                # close graphics device if pdf

  if (outtype != "") {  dev.off() }   # close graphics device if png
}

knitr::opts_chunk$set(echo = F, warning = F, message = F)                       # fix suggested by B. Best

# end of function

