#' Plotting indicator time series
#'
#' This function is for converting deprecated .csv files into an object of class 'indicatordata'.
#'
#' @param filename a .csv file with indicator names in the initial rows of the file, and dates and indicator values in columns below.
#' For default settings, see details below.
#' @param labrows a vector indicating the rows at the top of the file containing label information. Typically there are three rows with the indicator name, measurement unit, and sublabel.
#' @param datecols an integer which represents the column with the vector of dates.
#' @param indcols an integer of vector indicating which columns of the file contain the indicator values.
#' @param ulimcols an integer of vector indicating which columns of the file contain the upper limits of the confidence intervals, if display is desired.
#' @param llimcols an integer of vector indicating which columns of the file contain the lower limits of the confidence intervals, if display is desired.
#' @param default a logical value indicating whether the .csv file is in default format, in which case the other options do not need to
#'
#' The "default" version of the .csv file should be formatted as follows:
#' \itemize{
#'  \item **Row 1** contains a character string of the indicator name (main title of plot).
#'  \item **Row 2** contains a character string of the indicator units (y-axis label).
#'  \item **Row 3** contains an optional character string with information on the sublabel (when multiple panels are used).
#'  \item **Column 1** is time values, **columns 2** and on are indicator data.  The first three values of column 1 are NA.
#'  \item Time can be in year (with century), or monthly time step in a variety of formats (e.g, Jan1986, Jan-86, 1986jan), including or excluding day of month.
#'  }
#' @export
conv2indicatordata <-  function(filename, labrows = 1, datecols = 1, indcols = 2, ulimcols = NA, llimcols = NA, default = F)  {

# read in file --------------------------------------------------------

if (default == T)  {
  labrows <- 1:3
  datecols <- 1
  indcols <- 2: ncol(read.table(filename, header=F, sep=",", skip = 0, quote="", stringsAsFactors = FALSE))
  ulimcols <- NA
  llimcols <- NA
}

if (sum(is.na(ulimcols)) == 1) { ulimcols <- c() }
if (sum(is.na(llimcols)) == 1) { llimcols <- c() }

indnames <- read.table(filename, header=F, sep=",", skip = 0, quote="", stringsAsFactors = FALSE)[labrows, indcols] # load data file labels
datdata <- read.table(filename, header=F, sep=",", skip = max(labrows), quote="", stringsAsFactors = FALSE)[, datecols]     # load list of dates
inddata <- read.table(filename, header=F, sep=",", skip = max(labrows), quote="", stringsAsFactors = FALSE)[, indcols]      # load indicators
ulidata <- read.table(filename, header=F, sep=",", skip = max(labrows), quote="", stringsAsFactors = FALSE)[, ulimcols]     # load upper confidence intervals
llidata <- read.table(filename, header=F, sep=",", skip = max(labrows), quote="", stringsAsFactors = FALSE)[, llimcols]     # load lower confidence intervals

indnames <- data.frame(indnames)
inddata <- data.frame(inddata)
ulidata <- data.frame(ulidata)
llidata <- data.frame(llidata)

names(indnames) <- c()
names(inddata) <- c()
if (ncol(ulidata) >0)  {
  names(ulidata) <- c()
  names(llidata) <- c()
  }

if (nrow(indnames) == 1) { indnames <- data.frame(rbind(indnames, rep(NA, ncol(inddata))))   }
if (nrow(indnames) == 2) { indnames <- data.frame(rbind(indnames, rep(NA, ncol(inddata))))
                                                  names(indnames) <- c()           }

if (ncol(ulidata) > 0)  {
  s <- list(labels = indnames, indicators = inddata, datelist = datdata, ulim = ulidata, llim = llidata)
   } else {
  s <- list(labels = indnames, indicators = inddata, datelist = datdata)
  }

class(s) <- "indicatordata"

return(s)

}

