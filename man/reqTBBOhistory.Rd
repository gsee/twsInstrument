\name{reqTBBOhistory}
\alias{reqTBBOhistory}
\title{Download and save to disk historical Bid, Ask, and Trades data}
\usage{
  reqTBBOhistory(Symbols, base_dir = "/mnt/W", ndays = 95,
    endDateTime, tws, barSize = "1 min", useRTH = "1",
    env = .GlobalEnv, save = FALSE, chronological = FALSE)
}
\arguments{
  \item{Symbols}{names of instruments for which to request
  data}

  \item{base_dir}{base_dir that contains sub-directories
  \sQuote{BID}, \sQuote{ASK}, \sQuote{TRADES}, and
  \sQuote{BAT}}

  \item{ndays}{total number of days to retrieve. Default is
  95. Max is 365}

  \item{endDateTime}{\sQuote{YYYYMMDD H:M:S}}

  \item{tws}{twsConnection object (Not yet implemented)}

  \item{barSize}{barSize to retrieve. see
  \code{\link[IBrokers]{reqHistoricalData}}}

  \item{useRTH}{Use regular trading hours? 1 for TRUE, 0
  for FALSE}

  \item{env}{where to put the final BATVM or BAM xts
  object}

  \item{save}{logical. Save to disk the BATVM or BAM
  object?}

  \item{chronological}{should the requests be made in
  chronological order. Default is FALSE because, at least
  for futures and options, requests for old data are more
  likely to fail than requests for recent data.  For
  stocks, it might make sense to use TRUE so that if a
  request is interrupted, you will not have gaps in your
  data. \code{update.data} uses \code{chronological=TRUE}}
}
\value{
  called for side-effect. Returns the names of Symbols.
}
\description{
  This will make several requests for historical data from
  IBrokers repecting the historical data request
  limitations.
}
\details{
  Experimental code -- see Note section

  Important: You should have a base directory that contains
  these sub-directories: \sQuote{BID}, \sQuote{ASK},
  \sQuote{TRADES}, and \sQuote{BAT}.

  The default \code{base_dir} is \dQuote{/mnt/W}.  If you
  do not have a directory there, you should either create
  it, or not use the default value for \code{base_dir}.

  \code{reqTBBOhistory} will make requests for 5 days of
  bid, ask, and trade data and then pause for 30 seconds.
  In between requests, all BID, ASK, and TRADES data are
  saved into the respective subdirectories. Data are saved
  in rda files named with the format YYYY.MM.DD.Symbol.rda.
  After all requests have been made (approx. 37 minutes),
  the data are re-read and merged together into BATMV
  format and (if \code{save=TRUE}) stored in the BAT
  directory

  IBrokers does not disseminate TRADES data for FX (CASH).
  Therefore, \code{reqTBBOhistory} will make 2 requests
  every 20 seconds (BID and ASK data) instead of 3 requests
  every 30 seconds.

  For best results, you should define your instruments
  before calling this function.

  This has only been tested on a debian-based linux system.

  IB limits historical data requests to 6 every 60 seconds
}
\note{
  Warning: Interactive Brokers *back adjusts* their data
  for stock splits (but not for dividends). If you are
  storing stock data, you should probably unadjust the last
  year's worth of data, then, make sure you download the
  most recent data each day, being careful not to overwrite
  any data already stored on disk.  This way you could have
  all unadjusted data that you could adjust (with
  qmao:::adjustIntraday.yahoo, for example).  Otherwise,
  you'll have some data that is split-adjusted (or
  partially split-adjusted) and some that isn't.
}
\examples{
\dontrun{
library(twsInstrument)
dir.create("tmpdata/BID", recursive=TRUE)
dir.create("tmpdata/ASK")
dir.create("tmpdata/TRADES")
dir.create("tmpdata/BAT")
define_stocks("SPY")
# get the last 20 days of minutely data
# (this will take ~2 minutes)
reqTBBOhistory("SPY",
                base_dir="tmpdata",
                ndays=20,
                save=TRUE,
                chronological=TRUE)
# Now to prove it's on disk, delete from workspace
# and load it.
rm("SPY")
getSymbols("SPY", src='FI', dir='tmpdata/BAT',
            from=Sys.Date() - 20, verbose=TRUE)

# If you had called reqTBBOhistory with save=FALSE (the default)
# or if your "BAT" directory were wiped out, you can use
# the unexported makeBATs function to get the BID, ASK, TRADES
# data from disk and rebuild the BAT data.
unlink("tmpdata/BAT")
twsInstrument:::makeBATs("SPY", base_dir='tmpdata', ndays=20)
rm("SPY")
getSymbols("SPY", src='FI', dir='tmpdata/BAT',
            from=Sys.Date() - 20, verbose=TRUE)

# Clean up -- Delete everything
unlink("tmpdata", recursive=TRUE)
}
}
\author{
  Garrett See
}
\references{
  InteractiveBrokers \url{www.interactivebrokers.com}

  IB API
  \url{http://interactivebrokers.com/php/apiUsersGuide/apiguide.htm}

  IB Historic Data Limitations
  \url{http://individuals.interactivebrokers.com/php/apiUsersGuide/apiguide/api/historical_data_limitations.htm#XREF_93621_Historical_Data}
}
\seealso{
  \code{twsInstrument:::update.data} (unexported due to
  possibility of name change, and current lack of
  documentation),

  \code{\link{getBAT}}, \code{\link{makeBATs}},
  \code{\link[IBrokers]{reqHistoricalData}},
  \code{\link[IBrokers]{reqHistory}}
}

