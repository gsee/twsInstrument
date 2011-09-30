\name{define_futures.IB}
\alias{define_futures}
\alias{define_futures.IB}
\title{futures contract contstructor}
\usage{
  define_futures.IB(symbols, exch, expiries =
  as.Date(Sys.time()), currency = "USD", include_expired =
  "1", ...)

  define_futures(symbols, exch, expiries =
  as.Date(Sys.time()), currency = "USD", include_expired =
  "1", src = "IB", ...)
}
\arguments{
  \item{symbols}{chr name of underlying}

  \item{exch}{chr name of exchange
  (\sQuote{GLOBEX},\sQuote{CFE}, \sQuote{eCBOT}, etc)}

  \item{expiries}{vector of type chr. YYYYMM or YYYYMMDD}

  \item{currency}{denominating currency. default "USD"}

  \item{include_expired}{is the contract expired?
  \sQuote{0} for no, \sQuote{1} for yes.}

  \item{src}{currently only sQuoteIB}

  \item{\dots}{any other parameters to pass to twsFuture:
  \sQuote{primary}, \sQuote{currency}, \sQuote{local},
  \sQuote{multiplier}, \sQuote{conId}}
}
\value{
  names of instruments that were created. Called for
  side-effect
}
\description{
  Define one or many futures contracts for a given
  underlying.
}
\details{
  \code{define_futures} is currently just a wrapper that
  calls \code{define_futures.IB}

  \code{define_futures.IB} loops through all of the symbols
  and expiries calling twsInstrument.  The exchange has to
  be the same for all of the futures you are trying to
  define.  If you want to define futures from different
  exchanges, you must make a separate call for each.

  The name of the future contract will usually be the value
  of \sQuote{local} (retrieved with a call to
  reqContractDetails) with an underscore separating the
  root symbol and the suffix_id.  If \code{symbol} is not
  found in \sQuote{local} (e.g. \sQuote{VIX} is not in
  \sQuote{VXU1}) the suffix_id will be of format MMMYY
  (e.g. JUN11)
}
\examples{
\dontrun{
define_futures("ES","GLOBEX",c("201109","201112"))
define_futures("ES","GLOBEX","201103",include_expired="1")

months <- c(paste("0",1:9,sep=""),paste(10:12))
define_futures("VIX","CFE",paste("2010",months,sep=""),include_expired="1")

define_futures("YM","eCBOT","201109")

define_futures(c("NKD","NQ"), "GLOBEX", "201109")
}
}
\author{
  Garrett See
}
\seealso{
  twsInstrument, twsFuture, future, future_series,
  instrument
}

