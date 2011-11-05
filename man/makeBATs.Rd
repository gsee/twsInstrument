\name{makeBATs}
\alias{makeBATs}
\title{Build Bid Ask Trade object}
\usage{
  makeBATs(Symbols, base_dir = "/mnt/W", env = .GlobalEnv,
  ndays)
}
\arguments{
  \item{Symbols}{chr name of instrument}

  \item{base_dir}{base_dir that holds TRADES, BID, and ASK
  directories (each of those directories should hold
  directories named for instruments.)}

  \item{env}{environment to put BATs in}

  \item{ndays}{optional. If given and numeric,
  \sQuote{from} will become \sQuote{to}-\code{ndays}}
}
\value{
  called for side-effect.  Objects that are stored will
  have Bid, Ask, Trade, Mid, and Volume columns unless
  there are no TRADES data in which case they will have
  Bid, Ask, and Mid columns.
}
\description{
  Reads from disk and merges closing Bid, Ask, Trade data
}
\note{
  used by \code{\link{reqTBBOhistory}}
}
\examples{
\dontrun{
#reqTBBOhistory("RIMM", ndays=5)
makeBATs("RIMM")
}
}
\author{
  Garrett See
}
\seealso{
  \code{\link{getBAT}}, \code{\link{reqTBBOhistory}}
}

