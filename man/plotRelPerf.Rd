\name{plotRelPerf}
\alias{plotRelPerf}
\title{
Plot several stocks on the same chart}
\description{
Plot the cumulative return of the closing prices of several stocks
on the same chart.
}
\usage{
plotRelPerf(symbols, prefer=NULL, env=.GlobalEnv)
}
\arguments{
  \item{symbols}{character strings. vector of names of xts objects containing Close column}
  \item{prefer}{which price column to use}
  \item{env}{the environment that holds the xts data}
}
\details{
A VERY simple wrapper for ts.plot.

prefer is passed to \code{getPrice}

Does not account for dividends/splits.
}
\value{plots a chart}
\author{
Garrett See
}

\seealso{
ts.plot, chart_Series, ggplot2, cumsum, ROC, getPrice, environment
}
\examples{
\dontrun{
    syms <- c('SPY','TLT','DBA')
    getSymbols(syms)
    plotRelPerf(syms)

    #Use intraday data from IB.
    #and demonstrate how you can put data in different environments    
    twsData <- new.env()     
    #The next block will take at least 60 seconds to run
    getBAT(twsSTK('TLT','SMART'),env=twsData)
    getBAT(twsSTK('DBA','SMART'),env=twsData)
    plotRelPerf(c('TLT','DBA'),env=twsData)    
}
}

