\name{SP500desc}
\alias{SP500desc}
\docType{data}
\title{
S&P 500 symbols
}
\description{
Symbols and descriptions of all stocks in the S&P 500 as of 2011-05-27
}
\usage{data(SP500desc)}
\format{
  A data frame with 498 observations on the following 5 variables.
  \describe{
    \item{\code{Symbol}}{stock symbol}
    \item{\code{Description}}{company description}
    \item{\code{Industry.Division}}{industry division}
    \item{\code{Industry.Group}}{industry group}
    \item{\code{Industry.Sector}}{industry sector}
  }
}
\details{
This data can easily be downloaded from thinkorswim from tdameritrade. If you have
the thinkorswim desktop application, open a quote tab/window. Right click a column
header and select "Customize."  Select the columns you want, and click ok. Then,
click the print icon and select either Export to Excel, or Export to Calc. In Excel,
or calc, save as csv.

\code{define_stocks} expects it to have the following columns Symbols, descriptions, industry.groups, industry.sectors, industry.division
}
\source{
http://www.tdameritrade.com
http://www.thinkorswim.com
}
\seealso{
define_stocks, load.instruments
}
\examples{
data(SP500desc)
}
\keyword{datasets}
