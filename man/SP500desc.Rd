\docType{data}
\name{SP500desc}
\alias{SP500desc}
\title{S&P 500 symbols}
\format{A data frame with 498 observations on the following 5 variables.
\describe{ \item{Symbol}{stock symbol}
\item{Description}{company description}
\item{Industry.Division}{industry division}
\item{Industry.Group}{industry group}
\item{Industry.Sector}{industry sector} }}
\source{
  http://www.tdameritrade.com http://www.thinkorswim.com
}
\description{
  Symbols and descriptions of all stocks in the S&P 500 as
  of 2011-05-27
}
\details{
  This data can easily be downloaded from thinkorswim from
  tdameritrade. If you have the thinkorswim desktop
  application, open a quote tab/window. Right click a
  column header and select "Customize."  Select the columns
  you want, and click ok. Then, click the print icon and
  select either Export to Excel, or Export to Calc. In
  Excel, or calc, save as csv.

  \code{define_stocks} expects it to have the following
  columns Symbols, descriptions, industry.groups,
  industry.sectors, industry.division
}
\examples{
data(SP500desc)
}
\seealso{
  define_stocks, load.instruments
}
\keyword{dataset}
\keyword{datasets}

