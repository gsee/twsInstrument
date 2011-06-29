\name{update_instruments.IB}
\alias{update_instruments.IB}
\alias{update_instruments.yahoo}
\alias{update_instruments.all}
\title{
updates instrument metadata with data from IB or yahoo
}
\description{
Adds/updates information in instrument with data downloaded from IB or yahoo
}
\usage{
update_instruments.IB(symbols = c("all", "stocks", "futures", "options", "currencies"), addIBslot = TRUE, updateInstrument = TRUE, assign_i=TRUE, assign_c=TRUE)
update_instruments.yahoo(symbols=c('stocks','all'), verbose=FALSE)
update_instruments.all(symbols='all', ...)
}
\arguments{
  \item{symbols}{
can be a vector of instrument names, 
or, for update_instruments.IB, can be \sQuote{all} or \sQuote{stocks}
or, for update_instruments.yahoo, can be \sQuote{all} or \sQuote{stocks}, 
\sQuote{futures}, \sQuote{options}, \sQuote{currencies}
}
  \item{addIBslot}{
Boolean. should an IB slot be added to the instrument, making it a twsInstrument?
}
  \item{updateInstrument}{
Boolean. Should data outside the IB slot also be updated?
}
  \item{assign_i}{
should the instrument be stored in .instrument environment.
}
  \item{assign_c}{
If a new currency is discovered, should it be created
}  
  \item{...}{anything to pass through it update_instruments.IB
}
  \item{verbose}{ be verbose?
}
}
\details{
These are basically wrappers for buildIBcontract. With these functions you can
update some or all instruments' information from yahoo or IB, or both.

if you call update_instruments.IB with one of \sQuote{all} or \sQuote{stocks}, 
\sQuote{futures}, \sQuote{options}, \sQuote{currencies}, it is the same as 
calling it with the relevant ls_ function (e.g. ls_stocks()). Therefore,
functionality can be extended by using ls_ functions instead of a descriptive
string.
}
\value{
called for side-effect
}
\references{
Yahoo! Finance \url{finance.yahoo.com} 
YahooQuote \url{http://dirk.eddelbuettel.com/code/yahooquote.html}
gummy-stuff.org \url{www.gummy-stuff.org/Yahoo-data.htm}
InteractiveBrokers \url{www.interactivebrokers.com}
IB API \url{http://interactivebrokers.com/php/apiUsersGuide/apiguide.htm}?
}
\author{
Garrett See
}
\seealso{
twsInstrument, define_stocks, getIBEquities, instrument, stock, future, option, currency
}
\examples{
\dontrun{	
	currency('USD')
	stock('GS','USD',1)
	option('GS','USD',100,right='P',strike=135,
         expiry=201112,underlying_id="GS")
  update_instruments.yahoo('GS')
	getInstrument('GS')
	update_instruments.IB('GS') 
}
}


