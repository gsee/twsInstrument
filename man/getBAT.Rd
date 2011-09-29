\name{getBAT}
\alias{getBAT}
%\alias{getBAThistory}
\alias{reqTBBO}
\title{download Bid Ask Trade data and merge into BAT object}
\description{
getBAT downloads and merges data for Bid, Ask, and Trade.
reqTBBO is an alias for getBAT.
%getBAThistory downloads the max allowed by InteractiveBrokers.
}

\usage{
getBAT(Symbols, endDateTime, tws=NULL, barSize='1 min', 
duration='5 D', useRTH="1", auto.assign=TRUE, env=.GlobalEnv)
reqTBBO(Symbols, endDateTime, tws=NULL, barSize='1 min', 
duration='5 D', useRTH="1", auto.assign=TRUE, env=.GlobalEnv)
}

\arguments{
  \item{Symbols}{can be a twsInstrument, a twsContract, an instrumnet
or the name of an instrument. (you can give it a character vector to get multiple Symbols) }
  \item{endDateTime}{ end date/time for request. See details.}
  \item{tws}{a twsConnection object. optional.}
  \item{barSize}{ bar size to retrieve. default='1 min'}
  \item{duration}{time span the request will cover. default='5 D'}
  \item{useRTH}{only include regular trading hours?}
  \item{auto.assign}{if TRUE, data will be assigned to symbols in the env or .GlobalEnv if no env given.}
  \item{env}{environment in which to save results}
%  \item{store}{should the data be saved to disk}
%  \item{file.path}{where to store the data}
}

\details{
%for getBAThistory, the symbol argument must be the name 
%of a FinancialInstrument instrument. 

reqTBBO is an alias for getBAT.

symbol can be one of: an instrument, the name of an instrument, the name of an undefined stock, or a twsContract object. 

Before making a request for historical data, symbol will be passed through \code{Contr_From_Instr} 
(which is a wrapper for \code{buildIBcontract}) which will convert symbol into an updated, complete twsContract. 

If you want to get data for something other than a stock, then \code{symbol} should be an instrument object, 
a twsContract object or the name of a previosly defined instrument

Unlike reqHistoricalData, The \code{endDateTime} argument must be of the form
'CCYY-MM-DD HH:MM:SS'

If the contract is expired, the minimum (i.e. oldest) of \code{endDateTime}, if given, and 1 minute before midnight on the expiration date, will be used for endDateTime in the call to reqHistoricalData.

!!! Ignore the rest of this details section until I upload getBAThistory function !!!
(Below is mostly copied from reqHistoricalData help)
If endDateTime is not specified the current
time as returned from the TWS server will be used. This
is the preferred method for backfilling data. The \sQuote{TZ}
portion of the string is optional.

Legal \code{barSize} settings are technically
\sQuote{1 secs},\sQuote{5 secs},\sQuote{15 secs},
\sQuote{30 mins},\sQuote{1 min},\sQuote{2 mins},
\sQuote{3 mins},\sQuote{5 mins},\sQuote{15 mins},
\sQuote{30 mins},\sQuote{1 hour},\sQuote{1 day},
\sQuote{1 week},\sQuote{1 month},\sQuote{3 months},
and \sQuote{1 year}. They must be specified exactly
and there is no guarantee from the API that all
will work for all securities or durations. 

The duration string must be of the form \sQuote{n S}
where the last character may be any one of
\sQuote{S} (seconds),
\sQuote{D} (days),
\sQuote{W} (weeks),
\sQuote{M} (months),
and \sQuote{Y} (year).
At present the limit for years is 1.


%getBAThistory needs to be updated;
%getBAThistory requests the maximal history from IB. 
}
\value{
Returns (or assigns) an xts object containing Bid.Price, Ask.Price, Trade.Price, Mid.Price, 
along with additional information stored in the objects xtsAttributes, 
}
\references{
InteractiveBrokers \url{www.interactivebrokers.com}

IB API \url{http://interactivebrokers.com/php/apiUsersGuide/apiguide.htm}

IB Historic Data Limitations \url{http://individuals.interactivebrokers.com/php/apiUsersGuide/apiguide/api/historical_data_limitations.htm#XREF_93621_Historical_Data}
}

\author{Garrett See}
\seealso{Contr_From_Instr, IBrokers, reqHistoricalData, reqHistory, getIBequities}

\examples{
\dontrun{
#If no instrument is defined for symbol, 
#it will assume you are trying to get a stock
getBAT("XOM")

#Alternatively, pass a twsContract
contract <- twsSTK("XOM","SMART","NYSE")
getBAT(contract)

#or, if you use FinancialInstrument, add an IB slot to hold
#the twsContract object
currency('USD')
stock('XOM', 'USD', 1, IB=twsSTK('XOM'))
getBAT('XOM') #gets from .instrument
}
}

