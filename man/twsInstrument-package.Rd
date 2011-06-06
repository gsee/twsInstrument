\name{twsInstrument-package}
\alias{twsInstrument-package}
\alias{twsInstrument-help}

\docType{package}
\title{
twsInstrument package: Integrates IBrokers and FinancialInstrument.
}
\description{
Easily create twsContract and instrument objects
Includes functions to make requesting historical data (esp. TBBO data) from IB easier
defines stocks from vector of ticker symbols, a csv file, or a data.frame (package includes dataset)

Mostly just wrappers for IBrokers and FinancialInstrument
See \code{?buildIBcontract} for the twsInstrument function help page
}
\details{
\tabular{ll}{
Package: \tab twsInstrument\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Depends: \tab FinancialInstrument, IBrokers, quantmod\cr
Date: \tab 2011-05-27\cr
License: \tab GPL-3\cr
LazyLoad: \tab yes\cr
}
An instrument object that has a slot called IB containing a twsContract will be classed 
as twsInstrument.

The \code{twsInstrument} function makes it easy to create twsContracts (for the IBrokers package) or instruments (for the FinancialInstrument package) or both. You can give it a twsContract, a twsInstrument, an instrument, or the name of an instrument, and get back an instrument, a twsContract, or both, updated with information from the other. Most functions have an \code{addIBslot} to control whether to add a slot with the twsContract to (and add twsInstrument class to) the instrument you're dealing with.

\code{update_instruments.yahoo} can be called with a vector of instrument names or with
\sQuote{all} in which case all stocks will be update. (Currently, yahoo is only implemented
for stocks.)

\code{update_instruments.IB} can accept a vector of instrument names, or sQuote{all} or 
sQuote{stocks}, but it also accepts one of, sQuote{futures}, sQuote{options}, or sQuote{currencies}

\code{reqTBBO} will download and merge the closing price of Bid, Ask, Trade, and Midpoint

\code{instrument.table} creates a data.frame with instruments in rows and attributes in columns. It is a wrapper for \code{buildHierarchy} that requires less input from the user.


Before using, enable ActiveX and socket clients and add your computer 
to trusted IP addresses in your IB TWS global configuration.
Read the IBrokers documentation before using.

Most of the examples were tested on a paper trading account. You should not use this package with a live trading account, but if you do, you need a market data subscription for the exchanges with the contracts you're interested in.

Use a paper trading account.

See ?IBrokers and vignette("IBrokers") 

See examples.
}
\author{
Garrett See

Maintainer: Garrett See <gsee000@gmail.com>
}
\references{
Yahoo! Finance \url{finance.yahoo.com} 
gummy-stuff.org \url{www.gummy-stuff.org/Yahoo-data.htm}
InteractiveBrokers \url{www.interactivebrokers.com}
IB API \url{http://interactivebrokers.com/php/apiUsersGuide/apiguide.htm}
tdameritrade \url{www.tdameritrade.com}
}
\keyword{ package }
\keyword{ IB }
\examples{
#Before using, enable ActiveX and socket clients and add your computer 
#to trusted IP addresses in your IB TWS global configuration.
#See ?IBrokers and vignette("IBrokers") 

\dontrun{

twsInstrument(twsSTK("SPY","USD"))
twsInstrument(twsSTK("DIA","USD"))

option('.SPY','USD',100,right='P',strike=135,
     expiry=201112,underlying_id="SPY")

contract <- Contr_From_Instr('.SPY') #doesn't change instrument

Instr_From_Contr(contract)
Instr_From_Contr(twsFUT('ES',exch='GLOBEX',expiry='201109'))

option('.DIA','USD',100,right='P',strike=125,
     expiry=201112,underlying_id="DIA")

define_stocks(c('G','GA','GAR'), use.yahoo=FALSE)
define_stocks(c('S','SE','SEE'),currency="USD", use.IB=FALSE)

ls_stocks()
ls_derivatives()
ls_options()
ls_twsInstruments()

instrument.table(ls_yahoo(),attrs.of='S')
instrument.table(ls_options(), attrs.of='.SPY')

getInstrument('SEE')
update_instruments.IB('SEE')
getContract('SEE')

getInstrument('GAR')
update_instruments.yahoo()
getInstrument('GAR')

#create twsInstruments for SP500 stocks, updated with
#info from attached SP500desc data, IB, and yahoo,
define_stocks() 
ls_stocks()


## Using the IB stuff without Using FinancialInstrument
rm_instruments(keep.currencies=FALSE) 

getBAT('GS')   #Not recommended
#Assumes 'GS' is the name of an equity

#alternatively, pass a twsContract object
contract <- twsSTK('GR') 
getBAT(contract) #recommended

#for an instrument:
instr <- stock('GT','USD')
getBAT(instr) #recommended

#getBAT and reqTBBO are the exact same function
reqTBBO(twsFUT(symbol='ES',exch='GLOBEX',expiry='201106',multiplier=5))

#get all fields for TRADES, for one or more stocks
#by default they are assigned to .GlobalEnv similarly to getSymbols
getIBEquities(c('S','SE', 'SEE')) 

ls_IB()
ls_IB(ls_stocks())
ls_IB(ls_derivatives()

plotRelPerf(c('GT','SEE'))

#### Using FinancialInstrument but not IB ####

rm_instruments(keep.currencies=FALSE) #remove all instruments and currencies

define_stocks(c('GA','GAR'),"EUR",use.IB=FALSE, use.yahoo=FALSE)
define_stocks(c('RRE','TT'),"CAD",use.IB=FALSE, use.yahoo=FALSE)
define_stocks(c('EE','GS','SEE'),"USD",use.IB=FALSE)
ls_instruments()
ls_currencies()
ls_EUR() #GA GAR
ls_CAD() #RRE TT
ls_USD() #EE GS SEE

#data.frame view of stocks in .instrument environment. 
#columns will be defined by 'GAR' instrument
instrument.table(ls_stocks(), attrs.of='GAR') 

#create instrument for stocks (S&P500 stocks by default)
define_stocks( ,"USD",use.IB=FALSE)  
ls_yahoo()

ls_instruments("SE") #only the one stock
rm_stocks('SE')
ls_instruments(c('S','SE','SEE')) #only the 2 that exist are returned
ls_instruments("S", match=FALSE) #anything with "S" in name

stock('SPY','USD',1)
#derivatives
option('.SPY','USD',multiplier=100,expiry='201106', strike=130, right='P', underlying_id='SPY')
future('ES', 'USD', multiplier=50, expiry='201106', underlying_id='ES')
option('.ES','USD',multiplier=1, expiry='201106',strike=1350, right='C', underlying_id='ES')

ls_currencies()
ls_options() 
ls_futures() 
ls_derivatives()
ls_non_derivatives()

rm_options('.SPY')
rm_futures()
ls_instruments()
#rm_instruments('EUR') # <-- INCORRECT USAGE
rm_instruments('EUR', keep.currencies=FALSE) 
ls_currencies()
rm_currencies('CAD') 
ls_instruments()
rm_instruments() #remove all but currency
rm_instruments(keep.currencies=FALSE) #remove everything from .instrument


SP500.symbols <- as.character(SP500desc[,1]) 
addIBslot(SP500.symbols) #Add IB slot to S&P 500 instrument metadata

getBAT('XOM') #uses twsContract as defined by addIBslot
#head(XOM)

syms <- SP500.symbols[1:3]
getIBEquities(syms) #get all fields for TRADES

plotRelPerf(syms)


####### Misc. ############

ls_twsInstruments()
ls_non_twsInstruments()

## make twsContracts using information stored in instruments
#just make a twsContract without changing anything about the instrument
buildIBcontract('XOM',addIBslot=FALSE) 

#or add a slot in the instrument to hold the twsContract
buildIBcontract('XOM')

#or update everything with the new information retrieved from IB.
buildIBcontract('XOM',updateInstrument=TRUE)

#reqTBBO('XOM') #will use twsContract object in IB slot of XOM instrument.
}
}
