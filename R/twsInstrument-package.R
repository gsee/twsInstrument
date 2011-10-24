

#' S&P 500 symbols
#' 
#' Symbols and descriptions of all stocks in the S&P 500 as of 2011-05-27
#' 
#' This data can easily be downloaded from thinkorswim from tdameritrade. If
#' you have the thinkorswim desktop application, open a quote tab/window. Right
#' click a column header and select "Customize."  Select the columns you want,
#' and click ok. Then, click the print icon and select either Export to Excel,
#' or Export to Calc. In Excel, or calc, save as csv.
#' 
#' \code{define_stocks} expects it to have the following columns Symbols,
#' descriptions, industry.groups, industry.sectors, industry.division
#' 
#' @name SP500desc
#' @docType data
#' @format A data frame with 498 observations on the following 5 variables.
#' \describe{ \item{Symbol}{stock symbol}
#' \item{Description}{company description}
#' \item{Industry.Division}{industry division}
#' \item{Industry.Group}{industry group}
#' \item{Industry.Sector}{industry sector} }
#' @seealso define_stocks, load.instruments
#' @source http://www.tdameritrade.com http://www.thinkorswim.com
#' @keywords datasets
#' @examples
#' 
#' data(SP500desc)
#' 
NULL





#' twsInstrument package: Integrates IBrokers and FinancialInstrument.
#' 
#' Easily create twsContract and instrument objects Includes functions to make
#' requesting historical data (esp. TBBO data) from IB easier defines stocks
#' from vector of ticker symbols, a csv file, or a data.frame (package includes
#' dataset)
#' 
#' Mostly just wrappers for IBrokers and FinancialInstrument See
#' \code{?buildIBcontract} for the \code{\link{twsInstrument}} function help page
#' 
#' \tabular{ll}{ Package: \tab twsInstrument\cr Type: \tab Package\cr Version:
#' \tab 1.0-13\cr Depends: \tab FinancialInstrument, IBrokers, quantmod\cr
#' Date: \tab 2011-05-27\cr License: \tab GPL-3\cr LazyLoad: \tab yes\cr } An
#' instrument object that has a slot called IB containing a twsContract will be
#' classed as twsInstrument.
#'
#' The \code{twsInstrument} function makes it easy to create twsContracts (for
#' the IBrokers package) or instruments (for the FinancialInstrument package)
#' or both. You can give it a twsContract, a twsInstrument, an instrument, the
#' name of an instrument, or an Interactive Brokers conId, and get back an
#' instrument, a twsContract, or both, updated with information from the other.
#' Most functions have an \code{addIBslot} to control whether to add a slot
#' with the twsContract (and add twsInstrument class) to the instrument you're
#' dealing with.
#' 
#' \code{update_instruments.yahoo} can be called with a vector of instrument
#' names or with \sQuote{all} in which case all stocks will be updated.
#' (Currently, yahoo is only implemented for stocks and synthetics (e.g. stock
#' indexes). \code{update_instruments.TTR} uses the data frame given by
#' \code{TTR:::stockSymbols} to define/update stocks.
#' \code{update_instruments.IB} can accept a vector of instrument names, or
#' sQuoteall or sQuotestocks, but it also accepts one of, sQuotefutures,
#' sQuoteoptions, or sQuotecurrencies
#' 
#' \code{\link{define_stocks}}, \code{\link{define_options}},
#' \code{\link{define_futures}} and \code{\link{define_FX}} make it easy to define
#' several instruments of the same type.  \code{\link{front_future}}
#' attempts to define the near term liquid futures contract of a given
#' future_series.
#' 
#' \code{get_quote} will return a recent quote snapshot using IBrokers by
#' default (although if called with src='yahoo', it will call
#' \code{quantmod::getQuote}.
#' 
#' \code{getBAT} will download and merge the closing price of Bid, Ask, Trade,
#' and Midpoint
#' 
#' \code{getContract} will get a twsContract. It will search the .instrument
#' environment for an instrument that holds the contract.  If not found, it
#' will download it with a call to \code{\link{reqContractDetails}}
#' 
#' \code{conId} is a generic that will get the conId.  If it cannot find it
#' easily, it will call getContract and extract the conId from that.
#' 
#' The following will eventually be moved to the FinancialInstrument package
#' \code{\link{saveInstruments}} and \code{\link{loadInstruments}} allow you to
#' save your .instrument environment. \code{loadInstruments} will add to your
#' \code{.instrument} environment all the instruments found in a file that was
#' created with \code{saveInstruments}.
#' 
#' \code{\link{ls_instruments}} and the several related functions
#' (\code{\link{ls_stocks}}, \code{\link{ls_synthetics}}, etc) help you see the
#' names of your defined instruments. \code{\link{ls_instruments_by}} help you
#' subset the names of your instruments based on values of attributes they
#' have.
#' 
#' \code{\link{rm_instruments}} and related functions remove instruments from
#' your .instrument environment.
#' 
#' Before using, enable ActiveX and socket clients and add your computer to
#' trusted IP addresses in your IB TWS global configuration. Read the IBrokers
#' documentation before using.
#' 
#' Use a paper trading account.
#' 
#' See ?IBrokers and vignette("IBrokers")
#' 
#' See examples.
#'
#' Many functions in this package need data from IB.  The ones that do will
#' automatically connect and disconnect using the \code{\link[IBrokers]{twsConnect}}
#' and \code{\link[IBrokers]{twsDisconnect}}.  This package will only use clientIds
#' 100 through 150( buildIBcontract may use 100:104, getContract may use 110:114,
#' getBAT may use 120:124, getIBEquities may use 125:129, get_quote.IB may use 130:132, 
#' and twsClock may use 140:144). 
#' clientId 150 is used as a last resort, and any time it is used, you will be 
#' notified with a warning message.
#'
#' @name twsInstrument-package
#' @aliases twsInstrument-package twsInstrument-help
#' @docType package
#' @author Garrett See
#' 
#' Maintainer: Garrett See <gsee000@@gmail.com>
#' @references Yahoo! Finance \url{finance.yahoo.com} gummy-stuff.org
#' \url{www.gummy-stuff.org/Yahoo-data.htm} InteractiveBrokers
#' \url{www.interactivebrokers.com} IB API
#' \url{http://interactivebrokers.com/php/apiUsersGuide/apiguide.htm}
#' tdameritrade \url{www.tdameritrade.com}
#' @keywords package IB
#' @examples
#' 
#' #Before using, enable ActiveX and socket clients and add your computer 
#' #to trusted IP addresses in your IB TWS global configuration.
#' #See ?IBrokers and vignette("IBrokers") 
#' 
#' \dontrun{
#' 
#' twsInstrument(twsSTK("SPY","USD"))
#' twsInstrument(twsSTK("DIA","USD"))
#' 
#' option('.SPY','USD',100,right='P',strike=135,
#'      expiry=201112,underlying_id="SPY")
#' 
#' contract <- Contr_From_Instr('.SPY') #doesn't change instrument
#' 
#' Instr_From_Contr(contract)
#' Instr_From_Contr(twsFUT('ES',exch='GLOBEX',expiry='201109'))
#' 
#' option('.DIA','USD',100,right='P',strike=125,
#'      expiry=201112,underlying_id="DIA")
#' 
#' define_stocks(c('G','GA','GAR'), use.yahoo=FALSE)
#' define_stocks(c('S','SE','SEE'),currency="USD", use.IB=FALSE)
#' 
#' ls_stocks()
#' ls_derivatives()
#' ls_options()
#' ls_twsInstruments()
#' 
#' instrument.table(ls_yahoo(),attrs.of='S')
#' instrument.table(ls_options(), attrs.of='.SPY')
#' 
#' getInstrument('SEE')
#' update_instruments.IB('SEE')
#' getContract('SEE')
#' 
#' getInstrument('GAR')
#' update_instruments.yahoo()
#' getInstrument('GAR')
#' 
#' #create twsInstruments for SP500 stocks, updated with
#' #info from attached SP500desc data, IB, and yahoo,
#' define_stocks() 
#' ls_stocks()
#' 
#' 
#' ## Using the IB stuff without Using FinancialInstrument
#' rm_instruments(keep.currencies=FALSE) 
#' 
#' getBAT('GS')   #Not recommended
#' #Assumes 'GS' is the name of an equity
#' 
#' #alternatively, pass a twsContract object
#' contract <- twsSTK('GR') 
#' getBAT(contract) #recommended
#' 
#' #for an instrument:
#' instr <- stock('GT','USD')
#' getBAT(instr) #recommended
#' 
#' #getBAT and reqTBBO are the exact same function
#' reqTBBO(twsFUT(symbol='ES',exch='GLOBEX',expiry='201106',multiplier=5))
#' 
#' #get all fields for TRADES, for one or more stocks
#' #by default they are assigned to .GlobalEnv similarly to getSymbols
#' getIBEquities(c('S','SE', 'SEE')) 
#' 
#' ls_IB()
#' ls_IB(ls_stocks())
#' ls_IB(ls_derivatives()
#' 
#' plotRelPerf(c('GT','SEE'))
#' 
#' #### Using FinancialInstrument but not IB ####
#' 
#' rm_instruments(keep.currencies=FALSE) #remove all instruments and currencies
#' 
#' define_stocks(c('GA','GAR'),"EUR",use.IB=FALSE, use.yahoo=FALSE)
#' define_stocks(c('RRE','TT'),"CAD",use.IB=FALSE, use.yahoo=FALSE)
#' define_stocks(c('EE','GS','SEE'),"USD",use.IB=FALSE)
#' ls_instruments()
#' ls_currencies()
#' ls_EUR() #GA GAR
#' ls_CAD() #RRE TT
#' ls_USD() #EE GS SEE
#' 
#' #data.frame view of stocks in .instrument environment. 
#' #columns will be defined by 'GAR' instrument
#' instrument.table(ls_stocks(), attrs.of='GAR') 
#' 
#' #create instrument for stocks (S&P500 stocks by default)
#' define_stocks( ,"USD",use.IB=FALSE)  
#' ls_yahoo()
#' 
#' ls_instruments("SE") #only the one stock
#' rm_stocks('SE')
#' ls_instruments(c('S','SE','SEE')) #only the 2 that exist are returned
#' ls_instruments("S", match=FALSE) #anything with "S" in name
#' 
#' stock('SPY','USD',1)
#' #derivatives
#' option('.SPY','USD',multiplier=100,expiry='201106', strike=130, right='P', underlying_id='SPY')
#' future('ES', 'USD', multiplier=50, expiry='201106', underlying_id='ES')
#' option('.ES','USD',multiplier=1, expiry='201106',strike=1350, right='C', underlying_id='ES')
#' 
#' ls_currencies()
#' ls_options() 
#' ls_futures() 
#' ls_derivatives()
#' ls_non_derivatives()
#' 
#' rm_options('.SPY')
#' rm_futures()
#' ls_instruments()
#' #rm_instruments('EUR') # <-- INCORRECT USAGE
#' rm_instruments('EUR', keep.currencies=FALSE) 
#' ls_currencies()
#' rm_currencies('CAD') 
#' ls_instruments()
#' rm_instruments() #remove all but currency
#' rm_instruments(keep.currencies=FALSE) #remove everything from .instrument
#' 
#' 
#' SP500.symbols <- as.character(SP500desc[,1]) 
#' addIBslot(SP500.symbols) #Add IB slot to S&P 500 instrument metadata
#' 
#' getBAT('XOM') #uses twsContract as defined by addIBslot
#' #head(XOM)
#' 
#' syms <- SP500.symbols[1:3]
#' getIBEquities(syms) #get all fields for TRADES
#' 
#' plotRelPerf(syms)
#' 
#' 
#' ####### Misc. ############
#' 
#' ls_twsInstruments()
#' ls_non_twsInstruments()
#' 
#' ## make twsContracts using information stored in instruments
#' #just make a twsContract without changing anything about the instrument
#' buildIBcontract('XOM',addIBslot=FALSE) 
#' 
#' #or add a slot in the instrument to hold the twsContract
#' buildIBcontract('XOM')
#' 
#' #or update everything with the new information retrieved from IB.
#' buildIBcontract('XOM',updateInstrument=TRUE)
#' 
#' #reqTBBO('XOM') #will use twsContract object in IB slot of XOM instrument.
#' }
#' 
NULL



