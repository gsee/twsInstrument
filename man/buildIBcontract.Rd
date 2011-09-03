\name{buildIBcontract}
\alias{buildIBcontract}
\alias{twsInstrument}
\alias{Instr_From_Contr}
\alias{Contr_From_Instr}
\alias{is.twsInstrument}
\title{
create twsInstrument, or
create twsContracts using previously defined FinancialInstruments, or
create FinancialInstruments from previously defined twsContract
see ?\sQuote{twsInstrument-package} for package help page.
}
\description{
Using metadata that has already been defined for an instrument, 
create a twsContract object and fill in any missing information.
Can either add an IB slot to the instrument, or update the entire 
instrument creating slots as needed.
}
\usage{
buildIBcontract(symbol, tws=NULL, 
    	addIBslot=FALSE, updateInstrument=FALSE, 
    	output=c('contract','instrument','symbol','nothing'), 
    	include_expired="0", assign_i=FALSE, assign_c=TRUE, verbose=TRUE)
twsInstrument(symbol, tws=NULL, 
        addIBslot=TRUE, updateInstrument=TRUE, 
        output=c('symbol','nothing','instrument','contract'), 
        include_expired="0", assign_i=TRUE, assign_c=TRUE, verbose=TRUE)
Instr_From_Contr(contract, tws=NULL, 
    	addIBslot=FALSE, updateInstrument=TRUE, 
    	output=c('instrument','symbol','nothing','contract'), 
    	include_expired="0", assign_i=FALSE, assign_c=TRUE, verbose=TRUE) 
Contr_From_Instr(instrument, tws=NULL, 
    	addIBslot=FALSE, updateInstrument=FALSE, 
    	output=c('contract','symbol','nothing','instrument'), 
    	include_expired="0", assign_i=FALSE, assign_c=TRUE, verbose=TRUE) 

is.twsInstrument(x)
}
\arguments{
  \item{symbol}{An instrument, The name of an instrument, or a twsContract.} 
  \item{tws}{twsconn object. Not required.}
  \item{addIBslot}{Boolean. Should an IB slot be created in the instrument object?)}
  \item{updateInstrument}{Should all the attributes of the instrument be updated with the information retrieved from IB?}
  \item{output}{what should be return. one of 'nothing','symbol','instrument','contract'}
  \item{include_expired}{is the requested contract expired?}
  \item{assign_i}{boolean. Should the instrument be stored in the instrument environment?}
  \item{assign_c}{boolean. If a currency isn't defined, should it be?}
  \item{verbose}{be verbose?}

  \item{instrument}{for wrapper, alias for symbol}
  \item{contract}{for wrapper, alias for symbol}

  \item{x}{what to test for is.twsInstrument}
}
\details{
buildIBcontract is the main function; the rest are wrappers.

symbol can be the name of an instrument, an instrument, a twsContract, or a 
twsInstrument.  Using the information given, it will create an instrument 
and a twsContract.  It will then make a call to reqContractDetails to fill in 
any missing information. (If you give it a string and there is no instrument
by that name, then: (a) If the string ends in a period, it will treat it as a currency pair 
using "USD" as the base currency. (b) If the string does not end in a period, it will
assume it is a stock.) 

if addIBslot is TRUE it will store the contract in the IB slot of the instrument 
(creating the slot if necessary.)  If updateInstrument is TRUE it will add/replace 
information directly in the instrument object.

It is recommended that you do not pass it a twsconn object, in which case it will 
create a temporary one. If you pass it a twsconn object you are more likely to 
encounter errors.  If you pass a connected twsconn object it will be disconnected
after the request.

twsInstrument is a wrapper that will create a twsInstrument classed object. By
default, It creates a twsContract and an instrument (if necessary) and places the 
twsContract in the IB slot of the instrument. The twsInstrument class is 
automatically added to any instrument that has an IB slot.

Instr_From_Contr is a wrapper to create an instrument using a twsContract. It does
not create an IB slot or store the twsContract in the instrument by default, and 
therefore does not add the twsInstrument class to the instrument.

Contr_From_Instr is a wrapper to create a twsContract using an instrument. By
default, it does not make any changes to the instrument, and therefore does 
not add the twsInstrument class to it.

Instr_From_Contr and Contr_From_Instr are essentially the same functions, but
with different default outputs.

ouput should be a character string describing what to return. Valid values are c(\sQuote{"nothing}, \sQuote{symbol}, \sQuote{instrument}, \sQuote{contract}
}
\value{
Usually called for its side-effect. It will return what is defined by the output
argument. See details.
}
\author{
Garrett See
}
\seealso{
instrument, twsContract, addIBslot, 
}
\examples{

\dontrun{

twsInstrument('SPY') #assumes it's a stock

#Now something that isn't denominated in USD
twsInstrument(twsFUT(symbol='NIY',exch='GLOBEX',expiry='201109',
                     currency='JPY'))

#As a wrapper for contract wrappers
Contr_From_Instr(option(primary_id='.SPY', currency='USD', 
    multiplier=100,tick_size=0.01,expiry='201106',callput='call',
    strike=135,underlying_id='SPY'))
Instr_From_Contr(twsFUT(symbol='ES',exch='GLOBEX',expiry='201106', currency='USD'))

buildIBcontract(twsSTK("GOOG"),updateInstrument=TRUE, addIBslot=FALSE, output='nothing')

stock('AAPL','USD')
buildIBcontract('AAPL', updateInstrument=FALSE) #uses instrument

ls(.instrument,all.names=TRUE)

}
}
