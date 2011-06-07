
### This is intended to be run line-by-line.
### NOT as a script. 

## You must be connected to Interactive Brokers 
## TraderWorkstation for this to work

#################################################

twsInstrument(twsSTK("SPY","USD",exch="ARCA"))
twsInstrument(twsSTK("DIA","USD",exch="ARCA"))

option('.SPY','USD',100,right='P',strike=135,
     expires=201112,underlying_id="SPY")
option('.DIA','USD',100,right='C',strike=120,
     expires=201112,underlying_id="DIA")

contract <- Contr_From_Instr('.SPY') #doesn't change instrument
contract
Instr_From_Contr(contract) #same as twsInstrument(contract, addIBslot=FALSE, output='instrument')

Instr_From_Contr(twsFUT('ES',exch='GLOBEX',expiry='201109'))

define_stocks(c('G','GA','GAR','GS'), use.yahoo=FALSE)
define_stocks(c('S','SE','SEE'),currency="USD", use.IB=FALSE)

ls_stocks()
ls_derivatives()
ls_options()
ls_twsInstruments()


#option_series.yahoo('XOM')
FinancialInstrument:::option_series.yahoo('XOM')

ls_instruments_by('underlying_id','XOM') #underlying_id must exactly match 'XOM'
ls_derivatives('XOM',match=FALSE) #primary_ids that contain 'XOM'


instrument.table(ls_yahoo(),attrs.of='S')
instrument.table(ls_options(), attrs.of='.SPY')
update_instruments.IB('.DIA')
instrument.table(ls_derivatives(),attrs.of='.SPY')

getInstrument('SEE')
update_instruments.IB('SEE')
getContract('SEE')

getInstrument('GS')
update_instruments.yahoo('GS')
getInstrument('GS')

ls_stocks()
ls_IB()
ls_non_twsInstruments()
ls_EUR()
ls_CAD()
ls_non_derivatives()

#several ways to remove groups
#get some more stocks to play with
define_stocks(c('A','B','U','N','CH','O','SY','MB','L', 'S'),use.yahoo=FALSE)
ls_stocks()
#a couple ways to delete groups
rm_instruments(ls_CAD(show.currencies=TRUE),keep.currencies=FALSE) #remove CAD denominated instruments and CAD currency.
# or, to not remove currencies, rm_instruments(ls_CAD()) 
rm_stocks(ls_EUR(show.currencies=TRUE)) #remove EUR denominated stocks & EUR currency
# or, to not remove EUR currency, rm_stocks(ls_EUR())
rm_by_currency( , "USD") #remove USD denominated instruments

#Often useful to nest:
#rm_by_currency(ls_options(), "USD") 

rm_non_derivatives(ls_stocks())

#create twsInstruments for SP500 stocks, updated with
#info from attached SP500desc data, IB, and yahoo,
define_stocks( ,currency="USD", use.IB=FALSE) 
ls_stocks()
update_instruments.IB() 


###########################
# Getting Historical Data #
###########################










