##FUTURES tests

library(twsInstrument)

#twsInstrument from twsContract with twsInstrument
twsInstrument(twsFUT('ES',exch='GLOBEX',expiry='201109'))
twsInstrument(twsFUT(symbol='SPY',exch='ONE',expiry='201109'))
ls_futures()
getContract('ES');getContract('SPY')
getInstrument('ES')
getInstrument('SPY')
rm_futures()

# instrument from twsContract with Instr_From_Contr (contract not stored)
Instr_From_Contr(twsFUT('ES',exch='GLOBEX',expiry='201109'))
Instr_From_Contr(twsFUT('SPY',exch='ONE',expiry='201109'))
ls_futures()
getInstrument('ES')
rm_instruments(keep.currencies=FALSE)

#twsInstrument from instrument
twsInstrument(future('ES','USD',50,exchange='GLOBEX',expires='201109'))
twsInstrument(future('SPY','USD',1000,exchange='ONE',expires='201109',underlying_id='SPY'))
ls_futures()
ls_underlyings()
rm_instruments();rm_currencies()

#twsContract from instrument 
Contr_From_Instr(future('ES','USD',50,exchange='GLOBEX',expires='201109'))
Contr_From_Instr(future('SPY','USD',1000,exchange='ONE',expires='201109',underlying_id='SPY'))
ls_instruments()
getInstrument('ES')
#nothing is stored in Contr_From_Instr function, but 
#future() cannot be called with assign_i=FALSE...that's where it's getting assigned.
rm_futures()
fut <- instrument(primary_id='ES',currency='USD',multiplier=50,tick_size=NULL,
                    expires='201109', identifiers=NULL,type='future',assign_i=FALSE)
Contr_From_Instr(fut)
ls_futures()
###


future('ES','USD',5000,exchange='GLOBEX',expires='201109') #try with incorrect multipliers
future('SPY','USD',100,exchange='ONE',expires='201109')
update_instruments.IB(c('ES','SPY'))
getContract("ES");getContract("SPY")
getInstrument('ES')$multiplier; getInstrument('SPY')$multiplier #they were corrected.
rm_futures()

#Store an instrument, then call with it's name
future('ES','USD',50,exchange='GLOBEX',expires='201109')
Contr_From_Instr('ES')


#Get front-month future; let IB figure out expiry 
rm_instruments(); rm_currencies()
contract <- twsContract()
contract$symbol <- 'ES'
contract$currency <- 'USD'
contract$sectype <- "FUT"

twsInstrument(contract)
getInstrument('ES')






