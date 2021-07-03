//@version=4
//BTC, ETH ,BNB tf = 1H lot 6 lev 15
strategy("EMA CROSS MARK", overlay = true, default_qty_type=strategy.cash, default_qty_value=25, currency = currency.USD , initial_capital = 1000 , commission_type = strategy.commission.percent,commission_value = 0.02)

startDate = input(defval = timestamp("01 Jan 1970 00:00"), title = "Start Date", type=input.time)
endDate = input(defval =timestamp("31 Jun 2021 00:00"), title = "End Date", type=input.time)


_src = input(title = "Source", defval=close)
ma_Fast = input(title = "EMA Fast", defval=8)
ma_Middle = input(title = "EMA Middle", defval=25)
ma_Slow = input(title = "EMA Slow", defval=50)


//-----Calculattions
MA_fast = ema(_src , ma_Fast)
MA_middle = ema(_src , ma_Middle)
MA_slow = ema(_src , ma_Slow)

EACH_LOT = input( defval = 4 , title = "LOTS qty" , type = input.integer , group = "POSITION CONFIG" )
LEVERAGE = input( defval = 10 , title = "Leverage" , type = input.integer , group = "POSITION CONFIG"  )
POSITION_SIZE = (strategy.equity/EACH_LOT) * LEVERAGE
CONTRACT_SIZE = round(POSITION_SIZE/close,6)

longTrailPerc = input(title="Trail Long Loss (%)",type=input.float, minval=0.2, step=0.1, defval=3) * 0.01
shortTrailPerc = input(title="Trail Short Loss (%)",type=input.float, minval=0.2, step=0.1, defval=3) * 0.01

stopValueL = close * (1 - longTrailPerc)
if MA_fast > MA_middle and  MA_middle > MA_slow
    if _src > MA_fast and (strategy.position_size <= 0)
        strategy.close("ES" , alert_message = "CLOSE SHORT")
        strategy.entry(id="EL", long = true , qty = CONTRACT_SIZE ,alert_message = "OPEN LONG")
if MA_middle > MA_fast and  MA_fast > MA_slow 
    strategy.exit(id="XL",from_entry = "EL" , stop = stopValueL)   
    
   
stopValueS = close * (1 + shortTrailPerc)
if MA_slow > MA_middle and  MA_middle > MA_fast
    if _src < MA_slow and (strategy.position_size > 0)
        strategy.close("EL" , alert_message = "CLOSE LONG")
        strategy.entry(id="ES", long = true , qty = CONTRACT_SIZE ,alert_message = "OPEN SHORT")
if MA_slow > MA_fast and  MA_fast > MA_middle  
    strategy.exit(id="XS",from_entry = "ES", stop = stopValueS ) 
    

    
