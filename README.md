// This source code is subject to the terms of the Mozilla Public License 2.0 at https://mozilla.org/MPL/2.0/
// © jpdcrypto
//@version=4
strategy("MOM_SQZ-RSI SCALPING JIB", overlay = true, pyramiding = 0, default_qty_type=strategy.cash, currency = currency.USD,initial_capital = 1000 , commission_type = strategy.commission.percent,commission_value = 0.02)
startDate = input(defval = timestamp("14 Feb 2021 00:00"), title = "Start Date", type=input.time)
endDate = input(defval =timestamp("15 Jul 2021 00:00"), title = "End Date", type=input.time)
PeriodTB = time >= startDate and time <= endDate ? true : false

lengthATR = input(title="Length", defval=8, minval=1 ,group = "-----ATR------"  )
smoothing = input(title="Smoothing", defval="SMA", options=["RMA", "SMA", "EMA", "WMA"],group = "-----ATR------")

lengthMOM = input(22, title="BB Length",group = "---- SQZMOM ----")
mult = input(4.0,title="BB MultFactor",group = "---- SQZMOM ----")
lengthKC=input(21, title="KC Length" ,group = "---- SQZMOM ----")
multKC = input(4.0, title="KC MultFactor" ,group = "---- SQZMOM ----")

useTrueRange = input(true, title="Use TrueRange (KC)", type=input.bool)

smoothK = input(5, "K", minval=1 ,group = "---- RSI-STO ----")
smoothD = input(5, "D", minval=1,group = "---- RSI-STO ----")
lengthRSI = input(13, "RSI Length", minval=1 ,group = "---- RSI-STO ----")
lengthStoch = input(15, "Stochastic Length", minval=1 ,group = "---- RSI-STO ----")

src = input(close, title="Source")
//fast = input(5, minval=1, title="Lips Length")
//medium = input(9, minval=1, title="Teeth Length")
//slow = input(13, minval=1, title="Jaw Length")

//-----Calculattions
//Alligator


smma(src, length) =>
	smma =  0.0
	smma := na(smma[1]) ? sma(src, length) : (smma[1] * (length - 1) + src) / length
	smma
jawLength = input(12, minval=1, title="Jaw Length" ,group = "---- ALLIGATOR ----" )
teethLength = input(8, minval=1, title="Teeth Length" ,group = "---- ALLIGATOR ----")
lipsLength = input(5, minval=1, title="Lips Length" ,group = "---- ALLIGATOR ----")
jawOffset = input(8, title="Jaw Offset" ,group = "---- ALLIGATOR ----")
teethOffset = input(5, title="Teeth Offset" ,group = "---- ALLIGATOR ----")
lipsOffset = input(5, title="Lips Offset" ,group = "---- ALLIGATOR ----")

j = smma(close, jawLength)  //Slow
t = smma(close, teethLength)  //Middle
l = smma(close, lipsLength)  //Fast
plot(j, "Jaw", offset = jawOffset, color=#3BB3E4)  
plot(t, "Teeth", offset = teethOffset, color=#FF006E)  
plot(l, "Lips", offset = lipsOffset, color=#36C711)   

//l = ema(close, fast)
//t = sma(close, medium)
//j = sma(close, slow)
//plot(l, color=color.green)
//plot(t, color=color.blue)
//plot(j, color=color.red)

//ATR
ma_function(src, lengthATR) =>
	if smoothing == "SMA"
		sma(src, lengthATR)
	else
		if smoothing == "RMA"
			rma(src, lengthATR)
		else
			if smoothing == "EMA"
				ema(src, lengthATR)
			else
				wma(src, lengthATR)

//plot(ma_function(tr(true), lengthATR), title = "ATR", color=#991515, transp=0)				
Atr_Val = ma_function(tr(true),lengthATR)				


//SQZMOM_LB
// Calculate BB
basis = sma(src, lengthMOM)
dev = multKC * stdev(src, lengthMOM)
upperBB = basis + dev
lowerBB = basis - dev

// Calculate KC
ma = sma(src, lengthKC)
range = useTrueRange ? tr : (high - low)
rangema = sma(range, lengthKC)
upperKC = ma + rangema * multKC
lowerKC = ma - rangema * multKC

sqzOn  = (lowerBB > lowerKC) and (upperBB < upperKC)
sqzOff = (lowerBB < lowerKC) and (upperBB > upperKC)
noSqz  = (sqzOn == false) and (sqzOff == false)

h = linreg(src  -  avg(avg(highest(high, lengthKC), lowest(low, lengthKC)),sma(src,lengthKC)),lengthKC,0)

//bcolor = iff( sqm_h > 0,iff( sqm_h > nz(sqm_h[1]), color=color.lime, color=color.green),iff( sqm_h < nz(sqm_h[1]), color=color.red, color=color.maroon))
//scolor = noSqz ? color.blue : sqzOn ? color.black : color.gray 
//plot(sqm_h, color=(iff( sqm_h > 0,iff( sqm_h > nz(sqm_h[1]), color=color.lime, color=color.green),iff( sqm_h < nz(sqm_h[1]), color=color.red, color=color.maroon))), style=plot.style_histogram , linewidth=4)

rsi1 = rsi(ohlc4, lengthRSI)
k = sma(stoch(rsi1, rsi1, rsi1, lengthStoch), smoothK)  // Blue
d = sma(k, smoothD) // Orange

//Signals
allEntryTrend = j[2] > t[2] and t[2] > l[2] and j[1] > t[1] and t[1] > l[1] and j[0] > t[0] and t[0] > l[0] ? 1 : j[2] < t[2] and t[2] < l[2] and j[1] < t[1] and t[1] < l[1] and j[0] < t[0] and t[0] < l[0] ? -1 : 0 //1 = Long, -1 = Short , 0 = sideway
inTrendorXT = l[4] < l[3] and l[3] < l[2] and l[2] < l[1] and l[1] < l[0] and t[4] < t[3] and t[3] < t[2] and t[2] < t[1] and t[1] < t[0] and j[4] < j[3] and j[3] < j[2] and j[2] < j[1] and j[1] < j[0] and l > t and t > j ? 1 : l[4] > l[3] and l[3] > l[2] and l[2] > l[1] and l[1] > l[0] and t[4] > t[3] and t[3] > t[2] and t[2] > t[1] and t[1] > t[0] and j[4] > j[3] and j[3] > j[2] and j[2] > j[1] and j[1] > j[0] and l < t and t < j? -1 : 0
sqmomSignal01 = h < 0 ? 1 : -1
sqmomSignal02 = h[3] < 0 and h[2] < 0 and h[1] < 0 and h[0] > 0 ? 1 : h[3] > 0 and h[2] > 0 and h[1] > 0 and h[0] < 0 ? -1 : na
rstoSignal  = crossover(k[2],d[2]) or crossover(k[1],d[1]) or crossover(k[0],d[0]) ? 1 : crossover(d[2],k[2]) or crossover(d[1],k[1]) or crossover(d[0],k[0]) ? -1 : 0
posSignalT  = k >= 10 and k <= 50 and d >= 10 and d <= 50 ? 1 : k > 50 and k <= 90 and d > 50 and d <= 90 ? -1 : na
posSigSidw  = k > 0 and k < 15 and d > 0 and d < 15 ? 1 : k > 85 and k < 100 and d > 85 and d < 100 ? -1 : na

E = ""
if allEntryTrend == 1 and sqmomSignal01 == 1 and rstoSignal == 1 and posSignalT == 1 //and inTrendorXT == 1
    E := "LONG"
    
if allEntryTrend == -1 and sqmomSignal01 == -1 and rstoSignal == -1 and posSignalT == -1 //and inTrendorXT == -1
    E := "SHORT"

if allEntryTrend == 0 and rstoSignal == 0 and posSignalT == 0 and posSigSidw == 1
    E := "SIDEL"
    
if allEntryTrend == 0 and rstoSignal == 0 and posSignalT == 0 and posSigSidw == -1
    E := "SIDES"    

rstoXL = k > 50 and k < 85 and d > 50 and d < 85 ? 1 : na
rstoXS = k > 15 and k < 50 and d > 15 and d < 50 ? 1 : na
alliXL = l > t and t > j ? 1 : na
alliXS = j > t and t > l ? 1 : na
rsiCross = crossunder(k,d) ? 1 : crossover(k,d) ? 2 : na
sqh = h > 0 ? 1 : h < 0 ? 2 : na

X = ""
if rstoXL == 1 and alliXL == 1 and rsiCross == 1 and sqh == 1
    X := "XLONG" 
   
if rstoXS == 1 and alliXS == 1 and rsiCross == 2 and sqh == 2
    X := "XSHORT"    

XSlong = crossunder(l,t)
XSshort = crossover(l,t)

EACH_LOT = input( defval = 13 , title = "LOTS qty" , type = input.integer , group = "---- POSITION CONFIG ----" )
LEVERAGE = input( defval = 10 , title = "Leverage" , type = input.integer , group = "---- POSITION CONFIG ----" )
POSITION_SIZE = (strategy.equity/EACH_LOT) * LEVERAGE
CONTRACT_SIZE = round(POSITION_SIZE/close,6)

longTrailPerc = input(title="Trail Long Loss (%)",type=input.float, minval=0.2, step=0.1, defval=4 ,group = "---- POSITION CONFIG ----" ) * 0.01
shortTrailPerc = input(title="Trail Short Loss (%)",type=input.float, minval=0.2, step=0.1, defval=4 ,group = "---- POSITION CONFIG ----" ) * 0.01

stopTrailperL = close - Atr_Val
stopLqL    = close * (1 - 1/LEVERAGE)
if E == "LONG" 
    if (strategy.position_size <= 0) and PeriodTB
        strategy.close(id="ES01" , alert_message = "CLOSE SHORT")
        strategy.close(id="ES02" , alert_message = "CLOSE SHORT")
        //strategy.close(id="ES02" , alert_message = "CLOSE SHORT")
        strategy.entry(id="EL01", long = true , qty = CONTRACT_SIZE ,stop =stopLqL ,alert_message = "OPEN LONG")
        strategy.entry(id="EL02", long = true , qty = CONTRACT_SIZE ,stop =stopLqL ,alert_message = "OPEN LONG")
if X == "XLONG"
    strategy.exit(id="XL01",from_entry = "EL01" , stop = stopTrailperL, alert_message = "EXIT LONG")  
    strategy.exit(id="XL01",from_entry = "EL02" , stop = stopTrailperL, alert_message = "EXIT LONG")  

if E == "SIDEL"
    if (strategy.position_size <= 0) and PeriodTB
        strategy.close(id="ES03" , alert_message = "CLOSE SHORT")
        strategy.close(id="ES04" , alert_message = "CLOSE SHORT")
            //strategy.close(id="ES02" , alert_message = "CLOSE SHORT")
        strategy.entry(id="EL03", long = true , qty = CONTRACT_SIZE ,stop =stopLqL ,alert_message = "OPEN LONG")
        strategy.entry(id="EL04", long = true , qty = CONTRACT_SIZE ,stop =stopLqL ,alert_message = "OPEN LONG")
if XSlong == 1
    strategy.exit(id="XL03",from_entry = "EL03" ,limit = close + Atr_Val , stop = stopTrailperL, alert_message = "EXIT LONG")  
    strategy.exit(id="XL04",from_entry = "EL04" ,limit = close + Atr_Val , stop = stopTrailperL, alert_message = "EXIT LONG")    

stopTrailperS = close + Atr_Val
stopLqS    = close * (1 + 1/LEVERAGE)
if E == "SHORT"
    if (strategy.position_size > 0) and PeriodTB
        strategy.close(id="EL01" , alert_message = "CLOSE LONG")
        strategy.close(id="EL02" , alert_message = "CLOSE LONG")
        strategy.entry(id="ES01", long = false , qty = CONTRACT_SIZE ,stop=stopLqS ,alert_message = "OPEN SHORT")
        strategy.entry(id="ES02", long = false , qty = CONTRACT_SIZE ,stop=stopLqS ,alert_message = "OPEN SHORT") 
if X == "XSHORT"
    strategy.exit(id="XS01",from_entry = "ES01", stop = stopTrailperS ,alert_message = "EXIT SHORT")
    strategy.exit(id="XS02",from_entry = "ES02", stop = stopTrailperS ,alert_message = "EXIT SHORT")
    
if E == "SIDES" 
    if (strategy.position_size > 0) and PeriodTB
        strategy.close(id="EL03" , alert_message = "CLOSE LONG")
        strategy.close(id="EL04" , alert_message = "CLOSE LONG")
        strategy.entry(id="ES03", long = false , qty = CONTRACT_SIZE ,stop=stopLqS ,alert_message = "OPEN SHORT")
        strategy.entry(id="ES04", long = false , qty = CONTRACT_SIZE ,stop=stopLqS ,alert_message = "OPEN SHORT") 
if XSshort == 2
    strategy.exit(id="XS03",from_entry = "ES03", limit = close - Atr_Val, stop = stopTrailperS, alert_message = "EXIT SHORT")
    strategy.exit(id="XS04",from_entry = "ES04", limit = close - Atr_Val, stop = stopTrailperS, alert_message = "EXIT SHORT")
