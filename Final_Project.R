##Final Project
library(TSA)
library(ecm)
library(quantmod)
library(forecast)

GetTicker<-function(symbol, start, end){
  out<-tryCatch({
    #Try to open the data if it exists
    #find the path to the current symbol
    path =  paste("data/",symbol,"_",as.character(start),"_to_",as.character(end),".csv", sep = "")
    #create a temp df with the current symbol data
    temp <-read.csv(file = path, header = TRUE, sep="," )
    #print(paste("sucessfully read ",symbol," from data directory",sep = ""))
    #rename 
    colnames(temp)[colnames(temp)==colnames(temp)[7]] <- "Price"
    colnames(temp)[colnames(temp)=="X"] <- "Date"
    #drop all but closing price and date
    temp = subset(temp, select = c(Date, Price))
    #if this is the response variable shift the data to the future interval
    #Rename price to symbol name
    colnames(temp)[colnames(temp)=="Price"] <- symbol
    return(temp)
    e<-"couldnt find data"
    
    
  },warning = function(e){
    #if data does not exist, fetch new data and store it
    temp <- getSymbols(symbol, env = NULL, from=as.Date(start, "%d-%b-%y"), to=as.Date(end, "%d-%b-%y"))
    temp= as.data.frame(temp)
    write.csv(as.data.frame(temp), file = paste("data/",symbol,"_",as.character(start),"_to_",as.character(end),".csv", sep = ""))
    #Rename
    colnames(temp)[colnames(temp)==colnames(temp)[6]] <- "Price"
    temp$Date<-rownames(temp)
    #drop all but closing price and date
    temp = subset(temp, select = c(Date, Price))
    #if this is the response variable shift the data to the future interval
    #Rename price to symbol name
    colnames(temp)[colnames(temp)=="Price"] <- symbol
    rownames(temp)<-NULL
    #add to final df
    return(temp)
  },error = function(e){
    print(paste("Could not find ",symbol,".csv"," in data directory and failed fetching from yahoo", sep=""))
  })
}

GetData<-function(symbols, start, end){
  #need to formate the dates from the csv
  dates = seq(as.Date(start, "%d-%b-%y"), as.Date(end, "%d-%b-%y"), by = "days")
  #create the final df with the 1 column of the dates
  df_final = data.frame(dates)
  #renames dates to Date
  colnames(df_final)[colnames(df_final)=="dates"] <- "Date"
  #convert dates to characters
  df_final$Date<-as.character(df_final$Date)
  for (symbol in symbols){
    temp = GetTicker(symbol, start, end)
    df_final = merge(df_final,temp, by="Date", sort = F)
  }
  df_final['Date'] <- as.Date(df_final['Date'][,1])
  return(df_final)
}

TestForUnitRoot <- function(ts.df, alpha = 0.05, lag = NULL){
  unit.root <- vector("list", 0)
  ts.df <- subset(ts.df, select = -c(Date))
  for (i in names(ts.df)){
    adf.res <- 100
    if (!is.null(lag)){
      adf.res <- adf.test(ts.df[[i]], k = lag)
    } else{
      adf.res <- adf.test(ts.df[[i]])
    }
    adf.res.p.value <- adf.res$p.value
    pp.res <-  pp.test(ts.df[[i]])
    pp.res.p.value <- pp.res$p.value
    
    if (adf.res.p.value > alpha || pp.res.p.value > alpha){
      unit.root[i] = T
    } else{
      unit.root[i] = F
    }
  }
  return(unit.root)
}

EstimateCointegrationVector <- function(ts1, ts2){
  fit = lm(as.ts(ts1)~as.ts(ts2))
  return(fit$coefficients)
}

GetCointegratedTS <- function(ts1, ts2, coeff){
  coeff <- EstimateCointegrationVector(ts1,ts2)
  intercept <- coeff[1]
  beta <- coeff[2]
  coint.ts = as.ts(ts1) - intercept - beta*as.ts(ts2)
  return (coint.ts)
}

EGTestForCointegration <- function(ts1, ts2, dates, alpha = 0.05, lag = NULL){
  coint.ts <- GetCointegratedTS(ts1,ts2)
  coint.df <- data.frame(dates)
  colnames(coint.df)[colnames(coint.df)=="dates"] <- "Date"
  coint.df$coint.ts <- coint.ts
  #init df to dates add with name and then test
  #df$coint.ts = coint.ts
  if (TestForUnitRoot(coint.df, alpha, lag) == T){
    return(F)
  } else{
    return(T)
  }
}

BuildECM <- function(df, target, equil, trans, include.intercept = T){
  model = ecm(df[[target]], df[equil], df[trans], include.intercept)
  return(model)
}

EngleGranger<-function(symbol1, symbol2, start, end, alpha = 0.05, lag = NULL){
  df1 = GetData(symbol1, start, end)
  df2 = GetData(symbol2, start, end)
  #Test for unit roots in first stock
  if(TestForUnitRoot(df1) == T){
    print(paste("Times series", symbol2, "Did have a unit root at significance", alpha, "and ADF lag", lag, sep = " "))
  } else{
    print(paste("Times series", symbol1, "Did not have a unit root at significance", alpha, " and ADF lag", lag, sep = " "))
    return()
  }
  #Test for unit root in second stock
  if (TestForUnitRoot(df2) == T){
    print(paste("Times series", symbol2, "Did have a unit root at significance", alpha, "and ADF lag", lag, sep = " "))
  } else{
    print(paste("Times series", symbol2, "Did not have a unit root at significance", alpha, "and ADF lag", lag, sep = " "))
    return()
  }
  #Test for stationarity in the residual of the linear fit
  if (EGTestForCointegration(df1[symbol1],df2[symbol2], df1["Date"])){
    print(paste("pair", symbol1, symbol2, "were cointegrated at significance", alpha, "and ADF lag", lag, sep = " "))
  }else{
    print(paste("pair", symbol1, symbol2, "were not cointegrated at significance", alpha, "and ADF lag", lag, sep = " "))
    return()
  }
  #Develop ECM Model
  #df <- df1
  #df[symbol2]<-df2[symbol2]
  #model = BuildECM(df1, symbol1, symbol2, symbol2)
  #Forecast Model
  #test <-df
  #test$pred <- ecmpredict(model, test[symbol2], test[symbol1][1,] )
  #plot.ts(df[symbol1], type = 'l', col = "blue")
  #par(new = T)
  #plot.ts(test$pred, type = 'l', col = "red")
  #Track PnL
}

OutOfSample<- function(df, symbol1, symbol2, train_start, train_end, forecast_start, forecast_end){
  df1 = GetData(c(symbol1,symbol2), train_start, train_end)
  df2 = GetData(c(symbol1,symbol2), forecast_start, forecast_end)
  #Develop ECM Model
  model = BuildECM(df1, symbol1, symbol2, symbol2)
  #Forecast Model
  test <-df2
  test$pred <- ecmpredict(model, df2[symbol2], df2[symbol1][1,] )
  plot.ts(df2[symbol1], type = 'l', col = "blue")
  par(new = T)
  plot.ts(test$pred, type = 'l', col = "red")
}

ForecastECM <- function(df,symbol1, symbol2, lookahead = 50){
  model = BuildECM(df, symbol1, symbol2, symbol2)
  arima_df = ForecastARIMA(df, symbol2, lookahead)
  pred_df <- data.frame(arima_df$pr)
  pred_df[symbol2] <- arima_df$pr
  forecast <- ecmpredict(model, pred_df[symbol2], tail(df[symbol1],1)[,1] )
  return(forecast)
}

ForecastARIMA<- function(df, symbol, lookahead = 50){
  model=arima(df[symbol],order=c(0,1,0))
  predx=predict(model,n.ahead=lookahead)
  pr=predx$pred
  uci=pr+2*predx$se
  lci=pr-2*predx$se
  df <- data.frame(pr)
  df$uci <- uci
  df$lci <- lci
  return(df)
}

PlotARIMAForecast <- function(df, symbol1, symbol2, lookahead = 50){
  dates <- df['Date']
  orig = data.frame(df['Date'])
  orig['Date'] <-as.Date(orig['Date'][,1])
  orig[symbol1] <- df[symbol1]
  last_date = tail(df['Date'],1)[,1]
  forecast_dates <- as.Date(last_date)+0:lookahead
  forecast_dates <- forecast_dates[-1]
  forecast_df <- data.frame(forecast_dates)
  colnames(forecast_df)[colnames(forecast_df)=='forecast_dates'] <- 'Date'
  forecast <- ForecastECM(df, symbol1, symbol2, lookahead)
  forecast_df[symbol2] <- forecast
  plot_start = orig['Date'][1,]
  plot_end = tail(forecast_df['Date'],1)[,1]
  plot(orig, type = 'l', xlim = c(plot_start,plot_end))
  lines(forecast_df, col =2)
}


# long_signal_dates <- df[df['Spread']<down,]['Date']
# short_signal_dates <- df[df['Spread']>up,]['Date']
# square_off_dates <- df[(df['Spread']>0 & df['Last']<0)| (df['Spread']<0 & df['Last']>0),]['Date']
# long_df = data.frame(long_signal_dates)
# short_df = data.frame(short_signal_dates)
# square_df = data.frame(square_off_dates)
# colnames(long_df)[colnames(long_df)=='long_signal_dates'] <- 'Date'
# colnames(short_df)[colnames(short_df)=='short_signal_dates'] <- 'Date'
# colnames(square_df)[colnames(square_df)=='square_off_dates'] <- 'Date'
# long_df["Long_Signal"]<- pos.size
# short_df["Short_Signal"]<- -pos.size
# square_df["Square_Signal"]<- 0
# new = merge(df, long_df, by = 'Date', all.x = T, all.y=T)
# new = merge(new, short_df, by = 'Date', all.x = T, all.y=T)
# new = merge(new, square_df, by = 'Date', all.x = T, all.y=T)
# sigs = c('Long_Signal', 'Short_Signal', 'Square_Signal')
# new['Signal']<- rowSums(new[sigs])
TrackPNL<-function(df, symbols, resid, beta, std.dev, pos.size){
  df$Spread <- resid
  
  symbol1 <- symbols[1]
  symbol2 <- symbols[2]
  df[paste(symbol1, "Returns", sep= "_")]<-NA
  df[paste(symbol1, "Returns", sep= "_")][2:nrow(df),]<- beta*diff(df[symbol1][,1])
  df[paste(symbol2, "Returns", sep= "_")]<-NA
  df[paste(symbol2, "Returns", sep= "_")][2:nrow(df),]<- diff(df[symbol2][,1])
  df['Last']<- shift(df["Spread"],1)
  df = na.omit(df)
  up = std.dev*2
  down = std.dev*-2
  df['Signal']<-NA
  df[(df['Spread']>0 & df['Last']<0)| (df['Spread']<0 & df['Last']>0),]['Signal']<- 0 
  df[df['Spread']<down,]['Signal'] <- pos.size
  df[df['Spread']>up,]['Signal']<- -pos.size
  df['Signal'] <-  na.locf(na.rm = F, df['Signal'])
  if(nrow(df[is.na(df['Signal']),])>0){
    df[is.na(df['Signal']),]['Signal']<- 0
  }
  df['Signal_Shifted']<-NA
  df['Signal_Shifted']<-shift(df['Signal'],1)
  df[is.na(df['Signal_Shifted']),]['Signal_Shifted']<- 0
  df[df['Signal'] == 0 & df['Signal_Shifted']== pos.size,]['Signal']<-pos.size
  df[df['Signal'] == 0 & df['Signal_Shifted']== -pos.size,]['Signal']<- -pos.size
  df[paste(symbol1, "PnL", sep= "_")]<- -df['Signal_Shifted']*df[paste(symbol1, "Returns", sep= "_")]
  df[paste(symbol2, "PnL", sep= "_")]<- df['Signal_Shifted']*df[paste(symbol2, "Returns", sep= "_")]
  df["PnL"]<- rowSums(df[c(paste(symbol1, "PnL", sep= "_"),paste(symbol2, "PnL", sep= "_"))])
  df['Cummulative_PnL'] <- cumsum(df["PnL"])
  plot.ts(df['Cummulative_PnL'])
  
}


# 
# train_start = "26-Apr-01"
# train_end = "09-Apr-05"
# forecast_start = "09-Apr-05"
# forecast_end = "09-Apr-10"
# start = "26-Apr-06"
# end = "09-Apr-12"
start = Sys.Date()-365
end = Sys.Date()
symbols = c("EWC", "EWA")
symbols = c("XOM","APC","PXD","EOG") #Texas Oil
symbols = c("BRK-A", "BRK-B")
# gold = c("BDD", "SGOL", "IAU", "GLD", "DGP", "DGL", "GDXJ", "DBP", "PICK")
# oil = c("COP", "VLO", "HFC", "WRD", "VNOM")
# symbols = c("SPY", "IVV")
# symbols = c("BDD", "PICK")
# symbols = c("SGOL", "GLD")
# symbols = c("SGOL", "DGL")
# symbols = c("GDX", "GDXJ")
# EngleGranger(symbols[1], symbols[2], Sys.Date()-365, Sys.Date())
# 
# stocks = c("SPY", "IVV", "VOO", "SPDN")
 res = as.data.frame(allpairs.egcm(symbols, i1test = 'adf', urtest = 'adf'))

df = GetData(symbols,start, end)
temp = egcm(df[symbols])
TrackPNL(df, symbols, temp$residuals, temp$beta, temp$residuals.sd,1)
df = GetData(symbols,Sys.Date()-365, Sys.Date())

PlotARIMAForecast(df, symbols[1], symbols[2], 365)
#alpha = .05                 
#TestForUnitRoot(df,alpha)
#ts1 = df$EWC
#ts2 = df$EWA
#dates = df$Date
#adf.test.lag = NULL
#EGTestForCointegration(ts1,ts2, dates, alpha,adf.test.lag)


