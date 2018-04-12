##Final Project
library(TSA)
library(ecm)
library(quantmod)

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
  return(df_final)
}

TestForUnitRoot <- function(ts.df, alpha, lag = NULL){
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
  fit = lm(ts1~ts2)
  return(fit$coefficients)
}

GetCointegratedTS <- function(ts1, ts2, coeff){
  coeff <- EstimateCointegrationVector(ts1,ts2)
  intercept <- coeff[1]
  beta <- coeff[2]
  coint.ts = ts1 - intercept - beta*ts2
  return (coint.ts)
}

EGTestForCointegration <- function(ts1, ts2, dates, alpha, lag){
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

start = "26-Apr-00"
end = "09-Apr-18"
symbols = c("EWC", "EWA")
df = GetData(symbols, start, end)
alpha = .05                 
TestForUnitRoot(df,alpha)
ts1 = df$EWC
ts2 = df$EWA
dates = df$Date
adf.test.lag = NULL
EGTestForCointegration(ts1,ts2, dates, alpha,adf.test.lag)


