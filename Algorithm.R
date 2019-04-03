  library('finreportr') #For financial statement data
  library('stringi') #For reading the start of the dates
  library('quantmod') #For stock price development
  library('ustyc') #For US T bill yields
  
  setwd("C:/Users/atteb/Dropbox/Knowledge/self studies/R projects/Automated Valuation Model")
  
  #Use Cashflowstatements?
CashFlowStatements <- FALSE
  #Do u want to save the calculations as a CSV file after algo is done?
SaveCalculations <- FALSE
  #Get tickers from the stocks file?
UseDefaultlist <- FALSE
  #If above is FALSE, insert specific stocks to analyze

Specifics <- c("FB")
  #In case of stupid error, TRUE will start algo from where the stock where the error occured
  #Remember to make a copy of mycalculations before running......

ForceRestart <- FALSE
  
  
  #___________________________________Start_______________________________________________________________
  #____________________________________________________________________________________________________
  
  MyTickers <- if (UseDefaultlist ==TRUE) {
    read.csv("Stocks for analysis.csv")
  } else (Specifics)

  MyTickers <- unlist(MyTickers)                       
  
  
  if (ForceRestart == TRUE) {
    MyTickers <- MyTickers[which(MyTickers==Ticker):length(MyTickers)]
    write.csv(MyCalculations,file = "MyCalculationsUpToError.csv")
  }
  
  #_______________________________Initiate dataframe in which data can be recorded____________________________________________________________________________________________
  #For successful tickers
  Informations <- c("Ticker",
                    "Date of latest report",
                    "Revenue (Billion)",
                    "Market Cap (Billion)",
                    "Enterprice Value (Billion)",
                    "Debt to Assets",
                    "Net Working Capital (Billion)",
                    "DSO",
                    "DPO",
                    "DIO",
                    "Cash Conversion Cycle",
                    "Latest Earnings",
                    "Price to Earnings",
                    "Dividend yield",
                    "Market to Book",
                    "Price to Sales",
                    "Price to NWC + Cash",
                    "NWC to Sales",
                    "Return on Sales",
                    "Expectations",
                    "Current Ratio",
                    "Current Ratio test",
                    "Quick Ratio",
                    "AcidTest",
                    "Interest Coverage Ratio",
                    "Beta to SPX",
                    "Beta to Nasdaq100",
                    "Beta to VTI",
                    "Beta to DJIA",
                    "LTM Return",
                    "LTM Standard deviation of stock (in %)",
                    "LTM Sharpe Ratio",
                    "LTM Treynor Ratio"
                    )
  
  MyCalculations <- data.frame(matrix(ncol = length(Informations), nrow = 0))
  colnames(MyCalculations)<- Informations
  #For tickers with errors
  Errortickers <- "these tickers had errors:"
  #__________________________________________Download marketdata_________________________________________________________________
  #______________________________________________________________________________________________________________________________________________
  #Indices for benchmarking
  VTI <-                getSymbols(Symbols = 'VTI',
                                   auto.assign = FALSE,
                                   from = as.Date(Sys.time())-365)
  VTI$RelativeReturns <- diff(log(VTI$VTI.Adjusted))[-1]
  
  SPX <-                getSymbols(Symbols = 'SPY',
                                   auto.assign = FALSE,
                                   from = as.Date(Sys.time())-365)
  SPX$RelativeReturns <- diff(log(SPX$SPY.Adjusted))[-1]
  
  DJIA <-               getSymbols(Symbols = 'DIA',
                                   auto.assign = FALSE,
                                   from = as.Date(Sys.time())-365)
  DJIA$RelativeReturns <- diff(log(DJIA$DIA.Adjusted))[-1]
  
  Nasdaq100 <-          getSymbols(Symbols = 'QQQ',
                                   auto.assign = FALSE,
                                   from = as.Date(Sys.time())-365)
  Nasdaq100$RelativeReturns <- diff(log(Nasdaq100$QQQ.Adjusted))[-1]
  
  #Tbills for risk free rate
  USYieldCurves <- getYieldCurve(year = c(2017), 
                                 month = NULL, 
                                 base = "http://data.treasury.gov/feed.svc/DailyTreasuryYieldCurveRateData", 
                                 allowParallel = FALSE
  )
  
  Yields <- USYieldCurves$df
  rownames(Yields) <- as.Date(rownames(Yields))
  Yields$TimeStamp <- as.Date(rownames(Yields))
  #1 year riskfree rate
  RiskfreeRate12month <- head(Yields[Yields$TimeStamp >= as.Date(Sys.time())-365,],1)$BC_1YEAR
  #_______________________________________________________________________________________________________________________________
  
  #Make n that is used to count down how many stocks are left
  n <- 1
  
  #___________________________________________________________________________________________________________________________________________
  #_________________________________________LOOP STARTS____________________________________________________________________________________________
  #____________________________________________________________________________________________________________________________________________
  
  for(Ticker in MyTickers){
    #Time used for measuring runtime
    starttime <- Sys.time()  
    
  #_______________________Downloading and formatting company specific data____________________________________________________________
  #___________________________________________________________________________________________________________________________________________
  #Edgar downloads
    Incomestatement <-    tryCatch(GetIncome(Ticker,year = 2018),
                                   error = function(cond) paste("ERROR IN DOWNLOAD"))
    BalanceSheet <-       tryCatch(GetBalanceSheet(Ticker, year = 2018),
                                   error = function(cond) paste("ERROR IN DOWNLOAD"))
    if (CashFlowStatements == TRUE) {
      CashFlows <-          tryCatch(GetCashFlow(Ticker, year = 2018),
                                     error = function(cond) paste("ERROR IN DOWNLOAD"))
    }
    
    if (length(Incomestatement)==1) {
      print(paste("Statements download for",Ticker, "had an error"))
      Incomestatement <- paste("ERROR IN DOWNLOAD")
      Errortickers <- rbind(Errortickers,Ticker)
    }
    
  
    #Quantmod
    Stock <-              tryCatch(getSymbols(Symbols = Ticker,
                                              auto.assign = FALSE,
                                              from = as.Date(Sys.time())-365),
                                   error = function(cond) paste("ERROR IN DOWNLOAD")
                                   )
    
    #Get dividend yield
    div <- tryCatch(getDividends(Ticker, from=as.Date(Sys.time())-365, to=as.Date(Sys.time())),
                    error = function(cond) paste("ERROR IN DOWNLOAD")
    )
    if (nrow(div) > 0) {
      ydiv <- runSum(div, n=4) # rolling sum of last 4 quarters
      out <- na.locf(merge(Stock[,4], ydiv, all=TRUE)) # merge and fill in NAs with previous values
      out$yld <- out[, 2] / out[, 1] # "current yield"
      DividendYield <- mean(tail(out)$yld)
    } else {
      DividendYield <- 0
    }
    
  
  #Errorcatcher which assesses which operations to perform and skips stock if downloads didnt work
  #Check if the stock data contains NAs
    
  if (length(Stock)>1) {
    if (sum(is.na(Stock$Close)==TRUE)>0 | length(Stock[,4]) != length(SPX$SPY.Close)) {
      Stock <- paste("ERROR IN DOWNLOAD")
      print(paste("Stock data download for",Ticker, "had an error"))
    }
  } 
    #Skip if BS stock
    if (length(Incomestatement)>1 & length(BalanceSheet)>1 & length(Stock)>1) {
  
      #Rename columns
      names(Stock) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
      
      #Add relative return
      Stock$RelativeReturns <- diff(log(Stock$Adjusted))[-1]
      
      #Save first recorded stock price
      FirstRecordedPriceDate <- paste(as.character(time(first(Stock))),"/",
                                      sep = "")
      
      #Converting Incomestatement to acceptable format
      Incomestatement$ReportType <- ifelse(as.Date(Incomestatement$endDate) - as.Date(Incomestatement$startDate)>360,
                                           yes = "10K",
                                           no = "10Q")
      LatestAnnualIncome <- Incomestatement[which(Incomestatement$endDate == max(Incomestatement[Incomestatement$ReportType=="10K",]$endDate) & Incomestatement$ReportType=="10K"),]
      LatestAnnualIncome$Amount <- as.numeric(LatestAnnualIncome$Amount)
      LatestAnnualIncome$Metric <- tolower(LatestAnnualIncome$Metric)
      
      
      #Converting Balance sheet to acceptable format
      LatestAnnualBalanceSheet <- BalanceSheet[BalanceSheet$endDate == max(Incomestatement[Incomestatement$ReportType=="10K",]$endDate),]
      LatestAnnualBalanceSheet$Amount <- as.numeric(LatestAnnualBalanceSheet$Amount)
      LatestAnnualBalanceSheet$Metric <- tolower(LatestAnnualBalanceSheet$Metric)
      
      if (CashFlowStatements == TRUE) {
        #Converting cashflowstatement to acceptable format
        CashFlows$ReportType <- ifelse(as.Date(CashFlows$endDate) - as.Date(CashFlows$startDate)>360,
                                       yes = "10K",
                                       no = "10Q")
        LatestAnnualCF <- CashFlows[which(CashFlows$endDate == max(Incomestatement[Incomestatement$ReportType=="10K",]$endDate) & CashFlows$ReportType=="10K"),]
        LatestAnnualCF$Amount <- as.numeric(LatestAnnualCF$Amount)
        LatestAnnualCF$Metric <- tolower(LatestAnnualCF$Metric)
      }  
      
      #The lates full year report
      LatestReportDate <- max(Incomestatement[Incomestatement$ReportType=="10K",]$endDate)
      
      
  #________________________Extracting key financial numbers based on word search____________________________________________________________
  #_________________________________________________________________________________________________________________________________________
      
      #Income Statement
      #Revenue
      Revenue <- max(LatestAnnualIncome[grepl("revenue|sales", LatestAnnualIncome$Metric),]$Amount)
      #COGS
      COGS <- head(LatestAnnualIncome[grepl("cost", LatestAnnualIncome$Metric),]$Amount,1)
      #Operating profit
      OperatingIncome <- head(LatestAnnualIncome[grepl("operating income", LatestAnnualIncome$Metric),]$Amount,1)
      #Interest Expense
      InterestExpense <- abs(head(LatestAnnualIncome[grepl("interest expense", LatestAnnualIncome$Metric),]$Amount,1))
      #Earnings
      Earnings <-  tail(LatestAnnualIncome[grepl("net income", LatestAnnualIncome$Metric),]$Amount,1)
      
      #Balancesheet    
      #CurrentAssets
      CurrentAssets <- ifelse(max(length(LatestAnnualBalanceSheet[grepl("assets, current|current assets|assets current", LatestAnnualBalanceSheet$Metric),]$Amount))==0,0,no = max(LatestAnnualBalanceSheet[grepl("assets, current|current assets|assets current", LatestAnnualBalanceSheet$Metric),]$Amount))      
      AR <- max(ifelse(length(LatestAnnualBalanceSheet[grepl("accounts receivable", LatestAnnualBalanceSheet$Metric),]$Amount)==0,0,LatestAnnualBalanceSheet[grepl("accounts receivable", LatestAnnualBalanceSheet$Metric),]$Amount))
      Cash <- max(LatestAnnualBalanceSheet[grepl("cash", LatestAnnualBalanceSheet$Metric),]$Amount)
      MarketableSecurities <- ifelse(length(LatestAnnualBalanceSheet[grepl("marketable securities", LatestAnnualBalanceSheet$Metric),]$Amount)==0,0,no = LatestAnnualBalanceSheet[grepl("marketable securities", LatestAnnualBalanceSheet$Metric),]$Amount)
      Inventory <- max(ifelse(length(LatestAnnualBalanceSheet[grepl("inventory", LatestAnnualBalanceSheet$Metric),]$Amount)==0,0,no = LatestAnnualBalanceSheet[grepl("inventory", LatestAnnualBalanceSheet$Metric),]$Amount)) 
      
      #CurrentLiabilities
      CurrentLiabilities <- max(LatestAnnualBalanceSheet[grepl("liabilities, current|current liabilities|liabilities current", LatestAnnualBalanceSheet$Metric),]$Amount)    
      AP <-max(LatestAnnualBalanceSheet[grepl("accounts payable", LatestAnnualBalanceSheet$Metric),]$Amount)
      
      #What could be liabilities
      PossibleLiabilities <- LatestAnnualBalanceSheet[grepl("liabilities|liability", LatestAnnualBalanceSheet$Metric),]
      #Remove equity and take biggest number
      TotalLiabilites <- tail(PossibleLiabilities[!grepl("equity", PossibleLiabilities$Metric),]$Amount,1)
      #Totalassets
      TotalAssets <- max(LatestAnnualBalanceSheet[grepl("assets", LatestAnnualBalanceSheet$Metric),]$Amount)
      
      #Outstanding Shares
      SharesOutstanding <- LatestAnnualIncome[LatestAnnualIncome$Units!="usd",]
      SharesOutstanding <- max(SharesOutstanding[grepl("share", SharesOutstanding$Units),]$Amount)
      
      #Cashflow statement
      
      if (CashFlowStatements == TRUE) {    
        #D&A
        DnA <-  LatestAnnualCF[grepl("depreciation|amortization", LatestAnnualCF$Metric),]$Amount

        #Change in NWC
        DeltaAR <- LatestAnnualCF[grepl("accounts receivable", LatestAnnualCF$Metric),]$Amount
        DeltaAP <- LatestAnnualCF[grepl("accounts payable", LatestAnnualCF$Metric),]$Amount
        DeltaInventory <- LatestAnnualCF[grepl("inventory", LatestAnnualCF$Metric),]$Amount
      }  
      
#_________________________________________Calculation of numbers__________________________________________________________________
#_______________________________________________________________________________________________________________________________
      
      #Key Statistic calculations
      
      if(CashFlowStatements == TRUE) {  
        FreeCashFlow <- Earnings + DnA
      }
      
      MarketCap <- as.numeric(last(Stock$Close)) * SharesOutstanding
      
      EV <- MarketCap + TotalLiabilites - Cash
      
      LeverageRatio <- TotalLiabilites/TotalAssets
      
      InterestCoverageRatio <- InterestExpense / OperatingIncome
      
      PE <- MarketCap / Earnings
      
      MB <- MarketCap / TotalAssets
      
      PS <- MarketCap / Revenue
      
      #Räkna ut EV/EBITDA, EV/EBIT, P/Owner earning
      
      ROS <- Earnings / Revenue
      
      
      GrowthExpectations <- ifelse(PE*MB<22.5&PE*MB>0,
                                   yes = "Value", 
                                   no = "Growth"
      )
      
      
      #Cash Conversion Cycle
      DSO <- (AR / Revenue) * 365
      DPO <- (AP / COGS) * 365
      DIO <- (Inventory / COGS) * 365
      NWC <- CurrentAssets - CurrentLiabilities
      CCC <- DSO + DIO - DPO
      
      #Price to NWC and Cash
      PNNWC <- MarketCap / NWC 
      
      #NWC to Sales
      NWCtoSales <- NWC / Revenue
      
      #Liquidity Tests
      #Current Ratio
      CurrentRatio <- CurrentAssets / CurrentLiabilities
      
      CurrentRatioTest <- ifelse(CurrentRatio<2,
                                 yes = "Fail",
                                 no = "Pass")
      
      #AcidTest / Quick Ratio
      AcidTestRatio <- (Cash + AR + MarketableSecurities) / AP
      
      AcidTest <- ifelse(AcidTestRatio<1,
                         yes = "Fail",
                         no = "Pass")
      
      
      #Calculate Betas
      SPXBeta <- cov(as.numeric(Stock$RelativeReturns[-1,]),as.numeric(SPX$RelativeReturns[-1,]))/as.numeric(var(SPX$RelativeReturns[-1]$RelativeReturns))
      DJIABeta <- cov(as.numeric(Stock$RelativeReturns[-1,]),as.numeric(DJIA$RelativeReturns[-1,]))/as.numeric(var(DJIA$RelativeReturns[-1,]))
      VTIBeta <- cov(as.numeric(Stock$RelativeReturns[-1,]),as.numeric(VTI$RelativeReturns[-1,]))/as.numeric(var(VTI$RelativeReturns[-1,]))
      Nasdaq100Beta <- cov(as.numeric(Stock$RelativeReturns[-1,]),as.numeric(Nasdaq100$RelativeReturns[-1,]))/as.numeric(var(Nasdaq100$RelativeReturns[-1,]))
      
      #Calculate LTM return and sharpe + treynor ratio
      LTMReturn <- (as.numeric(tail(Stock,1)$Close)/as.numeric(head(Stock$Close,1)))-1
      StandardDeviation <- sd(Stock$Close)/mean(Stock$Close)
      SharpeRatio <- (LTMReturn- (RiskfreeRate12month/100))/StandardDeviation
      TreynorRatio <- (LTMReturn- (RiskfreeRate12month/100))/ mean(SPXBeta,DJIABeta,VTIBeta,Nasdaq100)
      
      #Implied growth
  
      AssumptionFunction <- function(CAGR){
      WACC = ((Earnings * ( 1 + CAGR )) / EV ) + CAGR
      }
      
      
  #________________________________Bind calculations to dataframe_________________________________________________________________
  #______________________________________________________________________________________________________________________________  
      
      KPIs <-   c(Ticker
                  ,LatestReportDate
                  ,round(Revenue/(10^9),digits = 2)
                  ,ifelse(is.numeric(MarketCap),round(MarketCap/10^9,digits = 2),"Error")
                  ,round(EV/10^9,digits = 1)
                  ,round(LeverageRatio,digits = 2),round(NWC/10^9,3)
                  ,paste(round(DSO,0), "Days")
                  ,paste(round(DPO,0), "Days")
                  ,paste(round(DIO,0), "Days")
                  ,paste(round(CCC, 0),"Days")
                  ,Earnings
                  ,round(PE,digits = 2)
                  ,round(DividendYield, digits = 4)
                  ,round(MB,digits = 2)  
                  ,round(PS,digits = 2)
                  ,round(PNNWC,digits = 2)
                  ,round(NWCtoSales, digits = 2)
                  ,ifelse(length(ROS)==0,yes = NA,no = round(ROS, digits = 3))
                  ,GrowthExpectations
                  ,round(CurrentRatio,digits = 2)  
                  ,CurrentRatioTest
                  ,round(AcidTestRatio,digits = 2)
                  ,AcidTest
                  ,round(InterestCoverageRatio, digits = 2)
                  ,round(SPXBeta, digits = 2)
                  ,round(Nasdaq100Beta, digits = 2)
                  ,round(VTIBeta, digits = 2)
                  ,round(DJIABeta, digits = 2) 
                  ,round(LTMReturn, digits = 2)
                  ,round(StandardDeviation,digits = 2)
                  ,round(SharpeRatio,digits = 2)
                  ,round(TreynorRatio, digits = 2)
      )
      
      ColN <- 1  
      
      for (KPI in KPIs) {
        MyCalculations[n,ColN] <- ifelse(test = length(KPI)==0,
                                         yes = "Empty",
                                         no = KPI)
        ColN <- ColN + 1
      }
      
      
      Runtime <- difftime(Sys.time(), starttime, units = "mins")
      print(paste(Ticker, " completed, ", length(MyTickers)-n, " stocks remaining, ~", round((length(MyTickers)-n)*Runtime,digits = 0), " minutes left",sep = ""))
      
      n <- n+1
              
    } 
  }
  
#__________________________________________Finally done________________________________________________________________________________________________________
  
  View(MyCalculations)
  
  if (SaveCalculations == TRUE) {
  write.csv(MyCalculations, "My Calculations.csv")
  write.csv(Errortickers, "ErrorStocks.csv")
  }
  