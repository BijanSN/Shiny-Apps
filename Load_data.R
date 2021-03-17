#---- Load data -------------------------------------------

# 3 random stocks + Benchmark : Nvidia, Apple, Visa & S&P500 
getSymbols(c("NVDA", "AAPL", "V","SPY"))

# Get adjusted prices
prices.data <- merge.zoo(NVDA[,6], AAPL[,6], V[,6], SPY[,6])
prices.data %<>% na.omit() 

# Calculate returns
returns.data <- CalculateReturns(prices.data)
returns.data <- na.omit(returns.data)
returns.data <- fortify.zoo(returns.data)

returns.data %<>% 
  rename(
    Nvdia = NVDA.Adjusted,
    Apple = AAPL.Adjusted,
    Visa  = V.Adjusted,
    SP500  = SPY.Adjusted
  )

prices.data %<>% fortify.zoo() %>% 
  rename(
    Nvdia = NVDA.Adjusted,
    Apple = AAPL.Adjusted,
    Visa  = V.Adjusted,
    SP500  = SPY.Adjusted
  )
