# Set working directory to source file location. Do that through the tab session.
rm(list=ls())
options(digits=5)
#====== Packages and keys =============
library(easypackages)
libraries("Quandl","dygraphs")
Quandl.api_key("Xa5ufhXiSJNGhAwY_P6u")

#====== Various data formats ================
BTCoin=Quandl("BCHAIN/MKPRU", api_key="Xa5ufhXiSJNGhAwY_P6u")
head(BTCoin,10)
tail(BTCoin,10)

bitcoin <- Quandl("BCHARTS/BTC24USD", type = "xts")

dygraph(bitcoin$"Open", main = "Bitcoin Exchange Rate (BTC vs. USD)")

write.csv(bitcoin,"../Data/Bitcoin.csv")
