# Sett working directory to source file location. Do that through the tab session.
rm(list=ls()) #clean up everything, good for large stuff and time series
options(digits=10)
##===== Loads appropriate libraries. Make sure they are installed. If not install them from the tab "packages" in R-Studio======================================
library("easypackages")
libraries("htmltools","usefun","timetk","tidyverse","rlist","gridExtra","fpp2","quantmod","psych","broom","dygraphs","tidyquant")
source("./Functions/All_Functions.R") # adds all the custom functions 
##===== List of Nasdaq Symbols for your convenience ==========================================
Nasdaq100_Symbols <- c("AAPL", "ADBE", "ADI", "ADP", "ADSK", "AKAM", "ALTR", "ALXN", 
                       "AMAT", "AMGN", "AMZN", "ATVI", "AVGO", "BBBY", "BIDU", "BIIB", 
                       "BRCM", "CA", "CELG", "CERN", "CHKP", "CHRW", "CHTR", "CMCSA", 
                       "COST", "CSCO", "CTRX", "CTSH", "CTXS", "DISCA", "DISCK", "DISH", 
                       "DLTR", "DTV", "EBAY", "EQIX", "ESRX", "EXPD", "EXPE", "FAST", 
                       "FB", "FFIV", "FISV", "FOXA", "GILD", "GMCR", "GOOG", "GOOGL", 
                       "GRMN", "HSIC", "ILMN", "INTC", "INTU", "ISRG", "KLAC", "KRFT", 
                       "LBTYA", "LLTC", "LMCA", "LMCK", "LVNTA", "MAR", "MAT", "MDLZ", 
                       "MNST", "MSFT", "MU", "MXIM", "MYL", "NFLX", "NTAP", "NVDA", 
                       "NXPI", "ORLY", "PAYX", "PCAR", "PCLN", "QCOM", "QVCA", "REGN", 
                       "ROST", "SBAC", "SBUX", "SIAL", "SIRI", "SNDK", "SPLS", "SRCL", 
                       "STX", "SYMC", "TRIP", "TSCO", "TSLA", "TXN", "VIAB", "VIP", 
                       "VOD", "VRSK", "VRTX", "WDC", "WFM", "WYNN", "XLNX", "YHOO") 
#================= Load Data from yahoo finance simple view =========================
test=c("TSLA","GOOGL", "AAPL") #select only those 3 companies (Tesla, Google and Apple)


objList=test;
getSymbols(objList,src="yahoo",from= "2015-02-08"); #downloads data and saves them 
StockList = lapply(objList, get);  # list of objects     
#dygraph(GOOGL[,6])
#dygraph(GOOGL[,5])
#GGL=ts(GOOGL[,6],start=c(2015,2,08),frequency=365.25)
dygraph(AAPL[,6]) #creates an interactive plof of the time-series data to get insights into patterns, trends...
dygraph(AAPL[,5])
APL=ts(AAPL[,6],start=c(2015,2,08),frequency=365.25) #ts is to create a time-series function, it selects all rows of the 6th column in AAPL, start specifies the starting date of the time series and frequency specifies the number of observations per year in the time-series (daily because it is 365.25)
autoplot(APL)
##============== Basic Analysis =====================================================
#=============== Decomposing The Apple timeseries =================
decomp=decompose(APL,type="additive") #used to decompose the time series into its underlying components, trend, seasonal, and remainder components
autoplot(decomp) #plot the decomp

detrend=APL-decomp$trend #gets the trend component from decomp and subtract it from the APL
autoplot(detrend)

deseason=APL-decomp$seasonal #gets the seasonal component from decomp and subtract it from the APL
autoplot(deseason)

autoplot(decomp$random) #extrsacts the random component from decomp
#================ Combine multiple dygraphs ========================
dy_graph <- list()
for (i in 1:length(objList)){
  temp=StockList[[i]]
  dy_graph[[i]]=dygraph(temp[,c(5)], main = paste0(objList[i]," Exchange Rate"))
}
browsable(htmltools::tagList(dy_graph)) #webbrownser to display the results so you can check the value for each day on the graph (interactive)
##============= Correlations =========================== 
APPLPvec=as.vector(AAPL[,6])
APPLVvec=as.vector(AAPL[,5])
TSLAPvec=as.vector(TSLA[,6])
#============ Correlations among Stocks ===============
cor(APPLPvec,TSLAPvec) #it shows their correlation to each other (0.94879) which indicates a positive correlation of the two variables
ccf(APPLPvec,TSLAPvec) #cross correlation function between them. 
ccfvalues=ccf(APPLPvec,TSLAPvec,lag.max = 15) #creates a variable to store the output of the ccf function for setting a maximum lag of 15 
ccfvalues$acf #autocorrelation function  
ccfvalues$acf[which.max(abs(ccfvalues$acf))] #max correlation of 0.04879 

#============ Correlations with volume ===============
ccf(APPLPvec,APPLVvec)
ccfvalues=ccf(APPLPvec,APPLVvec)
ccfvalues$acf[which.max(abs(ccfvalues$acf))] #-0.38697 
ccfvalues$acf
which.max(abs(ccfvalues$acf))-30

#============ Dataset building ============================
DF=list()
for (i in 1:length(objList)){
df1=as.data.frame(StockList[[i]])
df1=df1%>%rownames_to_column("DATE")
df1=rename_with(df1,~ toupper(gsub(paste0(objList[i],"."), "", .x, fixed = TRUE)))
df1$STOCK=objList[i]
DF[[i]]=df1
}

df1=list.rbind(DF)
df1$DATE=as.Date(df1$DATE) #it forces to read as a date
df1$MA_7_VALUE=ma(df1$ADJUSTED,7) #creates a moving average for 7 days
df1$MA_7_VALUE=as.numeric(df1$MA_7_VALUE)

#=========== Simple Graphs ================================
p1=ggplot(df1, aes(x=DATE, y=MA_7_VALUE, colour=STOCK)) +
  geom_line()+
  scale_x_date(breaks = "1 month",date_labels="%b")+
  xlab("") + ylab("7 Day Moving Average Price")+
  ggtitle("MA(7) Smoothed Timeseries ")+
  theme_bw() + theme(axis.text.x = element_text(angle = 90))
p1
ggsave("../Results/Graphs/All_Stocks_Smoothed.jpg",width=10,heigh=10,p1) 

##============ Predictive algorithm comparisons ===================
#============= Train test split ================
df=df1%>%filter(STOCK=="AAPL")
freq=365.25
k=100
#d=0.1
#k=round(nrow(df1)*d)
train=TSSplit(df,k)[[1]]
test=TSSplit(df,k)[[2]]
#============= Creating Time series ============
TS_Train=TSConvert(train,"MA_7_VALUE",freq)
TS_Test=TSConvert(test,"MA_7_VALUE",freq)
TS_All=c(TS_Train,TS_Test) 


###STOP
#================ NN train and prediction =========================
fit1=nnetar(TS_Train,p=7,Size=10,repeats=50,lambda = "auto")
for1=forecast(fit1,k)
autoplot(for1)
predictions1=for1$mean
autoplot(predictions1)

fit2=nnetar(TS_Train)
for2=forecast(fit2,k)
autoplot(for2)
predictions2=for2$mean
autoplot(predictions2)
#================ ARIMA and prediction =========================
fit3=auto.arima(TS_Train) 
for3=forecast(fit3,h=k)
autoplot(for3)
predictions3=for3$mean
autoplot(predictions3)

fit4=arima(TS_Train,order=c(7,0,1))
for4=forecast(fit4,k)
autoplot(for4)
predictions4=for4$mean
autoplot(predictions4)
#================ Comparing Predictions vs Truth ===================
results=data.frame(DATE=test$DATE,TEST=test$MA_7_VALUE,PREDNN=as.vector(predictions1),
                   PREDAUTONN=as.vector(predictions2),PREDAUTOARIMA=as.vector(predictions3),
                   PREDARIMA=as.vector(predictions4))

results$ENS1=rowMeans(results[,c(4,5)])
results$ENS2=rowMeans(results[,3:5])

#================ Plotting Prediction vs Truth ====================
p1=ggplot() +
  geom_line(data=df, aes(x = DATE, y = MA_7_VALUE, colour = "Actual Values"))+
  geom_line(data = results, aes(x = DATE, y = TEST, colour = "Actual Values")) +
  geom_line(data = results, aes(x = DATE, y = PREDNN,   colour = "Predictions NN"))  +
  geom_line(data = results, aes(x = DATE, y = PREDARIMA,   colour = "Predictions ARIMA"))  +
  geom_line(data = results, aes(x = DATE, y = PREDAUTOARIMA,   colour = "Predictions Auto ARIMA"))  +
  geom_line(data = results, aes(x = DATE, y = PREDAUTONN,   colour = "Predictions Auto Neural Network"))  +
  geom_line(data = results, aes(x = DATE, y = ENS1,   colour = "Predictions Ensamble1"))  +
  geom_line(data = results, aes(x = DATE, y = ENS2,   colour = "Predictions Ensamble2"))  +
  ylab('PRICE')+
  scale_x_date(breaks = "1 month",date_labels="%b")+
  #scale_x_date(breaks=df1$Date ,labels=format(df1$Date,format="%m-%d"))+
  ggtitle(paste0("Comparison of Predicted vs True Number of Smoothed (7) Cases for ",k," days"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, hjust = 1))
  
ggsave(paste0("../Results/Graphs/Predictions_vs_Test_days_",k,".jpg"),p1,width=10,heigh=8)
p1
#=============== Create Root Mean Square Errors ==================
RMSE=matrix(0,nrow=1,ncol = 6) 
RMSE=as.data.frame(RMSE)
colnames(RMSE)=colnames(results[,3:8])
for (i in 1:6){
  temp=results[,c(2,i+2)]
  temp$Diff=temp[,1]-temp[,2]
  temp1=temp$Diff[!is.na(temp$Diff)]
  RMSE[1,i]=sqrt(mean(temp1^2))
}
write.csv(RMSE,paste0("../Results/Smoothed_7_RMSE_days_",k,".csv"))

res=(sort(RMSE[1,])[c(1,2)])
colnames(res)

##============== Increase - Decrease Analysis =====================
df3=df%>%select(DATE,MA_7_VALUE) %>%
tq_mutate(select= MA_7_VALUE,mutate_fun = lag.xts,k=1)%>%tq_mutate(select= MA_7_VALUE,mutate_fun = lag.xts,k=1)
rename_with(~ toupper(gsub(".", "", .x, fixed = TRUE)))











