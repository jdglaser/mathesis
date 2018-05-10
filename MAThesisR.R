###______________________Machine Learning with Google Searches to Predict Stock Price Changes______________________###
##_________________________________________MA Thesis: Jarred Glaser_________________________________________##

#_____Packages Used in this Script_____#
# + ggplot2
# + tidyverse
# + fpp2
# + pracma
# + data.tables
# + caret
# + mlbench
# + randomForest
# + gridExtra
# + ggpubr
# + mlbench
# + e1071

#_____Cleaning the Data_____#
citation('ggplot2')

setwd("C:\\Users\\jarre\\OneDrive\\MA Thesis\\FreshStart")

# Install Data
AAPL_data <- read.csv("AAPL_data_new.csv")
FB_data <- read.csv("FB_data_new.csv")
AMZN_data <- read.csv("AMZN_data_new.csv")
GOOGL_data <- read.csv("GOOGL_data_new.csv")

library(ggplot2)
AMZN_data$Date <- as.Date(AMZN_data$Date)

ggplot(AAPL_data,aes(as.Date(Date),Close,group=1)) + 
  geom_line(aes(color="AAPL")) +
  geom_line(data=FB_data,aes(color="FB")) +
  labs(color="Legend") +
  ggtitle("Closing Stock Prices: AAPL, FB") + 
  labs(x="Date") +
  theme(plot.title = element_text(lineheight=.7, face="bold"))

ggplot(AMZN_data,aes(as.Date(Date),Close,group=1)) + 
  geom_line(aes(color="AMZN")) +
  geom_line(data=GOOGL_data,aes(color="GOOGL")) +
  labs(color="Legend") +
  ggtitle("Closing Stock Prices: AMZN, GOOGL") + 
  labs(x="Date") +
  theme(plot.title = element_text(lineheight=.7, face="bold"))


#_____Remove Trend_____#

AMZN_data$Close_Difference <- c(NA, diff(AMZN_data$Close))
FB_data$Close_Difference <- c(NA, diff(FB_data$Close))
GOOGL_data$Close_Difference <- c(NA, diff(GOOGL_data$Close))
AAPL_data$Close_Difference <- c(NA, diff(AAPL_data$Close))

ggplot(AAPL_data,aes(as.Date(Date),Close_Difference,group=1)) + 
  geom_line(aes(color="AAPL")) +
  geom_line(data=FB_data,aes(color="FB")) +
  labs(color="Legend") +
  ggtitle("Closing Stock Prices: AAPL, FB") + 
  labs(x="Date") +
  theme(plot.title = element_text(lineheight=.7, face="bold"))

ggplot(AMZN_data,aes(as.Date(Date),Close_Difference,group=1)) + 
  geom_line(aes(color="AMZN")) +
  geom_line(data=GOOGL_data,aes(color="GOOGL")) +
  labs(color="Legend") +
  ggtitle("Closing Stock Prices: AMZN, GOOGL") + 
  labs(x="Date") +
  theme(plot.title = element_text(lineheight=.7, face="bold"))


#_____Exponential Smoothing_____#

## Simple Exponential Smoothing

library(tidyverse)
library(fpp2)

# identify optimal alpha parameter
tune_alpha <- function(df,h=100){
  train <- df[0:-h]
  test <- df[(length(df)-h+1):length(df)]
  alpha <- seq(.01, .99, by = .01)
  RMSE <- NA
  for(i in seq_along(alpha)) {
    fit <- ses(train, alpha = alpha[i], h = 100) # h-100 period out forecast
    RMSE[i] <- accuracy(fit, test)[2,2]
  }
  
  # convert to a data frame and idenitify min alpha value
  alpha.fit <- data_frame(alpha, RMSE)
  alpha.min <- filter(alpha.fit, RMSE == min(RMSE))
  return(alpha.min)
}

GOOGL_alpha <- tune_alpha(GOOGL_data$Close_Difference,h=100)[1,1]
FB_alpha <- tune_alpha(FB_data$Close_Difference,h=100)[1,1]
AMZN_alpha <- tune_alpha(AMZN_data$Close_Difference,h=100)[1,1]
AAPL_alpha <- tune_alpha(AAPL_data$Close_Difference,h=100)[1,1]

GOOGL_data$Close_Diff_SES <- c(NA,ses(GOOGL_data$Close_Difference, alpha = GOOGL_alpha$alpha, h = 100)$fitted)

FB_data$Close_Diff_SES <- c(NA,ses(FB_data$Close_Difference, alpha = FB_alpha$alpha, h = 100)$fitted)

AMZN_data$Close_Diff_SES <- c(NA,ses(AMZN_data$Close_Difference, alpha = AMZN_alpha$alpha, h = 100)$fitted)

AAPL_data$Close_Diff_SES <- c(NA,ses(AAPL_data$Close_Difference, alpha = AAPL_alpha$alpha, h = 100)$fitted)

plot(GOOGL_data$Close_Diff_SES, col="Red",type="l",xlab="Index",ylab="Google Stock Close Difference",main="Google Stock Close Difference Smoothing")
lines(GOOGL_data$Close_Difference, col=alpha("Blue",0.3))
legend("topleft",c("Smoothed","Actual"),col=c(alpha("Red"),alpha("Blue",0.3)),lty=1,cex=0.8)

#_____Calculating Features_____#

#____Technical Indicators____#

#___RSI___#

# Compute RSI
AAPL_data$RSI <- RSI(AAPL_data$Close,matype='SMA')
FB_data$RSI <- RSI(FB_data$Close,matype='SMA')
AMZN_data$RSI <- RSI(AMZN_data$Close,matype='SMA')
GOOGL_data$RSI <- RSI(GOOGL_data$Close,matype='SMA')


#___Stochastic Oscillator___#

# Compute Stochastic Momentum Index (SMI)
AAPL_data$SMI <- SMI(AAPL_data[,c("High","Low","Close")],matype=SMA)[,1]
FB_data$SMI <- SMI(FB_data[,c("High","Low","Close")],matype=SMA)[,1]
AMZN_data$SMI <- SMI(AMZN_data[,c("High","Low","Close")],matype=SMA)[,1]
GOOGL_data$SMI <- SMI(GOOGL_data[,c("High","Low","Close")],matype=SMA)[,1]

# Compute Stochastic Oscillator %K
AAPL_data$Stoch <- stoch(AAPL_data[,c("High","Low","Close")],matype=SMA,nFastD=14,nSlowD=14)[,1]
FB_data$Stoch <- stoch(FB_data[,c("High","Low","Close")],matype=SMA,nFastD=14,nSlowD=14)[,1]
AMZN_data$Stoch <- stoch(AMZN_data[,c("High","Low","Close")],matype=SMA,nFastD=14,nSlowD=14)[,1]
GOOGL_data$Stoch <- stoch(GOOGL_data[,c("High","Low","Close")],matype=SMA,nFastD=14,nSlowD=14)[,1]


#___William %R___#

# Compute Williams %R
AAPL_data$WPR <- WPR(AAPL_data[,c("High","Low","Close")])
FB_data$WPR <- WPR(FB_data[,c("High","Low","Close")])
AMZN_data$WPR <- WPR(AMZN_data[,c("High","Low","Close")])
GOOGL_data$WPR <- WPR(GOOGL_data[,c("High","Low","Close")])

#___Rate of Change___#

AAPL_data$ROC <- ROC(AAPL_data$Close)
FB_data$ROC <- ROC(FB_data$Close)
AMZN_data$ROC <- ROC(AMZN_data$Close)
GOOGL_data$ROC <- ROC(GOOGL_data$Close)

#___Moving Average Convergence / Divergence (MACD) Oscillator___#

AAPL_data$MACD <- MACD(AAPL_data$Close, maType = "EMA")[,1]
AAPL_data$MACD_Sig <- MACD(AAPL_data$Close, maType = "EMA")[,2]
FB_data$MACD <- MACD(FB_data$Close, maType = "EMA")[,1]
FB_data$MACD_Sig <- MACD(FB_data$Close, maType = "EMA")[,2]
AMZN_data$MACD <- MACD(AMZN_data$Close, maType = "EMA")[,1]
AMZN_data$MACD_Sig <- MACD(AMZN_data$Close, maType = "EMA")[,2]
GOOGL_data$MACD <- MACD(GOOGL_data$Close, maType = "EMA")[,1]
GOOGL_data$MACD_Sig <- MACD(GOOGL_data$Close, maType = "EMA")[,2]

#___On Balance volume___#

AAPL_data$OBV <- OBV(AAPL_data[,"Close"], AAPL_data[,"Volume"])
FB_data$OBV <- OBV(FB_data[,"Close"], FB_data[,"Volume"])
AMZN_data$OBV <- OBV(AMZN_data[,"Close"], AMZN_data[,"Volume"])
GOOGL_data$OBV <- OBV(GOOGL_data[,"Close"], GOOGL_data[,"Volume"])

#____Search Data Features____#

#___MA, Disparity, Disparity Change, EMA, MA Change, EMA Change, and RSI Change___#

library(pracma)

create_ma <- function(df,ma,name){
  for(i in ma){
    df[,paste(name,"_Stock_RSI_Change",sep="")] = c(NA,diff(RSI(df[,paste(name,"_Stock_Normalized_Daily",sep="")], matype="SMA")))
    df[,paste(name,"_RSI_Change",sep="")] = c(NA,diff(RSI(df[,paste(name,"_Normalized_Daily",sep="")], matype="SMA")))
    
    df[,paste(name,"_Stock_SMA",i,sep="")] = movavg(df[,paste(name,"_Stock_Normalized_Daily",sep="")], i, "s")
    df[,paste(name,"_Stock_SMA",i,"_Change",sep="")] = c(NA,diff(df[,paste(name,"_Stock_SMA",i,sep="")]))
    df[,paste(name,"_Stock_Disparity",i,sep="")] = df[,paste(name,"_Stock_Normalized_Daily",sep="")]/df[,paste(name,"_Stock_SMA",i,sep="")]
    df[,paste(name,"_Stock_Disparity",i,"_Change",sep="")] = c(NA,diff(df[,paste(name,"_Stock_Disparity",i,sep="")]))
    
    df[,paste(name,"_Stock_EMA",i,sep="")] = movavg(df[,paste(name,"_Stock_Normalized_Daily",sep="")], i, "s")
    df[,paste(name,"_Stock_EMA",i,"_Change",sep="")] = c(NA,diff(df[,paste(name,"_Stock_SMA",i,sep="")]))
    
    df[,paste(name,"_SMA",i,sep="")] = movavg(df[,paste(name,"_Normalized_Daily",sep="")], i, "s")
    df[,paste(name,"_SMA",i,"_Change",sep="")] = c(NA,diff(df[,paste(name,"_SMA",i,sep="")]))
    df[,paste(name,"_Disparity",i,sep="")] = df[,paste(name,"_Normalized_Daily",sep="")]/df[,paste(name,"_SMA",i,sep="")]
    df[,paste(name,"_Disparity",i,"_Change",sep="")] = c(NA,diff(df[,paste(name,"_Disparity",i,sep="")]))
    
    df[,paste(name,"_EMA",i,sep="")] = movavg(df[,paste(name,"_Normalized_Daily",sep="")], i, "s")
    df[,paste(name,"_EMA",i,"_Change",sep="")] = c(NA,diff(df[,paste(name,"_EMA",i,sep="")]))
    
  }
  return(df)
}

ma <- c(6,8,10,20)
df_list <- list(AAPL_data,FB_data,AMZN_data,GOOGL_data)
names <- c("Apple","Facebook","Amazon","Google")

for(i in 1:4){
  df_list[[i]] <- create_ma(df_list[[i]],ma=ma,name=names[i])
}

AAPL_data <- df_list[[1]]
FB_data <- df_list[[2]]
AMZN_data <- df_list[[3]]
GOOGL_data <- df_list[[4]]


#_____Create Target_____#

# Create Targets sign(Close(t+d)-Close(t))
library(data.table)
forecast_out = c(5,15,30)
for(f in forecast_out){
  AAPL_data[,paste("T",f,sep="")] <- sign(shift(AAPL_data$Close,n=f,fill=NA,type=c("lead")) - AAPL_data$Close)
  AAPL_data[,paste("T",f,sep="")][AAPL_data[,paste("T",f,sep="")] == 1] <- 1
  AAPL_data[,paste("T",f,sep="")][AAPL_data[,paste("T",f,sep="")] <= 0] <- 0
  AAPL_data[,paste("T",f,sep="")] <- as.factor(AAPL_data[,paste("T",f,sep="")])
  
  FB_data[,paste("T",f,sep="")] <- sign(shift(FB_data$Close,n=f,fill=NA,type=c("lead")) - FB_data$Close)
  FB_data[,paste("T",f,sep="")][FB_data[,paste("T",f,sep="")] == 1] <- 1
  FB_data[,paste("T",f,sep="")][FB_data[,paste("T",f,sep="")] <= 0] <- 0
  FB_data[,paste("T",f,sep="")] <- as.factor(FB_data[,paste("T",f,sep="")])
  
  AMZN_data[,paste("T",f,sep="")] <- sign(shift(AMZN_data$Close,n=f,fill=NA,type=c("lead")) - AMZN_data$Close)
  AMZN_data[,paste("T",f,sep="")][AMZN_data[,paste("T",f,sep="")] == 1] <- 1
  AMZN_data[,paste("T",f,sep="")][AMZN_data[,paste("T",f,sep="")] <= 0] <- 0
  AMZN_data[,paste("T",f,sep="")] <- as.factor(AMZN_data[,paste("T",f,sep="")])
  
  GOOGL_data[,paste("T",f,sep="")] <- sign(shift(GOOGL_data$Close,n=f,fill=NA,type=c("lead")) - GOOGL_data$Close)
  GOOGL_data[,paste("T",f,sep="")][GOOGL_data[,paste("T",f,sep="")] == 1] <- 1
  GOOGL_data[,paste("T",f,sep="")][GOOGL_data[,paste("T",f,sep="")] <= 0] <- 0
  GOOGL_data[,paste("T",f,sep="")] <- as.factor(GOOGL_data[,paste("T",f,sep="")])
}


# Remove All NAs to get full data set
AAPL_data <- na.omit(AAPL_data)
rownames(AAPL_data) <- seq(1:nrow(AAPL_data))
FB_data <- na.omit(FB_data)
rownames(FB_data) <- seq(1:nrow(FB_data))
AMZN_data <- na.omit(AMZN_data)
rownames(AMZN_data) <- seq(1:nrow(AMZN_data))
GOOGL_data <- na.omit(GOOGL_data)
rownames(GOOGL_data) <- seq(1:nrow(GOOGL_data))

# Create X and Y
AAPL_Y <- AAPL_data[,c("T5","T15","T30")]
AAPL_X <- AAPL_data[,!(colnames(AAPL_data) %in% c("Date","Apple_Stock_Normalized_Daily","Apple_Normalized_Daily","T5","T15","T30","Open","High","Low","Close","Volume","Adj.Close","Close_Difference"))]

AMZN_Y <- AMZN_data[,c("T5","T15","T30")]
AMZN_X <- AMZN_data[,!(colnames(AMZN_data) %in% c("Date","Amazon_Stock_Normalized_Daily","Amazon_Normalized_Daily","T5","T15","T30","Open","High","Low","Close","Volume","Adj.Close","Close_Difference"))]

FB_Y <- FB_data[,c("T5","T15","T30")]
FB_X <- FB_data[,!(colnames(FB_data) %in% c("Date","Facebook_Stock_Normalized_Daily","Facebook_Normalized_Daily","T5","T15","T30","Open","High","Low","Close","Volume","Adj.Close","Close_Difference"))]

GOOGL_Y <- GOOGL_data[,c("T5","T15","T30")]
GOOGL_X <- GOOGL_data[,!(colnames(GOOGL_data) %in% c("Date","Google_Stock_Normalized_Daily","Google_Normalized_Daily","T5","T15","T30","Open","High","Low","Close","Volume","Adj.Close","Close_Difference"))]

#_____Select Features_____#

library(mlbench)
library(caret)

### Apple RFE
set.seed(123)
AAPL_X_Train <- AAPL_X[1:(0.5*nrow(AAPL_X)),]
AAPL_Y_Train <- AAPL_Y[1:(0.5*nrow(AAPL_Y)),]
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)

# run the RFE algorithm - COMMENTED OUT FOR SPEED - USE CSV
AAPL_results_T5 <- rfe(x=AAPL_X_Train, y=AAPL_Y_Train[,"T5"], sizes=c(1:20), rfeControl=control)
AAPL_results_T15 <- rfe(x=AAPL_X_Train, y=AAPL_Y_Train[,"T15"], sizes=c(1:20), rfeControl=control)
AAPL_results_T30 <- rfe(x=AAPL_X_Train, y=AAPL_Y_Train[,"T30"], sizes=c(1:20), rfeControl=control)

# list the chosen features
AAPL_pred_T5 <- predictors(AAPL_results_T5)
AAPL_pred_T15 <- predictors(AAPL_results_T15)
AAPL_pred_T30 <- predictors(AAPL_results_T30)

l = max(c(length(AAPL_pred_T5),length(AAPL_pred_T15),length(AAPL_pred_T30)))
AAPL_T5 <- c(AAPL_pred_T5,rep.int(NA,(l-length(AAPL_pred_T5))))
AAPL_T15 <- c(AAPL_pred_T15,rep.int(NA,(l-length(AAPL_pred_T15))))
AAPL_T30 <- c(AAPL_pred_T30,rep.int(NA,(l-length(AAPL_pred_T30))))

# plot the results
ggplot(AAPL_results_T30) + ggtitle("AAPL RFE")

AAPL_features <- data.frame(T5=AAPL_T5,T15=AAPL_T15,T30=AAPL_T30)
#write.csv(AAPL_features,"AAPL_RFE_Results.csv")
AAPL_features


### Facebook RFE
set.seed(123)
FB_X_Train <- FB_X[1:(0.5*nrow(AAPL_X)),]
FB_Y_Train <- FB_Y[1:(0.5*nrow(AAPL_Y)),]

# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)

# run the RFE algorithm - COMMENTED OUT FOR SPEED - USE CSV
FB_results_T5 <- rfe(x=FB_X_Train, y=FB_Y_Train[,"T5"], sizes=c(1:20), rfeControl=control)
FB_results_T15 <- rfe(x=FB_X_Train, y=FB_Y_Train[,"T15"], sizes=c(1:20), rfeControl=control)
FB_results_T30 <- rfe(x=FB_X_Train, y=FB_Y_Train[,"T30"], sizes=c(1:20), rfeControl=control)

# list the chosen features
FB_pred_T5 <- predictors(FB_results_T5)
FB_pred_T15 <- predictors(FB_results_T15)
FB_pred_T30 <- predictors(FB_results_T30)

l = max(c(length(FB_pred_T5),length(FB_pred_T15),length(FB_pred_T30)))
FB_T5 <- c(FB_pred_T5,rep.int(NA,(l-length(FB_pred_T5))))
FB_T15 <- c(FB_pred_T15,rep.int(NA,(l-length(FB_pred_T15))))
FB_T30 <- c(FB_pred_T30,rep.int(NA,(l-length(FB_pred_T30))))

FB_features <- data.frame(T5=FB_T5,T15=FB_T15,T30=FB_T30)

# plot the results
ggplot(FB_results_T15, type=c("g", "o"))

write.csv(FB_features,"FB_RFE_Results.csv")
FB_features


### GOOGL RFE
set.seed(123)
GOOGL_X_Train <- GOOGL_X[1:(0.5*nrow(AAPL_X)),]
GOOGL_Y_Train <- GOOGL_Y[1:(0.5*nrow(AAPL_Y)),]

# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)

# run the RFE algorithm - COMMENTED OUT FOR SPEED - USE CSV
GOOGL_results_T5 <- rfe(x=GOOGL_X_Train, y=GOOGL_Y_Train[,"T5"], sizes=c(1:20), rfeControl=control)
GOOGL_results_T15 <- rfe(x=GOOGL_X_Train, y=GOOGL_Y_Train[,"T15"], sizes=c(1:20), rfeControl=control)
GOOGL_results_T30 <- rfe(x=GOOGL_X_Train, y=GOOGL_Y_Train[,"T30"], sizes=c(1:20), rfeControl=control)

# list the chosen features
GOOGL_pred_T5 <- predictors(GOOGL_results_T5)
GOOGL_pred_T15 <- predictors(GOOGL_results_T15)
GOOGL_pred_T30 <- predictors(GOOGL_results_T30)

l = max(c(length(GOOGL_pred_T5),length(GOOGL_pred_T15),length(GOOGL_pred_T30)))
GOOGL_T5 <- c(GOOGL_pred_T5,rep.int(NA,(l-length(GOOGL_pred_T5))))
GOOGL_T15 <- c(GOOGL_pred_T15,rep.int(NA,(l-length(GOOGL_pred_T15))))
GOOGL_T30 <- c(GOOGL_pred_T30,rep.int(NA,(l-length(GOOGL_pred_T30))))

GOOGL_features <- data.frame(T5=GOOGL_T5,T15=GOOGL_T15,T30=GOOGL_T30)

# plot the results
plot(GOOGL_results_T15, type=c("g", "o"))

write.csv(GOOGL_features,"GOOGL_RFE_Results.csv")
GOOGL_features


### AMZN RFE
set.seed(123)
AMZN_X_Train <- AMZN_X[1:(0.5*nrow(AAPL_X)),]
AMZN_Y_Train <- AMZN_Y[1:(0.5*nrow(AAPL_Y)),]

# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)

# run the RFE algorithm - COMMENTED OUT FOR SPEED - USE CSV
AMZN_results_T5 <- rfe(x=AMZN_X_Train, y=AMZN_Y_Train[,"T5"], sizes=c(1:20), rfeControl=control)
AMZN_results_T15 <- rfe(x=AMZN_X_Train, y=AMZN_Y_Train[,"T15"], sizes=c(1:20), rfeControl=control)
AMZN_results_T30 <- rfe(x=AMZN_X_Train, y=AMZN_Y_Train[,"T30"], sizes=c(1:20), rfeControl=control)

# list the chosen features
AMZN_pred_T5 <- predictors(AMZN_results_T5)
AMZN_pred_T15 <- predictors(AMZN_results_T15)
AMZN_pred_T30 <- predictors(AMZN_results_T30)

l = max(c(length(AMZN_pred_T5),length(AMZN_pred_T15),length(AMZN_pred_T30)))
AMZN_T5 <- c(AMZN_pred_T5,rep.int(NA,(l-length(AMZN_pred_T5))))
AMZN_T15 <- c(AMZN_pred_T15,rep.int(NA,(l-length(AMZN_pred_T15))))
AMZN_T30 <- c(AMZN_pred_T30,rep.int(NA,(l-length(AMZN_pred_T30))))

AMZN_features <- data.frame(T5=AMZN_T5,T15=AMZN_T15,T30=AMZN_T30)

# plot the results
plot(AMZN_results_T15, type=c("g", "o"))

write.csv(AMZN_features,"AMZN_RFE_Results.csv")
AMZN_features

#_____Plot GOOGL RFE Results_____#

# GOOGL Plot
library(ggrepel)

Google_T5_Results <- GOOGL_results_T5$results
Google_T5_Results$Target <- "T5"
Google_T15_Results <- GOOGL_results_T15$results
Google_T15_Results$Target <- "T15"
Google_T30_Results <- GOOGL_results_T30$results
Google_T30_Results$Target <- "T30"

Google_T5_Results_Max <- Google_T5_Results[Google_T5_Results$Accuracy == max(Google_T5_Results$Accuracy),]
Google_T15_Results_Max <- Google_T15_Results[Google_T15_Results$Accuracy == max(Google_T15_Results$Accuracy),]
Google_T30_Results_Max <- Google_T30_Results[Google_T30_Results$Accuracy == max(Google_T30_Results$Accuracy),]

Google_RFE_all <- rbind(Google_T5_Results,Google_T15_Results,Google_T30_Results)
Google_RFE_all$Target <- as.factor(Google_RFE_all$Target)
Google_RFE_all$Target <- relevel(Google_RFE_all$Target,"T5")

ggplot(Google_RFE_all, aes(Variables,Accuracy,color=Target)) + 
  geom_line() + 
  geom_point() +
  ylim(0.5,0.95) +
  geom_point(data = Google_T15_Results_Max,color='green',size=2) +
  geom_text_repel(data = Google_T15_Results_Max, aes(Variables,Accuracy,label=round(Accuracy,4)),nudge_y = -0.025,show.legend = FALSE) +
  geom_point(data = Google_T5_Results_Max,color='red',size=2) +
  geom_text_repel(data = Google_T5_Results_Max, aes(Variables,Accuracy,label=round(Accuracy,4)),nudge_y = 0.025,show.legend = FALSE) +
  geom_point(data = Google_T30_Results_Max,color='blue',size=2) +
  geom_text_repel(data = Google_T30_Results_Max, aes(Variables,Accuracy,label=round(Accuracy,4)),nudge_y = 0.025,show.legend = FALSE)

#_____Random Forest_____#

library(randomForest)
cv_rf <- function(y,x,row,n){
  set.seed(123) #Seed set to 123 for reproducability
  n.end <- nrow(x)-row
  actual <- y[row:nrow(x)]
  predicted <- vector()
  actual <- vector()
  for(i in 1:n.end+1){
    train_x <- x[1:(row-2+i),]
    train_y <- y[1:(row-2+i)]
    test_x <- x[(row-1+i),]
    rf <- randomForest(train_y~.,data=train_x,ntree=n)
    pred <- predict(rf,newdata=test_x)
    predicted[i] = pred
    actual[i] <- y[(row-1+i)]
  }
  r <- data.frame(Predicted=predicted,Actual=actual)
  tp <- nrow(r[r$Predicted == 2 & r$Actual == 2,])
  tn <- nrow(r[r$Predicted == 1 & r$Actual == 1,])
  fp <- nrow(r[r$Predicted == 2 & r$Actual == 1,])
  fn <- nrow(r[r$Predicted == 1 & r$Actual == 2,])
  
  ac <- (tp+tn)/(tp+tn+fp+fn)
  pr <- tp/(tp+fp)
  re <- tp/(tp+fn)
  sp <- tn/(tn+fp)
  
  data.frame(Accuracy=ac,Precision=pr,Recall=re,Specificity=sp)
}


## Google RF

GOOGL_X_T5 <- GOOGL_X[,(colnames(GOOGL_X) %in% GOOGL_pred_T5)]
GOOGL_X_T15 <- GOOGL_X[,(colnames(GOOGL_X) %in% GOOGL_pred_T15)]
GOOGL_X_T30 <- GOOGL_X[,(colnames(GOOGL_X) %in% GOOGL_pred_T30)]

GOOGL_rf5 <- cv_rf(GOOGL_Y[,"T5"],GOOGL_X_T5,503,25)
GOOGL_rf15 <- cv_rf(GOOGL_Y[,"T15"],GOOGL_X_T15,503,25)
GOOGL_rf30 <- cv_rf(GOOGL_Y[,"T30"],GOOGL_X_T30,503,25)

final_GOOGL_rf <- rbind(GOOGL_rf5,GOOGL_rf15,GOOGL_rf30)
rownames(final_GOOGL_rf) <- c("5 Step","15 Step","30 Step")
final_GOOGL_rf
write.csv(final_GOOGL_rf,"RF_GOOGL_Results.csv")


## Amazon RF

AMZN_X_T5 <- AMZN_X[,(colnames(AMZN_X) %in% AMZN_pred_T5)]
AMZN_X_T15 <- AMZN_X[,(colnames(AMZN_X) %in% AMZN_pred_T15)]
AMZN_X_T30 <- AMZN_X[,(colnames(AMZN_X) %in% AMZN_pred_T30)]

AMZN_rf5 <- cv_rf(AMZN_Y[,"T5"],AMZN_X_T5,503,25)
AMZN_rf15 <- cv_rf(AMZN_Y[,"T15"],AMZN_X_T15,503,25)
AMZN_rf30 <- cv_rf(AMZN_Y[,"T30"],AMZN_X_T30,503,25)

final_AMZN_rf <- rbind(AMZN_rf5,AMZN_rf15,AMZN_rf30)
rownames(final_AMZN_rf) <- c("5 Step","15 Step","30 Step")
final_AMZN_rf
write.csv(final_AMZN_rf,"RF_AMZN_Results.csv")


## Facebook RF

FB_X_T5 <- FB_X[,(colnames(FB_X) %in% FB_pred_T5)]
FB_X_T15 <- FB_X[,(colnames(FB_X) %in% FB_pred_T15)]
FB_X_T30 <- FB_X[,(colnames(FB_X) %in% FB_pred_T30)]

FB_rf5 <- cv_rf(FB_Y[,"T5"],FB_X_T5,503,25)
FB_rf15 <- cv_rf(FB_Y[,"T15"],FB_X_T15,503,25)
FB_rf30 <- cv_rf(FB_Y[,"T30"],FB_X_T30,503,25)

final_FB_rf <- rbind(FB_rf5,FB_rf15,FB_rf30)
rownames(final_FB_rf) <- c("5 Step","15 Step","30 Step")
final_FB_rf
write.csv(final_FB_rf,"RF_FB_Results.csv")


## Apple RF

AAPL_X_T5 <- AAPL_X[,(colnames(AAPL_X) %in% AAPL_pred_T5)]
AAPL_X_T15 <- AAPL_X[,(colnames(AAPL_X) %in% AAPL_pred_T15)]
AAPL_X_T30 <- AAPL_X[,(colnames(AAPL_X) %in% AAPL_pred_T30)]

AAPL_rf5 <- cv_rf(AAPL_Y[,"T5"],AAPL_X_T5,503,25)
AAPL_rf15 <- cv_rf(AAPL_Y[,"T15"],AAPL_X_T15,503,25)
AAPL_rf30 <- cv_rf(AAPL_Y[,"T30"],AAPL_X_T30,503,25)

final_AAPL_rf <- rbind(AAPL_rf5,AAPL_rf15,AAPL_rf30)
rownames(final_AAPL_rf) <- c("5 Step","15 Step","30 Step")
final_AAPL_rf
write.csv(final_AAPL_rf,"RF_AAPL_Results.csv")


#_____SVM_____#

library(e1071)

cv_svm <- function(y,x,row){
  set.seed(123) #Seed set to 123 for reproducability
  n.end <- nrow(x)-row
  actual <- y[row:nrow(x)]
  predicted <- vector()
  actual <- vector()
  for(i in 1:n.end+1){
    train_x <- x[1:(row-2+i),]
    train_y <- y[1:(row-2+i)]
    test_x <- x[(row-1+i),]
    svm_model <- svm(train_y~.,data=train_x,kernel='radial',type='C-classification',cost=128,gamma=0.066666667)
    pred <- predict(svm_model,newdata=test_x)
    predicted[i] = pred
    actual[i] <- y[(row-1+i)]
  }
  r <- data.frame(Predicted=predicted,Actual=actual)
  tp <- nrow(r[r$Predicted == 2 & r$Actual == 2,])
  tn <- nrow(r[r$Predicted == 1 & r$Actual == 1,])
  fp <- nrow(r[r$Predicted == 2 & r$Actual == 1,])
  fn <- nrow(r[r$Predicted == 1 & r$Actual == 2,])
  
  ac <- (tp+tn)/(tp+tn+fp+fn)
  pr <- tp/(tp+fp)
  re <- tp/(tp+fn)
  sp <- tn/(tn+fp)
  
  data.frame(Accuracy=ac,Precision=pr,Recall=re,Specificity=sp)
}


## GOOGL SVM

GOOGL_svm5 <- cv_svm(GOOGL_Y[,"T5"],GOOGL_X_T5,503)
GOOGL_svm15 <- cv_svm(GOOGL_Y[,"T15"],GOOGL_X_T15,503)
GOOGL_svm30 <- cv_svm(GOOGL_Y[,"T30"],GOOGL_X_T30,503)

final_GOOGL_svm <- rbind(GOOGL_svm5,GOOGL_svm15,GOOGL_svm30)
rownames(final_GOOGL_svm) <- c("5 Step","15 Step","30 Step")
final_GOOGL_svm
write.csv(final_GOOGL_svm,"SVM_GOOGL_Results.csv")


## AMZN SVM

AMZN_svm5 <- cv_svm(AMZN_Y[,"T5"],AMZN_X_T5,503)
AMZN_svm15 <- cv_svm(AMZN_Y[,"T15"],AMZN_X_T15,503)
AMZN_svm30 <- cv_svm(AMZN_Y[,"T30"],AMZN_X_T30,503)

final_AMZN_svm <- rbind(AMZN_svm5,AMZN_svm15,AMZN_svm30)
rownames(final_AMZN_svm) <- c("5 Step","15 Step","30 Step")
final_AMZN_svm
write.csv(final_AMZN_svm,"SVM_AMZN_Results.csv")


## FB SVM

FB_svm5 <- cv_svm(FB_Y[,"T5"],FB_X_T5,503)
FB_svm15 <- cv_svm(FB_Y[,"T15"],FB_X_T15,503)
FB_svm30 <- cv_svm(FB_Y[,"T30"],FB_X_T30,503)

final_FB_svm <- rbind(FB_svm5,FB_svm15,FB_svm30)
rownames(final_FB_svm) <- c("5 Step","15 Step","30 Step")
final_FB_svm
write.csv(final_FB_svm,"SVM_FB_Results.csv")


## AAPL SVM

AAPL_svm5 <- cv_svm(AAPL_Y[,"T5"],AAPL_X_T5,503)
AAPL_svm15 <- cv_svm(AAPL_Y[,"T15"],AAPL_X_T15,503)
AAPL_svm30 <- cv_svm(AAPL_Y[,"T30"],AAPL_X_T30,503)

final_AAPL_svm <- rbind(AAPL_svm5,AAPL_svm15,AAPL_svm30)
rownames(final_AAPL_svm) <- c("5 Step","15 Step","30 Step")
final_AAPL_svm
write.csv(final_AAPL_svm,"SVM_AAPL_Results.csv")


#_____Comparing Models without Search Data_____#

AAPL_X_TI <- AAPL_X[,c("RSI","SMI","Close_Diff_SES","Stoch","WPR","ROC","MACD","MACD_Sig","OBV")]
AAPL_rf15_TI <- cv_rf(AAPL_Y[,"T15"],AAPL_X_TI,503,25)
AAPL_svm15_TI <- cv_svm(AAPL_Y[,"T15"],AAPL_X_TI,503)

final_TI <- rbind(AAPL_rf15_TI,AAPL_svm15_TI,AAPL_rf15,AAPL_svm15)
rownames(final_TI) <- c("RF-TI","SVM-TI","RF-SF","SVM-SF")
final_TI
write.csv(final_TI,"TIvsSF.csv")


#_____Plotting Results_____#

final_AAPL_rf$Company <- "AAPL"
final_AAPL_rf$Step <- rownames(final_AAPL_rf)
final_FB_rf$Company <- "FB"
final_FB_rf$Step <- rownames(final_FB_rf)
final_AMZN_rf$Company <- "AMZN"
final_AMZN_rf$Step <- rownames(final_AMZN_rf)
final_GOOGL_rf$Company <- "GOOGL"
final_GOOGL_rf$Step <- rownames(final_AMZN_rf)

final_AAPL_svm$Company <- "AAPL"
final_AAPL_svm$Step <- rownames(final_AAPL_svm)
final_FB_svm$Company <- "FB"
final_FB_svm$Step <- rownames(final_FB_svm)
final_AMZN_svm$Company <- "AMZN"
final_AMZN_svm$Step <- rownames(final_AMZN_svm)
final_GOOGL_svm$Company <- "GOOGL"
final_GOOGL_svm$Step <- rownames(final_GOOGL_svm)

RF <- rbind(final_AAPL_rf,final_FB_rf,final_AMZN_rf,final_GOOGL_rf)
RF$Model <- "RF"
SVM <- rbind(final_AAPL_svm,final_FB_svm,final_AMZN_svm,final_GOOGL_svm)
SVM$Model <- "SVM"

Final_Comparison <- rbind(RF,SVM)
rownames(Final_Comparison) <- seq(1:nrow(Final_Comparison))
Final_Comparison
write.csv(Final_Comparison,"Final_Comparison.csv")

library(gridExtra)
library(ggpubr)

RF$Step <- as.factor(RF$Step)
RF$Step <- relevel(RF$Step,"5 Step")
SVM$Step <- as.factor(SVM$Step)
SVM$Step <- relevel(SVM$Step,"5 Step")

g_rf <- ggplot(RF,aes(x=Company,y=Accuracy,fill=Step))
RF_Plot <- g_rf + geom_bar(stat="Identity",position="dodge") + ggtitle("Random Forest")

g_svm <- ggplot(SVM,aes(x=Company,y=Accuracy,fill=Step))
SVM_Plot <- g_svm + geom_bar(stat="Identity",position="dodge") + ggtitle("Support Vector Machine")

ggarrange(RF_Plot,SVM_Plot,common.legend = TRUE)




