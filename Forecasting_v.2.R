box_plot <- read_forecasting[1:175,1:2]

boxplot(box_plot$ILI_positive_Count, main="2009", 
        xlab="Year", ylab="Count")

median(box_plot$ILI_positive_Count)
mean(box_plot$ILI_positive_Count)
############################################################################################################
#######################################
#       Install Packages              #
#######################################
#install.packages("smooth")
require(smooth)
#install.packages("Mcomp")
require(Mcomp)
library('ggplot2')
library('forecast')
library('tseries')
#install.packages("forecast")
library(forecast)
library(dplyr)
library(readxl)
############################################################################################################

#Read File
#read_forecasting  <- read.csv(file.choose(),header = TRUE , stringsAsFactors = FALSE)
read_forecasting  <- read_excel(file.choose(),col_names = TRUE, sheet=1 )
read <- read_forecasting
x <- read_forecasting


summary(read_forecasting[,2])

#Check Significant outliers and possible replacements
#tsoutliers(read_forecasting$ILI_positive_Count)

#p <- read_forecasting$ILI_positive_Count
#p1 <- p  %>%
         #tsclean() %>%
         # ets()
#p1
#p2 <- data.frame(p1$fitted)
###Deal with outliers
#install.packages("outliers")
library(outliers)
x <- rm.outlier(read_forecasting$ILI_positive_Count, fill = TRUE, median = TRUE, opposite = FALSE)
#View(x)

#==============================================================================================#

x <- data.frame(x)
x <- cbind(read_forecasting,x)
#Create Train and Test data
#colnames(x)[44] <- "x"
train_series_3 =x[1:53,]
test_series_3 =x[54:60,]
#read_forecasting_total <- x[1:175,]
#train_series_4 =read_forecasting[1:200,]
#test_series_4 =read_forecasting[201:260,]
#########################################################################################
#install.packages("lmtest")
library(lmtest)
library(randomForest)

#library(kernlab) for svm
#fit_who <- dwtest(randomForest(x ~ ., data = subset(train_series_3, select=-c(YM)), importance =TRUE ))
fit_who <- randomForest(x ~ ., data = subset(train_series_3, select=-c(YM)), importance =TRUE )
dwtest(fit_who)
fit_who



fit_who_lm <- lm(x ~ ., data = subset(train_series_3, select=-c(YM)))
dwtest(fit_who_lm)
fit_who_lm



#x[x < 0.1] <- 1
#plot(log10(data),ylab='Log (ILI)')
#fit_who <- glm(x ~ ., family = binomial(link = "logit"), data = subset(train_series_3, select=-c(YM)))
#fit_who <- dwtest(glm(x ~ ., data = subset(train_series_3, select=-c(YM))))
#fit_who <- randomForest(ILI_positive_Count ~ ., data = subset(train_series_4, select=-c(YM)), importance =TRUE )
#fit_who_glm <- glm(ILI_positive_Count ~ ., data = subset(train_series_3, select=-YM))
#fit_who
#fit_who_glm
#summary(fit_who)
#summary(fit_who_glm)

#fitted(fit_who)

#newData_who <- data.frame(test_series_3$year)
newData_who <- subset(test_series_3, select = c(2:53)) 
#colnames(newData_who)[1] = "flu"
#colnames(newData_who)[2] = "Humidity"
#colnames(newData_who)[3] = "Temparature"
distPred_who_rf <- predict(fit_who, newData_who, interval = "prediction")
distPred_who_lm <- predict(fit_who_lm, newData_who, interval = "prediction")

#table(distPred_who, test_series_3$ILI_positive_Count)
#table(distPred_who_glm, test_series_3$ILI_positive_Count)

actuals_preds_who_rf <- data.frame(distPred_who_rf)
actuals_preds_who_lm <- data.frame(distPred_who_lm)

fit_who_df_rf <- as.data.frame(fit_who$predicted)
fit_who_df_lm <- as.data.frame(fit_who_lm$fitted.values)

#################################################


#############################################################################
########################### ARIMA MODELING ##################################
#############################################################################
colnames(fit_who_df_rf)[1] <- "ILI"
colnames(actuals_preds_who_rf)[1] <- "ILI"
merge_rf <- rbind(fit_who_df_rf,actuals_preds_who_rf[1])




colnames(fit_who_df_lm)[1] <- "ILI"
colnames(actuals_preds_who_lm)[1] <- "ILI"
merge_lm <- rbind(fit_who_df_lm,actuals_preds_who_lm[1])

#####################################################################################################
#train_merge_rf <- cbind(read_forecasting_total,merge_rf)
#data = merge_rf
data_rf = ts(merge_rf[1:111,1],start = c(2008,1),frequency = 12)
plot(data_rf, xlab='Years', ylab = 'ILI')
plot(diff(data_rf),ylab='Differenced ILI')
data_rf[data_rf < 0.1] <- 1
plot(log10(data_rf),ylab='Log (ILI)')
plot(diff(log10(data_rf)),ylab='Differenced Log (ILI)')

par(mfrow = c(1,2))
acf(ts(diff(log10(data_rf))),main='ACF ILI')
pacf(ts(diff(log10(data_rf))),main='PACF ILI')

ARIMAfit_rf <- auto.arima(log10(data_rf), approximation=TRUE,trace=FALSE,allowdrift = FALSE)
summary(ARIMAfit_rf)
require(forecast)
par(mfrow = c(1,1))
pred_rf <- predict(ARIMAfit_rf,26)
p1_rf <-pred_rf$pred
p1_rf

#write.csv(p1_rf,"pred_rf.csv")

#*********************************************************************************************************#

data_lm = ts(merge_lm[1:111,1],start = c(2008,1),frequency = 12)
plot(data_lm, xlab='Years', ylab = 'ILI')
plot(diff(data_lm),ylab='Differenced ILI')
data_lm[data_lm < 0.1] <- 1
plot(log10(data_lm),ylab='Log (ILI)')
plot(diff(log10(data_lm)),ylab='Differenced Log (ILI)')

par(mfrow = c(1,2))
acf(ts(diff(log10(data_lm))),main='ACF ILI')
pacf(ts(diff(log10(data_lm))),main='PACF ILI')

ARIMAfit_lm <- auto.arima(log10(data_lm), approximation=TRUE,trace=FALSE,allowdrift = FALSE)
summary(ARIMAfit_lm)
require(forecast)
par(mfrow = c(1,1))
pred_lm <- predict(ARIMAfit_lm,26)
p1_lm <-pred_lm$pred
p1_lm

#write.csv(p1_lm,"pred_lm.csv")
##################################################################################################################









plot(data,type='l',xlim=c(2008,2020),ylim=c(1,3000),xlab = 'Year',ylab = 'ILI')
lines(10^(pred$pred),col='blue')
lines(10^(pred$pred+0.2*pred$se),col='orange')
lines(10^(pred$pred-0.2*pred$se),col='orange')


#write.csv(pred$pred,"pred124.csv")



######################################################################
######################################################################
################ Graphical format for different outputs ##############
######################################################################
######################################################################
ILI <- data.frame(read_forecasting$ILI_positive_Count)
ILI = ts(ILI[,1],start = c(2004,1),frequency = 12)
plot(ILI,type='l',xlim=c(2004,2022),ylim=c(1,3500),xlab = 'Year',ylab = 'ILI')

p12 <- data.frame(rbind(p1,10^(2.382017043)))
lines(10^(2.382017043),col='black')
lines(10^(p1),col='blue')













