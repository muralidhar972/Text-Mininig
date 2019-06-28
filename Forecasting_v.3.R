##Install Packages              

#install.packages("smooth")
require(smooth)
install.packages("greybox")
install.packages("curl")
library(greybox)
library(curl)
library('ggplot2')
library('tseries')
#install.packages("tseries")
#install.packages("forecast")
library(forecast)
#install.packages("dplyr")
#install.packages("tidyselect")
library(tidyselect)
library(dplyr)
library(readxl)
library(randomForest)
#library(kernlab) for svm
#install.packages("outliers")
library(outliers)
require(forecast)
#install.packages("Mcomp")
require(Mcomp)
#install.packages("forecastHybrid")
library(forecastHybrid)

#Read File
read_forecasting  <- read.csv(file.choose(),header = TRUE , stringsAsFactors = FALSE)
read_forecasting  <- read_excel(file.choose(),col_names = TRUE, sheet= 2)

#Taking a backup of Actual Data
read <- read_forecasting


###Deal with outliers

outlier_optimization <- rm.outlier(read_forecasting$ILI_positive_Count, fill = TRUE, median = TRUE, opposite = FALSE)# replace outliers with mean
outlier_optimization

#converting list to data frame
df_outlier_optimization <- data.frame(outlier_optimization)
#Combining the data with master data
com_data <- cbind(read_forecasting,df_outlier_optimization) 

#Create Train and Test data
train_series_3 =com_data[1:140,]
test_series_3 =com_data[141:160,]
read_forecasting_total <- com_data[1:175,]

#Create Train & Test Data on Raw Data
#train_series_3 =read_forecasting[1:200,]
#test_series_3 =read_forecasting[201:260,]
#########################################################################################

#Run Model on Train Data

#fit_who <- randomForest(who ~ ., data = subset(train_series_3, select=-c(Week)), importance =TRUE )
fit_who <- randomForest(outlier_optimization ~ ., data = subset(train_series_3, select=-c(YM,ILI_positive_Count)), importance =TRUE ) # Dependent variable : Final data we received after outlier optimization
#fit_who_glm <- glm(ILI_positive_Count ~ ., data = subset(train_series_3, select=-YM))
fit_who
#fit_who_glm


summary(fit_who)
#summary(fit_who_glm)

#Check the fitted value
fitted(fit_who)

#Creating the subset of test data
#newData_who <- data.frame(test_series_3$year)
newData_who <- subset(test_series_3, select = c(3:43)) 
#colnames(newData_who)[1] = "flu"
#colnames(newData_who)[2] = "Humidity"
#colnames(newData_who)[3] = "Temparature"

#Run prediction function to get the predicted value
distPred_who <- predict(fit_who, newData_who, interval = "prediction")
#distPred_who_glm <- predict(fit_who_glm, newData_who, interval = "prediction")

#table(distPred_who, test_series_3$ILI_positive_Count)
#table(distPred_who_glm, test_series_3$ILI_positive_Count)

#Creating data frame of predicted value
actuals_preds_who <- data.frame(distPred_who)
#actuals_preds_who_glm <- data.frame(distPred_who_glm)

#Create the dataframe fitted value
fit_who_df <- as.data.frame(fit_who$predicted)
#fit_who_glm_df <- as.data.frame(fit_who_glm$fitted.values)

#==================================================================
fit1 <- hybridModel(fit_who$predicted, weights="equal")
fit2 <- hybridModel(fit_who$predicted, weights="insample")

h <- length(test_series_3)
fc1 <- forecast(fit1, h=h)
fc1.df <- data.frame(fc1)
fc2 <- forecast(fit2, h=h)
fc2.df <- data.frame(fc2)

write.csv(fc1.df,"fc1.csv")
write.csv(fc2.df,"fc2.csv")
write.csv(merge_rf,"merge-rf.csv")

df <- cbind(Data=read_forecasting, Hybrid1=fc1$mean, Hybrid2=fc2$mean)

autoplot(df) +
  xlab("Year") + ylab(expression("Atmospheric concentration of CO"[2]))

#=================================================================================

## ARIMA MODELING ##

#Rename the column name
colnames(fit_who_df)[1] <- "ILI"
colnames(actuals_preds_who)[1] <- "ILI"

#Consolidate the fitted value and predicted value
merge_rf <- rbind(fit_who_df,actuals_preds_who)
#train_merge_rf <- cbind(read_forecasting_total,merge_rf)

#Run the time series model
data = ts(merge_rf[,1],start = c(2004,1),frequency = 12)

#Plot the graphical representation
plot(data, xlab='Years', ylab = 'ILI')
plot(diff(data),ylab='Differenced ILI')

#Modify less than 0.1 values to 1
data[data < 0.1] <- 1

#plot the logarithimix graph
plot(log10(data),ylab='Log (ILI)')
plot(diff(log10(data)),ylab='Differenced Log (ILI)')

#Check the ACF and PACF
par(mfrow = c(1,2))
acf(ts(diff(log10(data))),main='ACF ILI')
pacf(ts(diff(log10(data))),main='PACF ILI')


#Run ARIMA model
ARIMAfit <- auto.arima(log10(data), approximation=TRUE,trace=FALSE,allowdrift = FALSE)
summary(ARIMAfit)

par(mfrow = c(1,1))
#predict value based on the output of ARIMA modeling
pred <- predict(ARIMAfit,27)
p1 <-pred$pred
write.csv(p1,"p1.csv")
#Plot the graphical representation
plot(data,type='l',xlim=c(2004,2022),ylim=c(1,3000),xlab = 'Year',ylab = 'ILI')
lines(10^(pred$pred),col='blue')
lines(10^(pred$pred+0.2*pred$se),col='orange')
lines(10^(pred$pred-0.2*pred$se),col='orange')

#Export the predicted value
write.csv(pred$pred,"pred124.csv")


###########################################################
## This for checking purpose only
#Box Plot
box_plot <- read_forecasting[1:175,1:2]

boxplot(box_plot$ILI_positive_Count, main="2009", 
        xlab="Year", ylab="Count")

median(box_plot$ILI_positive_Count)
mean(box_plot$ILI_positive_Count)