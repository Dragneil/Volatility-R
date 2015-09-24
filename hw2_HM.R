# =====================================================================
# CSE487/587
# Author: krishnakant chavali
# Email: kchavali@buffalo.edu
# =====================================================================

# need to install the following two packages in CCR(at least)
# install.packages("forecast")
# install.packages("fpp")
# data path /gpfs/courses/cse587/spring2015/data/hw2/data

library(forecast)
library(fpp)

# need to read the stocklist, and loop all files
### TO DO
output<-data.frame()
temp = list.files(path="/home/krishna/data/", pattern=".csv",full.names=TRUE)
for(i in 1:length(temp)) assign(temp[i],read.csv)

# just read one file
#filename = "/home/krishna/small/AAPL.csv"
j=1
for(filename in temp){
  # if file is not empty
  if(file.info(filename)[1]>0) {
    
    # read one csv file into variable (DO NOT EDIT)
    textData=read.csv(file=filename, header=T)
    # check if it has 36 months
    if(nrow(textData) == 754){
      # convert txt data to time-series data, in day unit (DO NOT EDIT)
      tsData = ts(rev(textData$Adj.Close),start=c(2012, 1),frequency=365)
      
      # define train data (DO NOT EDIT)
      trainData = window(tsData, end=c(2014,14))
      
      # define test data (DO NOT EDIT)
      testData = window(tsData, start=c(2014,15))
      
      # MAE row vector (DO NOT EDIT)
      MAE = matrix(NA,1,length(testData))
      
      # apply ARIMA model (DO NOT EDIT)
      fitData = HoltWinters(trainData)
      
      # the other two models
      ### TO DO
      
      # apply forecast(DO NOT EDIT)
      forecastData = forecast(fitData, h=length(testData))
      
      # print variable and see what is in the result data set
      print(forecastData)
      
      # calculate Mean Absolute Error 
      for(i in 1:length(testData))
      {
        MAE[1,i] = abs(forecastData$mean[i] - testData[i])
      }
      
      # this is the result you need for stock AAPL
      print(sum(MAE[1,1:10]))
      output[j,1] <- sum(MAE[1,1:10])
      output[j,2] <- basename(filename)
      ### TO DO
      j=j+1
    }  #end of 36 months if
  }#end of empty if
}#end of for loop
output<-output[order(output$V1),]
# plot the top 10 minimum sum of MAE in 3 models respectively
jpeg('hw.jpg')
plot(output[1:10,1], col = "blue")
lines(output[1:10,1], lw = 2, col = "red")
dev.off()
save(output,file="data-HW.Rda")
write.csv(output[1:10,1:2],'HW.csv')