library(XML)
library(htmltab)
library(ggplot2)
library(zoo)
library(data.table)
library(gridExtra)

saveBuoyData <- function(dataset,filename){
  newdata <- dataset
  #read old data
  olddata <- read.csv(file = filename)
  #OldData <- read.csv("taitungbuoy.csv")
  rowindex = which(as.character(olddata[,8]) == as.character(newdata[nrow(newdata),7]))
  longdata <- rbind(newdata,olddata[rowindex+1:nrow(olddata),-1])
  NArows = which(is.na(longdata$DateTime))
  longdata <- longdata[-NArows,]
  # save new data
  write.csv(longdata, file = filename)
}

GetLongBuoyData<- function(buoyUrl,tideUrl){
  
  
  dataset = htmltab(buoyUrl)
  buoydata = htmltab(tideUrl)
  
  # change the names of the columns
  dataset$Date = dataset[,1]
  dataset$Time = dataset[,2]
  dataset$WaveHeight = dataset[,4]
  dataset$WaveDir = dataset[,5]
  dataset$WavePeriod = dataset[,6]
  dataset$WindSpeed = dataset[,7]
  dataset$WindDirection = dataset[,9]
  dataset$Tide = buoydata[,3]
  
  # exclude the long names
  dataset = dataset[c(-1:-17)]
  
  # Format the Date and time
  # Join the two columns for date and time
  dataset$DateTime <- paste(dataset$Date, dataset$Time, sep=" ") 
  # remove the words in parentheses
  dataset$DateTime <- gsub("\\s*\\([^\\)]+\\)","",as.character(dataset$DateTime))
  # format as POSIXct 
  dataset$DateTime <- as.POSIXct(dataset$DateTime,format="%m/%d %H:%M")
  # get rid of the old date and time
  dataset = dataset [c(-1,-2)]
  # convert characters to numeric type
  # clean up NAs
  cols_to_change = c(1, 3, 4,6)
  for(i in cols_to_change){
    #Change to type numeric
    dataset[,i] = as.numeric(dataset[,i])
    #remove first NAs
    dataset[,i] <- as.numeric(na.fill(dataset[,i], c('extend',NA)))
    # aproximate all other NAs
    dataset[,i] <- as.numeric(na.approx(dataset[,i]))
  }
  
  return(dataset)
}

updateTaitung <- function() {
  filename = "taitungBuoy.csv"
  TaitungUrl = 'https://www.cwb.gov.tw/V7/marine/sea_condition/eng/tables/WRA007.html'
  FugangBuoyURL = 'https://www.cwb.gov.tw/V7/marine/sea_condition/eng/tables/1586.html'
  TaitungData <- (GetLongBuoyData(TaitungUrl,FugangBuoyURL))
  #write.csv(TaitungData, file = filename)
  saveBuoyData(TaitungData,filename)
}

updateYilan <- function() {
  filename = "yilanBuoy.csv"
  YilanUrl = 'https://www.cwb.gov.tw/V7/marine/sea_condition/eng/tables/46708A.html'
  YilanBuoy = 'https://www.cwb.gov.tw/V7/marine/sea_condition/eng/tables/1236.html'
  YilanData <- (GetLongBuoyData(YilanUrl,YilanBuoy))
  #write.csv(YilanData, file = filename)
  saveBuoyData(YilanData,filename)
}

updateSuAo <- function() {
  filename = "suaoBuoy.csv"
  SuAoUrl = 'https://www.cwb.gov.tw/V7/marine/sea_condition/eng/tables/46706A.html'
  SuAoBuoy = 'https://www.cwb.gov.tw/V7/marine/sea_condition/eng/tables/1246.html'
  SuAoData <- (GetLongBuoyData(SuAoUrl,SuAoBuoy))
  #write.csv(SuAoData, file = filename)
  saveBuoyData(SuAoData,filename)
}

updateXiaoLiuQiu <- function() {
  filename = "xiaoliuqiuBuoy.csv"
  XiaoLiuQiuUrl = 'https://www.cwb.gov.tw/V7/marine/sea_condition/eng/tables/46714D.html'
  XiaoLiuQiuBuoy = 'https://www.cwb.gov.tw/V7/marine/sea_condition/eng/tables/1386.html'
  XiaoLiuQiuData <- (GetLongBuoyData(XiaoLiuQiuUrl,XiaoLiuQiuBuoy))
  #write.csv(XiaoLiuQiuData, file = filename)
  saveBuoyData(XiaoLiuQiuData,filename)
}

updateHualien <- function() {
  filename = "hualienBuoy.csv"
  HualienUrl = 'https://www.cwb.gov.tw/V7/marine/sea_condition/eng/tables/46699A.html'
  HualienBuoyURL = 'https://www.cwb.gov.tw/V7/marine/sea_condition/eng/tables/1256.html'
  HualienData <- (GetLongBuoyData(HualienUrl,HualienBuoyURL))
  #write.csv(HualienData, file = filename)
  saveBuoyData(HualienData,filename)
}


updateHualien()
updateXiaoLiuQiu()
updateSuAo()
updateYilan()
updateTaitung()

