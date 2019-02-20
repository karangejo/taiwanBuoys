library(XML)
library(htmltab)
library(ggplot2)
library(zoo)
library(data.table)
library(gridExtra)



GetBuoyData<- function(buoyUrl,tideUrl){
  
  
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
  
  # subsetting dataset to last 3 days
  
  dataset <- dataset[1:72,]
  
  return(dataset)
}


GetTaitung <- function(){
  TaitungUrl = 'https://www.cwb.gov.tw/V7/marine/sea_condition/eng/tables/WRA007.html'
  FugangBuoyURL = 'https://www.cwb.gov.tw/V7/marine/sea_condition/eng/tables/1586.html'
  TaitungData <- (GetBuoyData(TaitungUrl,FugangBuoyURL))
  台東浮標 <-TaitungData
#  save(TaitungData,file="TaitungBuoyData.Rda")
  save(台東浮標,file="台東浮標.Rda")
  
}

GetHualien <- function(){
  HualienUrl = 'https://www.cwb.gov.tw/V7/marine/sea_condition/eng/tables/46699A.html'
  HualienBuoyURL = 'https://www.cwb.gov.tw/V7/marine/sea_condition/eng/tables/1256.html'
  HualienData <- (GetBuoyData(HualienUrl,HualienBuoyURL))
  花蓮浮標 <-HualienData
 # save(HualienData,file="HualienBuoyData.Rda")
  save(花蓮浮標,file="花蓮浮標.Rda")
}

GetYilan <- function(){
  YilanUrl = 'https://www.cwb.gov.tw/V7/marine/sea_condition/eng/tables/46708A.html'
  YilanBuoy = 'https://www.cwb.gov.tw/V7/marine/sea_condition/eng/tables/1236.html'
  YilanData <- (GetBuoyData(YilanUrl,YilanBuoy))
  宜蘭浮標 <- YilanData
 # save(YilanData,file="YilanBuoyData.Rda")
  save(宜蘭浮標,file="宜蘭浮標.Rda")
}

GetSuAo <- function(){
  SuAoUrl = 'https://www.cwb.gov.tw/V7/marine/sea_condition/eng/tables/46706A.html'
  SuAoBuoy = 'https://www.cwb.gov.tw/V7/marine/sea_condition/eng/tables/1246.html'
  SuAoData <- (GetBuoyData(SuAoUrl,SuAoBuoy))
  蘇澳浮標 <- SuAoData
 # save(SuAoData,file="SuAoBuoyData.Rda")
  save(蘇澳浮標,file="蘇澳浮標.Rda")
}

GetXiaoLiuQiu <- function(){
  XiaoLiuQiuUrl = 'https://www.cwb.gov.tw/V7/marine/sea_condition/eng/tables/46714D.html'
  XiaoLiuQiuBuoy = 'https://www.cwb.gov.tw/V7/marine/sea_condition/eng/tables/1386.html'
  XiaoLiuQiuData <- (GetBuoyData(XiaoLiuQiuUrl,XiaoLiuQiuBuoy))
  小琉球浮標 <- XiaoLiuQiuData
 # save(XiaoLiuQiuData,file="XiaoLiuQiuBuoyData.Rda")
  save(小琉球浮標,file="小琉球浮標.Rda")
}

GetTideData <- function(tideUrl){
  tideData <- htmltab(tideUrl,rm_nodata_cols = FALSE, rm_nodata_rows = FALSE)
  tideData <- transpose(tideData)
  colnames(tideData) <- as.character(unlist(tideData[1,]))
  tideData <- tideData[-1,]
  tideData[,3] <- gsub("morn-ing","morning",tideData[,3])
  tideData[,3] <- gsub("after-noon","afternoon",tideData[,3])
  tideData[is.na(tideData)] <- "-"
  tideData$Time <- paste(tideData[,2],tideData[,3])
  tideData <- tideData[,-c(1:4)]
  if(ncol(tideData) < 16){
    tideData <- tideData[,-c(3:10)]
    tideData <- tideData[,c(5,1,2,3,4)]
    
    return(tideData)
    
  }else{
    tideData <- tideData[,-c(6:13)]
    tideData <- tideData[,c(8,1,2,6,7,3,4,5)]
    tideData[,7] <- as.numeric(tideData[,7])
    tideData[,8] <- as.numeric(tideData[,8])
    tideData[,6] <- as.factor(tideData[,6])
    tideData$Number <- as.numeric(c(1:nrow(tideData)))
    
    return(tideData) 
  }
}

PredTaitung <- function(){
  TaitungTide = 'https://www.tide-forecast.com/locations/Taitung-City/forecasts/latest/six_day'
  TaitungTideData <- GetTideData(TaitungTide)
  台東潮 <- TaitungTideData
 # save(TaitungTideData,file="TaitungTideData.Rda")
  save(台東潮,file="台東潮.Rda")
}

PredHualien <- function(){
  HualienTide = 'https://www.tide-forecast.com/locations/Hualien-City/forecasts/latest/six_day'
  HualienTideData <- GetTideData(HualienTide)
  花蓮潮<- HualienTideData
 # save(HualienTideData,file="HualienTideData.Rda")
  save(花蓮潮,file="花蓮潮.Rda")
}

PredYilan <- function(){
  ChiLungTide = 'https://www.tide-forecast.com/locations/Chi-Lung-Taiwan/forecasts/latest/six_day'
  YilanTideData <- GetTideData(ChiLungTide)
  宜蘭潮 <- YilanTideData
 # save(YilanTideData,file="YilanTideData.Rda")
  save(宜蘭潮,file="宜蘭潮.Rda")
}

PredSuAo <- function(){
  ChiLungTide = 'https://www.tide-forecast.com/locations/Chi-Lung-Taiwan/forecasts/latest/six_day'
  SuAoTideData <- GetTideData(ChiLungTide)
  蘇澳潮 <- SuAoTideData
 # save(SuAoTideData,file="SuAoTideData.Rda")
  save(蘇澳潮,file="蘇澳潮.Rda")
}

PredXiaoLiuQiu <- function(){
  HengChunTide = 'https://www.tide-forecast.com/locations/Heng-ch-un/forecasts/latest/six_day'
  XiaoLiuQiuTideData <- GetTideData(HengChunTide)
  小琉球潮 <- XiaoLiuQiuTideData
 # save(XiaoLiuQiuTideData,file="XiaoLiuQiuTideData.Rda")
  save(小琉球潮,file="小琉球潮.Rda")
}

GetXiaoLiuQiu()
GetSuAo()
GetYilan()
GetHualien()
GetTaitung()

PredHualien()
PredSuAo()
PredTaitung()
PredXiaoLiuQiu()
PredYilan()


