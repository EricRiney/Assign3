# Eric 
# INFO 370

require(zoo)
library(lubridate)
library(ggplot2)

weather = read.csv(file.choose(), header = TRUE)
crime = read.csv(file.choose(), header = TRUE)

crimeDate = mdy_hms(crime$Event.Clearance.Date)

days = day(crimeDate)
months = month(crimeDate)
years = year(crimeDate)

# Make sure you use only numbers in the parameters, not strings (4 instead of '4')
# If you want non continuous frames, then use rbind(frame1, frame2) to add rows together
makeDataframe = function(startDay, endDay, startMonth, endMonth, startYear, endYear) {
  tempFrame = crime[(days >= startDay & days <= endDay &
                       months >= startMonth & months <= endMonth &
                       years >= startYear & years <= endYear &
                       !is.na(months)),]
  return(tempFrame)
}

weatherAll = weather[(weather$Year == '2015'),]

weatherAll$Month = formatC(weatherAll$Month, width = 2, format = "d", flag = "0")
weatherAll$Date = formatC(weatherAll$Date, width = 2, format = "d", flag = "0")
weatherAll$Date = paste(weatherAll$Month, weatherAll$Date, sep = '-')

crime$Year = year(crimeDate)
crime$Month = formatC(month(crimeDate), width = 2, format = "d", flag = "0")
crime$Day = formatC(day(crimeDate), width = 2, format = "d", flag = "0")

getCorrelation = function(crimeName, crimeYear) {
  crimeAllBURGLARY = crime[(crime$Event.Clearance.Group == toString(crimeName) &  crime$Year == toString(crimeYear)),]
  crimeAllBURGLARY$Date = paste(crimeAllBURGLARY$Month, crimeAllBURGLARY$Day, sep = '-')
  
  mergedSet = merge(x = weatherAll, y = crimeAllBURGLARY, by.x = 'Date', by.y = 'Date', all = T)
  
  correlationFrame = data.frame(table(mergedSet$Date)[1:(length(table(mergedSet$Date)) - 1)], weatherAll$Precip...in.)
  names(correlationFrame) = c('date', 'count', 'precip')
  
  correlationFrameAgg = aggregate(. ~  precip, data = correlationFrame, sum)
  correlationFrameAgg$precip = as.numeric(correlationFrameAgg$precip)
  correlationFrameAgg$count = as.numeric(correlationFrameAgg$count)
  
  summary(lm(log(count) ~ precip, data = correlationFrameAgg))
  cor.test(x = correlationFrameAgg$precip, y = log(correlationFrameAgg$count))
  
  ggplot(correlationFrameAgg, aes(x = precip, y = log(count))) +
    ylab(paste('Count of', crimeName, sep = ' ')) +
    xlab('Amount of Precipitation in inches') +
    geom_point(shape=1) +
    geom_smooth(method=lm)
}

getCorrelation('BURGLARY', '2015')

#year = vector(mode="numeric", length=365)


#cor(crimeAllBURGLARYResults,weatherAllResults)

#length(crimeAllBURGLARYResults)
#length(weatherAllResults)

#nrow(crimeAllBURGLARY)
