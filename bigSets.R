# Eric 
# INFO 370

require(zoo)
library(lubridate)

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

crimeAllBURGLARY = crime[(crime$Event.Clearance.Group == 'BURGLARY' &  crime$Year == '2015'),]

crimeAllBURGLARY$Date = paste(crimeAllBURGLARY$Month, crimeAllBURGLARY$Day, sep = '-')

mergedSet = merge(x = weatherAll, y = crimeAllBURGLARY, by.x = 'Date', by.y = 'Date', all = T)

length(which(weatherAll$Date == '01-01'))
length(which(crimeAllBURGLARY$Date == '01-01'))

#length(crimeAllBURGLARYResults)
#length(weatherAllResults)

#nrow(crimeAllBURGLARY)
