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
weatherAll = weather

weatherAll$Day = weatherAll$Date
weatherAll$Month = formatC(weatherAll$Month, width = 2, format = "d", flag = "0")
weatherAll$Date = formatC(weatherAll$Date, width = 2, format = "d", flag = "0")
weatherAll$Date = paste(weatherAll$Month, weatherAll$Date, sep = '-')
weatherAll$Date = paste(weatherAll$Year, weatherAll$Date, sep = '-')

crime$Year = year(crimeDate)
crime$Month = formatC(month(crimeDate), width = 2, format = "d", flag = "0")
crime$Day = formatC(day(crimeDate), width = 2, format = "d", flag = "0")

getCorrelation = function(crimeName, crimeYear) {
  crimeAllBURGLARY = crime[(crime$Event.Clearance.Group == toString(crimeName) &  crime$Year == toString(crimeYear)),]
  crimeAllBURGLARY$Date = paste(crimeAllBURGLARY$Month, crimeAllBURGLARY$Day, sep = '-')
  
  mergedSet = merge(x = weatherAll, y = crimeAllBURGLARY, by.x = 'Date', by.y = 'Date', all = T)
  
  correlationFrame = data.frame(table(mergedSet$Date)[1:365], weatherAll$Precip...in.)
  names(correlationFrame) = c('date', 'count', 'precip')
  
  correlationFrameAgg = aggregate(. ~  precip, data = correlationFrame, sum)
  correlationFrameAgg$precip = (as.numeric(correlationFrameAgg$precip) - 1) / 100
  correlationFrameAgg$count = as.numeric(correlationFrameAgg$count)
  
  print(summary(lm(log(count) ~ precip, data = correlationFrameAgg)))
  print(cor.test(x = correlationFrameAgg$precip, y = log(correlationFrameAgg$count)))
  
  ggplot(correlationFrameAgg, aes(x = precip, y = log(count))) +
    ylab(paste('Log() of Count of', crimeName, sep = ' ')) +
    xlab('Amount of Precipitation in inches') +
    geom_point(shape=1) +
    geom_smooth(method=lm)
}

getCorrelation('BURGLARY', '2015')
getCorrelation('TRESPASS', '2015')
getCorrelation('DISTURBANCES', '2015')
getCorrelation('OTHER PROPERTY', '2015')
getCorrelation('NUISANCE, MISCHIEF', '2015')
getCorrelation('TRAFFIC RELATED CALLS', '2015')

getCorrelation('SUSPICIOUS CIRCUMSTANCES', '2015')
getCorrelation('MENTAL HEALTH', '2015') 
getCorrelation('LIQUOR VIOLATIONS', '2015')
getCorrelation('ASSAULTS', '2015')
getCorrelation('NARCOTICS COMPLAINTS', '2015')
getCorrelation('ACCIDENT INVESTIGATION', '2015')
getCorrelation('SHOPLIFTING', '2015')
getCorrelation('PROWLER', '2015')
getCorrelation('HAZARDS', '2015')
getCorrelation('BIKE', '2015') #!!! 
getCorrelation('PROSTITUTION', '2015')
getCorrelation('MISCELLANEOUS MISDEMEANORS', '2015')
getCorrelation('ANIMAL COMPLAINTS', '2015')
getCorrelation('THREATS, HARASSMENT', '2015')
getCorrelation('PERSON DOWN/INJURY', '2015')
getCorrelation('AUTO THEFTS', '2015')
getCorrelation('PERSONS - LOST, FOUND, MISSING', '2015')
getCorrelation('FALSE ALARMS', '2015')
getCorrelation('ROBBERY', '2015')
getCorrelation('ARREST', '2015')
getCorrelation('FALSE ALACAD', '2015') #!!! 
getCorrelation('RECKLESS BURNING', '2015') #!!!
getCorrelation('BEHAVIORAL HEALTH', '2015') #!!!
getCorrelation('', '2015') #!!!


getCorrelation('BURGLARY', '2014')
getCorrelation('TRESPASS', '2014')
getCorrelation('DISTURBANCES', '2014')
getCorrelation('OTHER PROPERTY', '2014')
getCorrelation('NUISANCE, MISCHIEF', '2014')
getCorrelation('TRAFFIC RELATED CALLS', '2014')
getCorrelation('SUSPICIOUS CIRCUMSTANCES', '2014')
getCorrelation('MENTAL HEALTH', '2014') 
getCorrelation('LIQUOR VIOLATIONS', '2014')
getCorrelation('ASSAULTS', '2014')
getCorrelation('NARCOTICS COMPLAINTS', '2014')
getCorrelation('ACCIDENT INVESTIGATION', '2014')
getCorrelation('SHOPLIFTING', '2014')
getCorrelation('PROWLER', '2014')
getCorrelation('HAZARDS', '2014')
getCorrelation('BIKE', '2014') #!!! 
getCorrelation('PROSTITUTION', '2014')
getCorrelation('MISCELLANEOUS MISDEMEANORS', '2014')
getCorrelation('ANIMAL COMPLAINTS', '2014')
getCorrelation('THREATS, HARASSMENT', '2014')
getCorrelation('PERSON DOWN/INJURY', '2014')
getCorrelation('AUTO THEFTS', '2014')
getCorrelation('PERSONS - LOST, FOUND, MISSING', '2014')
getCorrelation('FALSE ALARMS', '2014')
getCorrelation('ROBBERY', '2014')
getCorrelation('ARREST', '2014')
getCorrelation('FALSE ALACAD', '2014') #!!! 
getCorrelation('RECKLESS BURNING', '2014') #!!!
getCorrelation('BEHAVIORAL HEALTH', '2014') #!!!
getCorrelation('', '2014') #!!!

getCorrelation('BURGLARY', '2013')
getCorrelation('TRESPASS', '2013')
getCorrelation('DISTURBANCES', '2013')
getCorrelation('OTHER PROPERTY', '2013')
getCorrelation('NUISANCE, MISCHIEF', '2013')
getCorrelation('TRAFFIC RELATED CALLS', '2013')
getCorrelation('SUSPICIOUS CIRCUMSTANCES', '2013')
getCorrelation('MENTAL HEALTH', '2013') 
getCorrelation('LIQUOR VIOLATIONS', '2013')
getCorrelation('ASSAULTS', '2013')
getCorrelation('NARCOTICS COMPLAINTS', '2013')
getCorrelation('ACCIDENT INVESTIGATION', '2013')
getCorrelation('SHOPLIFTING', '2014')
getCorrelation('PROWLER', '2014')
getCorrelation('HAZARDS', '2014')
getCorrelation('BIKE', '2014') #!!! 
getCorrelation('PROSTITUTION', '2014')
getCorrelation('MISCELLANEOUS MISDEMEANORS', '2014')
getCorrelation('ANIMAL COMPLAINTS', '2014')
getCorrelation('THREATS, HARASSMENT', '2014')
getCorrelation('PERSON DOWN/INJURY', '2014')
getCorrelation('AUTO THEFTS', '2014')
getCorrelation('PERSONS - LOST, FOUND, MISSING', '2014')
getCorrelation('FALSE ALARMS', '2014')
getCorrelation('ROBBERY', '2014')
getCorrelation('ARREST', '2014')
getCorrelation('FALSE ALACAD', '2014') #!!! 
getCorrelation('RECKLESS BURNING', '2014') #!!!
getCorrelation('BEHAVIORAL HEALTH', '2014') #!!!
getCorrelation('', '2014') #!!!

getCorrelation('DISTURBANCES', '2015')

crime$Date = paste(crime$Month, crime$Day, sep = '-')
crime$Date = paste(crime$Year, crime$Date, sep = '-')
mergedSet = merge(x = weatherAll, y = crime, by.x = 'Date', by.y = 'Date', all = T)

crimeCountDate = table(mergedSet$Date[which(mergedSet$Date != 'NA-NA-NA')])
crimeCountDate = crimeCountDate[563:length(crimeCountDate)]
crimeCountDate = data.frame(crimeCountDate)
names(crimeCountDate) = c('Date', 'count')
precipDate = data.frame(weatherAll$Date, weatherAll$Precip...in.)
names(precipDate) = c('Date', 'precip')
precipDate$precip = (as.numeric(precipDate$precip) - 1) / 100
precipCrimeMerged = merge(crimeCountDate, precipDate, by = 'Date', all = F)
noRain = precipCrimeMerged[which(precipCrimeMerged$precip == 0), 'count']
rain = precipCrimeMerged[which(precipCrimeMerged$precip != 0), 'count']
boxplotData = lapply(c('noRain', 'rain'), get, envir = environment())
names(boxplotData) = c('No Rain', 'Rain')

t.test(noRain, rain)

boxplot(boxplotData)