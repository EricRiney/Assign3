# Eric 
# INFO 370

weather = read.csv(file.choose(), header = TRUE)
crime = read.csv(file.choose(), header = TRUE)

weatherAll = weather[(weather$Year == '2015'),]


weatherAll$Month = formatC(weatherAll$Month, width = 2, format = "d", flag = "0")
weatherAll$Date = formatC(weatherAll$Date, width = 2, format = "d", flag = "0")
weatherAll$Date = paste(weatherAll$Month, weatherAll$Date, sep = '-')

crimeAllBURGLARY = crime[(crime$Summarized.Offense.Description == 'BURGLARY' &  crime$Year == '2015'),]
grainCrimeAllBURGLARY = data.frame(format(as.POSIXct(strptime(crimeAllBURGLARY$Date.Reported,
                                                   "%m/%d/%Y %H:%M",tz="")) ,format = "%m-%d"))
crimeAllBURGLARY$Date = grainCrimeAllBURGLARY

mergedSet = merge(x = weatherAll, y = crimeAllBURGLARY, by.x = Date, by.y = Date, all = T)

#year = vector(mode="numeric", length=365)


#cor(crimeAllBURGLARYResults,weatherAllResults)

#length(crimeAllBURGLARYResults)
#length(weatherAllResults)

#nrow(crimeAllBURGLARY)
