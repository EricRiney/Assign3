# Eric 
# INFO 370

setwd("/Users/ericriner/Documents/Code/UW/370/Assign3")

weather = read.csv(file.choose(), header = TRUE)
crime = read.csv(file.choose(), header = TRUE)

weatherAll = weather[(weather$Year == '2015'),]
grainWeatherAllResults = data.frame((paste(match(weatherAll$Month,month.abb), weatherAllResults$Date,sep="-")))


weatherAll$Date = grainWeatherAllResults
crimeAllBURGLARY = crime[(crime$Summarized.Offense.Description == 'BURGLARY' &  crime$Year == '2015'),]
grainCrimeAllBURGLARY = format(as.POSIXct(strptime(crimeAllBURGLARY$Date.Reported,
                                                   "%m/%d/%Y %H:%M",tz="")) ,format = "%m-%d")

year = vector(mode="numeric", length=365)


cor(crimeAllBURGLARYResults,weatherAllResults)

length(crimeAllBURGLARYResults)
length(weatherAllResults)

nrow(crimeAllBURGLARY)
