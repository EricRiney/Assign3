# Eric 
# INFO 370

setwd("/Users/ericriner/Documents/Code/UW/370/Assign3")

weather = read.csv(file.choose(), header = TRUE)
crime = read.csv(file.choose(), header = TRUE)

weatherAll = weather[(weather$Year == '2015'),]
weatherAllResults = weatherAll[(weatherAll$Precip...in. != 0),]

crimeAllBURGLARY = crime[(crime$Summarized.Offense.Description == 'BURGLARY' &  crime$Year == '2015'),]
year = vector(mode="numeric", length=365)


cor(crimeAllBURGLARYResults,weatherAllResults)

length(crimeAllBURGLARYResults)
length(weatherAllResults)

nrow(crimeAllBURGLARY)
