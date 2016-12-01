# Eric 
# INFO 370

setwd("/Users/ericriner/Documents/Code/UW/370/Assign3")

weather = read.csv(file.choose(), header = TRUE)
crime = read.csv(file.choose(), header = TRUE)

weatherAll = weather[(weather$Year == '2015'),]
weatherAllResults = weatherAll[(weatherAll$Precip...in. != 0),]

crimeAllBURGLARY = crime[(crime$Summarized.Offense.Description == 'BURGLARY' &  crime$Year == '2015'),]

bork = match(weatherAll$Month,month.abb)
meow = crimeAllBURGLARY$Month

crimeAllBURGLARYResults = vector(mode="numeric", length=nrow(weatherAll))

for (i in 1:nrow(crimeAllBURGLARY )) {
    if(crimeAllBURGLARY$Month[i] != 0) {
      crimeAllBURGLARYResults[i] = 1
    }
  }

cor(crimeAllBURGLARYResults,weatherAllResults)

length(crimeAllBURGLARYResults)
length(weatherAllResults)

nrow(crimeAllBURGLARY)
