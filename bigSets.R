# Eric 
# INFO 370

setwd("/Users/ericriner/Documents/Code/UW/370/Assign3")

weather = read.csv(file.choose(), header = TRUE)
crime = read.csv(file.choose(), header = TRUE)

weatherAll = weather[(weather$Year == '2012' || 
                        weather$Year == '2013' || 
                        weather$Year == '2014' || 
                        weather$Year == '2015'),]
weatherAllResults = vector(mode="numeric", length=nrow(weatherAll))

for (i in 1:nrow(weatherAll)) {
  #counter = 0
  #for (j in 1:length(weatherAllResults)) {
    if(weatherAll$Precip...in.[i] != 0) {
      weatherAllResults[i] = 1
    }
    # weatherAllResults[i] = (counter)
  }
#}

crimeAllBURGLARY = crime[(crime$Summarized.Offense.Description == 'BURGLARY' || 
                            crime$Year == '2012' || 
                            crime$Year == '2013' || 
                            crime$Year == '2014' || 
                            crime$Year == '2015'),]

crimeAllBURGLARYResults = vector(mode="numeric", length=nrow(weatherAll))
for (i in 1:nrow(crimeAllBURGLARY)) {
  counter = 0
  if(crimeAllBURGLARY$Summarized.Offense.Description[i] == 'BURGLARY'){
    counter = counter + 1
  }
  crimeAllBURGLARYResults[i] = counter
}

cor(crimeAllBURGLARYResults,weatherAllResults)

length(crimeAllBURGLARYResults)
length(weatherAllResults)

nrow(crimeAllBURGLARY)
