# Eric 
# INFO 370

setwd("/Users/ericriner/Documents/Code/UW/370/Assign3")

weather = read.csv(file.choose(), header = TRUE)
crime = read.csv(file.choose(), header = TRUE)

### 2012 burglary Crime info ###
burglaryCrime2012Vec = (seq(as.Date("2012-1-01"), as.Date("2012-12-31"), by="+1 day"))
burglaryCrime2012Results = vector(mode="numeric", length=length(burglaryCrime2012Vec))
for (i in 1:length(burglaryCrime2012Vec)) {
  counter = 0
  if(crime$Summarized.Offense.Description[i] == 'BURGLARY'){
    counter = counter + 1
  }
  burglaryCrime2012Results[i] = counter
}
cor(weather2012DatesResults,burglaryCrime2012Results)

boxplot((weather2012DatesResults),(burglaryCrime2012Results),
        main="Overcast vs Burglary",
        notch = FALSE, add = FALSE, col = "blue")
#plot(weather2012DatesResults)

summary(burglaryCrime2012Results)
summary(weather2012DatesResults)

### 2012 Carprowl Crime info ###
carprowlCrime2012Vec = (seq(as.Date("2012-1-01"), as.Date("2012-12-31"), by="+1 day"))
carprowlCrime2012Results = vector(mode="numeric", length=length(carprowlCrime2012Vec))
for (i in 1:length(carprowlCrime2012Vec)) {
  counter = 0
  if(crime$Summarized.Offense.Description[i] == 'CAR PROWL'){
    counter = counter + 1
  }
  carprowlCrime2012Results[i] = counter
}
summary(carprowlCrime2012Results)
cor(weather2012DatesResults,carprowlCrime2012Results)

### 2012 shoplifting Crime info ###
shopliftingCrime2012Vec = (seq(as.Date("2012-1-01"), as.Date("2012-12-31"), by="+1 day"))
shopliftingCrime2012Results = vector(mode="numeric", length=length(shopliftingCrime2012Vec))
for (i in 1:length(shopliftingCrime2012Vec)) {
  counter = 0
  if(crime$Summarized.Offense.Description[i] == 'SHOPLIFTING'){
    counter = counter + 1
  }
  shopliftingCrime2012Results[i] = counter
}
summary(shopliftingCrime2012Results)
cor(weather2012DatesResults,shopliftingCrime2012Results)

### 2012 vehicleTheft Crime info ###
vehicleTheftCrime2012Vec = (seq(as.Date("2012-1-01"), as.Date("2012-12-31"), by="+1 day"))
vehicleTheftCrime2012Results = vector(mode="numeric", length=length(vehicleTheftCrime2012Vec))
for (i in 1:length(vehicleTheftCrime2012Vec)) {
  counter = 0
  if(crime$Summarized.Offense.Description[i] == 'VEHICLE THEFT'){
    counter = counter + 1
  }
  vehicleTheftCrime2012Results[i] = counter
}
summary(vehicleTheftCrime2012Results)
cor(weather2012DatesResults,vehicleTheftCrime2012Results)
