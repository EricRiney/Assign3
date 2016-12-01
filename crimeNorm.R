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

### 2012 robbery Crime info ###
robberyCrime2012Vec = (seq(as.Date("2012-1-01"), as.Date("2012-12-31"), by="+1 day"))
robberyCrime2012Results = vector(mode="numeric", length=length(robberyCrime2012Vec))
for (i in 1:length(robberyCrime2012Vec)) {
  counter = 0
  if(crime$Summarized.Offense.Description[i] == 'ROBBERY'){
    counter = counter + 1
  }
  robberyCrime2012Results[i] = counter
}
summary(robberyCrime2012Results)
cor(weather2012DatesResults,robberyCrime2012Results)

### 2012 assault Crime info ###
assaultCrime2012Vec = (seq(as.Date("2012-1-01"), as.Date("2012-12-31"), by="+1 day"))
assaultCrime2012Results = vector(mode="numeric", length=length(assaultCrime2012Vec))
for (i in 1:length(assaultCrime2012Vec)) {
  counter = 0
  if(crime$Summarized.Offense.Description[i] == 'ASSAULT'){
    counter = counter + 1
  }
  assaultCrime2012Results[i] = counter
}
summary(assaultCrime2012Results)
cor(weather2012DatesResults, assaultCrime2012Results)

### 2012 threats Crime info ###
threatsCrime2012Vec = (seq(as.Date("2012-1-01"), as.Date("2012-12-31"), by="+1 day"))
threatsCrime2012Results = vector(mode="numeric", length=length(threatsCrime2012Vec))
for (i in 1:length(threatsCrime2012Vec)) {
  counter = 0
  if(crime$Summarized.Offense.Description[i] == 'THREATS'){
    counter = counter + 1
  }
  threatsCrime2012Results[i] = counter
}
summary(threatsCrime2012Results)
cor(weather2012DatesResults,robberyCrime2012Results)

### 2012 weapons Crime info ###
weaponsCrime2012Vec = (seq(as.Date("2012-1-01"), as.Date("2012-12-31"), by="+1 day"))
weaponsCrime2012Results = vector(mode="numeric", length=length(weaponsCrime2012Vec))
for (i in 1:length(weaponsCrime2012Vec)) {
  counter = 0
  if(crime$Summarized.Offense.Description[i] == 'WEAPON'){
    counter = counter + 1
  }
  weaponsCrime2012Results[i] = counter
}
summary(weaponsCrime2012Results)
cor(weather2012DatesResults,weaponsCrime2012Results)

### 2012 bikeTheft Crime info ###
bikeTheftCrime2012Vec = (seq(as.Date("2012-1-01"), as.Date("2012-12-31"), by="+1 day"))
bikeTheftCrime2012Results = vector(mode="numeric", length=length(bikeTheftCrime2012Vec))
for (i in 1:length(bikeTheftCrime2012Vec)) {
  counter = 0
  if(crime$Summarized.Offense.Description[i] == 'BIKE THEFT'){
    counter = counter + 1
  }
  bikeTheftCrime2012Results[i] = counter
}
summary(bikeTheftCrime2012Results)
cor(weather2012DatesResults,bikeTheftCrime2012Results)


### 2012 narcotics Crime info ###
narcoticsCrime2012Vec = (seq(as.Date("2012-1-01"), as.Date("2012-12-31"), by="+1 day"))
narcoticsCrime2012Results = vector(mode="numeric", length=length(narcoticsCrime2012Vec))
for (i in 1:length(narcoticsCrime2012Vec)) {
  counter = 0
  if(crime$Summarized.Offense.Description[i] == 'NARCOTICS'){
    counter = counter + 1
  }
  narcoticsCrime2012Results[i] = counter
}
summary(narcoticsCrime2012Results)
cor(weather2012DatesResults,narcoticsCrime2012Results)


### 2012 fraud Crime info ###
fraudCrime2012Vec = (seq(as.Date("2012-1-01"), as.Date("2012-12-31"), by="+1 day"))
fraudCrime2012Results = vector(mode="numeric", length=length(fraudCrime2012Vec))
for (i in 1:length(fraudCrime2012Vec)) {
  counter = 0
  if(crime$Summarized.Offense.Description[i] == 'FRAUD'){
    counter = counter + 1
  }
  fraudCrime2012Results[i] = counter
}
summary(fraudCrime2012Results)
cor(weather2012DatesResults,fraudCrime2012Results)


### 2012 liquorViolation info ###
liquorViolationCrime2012Vec = (seq(as.Date("2012-1-01"), as.Date("2012-12-31"), by="+1 day"))
liquorViolationCrime2012Results = vector(mode="numeric", length=length(liquorViolationCrime2012Vec))
for (i in 1:length(liquorViolationCrime2012Vec)) {
  counter = 0
  if(crime$Summarized.Offense.Description[i] == 'LIQUOR VIOLATION'){
    counter = counter + 1
  }
  liquorViolationCrime2012Results[i] = counter
}
summary(liquorViolationCrime2012Results)
cor(weather2012DatesResults,liquorViolationCrime2012Results)

### 2012 animalComplaint info ###
animalComplaintCrime2012Vec = (seq(as.Date("2012-1-01"), as.Date("2012-12-31"), by="+1 day"))
animalComplaintCrime2012Results = vector(mode="numeric", length=length(animalComplaintCrime2012Vec))
for (i in 1:length(animalComplaintViolationCrime2012Vec)) {
  counter = 0
  if(crime$Summarized.Offense.Description[i] == 'ANIMAL COMPLAINT'){
    counter = counter + 1
  }
  animalComplaintCrime2012Results[i] = counter
}
summary(animalComplaintCrime2012Results)
cor(weather2012DatesResults,animalComplaintCrime2012Results)

