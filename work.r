# Ashlyn and Eric 
# INFO 370

library(MASS) # need fractions
require(zoo)  # date stuff
library(lubridate)

setwd("/Users/ericriner/Documents/Code/UW/370/Assign3")

weather = read.csv(file.choose(), header = TRUE)
crime = read.csv(file.choose(), header = TRUE)

summary(weather)
summary(crime)

#GOAL: FIND OUT IF THERE IS A CORRELATION BETWEEN WEATHER AND CRIME RATES

hist(weather$AvgTemp....F.)

# Monthly Crime Dataframes for 2011
AUG2011df = crime[(crime$Month == '8' & crime$Year== '2011'),]
SEP2011df = crime[(crime$Month == "9" & crime$Year == "2011"),]
OCT2011df = crime[(crime$Month == "10" & crime$Year == "2011"),]
NOV2011df = crime[(crime$Month == "11" & crime$Year == "2011"),]
DEC2011df = crime[(crime$Month == "12" & crime$Year == "2011"),]

# Monthly Crime Dataframes for 2012
JAN2012df = crime[(crime$Month == "1" & crime$Year == "2012"),]
FEB2012df = crime[(crime$Month == "2" & crime$Year == "2012"),]
MAR2012df = crime[(crime$Month == "3" & crime$Year == "2012"),]
APR2012df = crime[(crime$Month == "4" & crime$Year == "2012"),]
MAY2012df = crime[(crime$Month == "5" & crime$Year == "2012"),]
JUN2012df = crime[(crime$Month == "6" & crime$Year == "2012"),]
JLY2012df = crime[(crime$Month == "7" & crime$Year == "2012"),]
AUG2012df = crime[(crime$Month == "8" & crime$Year == "2012"),]
SEP2012df = crime[(crime$Month == "9" & crime$Year == "2012"),]
OCT2012df = crime[(crime$Month == "10" & crime$Year == "2012"),]
NOV2012df = crime[(crime$Month == "11" & crime$Year == "2012"),]
DEC2012df = crime[(crime$Month == "12" & crime$Year == "2012"),]

# Monthly Crime Dataframes for 2013
JAN2013df = crime[(crime$Month == "1" & crime$Year == "2013"),]
FEB2013df = crime[(crime$Month == "2" & crime$Year == "2013"),]
MAR2013df = crime[(crime$Month == "3" & crime$Year == "2013"),]
APR2013df = crime[(crime$Month == "4" & crime$Year == "2013"),]
MAY2013df = crime[(crime$Month == "5" & crime$Year == "2013"),]
JUN2013df = crime[(crime$Month == "6" & crime$Year == "2013"),]
JLY2013df = crime[(crime$Month == "7" & crime$Year == "2013"),]
AUG2013df = crime[(crime$Month == "8" & crime$Year == "2013"),]
SEP2013df = crime[(crime$Month == "9" & crime$Year == "2013"),]
OCT2013df = crime[(crime$Month == "10" & crime$Year == "2013"),]
NOV2013df = crime[(crime$Month == "11" & crime$Year == "2013"),]
DEC2013df = crime[(crime$Month == "12" & crime$Year == "2013"),]

# Monthly Crime Dataframes for 2014
JAN2014df = crime[(crime$Month == "1" & crime$Year == "2014"),]
FEB2014df = crime[(crime$Month == "2" & crime$Year == "2014"),]
MAR2014df = crime[(crime$Month == "3" & crime$Year == "2014"),]
APR2014df = crime[(crime$Month == "4" & crime$Year == "2014"),]
MAY2014df = crime[(crime$Month == "5" & crime$Year == "2014"),]
JUN2014df = crime[(crime$Month == "6" & crime$Year == "2014"),]
JLY2014df = crime[(crime$Month == "7" & crime$Year == "2014"),]
AUG2014df = crime[(crime$Month == "8" & crime$Year == "2014"),]
SEP2014df = crime[(crime$Month == "9" & crime$Year == "2014"),]
OCT2014df = crime[(crime$Month == "10" & crime$Year == "2014"),]
NOV2014df = crime[(crime$Month == "11" & crime$Year == "2014"),]
DEC2014df = crime[(crime$Month == "12" & crime$Year == "2014"),]

# Monthly Crime Dataframes for 2015
JAN2015df = crime[(crime$Month == "1" & crime$Year == "2015"),]
FEB2015df = crime[(crime$Month == "2" & crime$Year == "2015"),]
MAR2015df = crime[(crime$Month == "3" & crime$Year == "2015"),]
APR2015df = crime[(crime$Month == "4" & crime$Year == "2015"),]
MAY2015df = crime[(crime$Month == "5" & crime$Year == "2015"),]
JUN2015df = crime[(crime$Month == "6" & crime$Year == "2015"),]
JLY2015df = crime[(crime$Month == "7" & crime$Year == "2015"),]
AUG2015df = crime[(crime$Month == "8" & crime$Year == "2015"),]
SEP2015df = crime[(crime$Month == "9" & crime$Year == "2015"),]
OCT2015df = crime[(crime$Month == "10" & crime$Year == "2015"),]
NOV2015df = crime[(crime$Month == "11" & crime$Year == "2015"),]
DEC2015df = crime[(crime$Month == "12" & crime$Year == "2015"),]

# Monthly Crime Dataframes for 2016
JAN2016df = crime[(crime$Month == "1" & crime$Year == "2016"),]
FEB2016df = crime[(crime$Month == "2" & crime$Year == "2016"),]
MAR2016df = crime[(crime$Month == "3" & crime$Year == "2016"),]
APR2016df = crime[(crime$Month == "4" & crime$Year == "2016"),]
MAY2016df = crime[(crime$Month == "5" & crime$Year == "2016"),]
JUN2016df = crime[(crime$Month == "6" & crime$Year == "2016"),]
JLY2016df = crime[(crime$Month == "7" & crime$Year == "2016"),]
AUG2016df = crime[(crime$Month == "8" & crime$Year == "2016"),]
SEP2016df = crime[(crime$Month == "9" & crime$Year == "2016"),]
OCT2016df = crime[(crime$Month == "10" & crime$Year == "2016"),]
NOV2016df = crime[(crime$Month == "11" & crime$Year == "2016"),]
DEC2016df = crime[(crime$Month == "12" & crime$Year == "2016"),]

# lil STD function
rineySTD = function(x) sd(x)/sqrt(length(x))        # assumes no missing values

# lil STE function
rineySTE = function(x) sqrt(var(x)/length(x))       # assumes no missing values


### need to run two sample t-test on shit ###

# aug 2011 Crime data
aug2011Vec = (seq(as.Date("2011-8-01"), as.Date("2011-8-31"), by="+1 day"))
aug2011CrimeDates <- format(as.POSIXct(strptime(AUG2011df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
aug2011CrimeDatesResults = vector(mode="numeric", length=0)
for (i in 1:length(aug2011Vec)) {
  counter = 0
  for (j in 1:length(aug2011CrimeDates)) {
    if(aug2011Vec[i] == aug2011CrimeDates[j]){
      counter = counter + 1
      aug2011CrimeDatesResults[i] = (counter)
    }
  }
}
aug2011CrimeDatesResults[is.na(aug2011CrimeDatesResults)] = 0

# nov 2011 Crime data
nov2011Vec = (seq(as.Date("2011-11-01"), as.Date("2011-11-30"), by="+1 day"))
nov2011CrimeDates <- format(as.POSIXct(strptime(NOV2011df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
nov2011CrimeDatesResults = vector(mode="numeric", length=0)
for (i in 1:length(nov2011Vec)) {
  counter = 0
  for (j in 1:length(nov2011CrimeDates)) {
    if(nov2011Vec[i] == nov2011CrimeDates[j]){
      counter = counter + 1
      nov2011CrimeDatesResults[i] = (counter)
    }
  }
}
nov2011CrimeDatesResults[is.na(nov2011CrimeDatesResults)] = 0

# dec 2011 Crime data
dec2011Vec = (seq(as.Date("2011-12-01"), as.Date("2011-12-31"), by="+1 day"))
dec2011CrimeDates <- format(as.POSIXct(strptime(DEC2011df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
dec2011CrimeDatesResults = vector(mode="numeric", length=0)
for (i in 1:length(dec2011Vec)) {
  counter = 0
  for (j in 1:length(dec2011CrimeDates)) {
    if(dec2011Vec[i] == dec2011CrimeDates[j]){
      counter = counter + 1
      dec2011CrimeDatesResults[i] = (counter)
    }
  }
}
dec2011CrimeDatesResults[is.na(dec2011CrimeDatesResults)] = 0



t.test(nov2011CrimeDatesResults,dec2011CrimeDatesResults)

totalCrimes = sum(complete.cases(crime$Month))


### Ash Code ###
(data.frame(table(crime$Event.Clearance.Date)))
table(format(crime$Event.Clearance.Date, "%b-%Y"))

install.packages("lubridate")
library(lubridate)

crimeDate = mdy_hms(crime$Event.Clearance.Date)
crime$Months = month(crimeDate)

boxplot(Temp.Avg ~ Month, data=weather)

View(crimeDate$months)

(data.frame(table(crimeMonths)))

crime$Months = factor(crime$Months)
contrasts(crime$Months) = "contr.sum"

t.test(sum(complete.cases(crime)),crime$Months)

totalCrimes = sum(complete.cases(crime$Event.Clearance.Group))

