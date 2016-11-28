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

#######################
### 2011 Crime Data ###
#######################

# aug 2011 Crime data
aug2011Vec = (seq(as.Date("2011-8-01"), as.Date("2011-8-31"), by="+1 day"))
aug2011CrimeDates <- format(as.POSIXct(strptime(AUG2011df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
aug2011CrimeDatesResults = vector(mode="numeric", length=length(aug2011Vec))
for (i in 1:length(aug2011Vec)) {
  counter = 0
  for (j in 1:length(aug2011CrimeDates)) {
    if(aug2011Vec[i] == aug2011CrimeDates[j]){
      counter = counter + 1
      aug2011CrimeDatesResults[i] = (counter)
    }
  }
}

# sep 2011 Crime data
sep2011Vec = (seq(as.Date("2011-9-01"), as.Date("2011-9-30"), by="+1 day"))
sep2011CrimeDates <- format(as.POSIXct(strptime(SEP2011df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
sep2011CrimeDatesResults = vector(mode="numeric", length=length(sep2011Vec))
for (i in 1:length(sep2011Vec)) {
  counter = 0
  for (j in 1:length(sep2011CrimeDates)) {
    if(sep2011Vec[i] == sep2011CrimeDates[j]){
      counter = counter + 1
      sep2011CrimeDatesResults[i] = (counter)
    }
  }
}

# oct 2011 Crime data
oct2011Vec = (seq(as.Date("2011-10-01"), as.Date("2011-10-31"), by="+1 day"))
oct2011CrimeDates <- format(as.POSIXct(strptime(OCT2011df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
oct2011CrimeDatesResults = vector(mode="numeric", length=length(oct2011Vec))
for (i in 1:length(oct2011Vec)) {
  counter = 0
  for (j in 1:length(oct2011CrimeDates)) {
    if(oct2011Vec[i] == oct2011CrimeDates[j]){
      counter = counter + 1
      oct2011CrimeDatesResults[i] = (counter)
    }
  }
}

# nov 2011 Crime data
nov2011Vec = (seq(as.Date("2011-11-01"), as.Date("2011-11-30"), by="+1 day"))
nov2011CrimeDates <- format(as.POSIXct(strptime(NOV2011df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
nov2011CrimeDatesResults = vector(mode="numeric", length=length(nov2011Vec))
for (i in 1:length(nov2011Vec)) {
  counter = 0
  for (j in 1:length(nov2011CrimeDates)) {
    if(nov2011Vec[i] == nov2011CrimeDates[j]){
      counter = counter + 1
      nov2011CrimeDatesResults[i] = (counter)
    }
  }
}

# dec 2011 Crime data
dec2011Vec = (seq(as.Date("2011-12-01"), as.Date("2011-12-31"), by="+1 day"))
dec2011CrimeDates <- format(as.POSIXct(strptime(DEC2011df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
dec2011CrimeDatesResults = vector(mode="numeric", length=length(dec2011Vec))
for (i in 1:length(dec2011Vec)) {
  counter = 0
  for (j in 1:length(dec2011CrimeDates)) {
    if(dec2011Vec[i] == dec2011CrimeDates[j]){
      counter = counter + 1
      dec2011CrimeDatesResults[i] = (counter)
    }
  }
}

#######################
### 2013 Crime Data ###
#######################

# jan 2013 Crime data
jan2013Vec = (seq(as.Date("2013-01-01"), as.Date("2013-01-31"), by="+1 day"))
jan2013CrimeDates <- format(as.POSIXct(strptime(JAN2013df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
jan2013CrimeDatesResults = vector(mode="numeric", length=length(jan2013Vec))
for (i in 1:length(jan2013Vec)) {
  counter = 0
  for (j in 1:length(jan2013CrimeDates)) {
    if(jan2013Vec[i] == jan2013CrimeDates[j]){
      counter = counter + 1
      jan2013CrimeDatesResults[i] = (counter)
    }
  }
}

# feb 2012 Crime data
feb2012Vec = (seq(as.Date("2012-02-01"), as.Date("2012-02-29"), by="+1 day"))
feb2012CrimeDates <- format(as.POSIXct(strptime(FEB2012df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
feb2012CrimeDatesResults = vector(mode="numeric", length=length(feb2012Vec))
for (i in 1:length(feb2012Vec)) {
  counter = 0
  for (j in 1:length(feb2012CrimeDates)) {
    if(feb2012Vec[i] == feb2012CrimeDates[j]){
      counter = counter + 1
      feb2012CrimeDatesResults[i] = (counter)
    }
  }
}

# mar 2012 Crime data
mar2012Vec = (seq(as.Date("2012-03-01"), as.Date("2012-03-31"), by="+1 day"))
mar2012CrimeDates <- format(as.POSIXct(strptime(MAR2012df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
mar2012CrimeDatesResults = vector(mode="numeric", length=length(mar2012Vec))
for (i in 1:length(mar2012Vec)) {
  counter = 0
  for (j in 1:length(mar2012CrimeDates)) {
    if(mar2012Vec[i] == mar2012CrimeDates[j]){
      counter = counter + 1
      mar2012CrimeDatesResults[i] = (counter)
    }
  }
}

# apr 2012 Crime data
apr2012Vec = (seq(as.Date("2012-04-01"), as.Date("2012-04-30"), by="+1 day"))
apr2012CrimeDates <- format(as.POSIXct(strptime(APR2012df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
apr2012CrimeDatesResults = vector(mode="numeric", length=length(apr2012Vec))
for (i in 1:length(apr2012Vec)) {
  counter = 0
  for (j in 1:length(apr2012CrimeDates)) {
    if(apr2012Vec[i] == apr2012CrimeDates[j]){
      counter = counter + 1
      apr2012CrimeDatesResults[i] = (counter)
    }
  }
}

# may 2012 Crime data
may2012Vec = (seq(as.Date("2012-05-01"), as.Date("2012-05-31"), by="+1 day"))
may2012CrimeDates <- format(as.POSIXct(strptime(MAY2012df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
may2012CrimeDatesResults = vector(mode="numeric", length=length(may2012Vec))
for (i in 1:length(may2012Vec)) {
  counter = 0
  for (j in 1:length(may2012CrimeDates)) {
    if(may2012Vec[i] == may2012CrimeDates[j]){
      counter = counter + 1
      may2012CrimeDatesResults[i] = (counter)
    }
  }
}

# jun 2012 Crime data
jun2012Vec = (seq(as.Date("2012-06-01"), as.Date("2012-06-30"), by="+1 day"))
jun2012CrimeDates <- format(as.POSIXct(strptime(JUN2012df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
jun2012CrimeDatesResults = vector(mode="numeric", length=length(jun2012Vec))
for (i in 1:length(jun2012Vec)) {
  counter = 0
  for (j in 1:length(jun2012CrimeDates)) {
    if(jun2012Vec[i] == jun2012CrimeDates[j]){
      counter = counter + 1
      jun2012CrimeDatesResults[i] = (counter)
    }
  }
}

# jly 2012 Crime data
jly2012Vec = (seq(as.Date("2012-07-01"), as.Date("2012-07-31"), by="+1 day"))
jly2012CrimeDates <- format(as.POSIXct(strptime(JLY2012df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
jly2012CrimeDatesResults = vector(mode="numeric", length=length(jly2012Vec))
for (i in 1:length(jly2012Vec)) {
  counter = 0
  for (j in 1:length(jly2012CrimeDates)) {
    if(jly2012Vec[i] == jly2012CrimeDates[j]){
      counter = counter + 1
      jly2012CrimeDatesResults[i] = (counter)
    }
  }
}

# aug 2012 Crime data
aug2012Vec = (seq(as.Date("2012-8-01"), as.Date("2012-8-31"), by="+1 day"))
aug2012CrimeDates <- format(as.POSIXct(strptime(AUG2012df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
aug2012CrimeDatesResults = vector(mode="numeric", length=length(aug2012Vec))
for (i in 1:length(aug2012Vec)) {
  counter = 0
  for (j in 1:length(aug2012CrimeDates)) {
    if(aug2012Vec[i] == aug2012CrimeDates[j]){
      counter = counter + 1
      aug2012CrimeDatesResults[i] = (counter)
    }
  }
}

# sep 2012 Crime data
sep2012Vec = (seq(as.Date("2012-9-01"), as.Date("2012-9-30"), by="+1 day"))
sep2012CrimeDates <- format(as.POSIXct(strptime(SEP2012df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
sep2012CrimeDatesResults = vector(mode="numeric", length=length(sep2012Vec))
for (i in 1:length(sep2012Vec)) {
  counter = 0
  for (j in 1:length(sep2012CrimeDates)) {
    if(sep2012Vec[i] == sep2012CrimeDates[j]){
      counter = counter + 1
      sep2012CrimeDatesResults[i] = (counter)
    }
  }
}

# oct 2012 Crime data
oct2012Vec = (seq(as.Date("2012-10-01"), as.Date("2012-10-31"), by="+1 day"))
oct2012CrimeDates <- format(as.POSIXct(strptime(OCT2012df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
oct2012CrimeDatesResults = vector(mode="numeric", length=length(oct2012Vec))
for (i in 1:length(oct2012Vec)) {
  counter = 0
  for (j in 1:length(oct2012CrimeDates)) {
    if(oct2012Vec[i] == oct2012CrimeDates[j]){
      counter = counter + 1
      oct2012CrimeDatesResults[i] = (counter)
    }
  }
}

# nov 2012 Crime data
nov2012Vec = (seq(as.Date("2012-11-01"), as.Date("2012-11-30"), by="+1 day"))
nov2012CrimeDates <- format(as.POSIXct(strptime(NOV2012df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
nov2012CrimeDatesResults = vector(mode="numeric", length=length(nov2012Vec))
for (i in 1:length(nov2012Vec)) {
  counter = 0
  for (j in 1:length(nov2012CrimeDates)) {
    if(nov2012Vec[i] == nov2012CrimeDates[j]){
      counter = counter + 1
      nov2012CrimeDatesResults[i] = (counter)
    }
  }
}

# dec 2012 Crime data
dec2012Vec = (seq(as.Date("2012-12-01"), as.Date("2012-12-31"), by="+1 day"))
dec2012CrimeDates <- format(as.POSIXct(strptime(DEC2012df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
dec2012CrimeDatesResults = vector(mode="numeric", length=length(dec2012Vec))
for (i in 1:length(dec2012Vec)) {
  counter = 0
  for (j in 1:length(dec2012CrimeDates)) {
    if(dec2012Vec[i] == dec2012CrimeDates[j]){
      counter = counter + 1
      dec2012CrimeDatesResults[i] = (counter)
    }
  }
}


#######################
### 2012 Crime Data ###
#######################

# jan 2012 Crime data
jan2012Vec = (seq(as.Date("2012-01-01"), as.Date("2012-01-31"), by="+1 day"))
jan2012CrimeDates <- format(as.POSIXct(strptime(JAN2012df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
jan2012CrimeDatesResults = vector(mode="numeric", length=length(jan2012Vec))
for (i in 1:length(jan2012Vec)) {
  counter = 0
  for (j in 1:length(jan2012CrimeDates)) {
    if(jan2012Vec[i] == jan2012CrimeDates[j]){
      counter = counter + 1
      jan2012CrimeDatesResults[i] = (counter)
    }
  }
}

# feb 2012 Crime data
feb2012Vec = (seq(as.Date("2012-02-01"), as.Date("2012-02-29"), by="+1 day"))
feb2012CrimeDates <- format(as.POSIXct(strptime(FEB2012df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
feb2012CrimeDatesResults = vector(mode="numeric", length=length(feb2012Vec))
for (i in 1:length(feb2012Vec)) {
  counter = 0
  for (j in 1:length(feb2012CrimeDates)) {
    if(feb2012Vec[i] == feb2012CrimeDates[j]){
      counter = counter + 1
      feb2012CrimeDatesResults[i] = (counter)
    }
  }
}

# mar 2012 Crime data
mar2012Vec = (seq(as.Date("2012-03-01"), as.Date("2012-03-31"), by="+1 day"))
mar2012CrimeDates <- format(as.POSIXct(strptime(MAR2012df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
mar2012CrimeDatesResults = vector(mode="numeric", length=length(mar2012Vec))
for (i in 1:length(mar2012Vec)) {
  counter = 0
  for (j in 1:length(mar2012CrimeDates)) {
    if(mar2012Vec[i] == mar2012CrimeDates[j]){
      counter = counter + 1
      mar2012CrimeDatesResults[i] = (counter)
    }
  }
}

# apr 2012 Crime data
apr2012Vec = (seq(as.Date("2012-04-01"), as.Date("2012-04-30"), by="+1 day"))
apr2012CrimeDates <- format(as.POSIXct(strptime(APR2012df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
apr2012CrimeDatesResults = vector(mode="numeric", length=length(apr2012Vec))
for (i in 1:length(apr2012Vec)) {
  counter = 0
  for (j in 1:length(apr2012CrimeDates)) {
    if(apr2012Vec[i] == apr2012CrimeDates[j]){
      counter = counter + 1
      apr2012CrimeDatesResults[i] = (counter)
    }
  }
}

# may 2012 Crime data
may2012Vec = (seq(as.Date("2012-05-01"), as.Date("2012-05-31"), by="+1 day"))
may2012CrimeDates <- format(as.POSIXct(strptime(MAY2012df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
may2012CrimeDatesResults = vector(mode="numeric", length=length(may2012Vec))
for (i in 1:length(may2012Vec)) {
  counter = 0
  for (j in 1:length(may2012CrimeDates)) {
    if(may2012Vec[i] == may2012CrimeDates[j]){
      counter = counter + 1
      may2012CrimeDatesResults[i] = (counter)
    }
  }
}

# jun 2012 Crime data
jun2012Vec = (seq(as.Date("2012-06-01"), as.Date("2012-06-30"), by="+1 day"))
jun2012CrimeDates <- format(as.POSIXct(strptime(JUN2012df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
jun2012CrimeDatesResults = vector(mode="numeric", length=length(jun2012Vec))
for (i in 1:length(jun2012Vec)) {
  counter = 0
  for (j in 1:length(jun2012CrimeDates)) {
    if(jun2012Vec[i] == jun2012CrimeDates[j]){
      counter = counter + 1
      jun2012CrimeDatesResults[i] = (counter)
    }
  }
}

# jly 2012 Crime data
jly2012Vec = (seq(as.Date("2012-07-01"), as.Date("2012-07-31"), by="+1 day"))
jly2012CrimeDates <- format(as.POSIXct(strptime(JLY2012df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
jly2012CrimeDatesResults = vector(mode="numeric", length=length(jly2012Vec))
for (i in 1:length(jly2012Vec)) {
  counter = 0
  for (j in 1:length(jly2012CrimeDates)) {
    if(jly2012Vec[i] == jly2012CrimeDates[j]){
      counter = counter + 1
      jly2012CrimeDatesResults[i] = (counter)
    }
  }
}

# aug 2012 Crime data
aug2012Vec = (seq(as.Date("2012-8-01"), as.Date("2012-8-31"), by="+1 day"))
aug2012CrimeDates <- format(as.POSIXct(strptime(AUG2012df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
aug2012CrimeDatesResults = vector(mode="numeric", length=length(aug2012Vec))
for (i in 1:length(aug2012Vec)) {
  counter = 0
  for (j in 1:length(aug2012CrimeDates)) {
    if(aug2012Vec[i] == aug2012CrimeDates[j]){
      counter = counter + 1
      aug2012CrimeDatesResults[i] = (counter)
    }
  }
}

# sep 2012 Crime data
sep2012Vec = (seq(as.Date("2012-9-01"), as.Date("2012-9-30"), by="+1 day"))
sep2012CrimeDates <- format(as.POSIXct(strptime(SEP2012df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
sep2012CrimeDatesResults = vector(mode="numeric", length=length(sep2012Vec))
for (i in 1:length(sep2012Vec)) {
  counter = 0
  for (j in 1:length(sep2012CrimeDates)) {
    if(sep2012Vec[i] == sep2012CrimeDates[j]){
      counter = counter + 1
      sep2012CrimeDatesResults[i] = (counter)
    }
  }
}

# oct 2012 Crime data
oct2012Vec = (seq(as.Date("2012-10-01"), as.Date("2012-10-31"), by="+1 day"))
oct2012CrimeDates <- format(as.POSIXct(strptime(OCT2012df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
oct2012CrimeDatesResults = vector(mode="numeric", length=length(oct2012Vec))
for (i in 1:length(oct2012Vec)) {
  counter = 0
  for (j in 1:length(oct2012CrimeDates)) {
    if(oct2012Vec[i] == oct2012CrimeDates[j]){
      counter = counter + 1
      oct2012CrimeDatesResults[i] = (counter)
    }
  }
}

# nov 2012 Crime data
nov2012Vec = (seq(as.Date("2012-11-01"), as.Date("2012-11-30"), by="+1 day"))
nov2012CrimeDates <- format(as.POSIXct(strptime(NOV2012df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
nov2012CrimeDatesResults = vector(mode="numeric", length=length(nov2012Vec))
for (i in 1:length(nov2012Vec)) {
  counter = 0
  for (j in 1:length(nov2012CrimeDates)) {
    if(nov2012Vec[i] == nov2012CrimeDates[j]){
      counter = counter + 1
      nov2012CrimeDatesResults[i] = (counter)
    }
  }
}

# dec 2012 Crime data
dec2012Vec = (seq(as.Date("2012-12-01"), as.Date("2012-12-31"), by="+1 day"))
dec2012CrimeDates <- format(as.POSIXct(strptime(DEC2012df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
dec2012CrimeDatesResults = vector(mode="numeric", length=length(dec2012Vec))
for (i in 1:length(dec2012Vec)) {
  counter = 0
  for (j in 1:length(dec2012CrimeDates)) {
    if(dec2012Vec[i] == dec2012CrimeDates[j]){
      counter = counter + 1
      dec2012CrimeDatesResults[i] = (counter)
    }
  }
}



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

