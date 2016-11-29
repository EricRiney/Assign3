# Ashlyn and Eric 
# INFO 370

library(MASS) # need fractions
require(zoo)  # date stuff
library(lubridate)
require(ggplot2)
install.packages( "mass", "zoo", "lubridate", "ggplot2", "reshape2", "plyr", "languageR",
                 "lme4", "psych"
                 )
setwd("/Users/ericriner/Documents/Code/UW/370/Assign3")

weather = read.csv(file.choose(), header = TRUE)
crime = read.csv(file.choose(), header = TRUE)

summary(weather)
summary(crime)

#GOAL: FIND OUT IF THERE IS A CORRELATION BETWEEN WEATHER AND CRIME RATES

hist(weather$AvgTemp....F.)

#############################
### 2011 Crime Dataframes ###
#############################

crime2011df = crime[crime$Year == '2011',]
# Monthly Crime Dataframes for 2011
AUG2011df = crime[(crime$Month == '8' & crime$Year== '2011'),]
SEP2011df = crime[(crime$Month == "9" & crime$Year == "2011"),]
OCT2011df = crime[(crime$Month == "10" & crime$Year == "2011"),]
NOV2011df = crime[(crime$Month == "11" & crime$Year == "2011"),]
DEC2011df = crime[(crime$Month == "12" & crime$Year == "2011"),]

#############################
### 2012 Crime Dataframes ###
#############################

crime2012df = crime[crime$Year == '2012',]
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


burglaryCrime2012Vec = (seq(as.Date("2012-1-01"), as.Date("2012-12-31"), by="+1 day"))
burglaryCrime2012Results = vector(mode="numeric", length=length(burglaryCrime2012Vec))
for (i in 1:length(burglaryCrime2012Vec)) {
  #counter = 0
  #for (j in 1:length(burglaryCrime2012Vec)) {
    if(crime$Summarized.Offense.Description[i] == 'BURGLARY'){
      #counter = counter + 1
      burglaryCrime2012Results[i] = 1
   # }
  }
}

boxplot(weather2012DatesResults, burglaryCrime2012Results)

#############################
### 2013 Crime Dataframes ###
#############################

crime2013df = crime[crime$Year == '2013',]
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

#############################
### 2014 Crime Dataframes ###
#############################

crime2014df = crime[crime$Year == '2014',]
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

#############################
### 2015 Crime Dataframes ###
#############################

crime2015df = crime[crime$Year == '2015',]
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

#############################
### 2016 Crime Dataframes ###
#############################

crime2016df = crime[crime$Year == '2016',]
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

# feb 2013 Crime data
feb2013Vec = (seq(as.Date("2013-02-01"), as.Date("2013-02-28"), by="+1 day"))
feb2013CrimeDates <- format(as.POSIXct(strptime(FEB2013df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
feb2013CrimeDatesResults = vector(mode="numeric", length=length(feb2013Vec))
for (i in 1:length(feb2013Vec)) {
  counter = 0
  for (j in 1:length(feb2013CrimeDates)) {
    if(feb2013Vec[i] == feb2013CrimeDates[j]){
      counter = counter + 1
      feb2013CrimeDatesResults[i] = (counter)
    }
  }
}

# mar 2013 Crime data
mar2013Vec = (seq(as.Date("2013-03-01"), as.Date("2013-03-31"), by="+1 day"))
mar2013CrimeDates <- format(as.POSIXct(strptime(MAR2013df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
mar2013CrimeDatesResults = vector(mode="numeric", length=length(mar2013Vec))
for (i in 1:length(mar2013Vec)) {
  counter = 0
  for (j in 1:length(mar2013CrimeDates)) {
    if(mar2013Vec[i] == mar2013CrimeDates[j]){
      counter = counter + 1
      mar2013CrimeDatesResults[i] = (counter)
    }
  }
}

# apr 2013 Crime data
apr2013Vec = (seq(as.Date("2013-04-01"), as.Date("2013-04-30"), by="+1 day"))
apr2013CrimeDates <- format(as.POSIXct(strptime(APR2013df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
apr2013CrimeDatesResults = vector(mode="numeric", length=length(apr2013Vec))
for (i in 1:length(apr2013Vec)) {
  counter = 0
  for (j in 1:length(apr2013CrimeDates)) {
    if(apr2013Vec[i] == apr2013CrimeDates[j]){
      counter = counter + 1
      apr2013CrimeDatesResults[i] = (counter)
    }
  }
}

# may 2013 Crime data
may2013Vec = (seq(as.Date("2013-05-01"), as.Date("2013-05-31"), by="+1 day"))
may2013CrimeDates <- format(as.POSIXct(strptime(MAY2013df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
may2013CrimeDatesResults = vector(mode="numeric", length=length(may2013Vec))
for (i in 1:length(may2013Vec)) {
  counter = 0
  for (j in 1:length(may2013CrimeDates)) {
    if(may2013Vec[i] == may2013CrimeDates[j]){
      counter = counter + 1
      may2013CrimeDatesResults[i] = (counter)
    }
  }
}

# jun 2013 Crime data
jun2013Vec = (seq(as.Date("2013-06-01"), as.Date("2013-06-30"), by="+1 day"))
jun2013CrimeDates <- format(as.POSIXct(strptime(JUN2013df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
jun2013CrimeDatesResults = vector(mode="numeric", length=length(jun2013Vec))
for (i in 1:length(jun2013Vec)) {
  counter = 0
  for (j in 1:length(jun2013CrimeDates)) {
    if(jun2013Vec[i] == jun2013CrimeDates[j]){
      counter = counter + 1
      jun2013CrimeDatesResults[i] = (counter)
    }
  }
}

# jly 2013 Crime data
jly2013Vec = (seq(as.Date("2013-07-01"), as.Date("2013-07-31"), by="+1 day"))
jly2013CrimeDates <- format(as.POSIXct(strptime(JLY2013df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
jly2013CrimeDatesResults = vector(mode="numeric", length=length(jly2013Vec))
for (i in 1:length(jly2013Vec)) {
  counter = 0
  for (j in 1:length(jly2013CrimeDates)) {
    if(jly2013Vec[i] == jly2013CrimeDates[j]){
      counter = counter + 1
      jly2013CrimeDatesResults[i] = (counter)
    }
  }
}

# aug 2013 Crime data
aug2013Vec = (seq(as.Date("2013-8-01"), as.Date("2013-8-31"), by="+1 day"))
aug2013CrimeDates <- format(as.POSIXct(strptime(AUG2013df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
aug2013CrimeDatesResults = vector(mode="numeric", length=length(aug2013Vec))
for (i in 1:length(aug2013Vec)) {
  counter = 0
  for (j in 1:length(aug2013CrimeDates)) {
    if(aug2013Vec[i] == aug2013CrimeDates[j]){
      counter = counter + 1
      aug2013CrimeDatesResults[i] = (counter)
    }
  }
}

# sep 2013 Crime data
sep2013Vec = (seq(as.Date("2013-9-01"), as.Date("2013-9-30"), by="+1 day"))
sep2013CrimeDates <- format(as.POSIXct(strptime(SEP2013df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
sep2013CrimeDatesResults = vector(mode="numeric", length=length(sep2013Vec))
for (i in 1:length(sep2013Vec)) {
  counter = 0
  for (j in 1:length(sep2013CrimeDates)) {
    if(sep2013Vec[i] == sep2013CrimeDates[j]){
      counter = counter + 1
      sep2013CrimeDatesResults[i] = (counter)
    }
  }
}

# oct 2013 Crime data
oct2013Vec = (seq(as.Date("2013-10-01"), as.Date("2013-10-31"), by="+1 day"))
oct2013CrimeDates <- format(as.POSIXct(strptime(OCT2013df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
oct2013CrimeDatesResults = vector(mode="numeric", length=length(oct2012Vec))
for (i in 1:length(oct2013Vec)) {
  counter = 0
  for (j in 1:length(oct2013CrimeDates)) {
    if(oct2013Vec[i] == oct2013CrimeDates[j]){
      counter = counter + 1
      oct2013CrimeDatesResults[i] = (counter)
    }
  }
}

# nov 2013 Crime data
nov2013Vec = (seq(as.Date("2013-11-01"), as.Date("2013-11-30"), by="+1 day"))
nov2013CrimeDates <- format(as.POSIXct(strptime(NOV2013df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
nov2013CrimeDatesResults = vector(mode="numeric", length=length(nov2013Vec))
for (i in 1:length(nov2013Vec)) {
  counter = 0
  for (j in 1:length(nov2013CrimeDates)) {
    if(nov2013Vec[i] == nov2013CrimeDates[j]){
      counter = counter + 1
      nov2013CrimeDatesResults[i] = (counter)
    }
  }
}

# dec 2013 Crime data
dec2013Vec = (seq(as.Date("2013-12-01"), as.Date("2013-12-31"), by="+1 day"))
dec2013CrimeDates <- format(as.POSIXct(strptime(DEC2013df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
dec2013CrimeDatesResults = vector(mode="numeric", length=length(dec2013Vec))
for (i in 1:length(dec2013Vec)) {
  counter = 0
  for (j in 1:length(dec2013CrimeDates)) {
    if(dec2013Vec[i] == dec2013CrimeDates[j]){
      counter = counter + 1
      dec2013CrimeDatesResults[i] = (counter)
    }
  }
}


#######################
### 2014 Crime Data ###
#######################

# jan 2014 Crime data
jan2014Vec = (seq(as.Date("2014-01-01"), as.Date("2014-01-31"), by="+1 day"))
jan2014CrimeDates <- format(as.POSIXct(strptime(JAN2014df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
jan2014CrimeDatesResults = vector(mode="numeric", length=length(jan2014Vec))
for (i in 1:length(jan2014Vec)) {
  counter = 0
  for (j in 1:length(jan2014CrimeDates)) {
    if(jan2014Vec[i] == jan2014CrimeDates[j]){
      counter = counter + 1
      jan2014CrimeDatesResults[i] = (counter)
    }
  }
}

# feb 2014 Crime data
feb2014Vec = (seq(as.Date("2014-02-01"), as.Date("2014-02-28"), by="+1 day"))
feb2014CrimeDates <- format(as.POSIXct(strptime(FEB2014df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
feb2014CrimeDatesResults = vector(mode="numeric", length=length(feb2014Vec))
for (i in 1:length(feb2014Vec)) {
  counter = 0
  for (j in 1:length(feb2014CrimeDates)) {
    if(feb2014Vec[i] == feb2014CrimeDates[j]){
      counter = counter + 1
      feb2014CrimeDatesResults[i] = (counter)
    }
  }
}

# mar 2014 Crime data
mar2014Vec = (seq(as.Date("2014-03-01"), as.Date("2014-03-31"), by="+1 day"))
mar2014CrimeDates <- format(as.POSIXct(strptime(MAR2014df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
mar2014CrimeDatesResults = vector(mode="numeric", length=length(mar2014Vec))
for (i in 1:length(mar2014Vec)) {
  counter = 0
  for (j in 1:length(mar2014CrimeDates)) {
    if(mar2014Vec[i] == mar2014CrimeDates[j]){
      counter = counter + 1
      mar2014CrimeDatesResults[i] = (counter)
    }
  }
}

# apr 2014 Crime data
apr2014Vec = (seq(as.Date("2014-04-01"), as.Date("2014-04-30"), by="+1 day"))
apr2014CrimeDates <- format(as.POSIXct(strptime(APR2014df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
apr2014CrimeDatesResults = vector(mode="numeric", length=length(apr2014Vec))
for (i in 1:length(apr2014Vec)) {
  counter = 0
  for (j in 1:length(apr2014CrimeDates)) {
    if(apr2014Vec[i] == apr2014CrimeDates[j]){
      counter = counter + 1
      apr2014CrimeDatesResults[i] = (counter)
    }
  }
}

# may 2014 Crime data
may2014Vec = (seq(as.Date("2014-05-01"), as.Date("2014-05-31"), by="+1 day"))
may2014CrimeDates <- format(as.POSIXct(strptime(MAY2014df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
may2014CrimeDatesResults = vector(mode="numeric", length=length(may2014Vec))
for (i in 1:length(may2014Vec)) {
  counter = 0
  for (j in 1:length(may2014CrimeDates)) {
    if(may2014Vec[i] == may2014CrimeDates[j]){
      counter = counter + 1
      may2014CrimeDatesResults[i] = (counter)
    }
  }
}

# jun 2014 Crime data
jun2014Vec = (seq(as.Date("2014-06-01"), as.Date("2014-06-30"), by="+1 day"))
jun2014CrimeDates <- format(as.POSIXct(strptime(JUN2014df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
jun2014CrimeDatesResults = vector(mode="numeric", length=length(jun2014Vec))
for (i in 1:length(jun2014Vec)) {
  counter = 0
  for (j in 1:length(jun2014CrimeDates)) {
    if(jun2014Vec[i] == jun2014CrimeDates[j]){
      counter = counter + 1
      jun2014CrimeDatesResults[i] = (counter)
    }
  }
}

# jly 2014 Crime data
jly2014Vec = (seq(as.Date("2014-07-01"), as.Date("2014-07-31"), by="+1 day"))
jly2014CrimeDates <- format(as.POSIXct(strptime(JLY2014df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
jly2014CrimeDatesResults = vector(mode="numeric", length=length(jly2014Vec))
for (i in 1:length(jly2014Vec)) {
  counter = 0
  for (j in 1:length(jly2014CrimeDates)) {
    if(jly2014Vec[i] == jly2014CrimeDates[j]){
      counter = counter + 1
      jly2014CrimeDatesResults[i] = (counter)
    }
  }
}

# aug 2014 Crime data
aug2014Vec = (seq(as.Date("2014-8-01"), as.Date("2014-8-31"), by="+1 day"))
aug2014CrimeDates <- format(as.POSIXct(strptime(AUG2014df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
aug2014CrimeDatesResults = vector(mode="numeric", length=length(aug2014Vec))
for (i in 1:length(aug2014Vec)) {
  counter = 0
  for (j in 1:length(aug2014CrimeDates)) {
    if(aug2014Vec[i] == aug2014CrimeDates[j]){
      counter = counter + 1
      aug2014CrimeDatesResults[i] = (counter)
    }
  }
}

# sep 2014 Crime data
sep2014Vec = (seq(as.Date("2014-9-01"), as.Date("2014-9-30"), by="+1 day"))
sep2014CrimeDates <- format(as.POSIXct(strptime(SEP2014df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
sep2014CrimeDatesResults = vector(mode="numeric", length=length(sep2014Vec))
for (i in 1:length(sep2014Vec)) {
  counter = 0
  for (j in 1:length(sep2014CrimeDates)) {
    if(sep2014Vec[i] == sep2014CrimeDates[j]){
      counter = counter + 1
      sep2014CrimeDatesResults[i] = (counter)
    }
  }
}

# oct 2014 Crime data
oct2014Vec = (seq(as.Date("2014-10-01"), as.Date("2014-10-31"), by="+1 day"))
oct2014CrimeDates <- format(as.POSIXct(strptime(OCT2014df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
oct2014CrimeDatesResults = vector(mode="numeric", length=length(oct2014Vec))
for (i in 1:length(oct2014Vec)) {
  counter = 0
  for (j in 1:length(oct2014CrimeDates)) {
    if(oct2014Vec[i] == oct2014CrimeDates[j]){
      counter = counter + 1
      oct2014CrimeDatesResults[i] = (counter)
    }
  }
}

# nov 2014 Crime data
nov2014Vec = (seq(as.Date("2014-11-01"), as.Date("2014-11-30"), by="+1 day"))
nov2014CrimeDates <- format(as.POSIXct(strptime(NOV2014df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
nov2014CrimeDatesResults = vector(mode="numeric", length=length(nov2014Vec))
for (i in 1:length(nov2014Vec)) {
  counter = 0
  for (j in 1:length(nov2014CrimeDates)) {
    if(nov2014Vec[i] == nov2014CrimeDates[j]){
      counter = counter + 1
      nov2014CrimeDatesResults[i] = (counter)
    }
  }
}

# dec 2014 Crime data
dec2014Vec = (seq(as.Date("2014-12-01"), as.Date("2014-12-31"), by="+1 day"))
dec2014CrimeDates <- format(as.POSIXct(strptime(DEC2014df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
dec2014CrimeDatesResults = vector(mode="numeric", length=length(dec2014Vec))
for (i in 1:length(dec2014Vec)) {
  counter = 0
  for (j in 1:length(dec2014CrimeDates)) {
    if(dec2014Vec[i] == dec2014CrimeDates[j]){
      counter = counter + 1
      dec2014CrimeDatesResults[i] = (counter)
    }
  }
}

#######################
### 2015 Crime Data ###
#######################

# jan 2015 Crime data
jan2015Vec = (seq(as.Date("2015-01-01"), as.Date("2015-01-31"), by="+1 day"))
jan2015CrimeDates <- format(as.POSIXct(strptime(JAN2015df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
jan2015CrimeDatesResults = vector(mode="numeric", length=length(jan2015Vec))
for (i in 1:length(jan2015Vec)) {
  counter = 0
  for (j in 1:length(jan2015CrimeDates)) {
    if(jan2015Vec[i] == jan2015CrimeDates[j]){
      counter = counter + 1
      jan2015CrimeDatesResults[i] = (counter)
    }
  }
}

# feb 2015 Crime data
feb2015Vec = (seq(as.Date("2015-02-01"), as.Date("2015-02-28"), by="+1 day"))
feb2015CrimeDates <- format(as.POSIXct(strptime(FEB2015df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
feb2015CrimeDatesResults = vector(mode="numeric", length=length(feb2015Vec))
for (i in 1:length(feb2015Vec)) {
  counter = 0
  for (j in 1:length(feb2015CrimeDates)) {
    if(feb2015Vec[i] == feb2015CrimeDates[j]){
      counter = counter + 1
      feb2015CrimeDatesResults[i] = (counter)
    }
  }
}

# mar 2015 Crime data
mar2015Vec = (seq(as.Date("2015-03-01"), as.Date("2015-03-31"), by="+1 day"))
mar2015CrimeDates <- format(as.POSIXct(strptime(MAR2015df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
mar2015CrimeDatesResults = vector(mode="numeric", length=length(mar2015Vec))
for (i in 1:length(mar2015Vec)) {
  counter = 0
  for (j in 1:length(mar2015CrimeDates)) {
    if(mar2015Vec[i] == mar2015CrimeDates[j]){
      counter = counter + 1
      mar2015CrimeDatesResults[i] = (counter)
    }
  }
}

# apr 2015 Crime data
apr2015Vec = (seq(as.Date("2015-04-01"), as.Date("2015-04-30"), by="+1 day"))
apr2015CrimeDates <- format(as.POSIXct(strptime(APR2015df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
apr2015CrimeDatesResults = vector(mode="numeric", length=length(apr2015Vec))
for (i in 1:length(apr2015Vec)) {
  counter = 0
  for (j in 1:length(apr2015CrimeDates)) {
    if(apr2015Vec[i] == apr2015CrimeDates[j]){
      counter = counter + 1
      apr2015CrimeDatesResults[i] = (counter)
    }
  }
}

# may 2015 Crime data
may2015Vec = (seq(as.Date("2015-05-01"), as.Date("2015-05-31"), by="+1 day"))
may2015CrimeDates <- format(as.POSIXct(strptime(MAY2015df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
may2015CrimeDatesResults = vector(mode="numeric", length=length(may2015Vec))
for (i in 1:length(may2015Vec)) {
  counter = 0
  for (j in 1:length(may2015CrimeDates)) {
    if(may2015Vec[i] == may2015CrimeDates[j]){
      counter = counter + 1
      may2015CrimeDatesResults[i] = (counter)
    }
  }
}

# jun 2015 Crime data
jun2015Vec = (seq(as.Date("2015-06-01"), as.Date("2015-06-30"), by="+1 day"))
jun2015CrimeDates <- format(as.POSIXct(strptime(JUN2015df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
jun2015CrimeDatesResults = vector(mode="numeric", length=length(jun2015Vec))
for (i in 1:length(jun2015Vec)) {
  counter = 0
  for (j in 1:length(jun2015CrimeDates)) {
    if(jun2015Vec[i] == jun2015CrimeDates[j]){
      counter = counter + 1
      jun2015CrimeDatesResults[i] = (counter)
    }
  }
}

# jly 2015 Crime data
jly2015Vec = (seq(as.Date("2015-07-01"), as.Date("2015-07-31"), by="+1 day"))
jly2015CrimeDates <- format(as.POSIXct(strptime(JLY2015df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
jly2015CrimeDatesResults = vector(mode="numeric", length=length(jly2015Vec))
for (i in 1:length(jly2015Vec)) {
  counter = 0
  for (j in 1:length(jly2015CrimeDates)) {
    if(jly2015Vec[i] == jly2015CrimeDates[j]){
      counter = counter + 1
      jly2015CrimeDatesResults[i] = (counter)
    }
  }
}

# aug 2015 Crime data
aug2015Vec = (seq(as.Date("2015-8-01"), as.Date("2015-8-31"), by="+1 day"))
aug2015CrimeDates <- format(as.POSIXct(strptime(AUG2015df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
aug2015CrimeDatesResults = vector(mode="numeric", length=length(aug2015Vec))
for (i in 1:length(aug2015Vec)) {
  counter = 0
  for (j in 1:length(aug2015CrimeDates)) {
    if(aug2015Vec[i] == aug2015CrimeDates[j]){
      counter = counter + 1
      aug2015CrimeDatesResults[i] = (counter)
    }
  }
}

# sep 2015 Crime data
sep2015Vec = (seq(as.Date("2015-9-01"), as.Date("2015-9-30"), by="+1 day"))
sep2015CrimeDates <- format(as.POSIXct(strptime(SEP2015df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
sep2015CrimeDatesResults = vector(mode="numeric", length=length(sep2015Vec))
for (i in 1:length(sep2015Vec)) {
  counter = 0
  for (j in 1:length(sep2015CrimeDates)) {
    if(sep2015Vec[i] == sep2015CrimeDates[j]){
      counter = counter + 1
      sep2015CrimeDatesResults[i] = (counter)
    }
  }
}

# oct 2015 Crime data
oct2015Vec = (seq(as.Date("2015-10-01"), as.Date("2015-10-31"), by="+1 day"))
oct2015CrimeDates <- format(as.POSIXct(strptime(OCT2015df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
oct2015CrimeDatesResults = vector(mode="numeric", length=length(oct2015Vec))
for (i in 1:length(oct2015Vec)) {
  counter = 0
  for (j in 1:length(oct2015CrimeDates)) {
    if(oct2015Vec[i] == oct2015CrimeDates[j]){
      counter = counter + 1
      oct2015CrimeDatesResults[i] = (counter)
    }
  }
}

# nov 2015 Crime data
nov2015Vec = (seq(as.Date("2015-11-01"), as.Date("2015-11-30"), by="+1 day"))
nov2015CrimeDates <- format(as.POSIXct(strptime(NOV2015df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
nov2015CrimeDatesResults = vector(mode="numeric", length=length(nov2015Vec))
for (i in 1:length(nov2015Vec)) {
  counter = 0
  for (j in 1:length(nov2015CrimeDates)) {
    if(nov2015Vec[i] == nov2015CrimeDates[j]){
      counter = counter + 1
      nov2015CrimeDatesResults[i] = (counter)
    }
  }
}

# dec 2015 Crime data
dec2015Vec = (seq(as.Date("2015-12-01"), as.Date("2015-12-31"), by="+1 day"))
dec2015CrimeDates <- format(as.POSIXct(strptime(DEC2015df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
dec2015CrimeDatesResults = vector(mode="numeric", length=length(dec2015Vec))
for (i in 1:length(dec2015Vec)) {
  counter = 0
  for (j in 1:length(dec2015CrimeDates)) {
    if(dec2015Vec[i] == dec2015CrimeDates[j]){
      counter = counter + 1
      dec2015CrimeDatesResults[i] = (counter)
    }
  }
}


#######################
### 2016 Crime Data ###
#######################

# jan 2016 Crime data
jan2016Vec = (seq(as.Date("2016-01-01"), as.Date("2016-01-31"), by="+1 day"))
jan2016CrimeDates <- format(as.POSIXct(strptime(JAN2016df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
jan2016CrimeDatesResults = vector(mode="numeric", length=length(jan2016Vec))
for (i in 1:length(jan2016Vec)) {
  counter = 0
  for (j in 1:length(jan2016CrimeDates)) {
    if(jan2016Vec[i] == jan2016CrimeDates[j]){
      counter = counter + 1
      jan2016CrimeDatesResults[i] = (counter)
    }
  }
}

# feb 2016 Crime data
feb2016Vec = (seq(as.Date("2016-02-01"), as.Date("2016-02-28"), by="+1 day"))
feb2016CrimeDates <- format(as.POSIXct(strptime(FEB2016df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
feb2016CrimeDatesResults = vector(mode="numeric", length=length(feb2016Vec))
for (i in 1:length(feb2016Vec)) {
  counter = 0
  for (j in 1:length(feb2016CrimeDates)) {
    if(feb2016Vec[i] == feb2016CrimeDates[j]){
      counter = counter + 1
      feb2016CrimeDatesResults[i] = (counter)
    }
  }
}

# mar 2016 Crime data
mar2016Vec = (seq(as.Date("2016-03-01"), as.Date("2016-03-31"), by="+1 day"))
mar2016CrimeDates <- format(as.POSIXct(strptime(MAR2016df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
mar2016CrimeDatesResults = vector(mode="numeric", length=length(mar2016Vec))
for (i in 1:length(mar2016Vec)) {
  counter = 0
  for (j in 1:length(mar2016CrimeDates)) {
    if(mar2016Vec[i] == mar2016CrimeDates[j]){
      counter = counter + 1
      mar2016CrimeDatesResults[i] = (counter)
    }
  }
}

# apr 2016 Crime data
apr2016Vec = (seq(as.Date("2016-04-01"), as.Date("2016-04-30"), by="+1 day"))
apr2016CrimeDates <- format(as.POSIXct(strptime(APR2016df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
apr2016CrimeDatesResults = vector(mode="numeric", length=length(apr2016Vec))
for (i in 1:length(apr2016Vec)) {
  counter = 0
  for (j in 1:length(apr2016CrimeDates)) {
    if(apr2016Vec[i] == apr2016CrimeDates[j]){
      counter = counter + 1
      apr2016CrimeDatesResults[i] = (counter)
    }
  }
}

# may 2015 Crime data
may2016Vec = (seq(as.Date("2016-05-01"), as.Date("2016-05-31"), by="+1 day"))
may2016CrimeDates <- format(as.POSIXct(strptime(MAY2016df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
may2016CrimeDatesResults = vector(mode="numeric", length=length(may2016Vec))
for (i in 1:length(may2016Vec)) {
  counter = 0
  for (j in 1:length(may2016CrimeDates)) {
    if(may2016Vec[i] == may2016CrimeDates[j]){
      counter = counter + 1
      may2016CrimeDatesResults[i] = (counter)
    }
  }
}

# jun 2016 Crime data
jun2016Vec = (seq(as.Date("2016-06-01"), as.Date("2016-06-30"), by="+1 day"))
jun2016CrimeDates <- format(as.POSIXct(strptime(JUN2016df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
jun2016CrimeDatesResults = vector(mode="numeric", length=length(jun2016Vec))
for (i in 1:length(jun2016Vec)) {
  counter = 0
  for (j in 1:length(jun2016CrimeDates)) {
    if(jun2016Vec[i] == jun2016CrimeDates[j]){
      counter = counter + 1
      jun2016CrimeDatesResults[i] = (counter)
    }
  }
}

# jly 2016 Crime data
jly2016Vec = (seq(as.Date("2016-07-01"), as.Date("2016-07-31"), by="+1 day"))
jly2016CrimeDates <- format(as.POSIXct(strptime(JLY2016df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
jly2016CrimeDatesResults = vector(mode="numeric", length=length(jly2016Vec))
for (i in 1:length(jly2016Vec)) {
  counter = 0
  for (j in 1:length(jly2016CrimeDates)) {
    if(jly2016Vec[i] == jly2016CrimeDates[j]){
      counter = counter + 1
      jly2016CrimeDatesResults[i] = (counter)
    }
  }
}

# aug 2016 Crime data
aug2016Vec = (seq(as.Date("2016-8-01"), as.Date("2016-8-31"), by="+1 day"))
aug2016CrimeDates <- format(as.POSIXct(strptime(AUG2016df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
aug2016CrimeDatesResults = vector(mode="numeric", length=length(aug2016Vec))
for (i in 1:length(aug2016Vec)) {
  counter = 0
  for (j in 1:length(aug2016CrimeDates)) {
    if(aug2016Vec[i] == aug2016CrimeDates[j]){
      counter = counter + 1
      aug2016CrimeDatesResults[i] = (counter)
    }
  }
}

# sep 2016 Crime data
sep2016Vec = (seq(as.Date("2016-9-01"), as.Date("2016-9-30"), by="+1 day"))
sep2016CrimeDates <- format(as.POSIXct(strptime(SEP2016df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
sep2016CrimeDatesResults = vector(mode="numeric", length=length(sep2016Vec))
for (i in 1:length(sep2016Vec)) {
  counter = 0
  for (j in 1:length(sep2016CrimeDates)) {
    if(sep2016Vec[i] == sep2016CrimeDates[j]){
      counter = counter + 1
      sep2016CrimeDatesResults[i] = (counter)
    }
  }
}

# oct 2016 Crime data
oct2016Vec = (seq(as.Date("2016-10-01"), as.Date("2016-10-31"), by="+1 day"))
oct2016CrimeDates <- format(as.POSIXct(strptime(OCT2016df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
oct2016CrimeDatesResults = vector(mode="numeric", length=length(oct2016Vec))
for (i in 1:length(oct2016Vec)) {
  counter = 0
  for (j in 1:length(oct2016CrimeDates)) {
    if(oct2016Vec[i] == oct2016CrimeDates[j]){
      counter = counter + 1
      oct2016CrimeDatesResults[i] = (counter)
    }
  }
}

# nov 2016 Crime data
# nov2016Vec = (seq(as.Date("2016-11-01"), as.Date("2016-11-30"), by="+1 day"))
# nov2016CrimeDates <- format(as.POSIXct(strptime(NOV2016df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
# nov2016CrimeDatesResults = vector(mode="numeric", length=length(nov2016Vec))
# for (i in 1:length(nov2016Vec)) {
#  counter = 0
#   for (j in 1:length(nov2016CrimeDates)) {
#    if(nov2016Vec[i] == nov2016CrimeDates[j]){
#       counter = counter + 1
#       nov2016CrimeDatesResults[i] = (counter)
#     }
#   }
# }

# dec 2016 Crime data
# dec2016Vec = (seq(as.Date("2016-12-01"), as.Date("2016-12-31"), by="+1 day"))
# dec2016CrimeDates <- format(as.POSIXct(strptime(DEC2016df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
# dec2016CrimeDatesResults = vector(mode="numeric", length=length(dec2016Vec))
# for (i in 1:length(dec2016Vec)) {
#   counter = 0
#   for (j in 1:length(dec2016CrimeDates)) {
#     if(dec2016Vec[i] == dec2015CrimeDates[j]){
#       counter = counter + 1
#       dec2016CrimeDatesResults[i] = (counter)
#     }
#   }
# }


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

