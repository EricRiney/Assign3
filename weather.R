# Eric 
# INFO 370

library(MASS) # need fractions
require(zoo)  # date stuff
library(lubridate)

setwd("/Users/ericriner/Documents/Code/UW/370/Assign3")

weather = read.csv(file.choose(), header = TRUE)
crime = read.csv(file.choose(), header = TRUE)

summary(weather)

###############################
### 2011 Weather Dataframes ###
###############################

weather2011 = weather[(weather$Year== '2011'),]
# Monthly Weather Dataframes for 2011
weatherAUG2011df = weather[(weather$Month == 'Aug' & weather$Year== '2011'),]
weatherSEP2011df = weather[(weather$Month == 'Sep' & weather$Year== '2011'),]
weatherOCT2011df = weather[(weather$Month == 'Oct' & weather$Year== '2011'),]
weatherNOV2011df = weather[(weather$Month == 'Nov' & weather$Year== '2011'),]
weatherDEC2011df = weather[(weather$Month == 'Dec' & weather$Year== '2011'),]

###############################
### 2012 Weather Dataframes ###
###############################

weather2012 = weather[(weather$Year== '2012'),]
# Monthly Weather Dataframes for 2012
weatherJAN2012df = weather[(weather$Month == 'Jan' & weather$Year== '2012'),]
weatherFEB2012df = weather[(weather$Month == 'Feb' & weather$Year== '2012'),]
weatherMAR2012df = weather[(weather$Month == 'Mar' & weather$Year== '2012'),]
weatherAPR2012df = weather[(weather$Month == 'Apr' & weather$Year== '2012'),]
weatherMAY2012df = weather[(weather$Month == 'May' & weather$Year== '2012'),]
weatherJUN2012df = weather[(weather$Month == 'Jun' & weather$Year== '2012'),]
weatherJUL2012df = weather[(weather$Month == 'Jul' & weather$Year== '2012'),]
weatherAUG2012df = weather[(weather$Month == 'Aug' & weather$Year== '2012'),]
weatherSEP2012df = weather[(weather$Month == 'Sep' & weather$Year== '2012'),]
weatherOCT2012df = weather[(weather$Month == 'Oct' & weather$Year== '2012'),]
weatherNOV2012df = weather[(weather$Month == 'Nov' & weather$Year== '2012'),]
weatherDEC2012df = weather[(weather$Month == 'Dec' & weather$Year== '2012'),]

weather2012Vec = (seq(as.Date("2012-1-01"), as.Date("2012-12-31"), by="+1 day"))
#weather2012CrimeDates <- format(as.POSIXct(strptime(AUG2011df$Date.Reported,"%m/%d/%Y %H:%M",tz="")) ,format = "%Y-%m-%d")
weather2012DatesResults = vector(mode="numeric", length=length(weather2012Vec))
for (i in 1:length(weather2012Vec)) {
  counter = 0
  for (j in 1:length(weather2012DatesResults)) {
    if(weather2012$Precip...in.[i] != 0){
      counter = 1
    }
    weather2012DatesResults[i] = (counter)
  }
}

weatherClum2012DatesResults = weather[weather$Year == '2012',]
weatherClum2012DatesResults[weatherClum2012DatesResults == 'T'] <- 0.2

###############################
### 2013 Weather Dataframes ###
###############################

weather2013 = weather[(weather$Year== '2013'),]
# Monthly Weather Dataframes for 2013
weatherJAN2013df = weather[(weather$Month == 'Jan' & weather$Year== '2013'),]
weatherFEB2013df = weather[(weather$Month == 'Feb' & weather$Year== '2013'),]
weatherMAR2013df = weather[(weather$Month == 'Mar' & weather$Year== '2013'),]
weatherAPR2013df = weather[(weather$Month == 'Apr' & weather$Year== '2013'),]
weatherMAY2013df = weather[(weather$Month == 'May' & weather$Year== '2013'),]
weatherJUN2013df = weather[(weather$Month == 'Jun' & weather$Year== '2013'),]
weatherJUL2013df = weather[(weather$Month == 'Jul' & weather$Year== '2013'),]
weatherAUG2013df = weather[(weather$Month == 'Aug' & weather$Year== '2013'),]
weatherSEP2013df = weather[(weather$Month == 'Sep' & weather$Year== '2013'),]
weatherOCT2013df = weather[(weather$Month == 'Oct' & weather$Year== '2013'),]
weatherNOV2013df = weather[(weather$Month == 'Nov' & weather$Year== '2013'),]
weatherDEC2013df = weather[(weather$Month == 'Dec' & weather$Year== '2013'),]

###############################
### 2014 Weather Dataframes ###
###############################

weather2014 = weather[(weather$Year== '2014'),]
# Monthly Weather Dataframes for 2013
weatherJAN2014df = weather[(weather$Month == 'Jan' & weather$Year== '2014'),]
weatherFEB2014df = weather[(weather$Month == 'Feb' & weather$Year== '2014'),]
weatherMAR2014df = weather[(weather$Month == 'Mar' & weather$Year== '2014'),]
weatherAPR2014df = weather[(weather$Month == 'Apr' & weather$Year== '2014'),]
weatherMAY2014df = weather[(weather$Month == 'May' & weather$Year== '2014'),]
weatherJUN2014df = weather[(weather$Month == 'Jun' & weather$Year== '2014'),]
weatherJUL2014df = weather[(weather$Month == 'Jul' & weather$Year== '2014'),]
weatherAUG2014df = weather[(weather$Month == 'Aug' & weather$Year== '2014'),]
weatherSEP2014df = weather[(weather$Month == 'Sep' & weather$Year== '2014'),]
weatherOCT2014df = weather[(weather$Month == 'Oct' & weather$Year== '2014'),]
weatherNOV2014df = weather[(weather$Month == 'Nov' & weather$Year== '2014'),]
weatherDEC2014df = weather[(weather$Month == 'Dec' & weather$Year== '2014'),]

###############################
### 2015 Weather Dataframes ###
###############################

weather2015 = weather[(weather$Year== '2014'),]
# Monthly Weather Dataframes for 2013
weatherJAN2015df = weather[(weather$Month == 'Jan' & weather$Year== '2015'),]
weatherFEB2015df = weather[(weather$Month == 'Feb' & weather$Year== '2015'),]
weatherMAR2015df = weather[(weather$Month == 'Mar' & weather$Year== '2015'),]
weatherAPR2015df = weather[(weather$Month == 'Apr' & weather$Year== '2015'),]
weatherMAY2015df = weather[(weather$Month == 'May' & weather$Year== '2015'),]
weatherJUN2015df = weather[(weather$Month == 'Jun' & weather$Year== '2015'),]
weatherJUL2015df = weather[(weather$Month == 'Jul' & weather$Year== '2015'),]
weatherAUG2015df = weather[(weather$Month == 'Aug' & weather$Year== '2015'),]
weatherSEP2015df = weather[(weather$Month == 'Sep' & weather$Year== '2015'),]
weatherOCT2015df = weather[(weather$Month == 'Oct' & weather$Year== '2015'),]
weatherNOV2015df = weather[(weather$Month == 'Nov' & weather$Year== '2015'),]
weatherDEC2015df = weather[(weather$Month == 'Dec' & weather$Year== '2015'),]

###############################
### 2016 Weather Dataframes ###
###############################

weather2016 = weather[(weather$Year== '2016'),]
# Monthly Weather Dataframes for 2013
weatherJAN2016df = weather[(weather$Month == 'Jan' & weather$Year== '2016'),]
weatherFEB2016df = weather[(weather$Month == 'Feb' & weather$Year== '2016'),]
weatherMAR2016df = weather[(weather$Month == 'Mar' & weather$Year== '2016'),]
weatherAPR2016df = weather[(weather$Month == 'Apr' & weather$Year== '2016'),]
weatherMAY2016df = weather[(weather$Month == 'May' & weather$Year== '2016'),]
weatherJUN2016df = weather[(weather$Month == 'Jun' & weather$Year== '2016'),]
weatherJUL2016df = weather[(weather$Month == 'Jul' & weather$Year== '2016'),]
weatherAUG2016df = weather[(weather$Month == 'Aug' & weather$Year== '2016'),]
weatherSEP2016df = weather[(weather$Month == 'Sep' & weather$Year== '2016'),]
weatherOCT2016df = weather[(weather$Month == 'Oct' & weather$Year== '2016'),]
weatherNOV2016df = weather[(weather$Month == 'Nov' & weather$Year== '2016'),]
weatherDEC2016df = weather[(weather$Month == 'Dec' & weather$Year== '2016'),]

