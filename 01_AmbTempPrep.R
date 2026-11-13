#### Temperature Logger Data
#### Lea Huber
#### September 2024

# Script for preparing weather data for use in analyses

##### load packages
library(readxl)
library(tidyverse)
library(ggpubr)
library(DHARMa)



#### Temp throughout experiment -----------------------------------------------------------------------------

## load Kestrel dat
Kestrel <- read.csv("../intermediate/AmbientTemp_Loggers/Kestrel_all.csv")
# remove useless X column
Kestrel <- dplyr::select(Kestrel, -X)
# fix the time to south africa time
Kestrel$DateTimereal <- parse_date_time(Kestrel$DateTime, orders = "d.m.Y H:M", tz = "Africa/Johannesburg")
# if not all are same format
Kestrel[which(is.na(Kestrel$DateTimereal)), "DateTimereal"] <- ymd_hms(Kestrel[which(is.na(Kestrel$DateTimereal)), "DateTime"], tz = "Africa/Johannesburg") 
Kestrel$Date <- as.Date(Kestrel$Date)

# check for jumps in the october and march time shifts
Oct <- subset(Kestrel, Date >= "2024-10-26" & Date < "2024-11-01", select = c(DateTime,DateTimereal,Date, Temperature))
ggplot(Oct, aes(x = DateTimereal, y = Temperature))+
  geom_line()+
  scale_x_datetime(date_breaks = "hour", date_labels = "%H")+
  theme_minimal()

March <- subset(Kestrel, Date >= "2025-03-29" & Date <= "2025-04-02", select = c(DateTime,DateTimereal,Date, Temperature))
plot(March$DateTimereal)
ggplot(March, aes(x = DateTimereal, y = Temperature))+
  geom_line()+
  scale_x_datetime(date_breaks = "hour", date_labels = "%H")+
  theme_minimal()
# There is an hour missing, the corresponding temperature values were just lost somewhere along the way
# no biggie

# add hour column that won't shift so I can always check
Kestrel$hour <- as.numeric(hour(Kestrel$DateTimereal))
# so don't need old one anymore
Kestrel <- dplyr::select(Kestrel, -DateTime)

### black globe data
## shade
GlobeShade <- read.csv("../intermediate/AmbientTemp_Loggers/GlobeShade.csv")
GlobeShade$DateTimereal <- ymd_hms(GlobeShade$DateTime, tz = "Africa/Johannesburg")
# some didn't work
GlobeShade[is.na(GlobeShade$DateTimereal),] # a midnight issue again it seems
GlobeShade[is.na(GlobeShade$DateTimereal), "DateTimereal"] <- ymd_hms(paste(GlobeShade[is.na(GlobeShade$DateTimereal), "DateTime"], "00:00:00"), tz = "Africa/Johannesburg")
GlobeShade$Date <- as.Date(GlobeShade$DateTimereal)
# will fix later if even needed, don't now

## sun & camp
load("../intermediate/AmbientTemp_Loggers/GlobeSunCamp.RData")

# check
head(GlobeSun$DateTimereal)
plot(GlobeSun$DateTimereal)
ggplot()+
  geom_line(data = filter(GlobeSun, Date >= "2024-12-25" & Date <= "2024-12-30"), aes(x = DateTimereal, y = Temperature), color = "orange")+
  scale_x_datetime(date_breaks = "hour", date_labels = "%H")+
  theme_minimal()

ggplot()+
  geom_line(data = filter(Kestrel, Date >= "2025-03-25" & Date <= "2025-03-30"), aes(x = DateTimereal, y = Temperature), color = "purple3")+
  geom_line(data = filter(GlobeSun, Date >= "2025-03-25" & Date <= "2025-03-30"), aes(x = DateTimereal, y = Temperature), color = "orange")+
  scale_x_datetime(date_breaks = "hour", date_labels = "%H")+
  theme_minimal()
# all looks good with sun

# double check
ggplot()+
  geom_line(data = filter(Kestrel, Date >= "2025-03-06" & Date <= "2025-03-14"), aes(x = DateTimereal, y = Temperature), color = "purple3")+
  geom_line(data = filter(GlobeSun, Date >= "2025-03-06" & Date <= "2025-03-14"), aes(x = DateTimereal, y = Temperature), color = "orange")+
  scale_x_datetime(date_breaks = "hour", date_labels = "%H")+
  theme_minimal()
# in beginning it's fine, also after 27th October, in march it's an hour late, after 30th two hours

# also found a chunk in Feb that is wrong for Kestrel, it's just a few days I through them out to be sure
Kestrel <- filter(Kestrel, DateTimereal < as.POSIXct("2025-02-01 14:10:00", tz = "Africa/Johannesburg") |
                    DateTimereal > as.POSIXct("2025-02-05 10:40:00", tz = "Africa/Johannesburg")) 

# and another in march from afternoon of 7th to 12th which is mostly missing
Kestrel <- filter(Kestrel, DateTimereal < as.POSIXct("2025-03-07 00:00:00", tz = "Africa/Johannesburg") |
                    DateTimereal > as.POSIXct("2025-03-12 23:00:00", tz = "Africa/Johannesburg")) 


# save 
#save(Kestrel, file = "../intermediate/AmbientTemp_Loggers/Kestrel.RData")
# also as csv for sharing
write.csv(Kestrel, "../intermediate/AmbientTemp_Loggers/Kestrel_all_clean.csv", row.names = FALSE)

load("../intermediate/AmbientTemp_Loggers/Kestrel.RData")

# checking
ggplot()+
  geom_line(data = filter(Kestrel, Date >= "2024-11-06" & Date < "2024-11-12"), aes(x = DateTimereal, y = Temperature), color = "purple3")+
  geom_line(data = filter(GlobeSun, Date >= "2024-11-06" & Date < "2024-11-12"), aes(x = DateTimereal, y = Temperature), color = "orange")+
  scale_x_datetime(date_breaks = "hour", date_labels = "%H", minor_breaks = NULL)+
  theme_minimal()

plot(filter(GlobeSun, Date >= "2025-04-06" & Date < "2025-04-12")$DateTimereal) # April

ggplot()+
  geom_line(data = filter(Kestrel, Date >= "2025-04-06" & Date < "2025-04-12"), aes(x = DateTimereal, y = Temperature), color = "purple3")+
  geom_line(data = filter(GlobeSun, Date >= "2025-04-06" & Date < "2025-04-12"), aes(x = DateTimereal, y = Temperature), color = "orange")+
  scale_x_datetime(date_breaks = "hour", date_labels = "%H", minor_breaks = NULL)+
  theme_minimal()

# it's already off here before and after, just some missing data again, have to find shift differently. Maybe it has to do with time shift in March?
ggplot()+
  geom_line(data = filter(Kestrel, Date >= "2025-03-28" & Date < "2025-04-03"), aes(x = DateTimereal, y = Temperature), color = "purple3")+
  geom_line(data = filter(GlobeSun, Date >= "2025-03-28" & Date < "2025-04-03"), aes(x = DateTimereal, y = Temperature), color = "orange")+
  scale_x_datetime(date_breaks = "hour", date_labels = "%H", minor_breaks = NULL)+
  theme_minimal()
# Kestrel does skip an hour there and goes from CET to CEST and makes difference to GlobeSun bigger
# but somehow temperatures do not seem wrong after

# what happened at first time shift in october?
ggplot()+
  geom_line(data = filter(Kestrel, Date >= "2024-10-26" & Date < "2024-10-30"), aes(x = DateTimereal, y = Temperature), color = "purple3")+
  geom_line(data = filter(GlobeSun, Date >= "2024-10-26" & Date < "2024-10-30"), aes(x = DateTimereal, y = Temperature), color = "orange")+
  scale_x_datetime(date_breaks = "hour", date_labels = "%H", minor_breaks = NULL)+
  theme_minimal()
# all looks great now

# also Kestrel and globe sun are very comparable, I will combine them to get continuous

### get continuous globe temperature from kestrel and globesun

# rename for clearness
GlobeSun <- rename(GlobeSun, BGSun = Temperature)
# which has more data
ggplot()+
  geom_line(data = GlobeSun, aes(x = DateTimereal, y = BGSun, colour = "BGsun"), alpha = 0.5)+
  geom_line(data = Kestrel, aes(x = DateTimereal, y = GlobeTemp, colour = "Kestrel"), alpha = 0.5)+
  #geom_line(data = Kestrel, aes(x = DateTimereal, y = Temperature, colour = "Temp"), alpha = 0.2)+
  theme_minimal()
# Kestrel longer in the end thankfully but sun more continuous throughout -> together cover most thankfully
# how much is missing entirely from kestrel?
plot(Kestrel$DateTimereal,Kestrel$Temperature) 
plot(Kestrel$DateTimereal) # 4 holes I can see, one in dec, one end of feb and two in mar
plot(GlobeSun$DateTime)
# thus, join kestrel to globe sun first and then add Kestrel to that in front and in end
# together
GlobeTemp <- left_join(GlobeSun, select(Kestrel, c(DateTimereal, Temperature, GlobeTemp)),
                       by = join_by(DateTimereal), multiple = "first") # only keep first match if there are many
# check
plot(GlobeTemp$DateTimereal)
ggplot(GlobeTemp, aes(x = DateTimereal))+
  geom_point(aes(y = GlobeTemp), colour = "blue", alpha = 0.5, size = 0.2)+
  geom_point(aes(y = BGSun), colour = "red", alpha = 0.5, size = 0.2)+
  geom_point(aes(y = Temperature), colour = "green", alpha = 0.5, size = 0.2)+
  theme_minimal()
# why only until november
summary(GlobeTemp) # lots of missing temps from Kestrel because globe sun is by minute
# why NAs
which(is.na(GlobeTemp$DateTimereal)) # all times seem fine
# now add kestrel in front and behind
Kestrel_join <- select(Kestrel, c(DateTimereal, X, Date, hour,Temperature, GlobeTemp))
Kestrel_join <- rename(Kestrel_join, BGSun = X)
Kestrel_join$BGSun <- NA
GlobeTemp <- rbind(filter(Kestrel_join, DateTimereal < as.POSIXct("2024-09-10 15:22:00",tz = "Africa/Johannesburg")), GlobeTemp) 
GlobeTemp <- rbind(GlobeTemp, filter(Kestrel_join, DateTimereal > as.POSIXct("2025-05-27 15:28:00",tz = "Africa/Johannesburg")))
summary(GlobeTemp)
ggplot(GlobeTemp, aes(x = DateTimereal))+
  geom_point(aes(y = BGSun), colour = "red", alpha = 0.5, size = 0.2)+
  geom_point(aes(y = GlobeTemp), colour = "blue", alpha = 0.5, size = 0.2)+
  #geom_point(aes(y = Temperature), colour = "green", alpha = 0.5)+
  theme_minimal()
plot(GlobeTemp$DateTime)
# looks good
# how to combine in one column
ggplot(GlobeTemp, aes(x = BGSun, y = GlobeTemp))+
  geom_point(size = 0.1, alpha = 0.2)+
  #stat_smooth()+
  geom_abline(slope= 1,intercept = 0, colour = "red")+
  theme_minimal()
# Kestrel measures cold temperatures a bit warmer, actualy a bit warmer in general
# they are different but not hugely
# those do not necessarily have to be errors, it's possible that one had shade and the other didn't
plot(GlobeTemp$GlobeTemp[310000:360000] - GlobeTemp$BGSun[310000:360000])
ggplot(GlobeTemp[312000:313000,], aes(x = DateTimereal))+
  geom_point(aes(y = BGSun), colour = "red", alpha = 0.5, size = 0.2)+
  geom_point(aes(y = GlobeTemp), colour = "blue", alpha = 0.5, size = 0.2)+
  #geom_point(aes(y = Temperature), colour = "green", alpha = 0.5)+
  theme_minimal()
# take the mean, and have either where there's either
GlobeTemp$GlobeTempComb <- rowMeans(cbind(GlobeTemp$BGSun, GlobeTemp$GlobeTemp), na.rm = TRUE)
ggplot(GlobeTemp, aes(x = DateTimereal, y = GlobeTempComb))+
  geom_line()+
  theme_pubr()
# just one small hole now
summary(GlobeTemp)
# do dates again to make sure
GlobeTemp$Date <- date(GlobeTemp$DateTimereal)
# save this
save(GlobeTemp, file = "../intermediate/AmbientTemp_Loggers/GlobeTempCombined.RData")

#### Kestrel with markings when inside --------------------------------------------------------------------------------
# inside data
load("../AmbientTemp_Loggers/insideTemp.RData")
insideOK <- filter(inside, DateTimereal < "2024-11-08 14:15:00")

rain <- list(in3 = as.POSIXct("2024-09-16 08:10:00","2024-09-20 08:00:00","2024-09-20 10:30:00","2024-09-30 10:55:00","2024-10-14 07:00:00","2024-10-18 07:00:00","2024-10-21 07:00:00",
                              "2024-10-21 16:30:00","2024-10-28 07:00:00","2024-10-29 07:00:00","2024-11-03 16:30:00","2024-11-04 07:00:00","2024-11-05 07:00:00","2024-11-05 16:30:00","2024-11-20 15:00:00",tz = "UTC"),
             out3 = POSIXct("2024-09-16 10:30","2024-09-20 09:35","2024-09-20 14:20","2024-09-30 15:20","2024-10-14 08:10","2024-10-18 08:10","2024-10-21 08:30",
                            NA,NA,NA,NA,NA,"2024-11-05 15:10",NA,NA),
             in5 = POSIXct("2024-09-30 10:45","2024-10-14 07:00","2024-10-18 07:00","2024-10-21 16:30","2024-10-21 07:00","2024-10-21 16:30","2024-10-28 07:00",
                           "2024-10-29 07:00","2024-11-03 16:30","2024-11-04 07:00","2024-11-05 07:00","2024-11-05 16:30","2024-11-20 15:00"),
             out5 = POSIXct(NA,"2024-10-14 08:20","2024-10-18 08:15","2024-10-21 10:30",NA,NA,
                            NA,NA,NA,"2024-11-05 15:10",NA,NA),
             in7 = POSIXct("2024-10-14 07:00","2024-10-18 07:00", "2024-10-18 12:45","2024-10-21 07:00","2024-10-28 07:00","2024-10-29 07:00","2024-11-03 16:30",
                           "2024-11-04 07:00","2024-11-05 07:00","2024-11-20 15:00"),
             out7 = POSIXct("2024-10-14 09:15","2024-10-18 08:20", NA,NA,NA,NA,NA,NA,NA))

ggplot(Kestrel, aes(x = DateTimereal, y = Temperature))+
  geom_line()+
  #geom_vline(xintercept = rain$in3)+
  theme_minimal()

#### Daily average, max and min temperatures --------------------------------------------------------------------------------

### for night and day separately
# where day goes from 07:00 until 19:00, thus from hour 7 to hour 18 (including) and night from 19 to 6
# and only take those that have full data at least every 10 minutes to get good average
# and night is a continuous night! So that means I cannot do it by date, so add an age column for one batch, where they always age at 7 in the morning

# first add daytime
nightday <- function(x){
  if(x %in% 7:18){
    daytime <- "day"
  }else{
    daytime <- "night"
  }
  daytime
}
Kestrel$daytime <- sapply(Kestrel$hour, nightday)
summary(Kestrel)
# agedate goes from 6 until 5:59 the next day, and I calculate as current date - hatching date of first batch (2024-08-27) at 7:00
Kestrel$agedate <- trunc(as.numeric(Kestrel$DateTimereal - as.POSIXct("2024-08-27 07:00:00", tz = "Africa/Johannesburg"))) # lovely
Kestrel_byagedate <- split(Kestrel, Kestrel$agedate) # split by agedate
# first get only agedays days that start in hour 7 and end in hour 18
test <- Kestrel_byagedate$`10`
fullday <- function(df, daytime){
  dfsub <- filter(df, daytime == "day")
  if (dfsub$hour[1] == 7 & dfsub$hour[nrow(dfsub)] == 18){ 
    return("good_day")
  }else{
    return("bad_day")
  }
}
fullnight <- function(df){
  dfsub <- filter(df, daytime == "night")
  if (nrow(dfsub) > 0){
    if (dfsub$hour[1] == 19 & dfsub$hour[nrow(dfsub)] == 6){ 
      return("good_night")
    }else{
      return("bad_night")
    }
  }
}
fullnight(test)
# add column for in and out
K_days <- lapply(Kestrel_byagedate, function(df) fullday(df))
K_days <- unlist(K_days)
bad_days <- as.numeric(names(which(K_days == "bad_day")))
bad_days # neat! and only 12

K_nights <- lapply(Kestrel_byagedate, function(df) fullnight(df))
K_nights <- unlist(K_nights)
bad_nights <- as.numeric(names(which(K_nights == "bad_night")))
bad_nights # only 6

Kestrel$complete <- ifelse(((Kestrel$agedate %in% bad_days) & Kestrel$daytime == "day") | ((Kestrel$agedate %in% bad_nights) & Kestrel$daytime == "night"), "no","yes")
table(Kestrel$complete,Kestrel$daytime) 

# summarise temperature and other that are not NA when temperature is not NA
Kestrel_daily <- Kestrel %>% filter(complete == "yes") %>%
  group_by(agedate,daytime) %>%
  summarise(avTemp = mean(Temperature), maxTemp = max(Temperature), minTemp = min(Temperature), sdTemp = sd(Temperature), ampTemp = max(Temperature)-min(Temperature),thirdQTemp = quantile(Temperature)[4],
            avWBGT = mean(WBGT), maxWBGT = max(WBGT), minWBGT = min(WBGT), avHumidity = mean(Humidity), avWindSpeed = mean(WindSpeed), maxWindSpeed = max(WindSpeed), avWindChill = mean(WindChill))
# make one row per agedate
Kestrel_daily <- pivot_wider(Kestrel_daily, names_from = daytime, values_from = avTemp:avWindChill, names_sep = "_")
# add date
Kestrel_day <- filter(Kestrel, daytime == "day")
Kestrel_daily$Date <- Kestrel_day[match(Kestrel_daily$agedate, Kestrel_day$agedate),"Date"] # matches to first, so just after midnight
# chekc
summary(Kestrel_daily)
duplicated(Kestrel_daily$agedate)
plot(Kestrel_daily$Date) # some missing because but very few, and looks same as agedate, all good
# save
save(Kestrel_daily, file = "../intermediate/AmbientTemp_Loggers/Kestrel_daily.RData")


### for combined black globe temp
load("../intermediate/AmbientTemp_Loggers/GlobeTempCombined.RData")
# add agedate
GlobeTemp$agedate <- trunc(as.numeric(GlobeTemp$DateTimereal - as.POSIXct("2024-08-27 07:00:00", tz = "Africa/Johannesburg")))
GlobeTemp$daytime <- sapply(GlobeTemp$hour, nightday)
# find dates with too many NA's for globeTemp
GlobeTemp_byagedate <- split(GlobeTemp, ~ agedate + daytime)
K_GTnafr <- lapply(GlobeTemp_byagedate, function(df) sum(is.na(df$GlobeTempComb))/nrow(df)) # get fraction of NAs of all measurements per day
K_GTnafr <- unlist(K_GTnafr)
K_GTnafr
good_GTdays <- names(which(K_GTnafr < 0.1)) # let's allow 10%
# there should also be a minimum amount of measurements, lets say 10 hours which is about 10*12 = 120 measurements (every 5 minutes = 12 measurements/hour)
tooshort <- lapply(GlobeTemp_byagedate, function(df) ifelse(nrow(df) < 120, "tooshort","ok"))
tooshort <- unlist(tooshort)
table(tooshort) # that's just the last day and night actually
names(tooshort[tooshort == "tooshort"])
good_GTdays <- separate(data.frame(good_GTdays),good_GTdays, c("agedate","daytime"))
GlobeTemp$complete <- ifelse(((GlobeTemp$agedate %in% filter(good_GTdays, daytime == "day")$agedate) & GlobeTemp$daytime == "day") |
                               ((GlobeTemp$agedate %in% filter(good_GTdays, daytime == "night")$agedate) & GlobeTemp$daytime == "night"),
                             "yes","no")
GlobeTemp[which(GlobeTemp$agedate == 304),"complete"] <- "no"
summary(GlobeTemp)
# ok now make daily stats
GTcombDaily <- GlobeTemp %>% filter(complete == "yes") %>%
  group_by(agedate,daytime) %>%
  summarise(avGTcomb = mean(GlobeTempComb), maxGTcomb = max(GlobeTempComb), minGTcomb = min(GlobeTempComb), sdGTcomb = sd(GlobeTempComb),
            ampGTcomb = max(GlobeTempComb)-min(GlobeTempComb), thirdQGTcomb = quantile(GlobeTempComb)[4])
# make one row per date, with the date starting by day and then night until following morning (i.e by agedate)
GTcombDaily <- pivot_wider(GTcombDaily, names_from = daytime, values_from = avGTcomb:thirdQGTcomb, names_sep = "_")
# add date
GlobeTemp_day <- filter(GlobeTemp, daytime == "day")
GTcombDaily$Date <- GlobeTemp_day[match(GTcombDaily$agedate, GlobeTemp_day$agedate),"Date"] # also matches Kestrel :)
summary(GTcombDaily)
plot(GTcombDaily$Date,GTcombDaily$avGTcomb_night)
# save
save(GTcombDaily, file = "../intermediate/AmbientTemp_Loggers/GTcombinedDaily.RData")



### for inside temp, make separate day and night only for nights and days they were inside
load("../intermediate/AmbientTemp_Loggers/insideTemp.RData")
inside$Date <- date(inside$DateTimereal)
## make average for nights (19:00 to 6:30) and day (6:30 to 19:00)
inside$hour <- hour(inside$DateTimereal) # this only does full hours, easy to work with
# let's say then night is until 6:50 and day starts at 7
nightday <- function(x){
  if (x %in% c(7:18)){ # 18 includes the hour from 18 to 19
    return("day")
  }else if (x %in% c(c(19:24),c(0:6))){ # 6 includes hour until 7
    return("night")
  }else{
    return(NA)
  }
}
inside$daytime <- unlist(sapply(inside$hour, nightday))
# summarise by night and day separately by age-date
inside$agedate <- trunc(as.numeric(inside$DateTimereal - as.POSIXct("2024-08-27 07:00:00", tz = "Africa/Johannesburg"))) # fits
insideDayNight <- inside %>% group_by(agedate,daytime) %>% 
  summarise(avIns3 = mean(inside_b3,na.rm = TRUE), avIns5 = mean(inside_b5,na.rm = TRUE), avIns7 = mean(inside_b7,na.rm = TRUE), 
            minIns3 = min(inside_b3, na.rm = TRUE), minIns5 = min(inside_b5,na.rm = TRUE),minIns7 = min(inside_b7,na.rm = TRUE))
# pivot wider so it has only one row per date like the others
insideDayNight <- pivot_wider(insideDayNight,names_from = daytime, values_from = c(avIns3,avIns5,avIns7,minIns3,minIns5,minIns7))
# add date
inside_day <- filter(inside, daytime == "day")
insideDayNight$Date <- inside_day[match(insideDayNight$agedate, inside_day$agedate),"Date"] # also matches Kestrel :)

# check
summary(insideDayNight)
ggplot(insideDayNight, aes(x = Date))+
  geom_line(aes(y = avIns3_day), color = "orange")+
  geom_line(aes(y = avIns3_night), color = "blue")+
  theme_minimal()
ggplot(insideDayNight, aes(x = Date))+
  geom_line(aes(y = minIns3_day), color = "orange")+
  geom_line(aes(y = minIns3_night), color = "blue")+
  theme_minimal()
ggplot(insideDayNight, aes(x = Date))+
  geom_line(aes(y = avIns5_day), color = "orange")+
  geom_line(aes(y = avIns5_night), color = "blue")+
  theme_minimal()
ggplot(insideDayNight, aes(x = Date))+
  geom_line(aes(y = avIns7_day), color = "orange")+
  geom_line(aes(y = avIns7_night), color = "blue")+
  theme_minimal()
# inside temp at night more stable


### combine them all for daily weather!
DailyWeather <- full_join(insideDayNight, GTcombDaily, by = join_by(agedate,Date))
DailyWeather <- full_join(DailyWeather,Kestrel_daily, by = join_by(agedate,Date))
plot(DailyWeather$agedate) # two days randomly in the end because it's missing in both gtcomb and inside, let's fix that
DailyWeather <- DailyWeather[order(DailyWeather$agedate),]
plot(DailyWeather$agedate) # fixed
plot(DailyWeather$Date) # also fits
## save!
save(DailyWeather, file = "../intermediate/AmbientTemp_Loggers/DailyWeather.RData")
