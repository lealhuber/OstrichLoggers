### Importing oddi logger data and saving it ijn usable forms for further analysis
## Lea Huber
## May 2025

# load packages
library(readxl)
library(readr)
library(tidyverse)
library(ggpubr)
library(hms)

#set wd to output directory
setwd("/Users/Lea/Library/CloudStorage/OneDrive-Aarhusuniversitet/LeaH/DataLoggerExp/intermediate/Oddi/")


#### load logger data automatically ---------------------------------------------------------------------------------------------------------
load("ChickData.RData")
dat_dir <- "/Users/Lea/Library/CloudStorage/OneDrive-Aarhusuniversitet/LeaH/DataLoggerExp/data/StarOddi/"
# Get all relevant file paths recursively
files <- list.files(path = dat_dir, pattern = "^MHL.*\\.xlsx$", 
                    full.names = TRUE, recursive = TRUE)
# exclude chick 8950 only lived a few days after implantation and was presumably sick throughout
# similar with 8900, survived a week but was inflamed from the beginning probably
# 8930 was the one with the logger in the yolk sack and also died < 2 weeks, exclude as well
# 8935 deteriorated 5 days after implantation, exclude as well

files <- files[-c(31,24,29,30)] # CHECK NUMBERS and adjust if file list changes!!!

# Load each file into a data frame named after the file (without extension)
for (file in files) {
  # Get just the filename without extension
  name <- tools::file_path_sans_ext(basename(file))
  # Read into a named list
  logger_list <- setNames(lapply(files, read_excel), 
                        tools::file_path_sans_ext(basename(files)))
}

## specify that all this is south africa time
fixtime <- function(df){
  colnames(df)[1] <- "Date_Time"
  df$Date_Time <- lubridate::force_tz(df$Date_Time, tz = "Africa/Johannesburg")
  return(df)
}
logger_list <- lapply(logger_list, function(df) fixtime(df))

# add date and add hour to each logger and daytime (they were outside from 07:00 till 18:30 when young)
# so day goes from 07:00 until 19:00
nightday <- function(x){
  if(x %in% 7:18){
    daytime <- "day"
  }else{
    daytime <- "night"
  }
  daytime
}
adddatehour <- function(df){
  df$Date <- as_date(df$Date_Time)
  df$hour <- hour(df$Date_Time)
  df$daytime <- sapply(df$hour, nightday)
  df
}

### start all loggers per batch at the same time at midnight as to not mess up the daily stats for the first day and to have it uniformly
# and same with end, at midnight before day of death, or, in case of the loggers that stopped recording inside living chicks, midnight before they stopped recording
logged_chicks <- c()
for (i in 1:length(logger_list)){
  colnames(logger_list[[i]]) <- c("Date_Time","Temp_C","Heartrate","QI","age_days","age_weeks")
  Chickno <- sub(".*_", "", names(logger_list)[i])
  setstart <- Chicks[which(Chicks$chickno == Chickno),]$date_time_implant # get chick implant time
  setstart <- as.POSIXct(date(setstart)+1) # get date of next day
  setend <- Chicks[which(Chicks$chickno == Chickno),]$mort_dat # get chick death time
  setend <- as.POSIXct(date(setend)) # get date of that day
  if (setend > as.POSIXct(date(logger_list[[i]]$Date_Time[nrow(logger_list[[i]])]))){ # if the logger died before the chick
    setend <- as.POSIXct(date(logger_list[[i]]$Date_Time[nrow(logger_list[[i]])])) # get the date of when the logger died
  } 
  logger_list[[i]] <- filter(logger_list[[i]], Date_Time > setstart & Date_Time < setend) # filter for times bigger than setstart and smaller than setend
  # add age column (assuming pipping on Tuesday) where they age at 07:00 in the morning, so as to easily calculate night and day separately
  logger_list[[i]]$age_days <- c(NA)
  logger_list[[i]]$age_days <- trunc(as.numeric(logger_list[[i]]$Date_Time - Chicks$hat_dat[which(Chicks$chickno == Chickno)]) - 0.29166) # date - hatching date minus 7 hours = 0.29166 days
  logger_list[[i]]$age_weeks <- trunc(logger_list[[i]]$age_days/7) # also add age in weeks
  # add date, hour and daytime
  logger_list[[i]] <- adddatehour(logger_list[[i]])
  logged_chicks <- c(logged_chicks, Chickno)
}


### cut out some buffer time before the day injured and sick ones died
# their temperature is probably affected by infection

## 8550 had broken foot on 2. Dec and was euthanised 13. january 
Tb8550 <- logger_list[[1]]
plot(Tb8550$Date_Time, Tb8550$Temp_C)
# take it out from 2. Dec
logger_list[[1]] <- filter(logger_list[[1]], Date_Time < "2024-12-02 02:00:00") # add 2h for time diff

# 8895 neck injury killed 30.1
Tb8895 <- logger_list[["MHL1437_8895"]]
plot(Tb8895$Date_Time, Tb8895$Temp_C)
# I'll just subtract 2 days to be sure
logger_list[["MHL1437_8895"]] <- filter(logger_list[["MHL1437_8895"]], Date_Time < "2025-01-28 02:00:00") # add 2 h bec of time difference

# 8637 was trampled in the night to 11. june, subtract one more day to be sure
Tb8637 <- logger_list[["MHL1419_8637"]]
plot(Tb8637$Date_Time, Tb8637$Temp_C)
logger_list[["MHL1419_8637"]] <- filter(logger_list[["MHL1419_8637"]], Date_Time < "2025-06-10 02:00:00") # add 2 h

# 8713 was moved to sick camp on 22. Jan for neck injury but it survived till end
Tb8713 <- logger_list[["MHL1416_8713"]]
plot(Tb8713$Date_Time, Tb8713$Temp_C)
# it was sick for a month
logger_list[["MHL1416_8713"]] <- filter(logger_list[["MHL1416_8713"]], Date_Time < "2025-01-20 02:00:00"| Date_Time > "2025-02-20 02:00:00") 

# 8656 swollen neck on 27 march
Tb8656 <- logger_list[["MHL1410_8656"]]
plot(Tb8656$Date_Time, Tb8656$Temp_C)
# nothing special though

# 8609 neck injury moved to hospital camp on 5. Feb but survived till end 
Tb8609 <- logger_list[["MHL1418_8609"]]
plot(Tb8609$Date_Time, Tb8609$Temp_C)
# it was sick for a month
logger_list[["MHL1418_8609"]] <- filter(logger_list[["MHL1418_8609"]], Date_Time < "2025-02-03 02:00:00"| Date_Time > "2025-03-03 02:00:00") 


# 8647 leg injury on 15. May that got worse on 6. June, killed sometime before 23. june
Tb8647 <- logger_list[["MHL1408_8647"]]
plot(filter(Tb8647, Date_Time > "2025-04-20 00:00:00")$Date_Time, filter(Tb8647, Date_Time > "2025-04-20 00:00:00")$Temp_C)
# I can see inflammation starting 14/15 May so I will only keep data until 13 May
logger_list[["MHL1408_8647"]] <- filter(logger_list[["MHL1408_8647"]], Date_Time < "2025-05-13 02:00:00") 




## plot body temp of all to see problems
ggplot(logger_list[[60]], aes(x = Date_Time, y = Temp_C))+
  geom_line()+
  xlab("Time") + ylab("Body Temperature [°C]")+
  theme_minimal(base_size = 22)
library(gridExtra)
pdf(paste0(figurepath,"allloggers_mar9.pdf"))
n_per_page <- 9
n <- length(logger_list)
for (i in seq(1, n, by = n_per_page)) {
  # make the plots for this page
  these_dfs <- logger_list[i:min(i+n_per_page-1, n)]
  plots <- lapply(these_dfs, function(d) {
    ggplot(d, aes(x = Date_Time, y = Temp_C))+
      geom_line()+
      xlab("Time") + ylab("Body Temperature [°C]")+
      theme_minimal()
  })
  # arrange them in a 3x4 grid
  grid.arrange(grobs = plots, ncol = 3, nrow = 3)
}
dev.off()
# all looking fine!




#### filtering ---------------------------
### too big temperature jumps?

# loop through loggers and find max jump
testlogger <- logger_list[[49]]
jumpsize <- c()
time = 5
for (i in time:nrow(testlogger)){
  jumpsize <- c(jumpsize, testlogger$Temp_C[i] - testlogger$Temp_C[i-time])
}
plot(jumpsize, cex = 0.2)
hist(jumpsize)
plot(density(jumpsize))
# does not seem to be an issue at all

plot(testlogger$Date_Time)


### Is there missing data, and if yes bias in it?
table(is.na(testlogger$Temp_C))
nas <- lapply(logger_list, function(df) sum(is.na(df[[2]])))
# no missing values


### Do the time stamps make sense?
ggplot(filter(testlogger, Date == "2024-10-25"), aes(x = Date_Time))+
  geom_line(aes(y = Temp_C))+
  #geom_line(aes(y = GlobeTempComb.y))+
  theme_minimal()
# I checked directly with excel and with the .DAT file from mercury software and yes they are right
# The times also fit with when I noted that they were implanted for batch 5 
# so we can trust the times of the loggers!


### mark (and maybe take away) times when disturbed
Disturb <- read_excel("../../data/DailyHappenings.xlsx", sheet = 2)
Disturb$length <- minute(Disturb$length)
summary(Disturb)
# from this make table with normal & strong disturbances and the measurements they affected
Disturb$DateTime_start <- ceiling_date(Disturb$DateTime_start, "10 mins") 
Disturb2 <- subset(Disturb, disturbance %in% c("normal","strong"), select = c(Run, DateTime_start, length, disturbance)) # select relevant ones
# Because the disturbance could affect the chicks for longer after it's over add some time in the end of each disturbance
# they seemed to recover quite quickly visually though, so I think 20 minutes is enough for normal disturbance, and for the baboon attack maybe 1 hour (strong disturbance)
Disturb2$length <- Disturb2$length + 20 # add 20 minutes to all
Disturb2[which(Disturb2$disturbance == "strong"),"length"] <- Disturb2[which(Disturb2$disturbance == "strong"),"length"] +40 # add another 40 minutes for strong disturbance

Disturb_new <- Disturb2[1,]
for (i in 1:nrow(Disturb2)){
  Disturb_new <- rbind(Disturb_new, Disturb2[i,])
  Disturb_new$length[nrow(Disturb_new)] <- Disturb_new$length[nrow(Disturb_new)] - 10
  while (Disturb_new[nrow(Disturb_new),"length"] > 0){
    disturb_row <- Disturb_new[nrow(Disturb_new),]
    disturb_row$DateTime_start <- disturb_row$DateTime_start + (10*60) # add 10 minutes
    disturb_row$length <- disturb_row$length - 10 # reduce length by 10 minutes
    Disturb_new <- rbind(Disturb_new, disturb_row)
  }
}
Disturb_new <- filter(Disturb_new, !is.na(DateTime_start))
Disturb_new$batch <- ifelse(Disturb_new$Run %in% c(2,3), "1",NA)
Disturb_new$batch <- ifelse(Disturb_new$Run %in% c(4,5), "2", Disturb_new$batch)
Disturb_new$batch <- ifelse(Disturb_new$Run %in% c(6,7), "3", Disturb_new$batch)
Disturb1 <- filter(Disturb_new, batch == "1")
Disturb2 <- filter(Disturb_new, batch == "2")
Disturb3 <- filter(Disturb_new, batch == "3")
# make sure each does not have duplicate DateTime_start, that might break join
Disturb1 <- Disturb1[!duplicated(Disturb1$DateTime_start),]
Disturb2 <- Disturb2[!duplicated(Disturb2$DateTime_start),]
Disturb3 <- Disturb3[!duplicated(Disturb3$DateTime_start),]

# now add it to logger list (NUMBERS MIGHT CHANGE CHECK BEFORE RERUN !!!)
logger_list[1:20] <- lapply(logger_list[1:20], function(df) left_join(df, dplyr::select(Disturb1, DateTime_start, disturbance),
                                                                      by = join_by(Date_Time == DateTime_start)))

logger_list[21:40] <- lapply(logger_list[21:40], function(df) left_join(df, dplyr::select(Disturb2, DateTime_start, disturbance),
                                                                      by = join_by(Date_Time == DateTime_start)))

logger_list[41:60] <- lapply(logger_list[41:60], function(df) left_join(df, dplyr::select(Disturb3, DateTime_start, disturbance),
                                                                      by = join_by(Date_Time == DateTime_start)))

## now there are just a few disturbances of single chicks left
# 8900, 8912 and 8884 were inflamed after implant (18. sept) and got medicine on 20. sept
# -> start only on 22. sept when hopefully healthy (as soon as data here)
logger_list[["MHL1440_8912"]] <- filter(logger_list[["MHL1440_8912"]], Date_Time > "2024-09-22 02:00:00") # have to add two hours bec of time dif 
plot(filter(logger_list[["MHL1440_8912"]], Date_Time < "2024-09-24 00:00:00")$Date_Time) # looking good


## there are 3 chicks with weird temperatures, mark those as sick in distub column (9149, 9246 and 9255)
# when did they get sick?
ggplot(filter(logger_list[["MHL1420_9149"]], Date_Time > "2025-03-15 00:10:00" & Date_Time < "2025-04-01 00:10:00"), aes(x = Date_Time, y = Temp_C))+
  geom_point()
# 9149 got sick around march 27, so remove from March 13
logger_list[["MHL1420_9149"]][which(logger_list[["MHL1420_9149"]]$Date_Time > "2025-03-13 01:00:00" ),"disturbance"] <- "sick" # add 1 h because of time change

ggplot(filter(logger_list[["MHL1422_9246"]], Date_Time > "2024-12-05 00:10:00" & Date_Time < "2025-01-01 00:10:00"), aes(x = Date_Time, y = Temp_C, colour = disturbance))+
  geom_point()
# 9246 got sick around December 16, so remove from dec 02
logger_list[["MHL1422_9246"]][which(logger_list[["MHL1422_9246"]]$Date_Time > "2024-12-02 01:00:00" ),"disturbance"] <- "sick" # add 1 h because of time change

ggplot(filter(logger_list[["MHL1431_9255"]], Date_Time > "2025-01-01 00:10:00" & Date_Time < "2025-02-28 00:10:00"), aes(x = Date_Time, y = Temp_C, colour = disturbance))+
  geom_point()
# 9255 was always quite high and weird, take out from Dec 15 
logger_list[["MHL1431_9255"]][which(logger_list[["MHL1431_9255"]]$Date_Time > "2024-12-15 01:00:00" ),"disturbance"] <- "sick" # add 1 h because of time change




### add info when they were inside

in3 <- as.POSIXct(c("2024-09-16 08:10:00","2024-09-20 08:00:00","2024-09-20 10:30:00","2024-09-30 10:55:00","2024-10-14 07:00:00","2024-10-18 07:00:00","2024-10-21 07:00:00",
                              "2024-10-21 16:30:00","2024-10-28 07:00:00","2024-10-29 07:00:00","2024-11-03 16:30:00","2024-11-04 07:00:00","2024-11-05 07:00:00","2024-11-05 16:30:00","2024-11-20 15:00:00"),tz = "UTC")
out3 <- c("2024-09-16 10:30","2024-09-20 09:35","2024-09-20 14:20","2024-09-30 15:20","2024-10-14 08:10","2024-10-18 08:10","2024-10-21 08:30",
          NA,NA,NA,NA,NA,"2024-11-05 15:10",NA,NA)
out3 <- as.POSIXct(out3,tz = "UTC")
in5 <- c("2024-09-30 10:45","2024-10-14 07:00","2024-10-18 07:00","2024-10-21 07:00","2024-10-21 16:30","2024-10-28 07:00",
                           "2024-10-29 07:00","2024-11-03 16:30","2024-11-04 07:00","2024-11-05 07:00","2024-11-05 16:30","2024-11-20 15:00")
in5 <- as.POSIXct(in5,tz = "UTC")
out5 <- c(NA,"2024-10-14 08:20","2024-10-18 08:15","2024-10-21 10:30",NA,NA,
                            NA,NA,NA,"2024-11-05 15:10",NA,NA)
out5 <- as.POSIXct(out5,tz = "UTC")
in7 <- c("2024-10-14 07:00","2024-10-18 07:00", "2024-10-18 12:45","2024-10-21 07:00","2024-10-28 07:00","2024-10-29 07:00","2024-11-03 16:30",
                           "2024-11-04 07:00","2024-11-05 07:00","2024-11-20 15:00")
in7 <- as.POSIXct(in7,tz = "UTC")
out7 <- c("2024-10-14 09:15","2024-10-18 08:20", NA,NA,NA,NA,NA,NA,NA,NA)
out7 <- as.POSIXct(out7,tz = "UTC")

# from this make table for each batch with measurement times they were inside
rain <- data.frame(start = in3, end = out3)
rain$end <- as.POSIXct(ifelse(is.na(rain$end), as.POSIXct(paste(date(rain$start), "18:30:00", sep = " "), tz = "UTC"),
                               rain$end), tz = "UTC") # where out NA they stayed inside until 18:30 (and then whole night but will add that in next step)
rain$length <- difftime(rain$end, rain$start, units = "mins")
rain$n_measurements <- floor(as.numeric(rain$length/10)) # divide by 10 minutes to get nr of measurements
# always round down because when 10 mins are not full the next measurement will already be outside again
rain$inside <- rep("inside_rain", nrow(rain))
rain_new <- rain[1,]
for (i in 1:nrow(rain)){
  rain_new <- rbind(rain_new, rain[i,])
  rain_new$length[nrow(rain_new)] <- rain_new$length[nrow(rain_new)] - 10
  while (rain_new[nrow(rain_new),"length"] >= 0){
    rain_row <- rain_new[nrow(rain_new),]
    rain_row$start <- rain_row$start + (10*60) # add 10 minutes
    rain_row$length <- rain_row$length - 10 # reduce length by 10 minutes
    rain_new <- rbind(rain_new, rain_row)
  }
}
# it worked
insideTimes3 <- data.frame(Date_Time = rain_new$start, inside = rain_new$inside)
insideTimes3 <-insideTimes3[!duplicated(insideTimes3$Date_Time),] # delete duplicate date_time rows (it's always just the first)

# do rain for batch 5
rain <- data.frame(start = in5, end = out5)
rain$end <- as.POSIXct(ifelse(is.na(rain$end), as.POSIXct(paste(date(rain$start), "18:30:00", sep = " "), tz = "UTC"),
                               rain$end), tz = "UTC") # where out NA they stayed inside until 18:30 (and then whole night but will add that in next step)
rain$length <- difftime(rain$end, rain$start, units = "mins")
rain$n_measurements <- floor(as.numeric(rain$length/10)) # divide by 10 minutes to get nr of measurements
# always round down because when 10 mins are not full the next measurement will already be outside again
rain$inside <- rep("inside_rain", nrow(rain))
rain_new <- rain[1,]
for (i in 1:nrow(rain)){
  rain_new <- rbind(rain_new, rain[i,])
  rain_new$length[nrow(rain_new)] <- rain_new$length[nrow(rain_new)] - 10
  while (rain_new[nrow(rain_new),"length"] >= 0){
    rain_row <- rain_new[nrow(rain_new),]
    rain_row$start <- rain_row$start + (10*60) # add 10 minutes
    rain_row$length <- rain_row$length - 10 # reduce length by 10 minutes
    rain_new <- rbind(rain_new, rain_row)
  }
}
insideTimes5 <- data.frame(Date_Time = rain_new$start, inside = rain_new$inside)
insideTimes5 <-insideTimes5[!duplicated(insideTimes5$Date_Time),] # delete duplicate date_time rows


# do rain for batch 7
rain <- data.frame(start = in7, end = out7)
rain$end <- as.POSIXct(ifelse(is.na(rain$end), as.POSIXct(paste(date(rain$start), "18:30:00", sep = " "), tz = "UTC"),
                              rain$end), tz = "UTC") # where out NA they stayed inside until 18:30 (and then whole night but will add that in next step)
rain$length <- difftime(rain$end, rain$start, units = "mins")
rain$n_measurements <- floor(as.numeric(rain$length/10)) # divide by 10 minutes to get nr of measurements
# always round down because when 10 mins are not full the next measurement will already be outside again
rain$inside <- rep("inside_rain", nrow(rain))
rain_new <- rain[1,]
for (i in 1:nrow(rain)){
  rain_new <- rbind(rain_new, rain[i,])
  rain_new$length[nrow(rain_new)] <- rain_new$length[nrow(rain_new)] - 10
  while (rain_new[nrow(rain_new),"length"] >= 0){
    rain_row <- rain_new[nrow(rain_new),]
    rain_row$start <- rain_row$start + (10*60) # add 10 minutes
    rain_row$length <- rain_row$length - 10 # reduce length by 10 minutes
    rain_new <- rbind(rain_new, rain_row)
  }
}
insideTimes7 <- data.frame(Date_Time = rain_new$start, inside = rain_new$inside)
insideTimes7 <-insideTimes7[!duplicated(insideTimes7$Date_Time),] # delete duplicate date_time rows


# now add rain times to logger list (NUMBERS MIGHT CHANGE, CHECK BEFORE RERUN !!!)
logger_list[1:20] <- lapply(logger_list[1:20], function(df) left_join(df, insideTimes3))
logger_list[21:40] <- lapply(logger_list[21:40], function(df) left_join(df, insideTimes5))
logger_list[41:60] <- lapply(logger_list[41:60], function(df) left_join(df, insideTimes7))


### also add times they were sleeping inside
# via age: batch 3 slept inside until age 41 (i.e. were outside evening of 41st day);
# batch 5 slept inside until age 43; and batch 7 until 44
# also note when in rings: batch 3 until age 17, batch 5 until age 18, and batch 7 until age 9 (all including)
sleepytime <- function(df, age, age_ring){
  df[which(df$age_days < age & df$daytime == "night"),"inside"] <- "inside_night"
  df[which(df$age_days < age_ring & df$daytime == "night"),"inside"] <- "inside_ring"
  return(df)
}
logger_list[1:20] <- lapply(logger_list[1:20], function(x) sleepytime(x,41,17))
logger_list[21:40] <- lapply(logger_list[21:40], function(x) sleepytime(x,43,18))
logger_list[41:60] <- lapply(logger_list[41:60], function(x) sleepytime(x,44,9))

## seems to all work!
testlogger <- logger_list[[2]]
plot(logger_list[[42]]$Date_Time)
testlogger$inside

#### calculate daily stats ---------------------------------------------------------------------------------------
library(DescTools)
## only take days with full 24h recording as to not bias it!!!

### Daily HI and other properties
TbDailyStats <- data.frame(chickno = character(1), age = numeric(1), Tbmax_day = numeric(1), Tbmin_day = numeric(1),
                           Tbmax_night = numeric(1), Tbmin_night = numeric(1),
                      Tbmax_time_day = as.POSIXct(1), Tbmin_time_day = as.POSIXct(1))
index <- 1
for (i in 1:length(logger_list)){
  loggerdat <- logger_list[[i]]
  Chickno <- sub(".*_", "", names(logger_list)[i])
  for (age in as.numeric(levels(as.factor(loggerdat$age_days)))){
    dat_day <- filter(loggerdat, age_days == age & daytime == "day")
    # are there disturbances?
    disturb <- nrow(filter(dat_day, is.na(disturbance))) < nrow(dat_day)
    dat_day <- filter(dat_day, is.na(disturbance)) # filtering out disturbances
    if(nrow(dat_day) > 0){
      # only for age-days that go from hour 7 to hour 18
      if (dat_day[1,"hour"] == 7 & dat_day[nrow(dat_day),"hour"] == 18){
        TbDailyStats[index, "chickno"] <- Chickno
        TbDailyStats[index, "age"] <- age
        TbDailyStats[index, "Tbmax_day"] <- filter(dat_day, daytime == "day")[which.max(dat_day$Temp_C),"Temp_C"]
        TbDailyStats[index, "Tbmax_time_day"] <- filter(dat_day, daytime == "day")[which.max(dat_day$Temp_C),"Date_Time"]
        TbDailyStats[index, "Tbmin_day"] <- filter(dat_day, daytime == "day")[which.min(dat_day$Temp_C),"Temp_C"]
        TbDailyStats[index, "Tbmin_time_day"] <- filter(dat_day, daytime == "day")[which.min(dat_day$Temp_C),"Date_Time"]
        TbDailyStats[index, "Tbmax_night"] <- filter(dat_day, daytime == "night")[which.max(dat_day$Temp_C),"Temp_C"]
        TbDailyStats[index, "Tbmax_time_night"] <- filter(dat_day, daytime == "night")[which.max(dat_day$Temp_C),"Date_Time"]
        TbDailyStats[index, "Tbmin_night"] <- filter(dat_day, daytime == "night")[which.min(dat_day$Temp_C),"Temp_C"]
        TbDailyStats[index, "Tbmin_time_night"] <- filter(dat_day, daytime == "night")[which.min(dat_day$Temp_C),"Date_Time"]
        TbDailyStats[index, "avTb_day"] <- mean(filter(dat_day, daytime == "day")$Temp_C) # daily average
        TbDailyStats[index, "avTb_night"] <- mean(filter(dat_day, daytime == "night")$Temp_C) # daily average
        TbDailyStats[index, "avQI"] <- mean(na.omit(dat_day$QI))
        TbDailyStats[index, "avHeartrate"] <- mean(na.omit(filter(dat_day, QI == 0)$Heartrate))
        # add info if they were inside due to rain, and if there was any kind of disturbance
        TbDailyStats[index, "inside"] <- ifelse(nrow(dat_day[!is.na(dat_day$inside),"inside"]) > 0, dat_day[which(!is.na(dat_day$inside)),"inside"][[1,1]], NA)
        TbDailyStats[index, "disturbance"] <- ifelse(disturb, "yes", NA)
      }
    }
    
    dat_night <- filter(loggerdat, age_days == age & is.na(disturbance) & daytime == "night") # filtering out disturbances
    if(nrow(dat_night) > 0){
      # only for age-nights that go from hour 19 to hour 6
      if (dat_night[1,"hour"] == 19 & dat_night[nrow(dat_night),"hour"] == 6){
        TbDailyStats[index, "Tbmin_time_day"] <- filter(dat_night, daytime == "day")[which.min(dat_night$Temp_C),"Date_Time"]
        TbDailyStats[index, "Tbmax_night"] <- filter(dat_night, daytime == "night")[which.max(dat_night$Temp_C),"Temp_C"]
        TbDailyStats[index, "Tbmax_time_night"] <- filter(dat_night, daytime == "night")[which.max(dat_night$Temp_C),"Date_Time"]
        TbDailyStats[index, "Tbmin_night"] <- filter(dat_night, daytime == "night")[which.min(dat_night$Temp_C),"Temp_C"]
        TbDailyStats[index, "Tbmin_time_night"] <- filter(dat_night, daytime == "night")[which.min(dat_night$Temp_C),"Date_Time"]
        TbDailyStats[index, "avTb_night"] <- mean(filter(dat_night, daytime == "night")$Temp_C) # daily average
        # add info if they slept inside and ring
        TbDailyStats[index, "inside"] <- ifelse(nrow(dat_night[!is.na(dat_night$inside),"inside"]) > 0, dat_night[which(!is.na(dat_night$inside)),"inside"][[1,1]], NA)
      }
    }
    
    index <- index + 1
  }
}

# need to filter a bit for stuff that went wrong but seems alright all in all
TbDailyStats <- filter(TbDailyStats, !(chickno == ""))

TbDailyStats$Date <- date(TbDailyStats$Tbmax_time_day) # this format might be easier let's see
TbDailyStats$Tbmax_time_day <- as_hms(TbDailyStats$Tbmax_time_day) # same for these
TbDailyStats$Tbmin_time_day <- as_hms(TbDailyStats$Tbmin_time_day) # same for these
TbDailyStats$Tbmax_time_night <- as_hms(TbDailyStats$Tbmax_time_night) # same for these
TbDailyStats$Tbmin_time_night <- as_hms(TbDailyStats$Tbmin_time_night) # same for these
# add age in weeks
TbDailyStats$age_weeks <- trunc(TbDailyStats$age/7)
# add batch and sex and run
TbDailyStats <- left_join(TbDailyStats, dplyr::select(Chicks, c(chickno, sex, batch, run)))

# check
summary(TbDailyStats)

TbDailyStats$chickno <- as.factor(TbDailyStats$chickno)
TbDailyStats$run <- as.factor(TbDailyStats$run)


#### add weather data ---------------------------------------------------------------------------------------------------------------------------------------------------
# load kestrel data
load("../AmbientTemp_Loggers/Kestrel.RData")
# load combined Kestrel globe and black lobe data
load("../AmbientTemp_Loggers/GlobeTempCombined.RData") # object GlobeTemp
# load inside EL data
load("../AmbientTemp_Loggers/insideTemp.RData")
# inside is every five minutes but shifted from somewheere in september 
# so from 2024-09-11 11:09:00 until 2024-11-07 09:59:00 (both including) just add one minute to make it fit
inside[which(inside$DateTimereal >= as.POSIXct("2024-09-11 11:09:00", tz = "Africa/Johannesburg") & inside$DateTimereal <= as.POSIXct("2024-11-07 09:59:00", tz = "Africa/Johannesburg")),"DateTimereal"] <-
  inside[which(inside$DateTimereal >= as.POSIXct("2024-09-11 11:09:00", tz = "Africa/Johannesburg") & inside$DateTimereal <= as.POSIXct("2024-11-07 09:59:00", tz = "Africa/Johannesburg")),"DateTimereal"] + 60


# remove if there's already weather data
logger_list <- lapply(logger_list, function(df) dplyr::select(df, Date_Time:inside))
# add Kestrel and black globe their combined average and inside temp data to oddi logger data
for (i in 1:length(logger_list)){
  logger_list[[i]] <- left_join(logger_list[[i]], Kestrel[,c("DateTimereal","Temperature","GlobeTemp","WBGT", "Humidity","WindSpeed","WindChill")], by = join_by(Date_Time == DateTimereal),
                                multiple = "first") # there are some doubles in weather data, just keep first one in that case
  logger_list[[i]] <- left_join(logger_list[[i]], GlobeTemp[,c("DateTimereal","GlobeTempComb")], by = join_by(Date_Time == DateTimereal), multiple = "first") # also has globe camp pasted to it
  logger_list[[i]] <- left_join(logger_list[[i]], inside[,c("Temp_inside", "DateTimereal")], by = join_by(Date_Time == DateTimereal), multiple = "first")
}

# did it work?
testlogger <- logger_list[[7]]
summary(testlogger) # looks good!
plot(inside$DateTimereal) # no duplicates straight line that's what I like!
dups <- testlogger[duplicated(testlogger),] # no duplicates


# does weather make sense?
ggplot(filter(testlogger, Date >= "2024-09-25" & Date <= "2024-09-29"), aes(x = Date_Time))+
  geom_line(aes(y = GlobeTempComb), colour = "blue")+
  geom_line(aes(y = Temp_C), colour = "red")+
  geom_line(aes(y = Temperature), colour = "orange")+
  geom_line(aes(y = Temp_inside), colour = "green2")+
  scale_x_datetime(date_breaks = "hour",date_labels = "%H")+
  theme_minimal()
# looking good! 

### add daily weather data to daily data
load("../AmbientTemp_Loggers/DailyWeather.RData")
# remove if something is already there
TbDailyStats <- dplyr::select(TbDailyStats, chickno:run)
# remove agedate as it is meaningless here
DailyWeather <- dplyr::select(ungroup(DailyWeather), avIns_day:avWindChill_day)
# add
TbDailyStats <- left_join(TbDailyStats, DailyWeather, by = join_by("Date"))
summary(TbDailyStats) # did it work? 



## Check data
ggplot(TbDailyStats, aes(x = Date, y = avWBGT_day))+
  geom_line()+
  theme_minimal(base_size = 15)
# there's a few days missing here and there 
plot(TbDailyStats[which(TbDailyStats$chickno == levels(as.factor(TbDailyStats$chickno))[60]),"Date"])
# smooth


### Add a column with the night temperature they were actually exposed to, so a mix of GTcomb_night and inside_night, inside is different per batch!

# mark rainy nights where they slept inside (until 26th Nov for batch 1 and later for the others, but it didn't rain then anyway so let's say until 26th nov)
TbDailyStats$inside <- ifelse(TbDailyStats$avHumidity_night > 90 & TbDailyStats$Date < "2024-11-26","rain",TbDailyStats$inside) # those agree with my notes
# nights of 19 & 20 oct are missing (only batch 1, others were inside anyway)
TbDailyStats[which(TbDailyStats$batch == "1" & TbDailyStats$Date %in% c("2024-10-19","2024-10-20")),"inside"] <- "rain"


exp_night <- function(TbDailyStats) {
  with(TbDailyStats, {
    ifelse(
      is.na(inside),
      avGTcomb_night,
      ifelse(
        batch == "1", avIns3_night,
        ifelse(
          batch == "2", avIns5_night,
          ifelse(batch == "3", avIns7_night, NA_real_)
        )
      )
    )
  })
}


TbDailyStats$Ta_exp_night <- exp_night(TbDailyStats)

# check
ggplot(filter(TbDailyStats, age < 100), aes(x = Date, y = Ta_exp_night, group = batch))+
  geom_line(aes(linetype = batch))+
  geom_point(aes(colour = inside, shape = batch))+
  theme_minimal()
summary(filter(TbDailyStats, age < 100)$Ta_exp_night)
# there's a hole for batch 3, but batch two was also inside at that time so likely very similar, let's just use that
TbDailyStats$Ta_exp_night <-
  ifelse(is.na(TbDailyStats$Ta_exp_night) & TbDailyStats$batch == "3" & TbDailyStats$age < 37,
         TbDailyStats$avIns5_night, TbDailyStats$Ta_exp_night)
# there's also a hole for batch 1 for when it rained after 20th October (only had EL until then), so fill with batch 2
TbDailyStats$Ta_exp_night <-
  ifelse(is.na(TbDailyStats$Ta_exp_night) & TbDailyStats$batch == "1" & TbDailyStats$inside == "rain" & TbDailyStats$Date > "2014-10-21",
         TbDailyStats$avIns5_night, TbDailyStats$Ta_exp_night)
# and on the 20th nov we only have batch 3 inside temp data so take that for all
TbDailyStats$Ta_exp_night <-
  ifelse(is.na(TbDailyStats$Ta_exp_night) & TbDailyStats$batch %in% c("1","2") & TbDailyStats$inside == "rain" & TbDailyStats$Date == "2024-11-20",
         TbDailyStats$avIns7_night, TbDailyStats$Ta_exp_night)

ggplot(TbDailyStats, aes(x = Date, y = Ta_exp_night, colour = batch))+
  geom_line()+
  theme_minimal()

### do the same for minimal night temperature
exp_night_min <- function(TbDailyStats) {
  with(TbDailyStats, {
    ifelse(
      is.na(inside),
      minGTcomb_night,
      ifelse(
        batch == "1", minIns3_night,
        ifelse(
          batch == "2", minIns5_night,
          ifelse(batch == "3", minIns7_night, NA_real_)
        )
      )
    )
  })
}


TbDailyStats$Min_exp_night <- exp_night_min(TbDailyStats)

# check
ggplot(filter(TbDailyStats, age < 100), aes(x = Date, y = Min_exp_night, group = batch))+
  geom_line(aes(linetype = batch))+
  geom_point(aes(colour = inside, shape = batch))+
  theme_minimal()
summary(filter(TbDailyStats, age < 100)$Min_exp_night)
# there's a hole for batch 3, but batch two was also inside at that time so likely very similar, let's just use that
TbDailyStats$Min_exp_night <-
  ifelse(is.infinite(TbDailyStats$Min_exp_night) & TbDailyStats$batch == "3" & TbDailyStats$age < 37,
         TbDailyStats$minIns5_night, TbDailyStats$Min_exp_night)
# there's also a hole for batch 1 for when it rained after 20th October (only had EL until then), so fill with batch 2
TbDailyStats$Min_exp_night <-
  ifelse(is.infinite(TbDailyStats$Min_exp_night) & TbDailyStats$batch == "1" & TbDailyStats$inside == "rain" & TbDailyStats$Date > "2014-10-21",
         TbDailyStats$minIns5_night, TbDailyStats$Min_exp_night)
# and on the 20th nov we only have batch 3 inside temp data so take that for all
TbDailyStats$Min_exp_night <-
  ifelse(is.infinite(TbDailyStats$Min_exp_night) & TbDailyStats$batch %in% c("1","2") & TbDailyStats$inside == "rain" & TbDailyStats$Date == "2024-11-20",
         TbDailyStats$minIns7_night, TbDailyStats$Min_exp_night)

# for the rest of the holes (where we unfortunately don't have similar data from other loggers), make NA instead of infinite
TbDailyStats$Min_exp_night <- ifelse(is.infinite(TbDailyStats$Min_exp_night),NA,TbDailyStats$Min_exp_night)


# check
ggplot(TbDailyStats, aes(x = Date, y = Min_exp_night, colour = batch))+
  geom_line()+
  theme_minimal()
summary(TbDailyStats$Min_exp_night)


#### save ---------------------------------------------------------------------------------------------------------------------------------
save(logger_list, TbDailyStats, file = "TbData.RData")


