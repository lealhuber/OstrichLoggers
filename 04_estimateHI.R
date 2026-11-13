#### Estimate heterothermy index
### Lea Huber


knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)
pacman::p_load(tidyverse,knitr,ggpubr,viridis,ggExtra,hms,lubridate,lme4,DHARMa,GGally,MuMIn,kableExtra)

## load data
load("../../intermediate/Oddi/TbData.RData") # pre filtered body temperature data of all chicks
load("../../intermediate/Oddi/ChickData.RData") # some data on each chick
load("../../intermediate/ModalTb/btw_Tmod.RData") # across individual weekly body temperature modes

## estimate HI

# make day and night separate columns
Tmod_week_daytime2 <- pivot_wider(Tmod_week_daytime, names_from = daytime, values_from = modTb, names_prefix = "modTb_")

# HI function (from Schielzeth & Nakagava)
calcHI <- function(Tbopt,Tb_vec){
  HI = sqrt(sum((Tbopt - Tb_vec)^2)/(length(Tb_vec)-1))
}

# function for calculating HI using modTb_day, filtering out disturbances
get_HI <- function(df, mode, time){
  df <- df %>% filter(is.na(disturbance) & daytime == time) %>% group_by(age_days) %>% 
    summarise(HI = calcHI(Tmod_week_daytime2[match(age_weeks, Tmod_week_daytime2$age_weeks),mode],Temp_C)) #
  df
}

# for day
HI_day <- lapply(logger_list, function(df) get_HI(df,"modTb_day", "day"))
HI_day <- bind_rows(HI_day, .id = "chickno")  # make into one df
HI_day$chickno <- sub(".*_", "", HI_day$chickno)
HI_day <- rename(HI_day, HI_day = HI)

# for night
HI_night <- lapply(logger_list, function(df) get_HI(df,"modTb_night", "night"))
HI_night <- bind_rows(HI_night, .id = "chickno")  # make into one df
HI_night$chickno <- sub(".*_", "", HI_night$chickno)
HI_night <- rename(HI_night, HI_night = HI)

# there went something wrong on the last day of the sick chicks and the value is Inf, does not matter so I will just delete those
HI_day <- filter(HI_day, !HI_day == Inf)
HI_night <- filter(HI_night, !HI_night == Inf)

# add HI to daily stats
TbDailyStats <- left_join(TbDailyStats, HI_day, by = join_by(chickno, age == age_days))
TbDailyStats <- left_join(TbDailyStats, HI_night, by = join_by(chickno, age == age_days))

save(TbDailyStats, file = "../../intermediate/Heterothermy/Daily_summary.RData")

