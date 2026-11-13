#### Estimate modal temperatures
### Lea Huber


knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)
pacman::p_load(readxl,tidyverse,knitr,ggpubr,viridis,ggExtra,hms,lubridate,DescTools,MCMCglmm)



load("../../intermediate/Oddi/TbData.RData") # pre filtered body temperature data of all chicks
load("../../intermediate/Oddi/ChickData.RData") # some data on each chick

## reshape
weekTb_df <- logger_list %>% lapply(subset, is.na(disturbance), select = c(Temp_C, age_weeks, daytime)) %>%
  bind_rows(.id = "chickno")
weekTb_df$chickno <- sub("MHL...._", "", weekTb_df$chickno) # make nice chickno

weekTb_df$batch <- unname(unlist(Chicks[match(weekTb_df$chickno, Chicks$chickno), "batch"])) # add batch info 

test <- weekTb_df %>% filter(!((age_weeks == 1 & daytime == "night" & batch %in% c("1","2")) | (age_weeks == 2 & daytime == "night" & batch == "2"))) # filter out some extreme events that likely put mode farther from theoretical optimum
Tmod_week_daytime <- weekTb_df %>% filter(!((age_weeks == 1 & daytime == "night" & batch %in% c("1","2")) | (age_weeks == 2 & daytime == "night" & batch == "2") | age_weeks == 0)) %>%
  group_by(age_weeks,daytime) %>%
  summarise(modTb = posterior.mode(as.mcmc(Temp_C),adjust = 0.4))

# plot
ggplot(Tmod_week_daytime, aes(x = age_weeks, y = modTb, linetype = daytime))+
  geom_line()+
  theme_pubr()

save(Tmod_week_daytime, file = "../../intermediate/ModalTb/btw_Tmod.RData")

