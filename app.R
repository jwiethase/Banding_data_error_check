# 
# 
rm(list = ls(all=TRUE))  
library(dplyr)
library(ggplot2)
library(lubridate)

data <- read.csv('OpWall_banding_data.csv')
effort <- read.csv('OpWall band data effort.csv')

effort <- effort %>% mutate(open = sprintf("%04d",open), close = sprintf("%04d",close)) %>% 
  mutate(open= sub("([[:digit:]]{2,2})$", ":\\1", open), close= sub("([[:digit:]]{2,2})$", ":\\1", close) ) %>% 
  mutate(open= dmy_hm(paste(Date, open, sep=" ")), close= dmy_hm(paste(Date, close, sep=" ")),
         hours= close-open, 
         net_hours_day= hours*Nets) %>% 
  group_by(Date) %>% 
  mutate(total_net_hours_day=sum(net_hours_day)) %>% ungroup()

