# 
# 
rm(list = ls(all=TRUE))  
library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)

full.data <- read.csv("joined.csv", stringsAsFactors= TRUE) %>% 
  mutate(date = ymd(Date), field.season=as.factor(year)) 
full.data$band_size <- substr(full.data$Band.ID, start = 1, stop = 2)
full.data$band_sequence <- as.integer(substrRight(as.character(full.data$Band.ID), 3))
write.csv("joined.csv")

# full.data <- subset(full.data, days_diff >= 300 | is.na(days_diff)) 

# Join effort data with data set
# effort <- read.csv('OpWall band data effort.csv')
# effort <- effort %>% mutate(open = sprintf("%04d",open), close = sprintf("%04d",close)) %>% 
#   mutate(open= sub("([[:digit:]]{2,2})$", ":\\1", open), close= sub("([[:digit:]]{2,2})$", ":\\1", close) ) %>% 
#   mutate(open= dmy_hm(paste(Date, open, sep=" ")), close= dmy_hm(paste(Date, close, sep=" ")),
#          hours= close-open, 
#          net_hours_day= hours*Nets,
#          Date= dmy(Date)) %>% 
#   group_by(Date) %>% 
#   mutate(total_net_hours_day=sum(net_hours_day)) %>% ungroup()
# 
# to_join <- data.frame(Loc=effort$Loc, Date=effort$Date,
#                       total_net_hours_day=effort$total_net_hours_day)
# data <- merge(data, to_join, by=c('Loc', 'Date'), all.x=T)
# write.csv(data, "joined.csv")


##### Data cleaning

# Are there any NA values for species or band number?
check1 <- full.data %>% filter(is.na(Band.ID), is.na(species)) %>% 

# Has any bird with same band number been identified as different species?
check2 <- full.data %>% group_by(Band.ID) %>% dplyr::filter(length(unique(species)) > 1) %>% View() 

# Are there still any same-season recaptures in the data?
data.table::setDT(full.data)[, days_diff := c(NA, round(difftime(date[-1L], date[-.N], 
                                                                 units='days'))), by= Band.ID]
check3 <- full.data %>%  filter(days_diff < 300) %>% View()

# Check the band number series
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


ggplot(full.data) +
  geom_point(aes(as.integer(substrRight(as.character(Band.ID), 3)), band_size)) +
  facet_grid(rows = vars(band_size), 
             scales = 'free')

