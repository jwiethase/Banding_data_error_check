# 
# 
rm(list = ls(all=TRUE))  
library(dplyr)
library(ggplot2)

data <- read.csv('OpWall_banding_data.csv')
effort <- read.csv('OpWall band data effort.csv')