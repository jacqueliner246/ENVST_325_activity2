## ENVST 325 Activity 1
## Author: Jacqueline Reynaga
## Date Created: 2-05-26
## Date Last Updated: 2-05-26

# in-class prompts
# libraries
library(lubridate)
library(dplyr)

# read in data
streamH <- read.csv("/cloud/project/activtiy02/stream_gauge.csv")
siteInfo <- read.csv("/cloud/project/activtiy02/site_info.csv")

# add formatted date column and year column
streamH$dateF <- ymd_hm(streamH$datetime)
streamH$year <- year(streamH$dateF)

# create data frame for heights only at peace river
peaceH <- streamH %>% 
  filter(siteID == 2295637)

plot(peaceH$dateF, peaceH$gheight.ft, 
     type = "b", 
     pch = 19, 
     xlab = "Date", 
     ylab = "Stage Height (ft)")

# join data frames together
# designate data frames as right and left 
## left join keeps left data frame the same and adds on from the right data frame
### fills in missing data with NA, repeats multiple observations
## full join keeps all data from both data frames and fills in NA values
## inner join only keeps ids with both values
floods <- full_join(streamH, siteInfo, by = "siteID")

# grouping
height.ave <- floods %>% 
  group_by(names) %>% 
  summarize(mean.height = mean(gheight.ft))

floods$doy <- yday(floods$dateF)

height.day <- floods %>% 
  group_by(names, doy) %>% 
  summarise(mean.height = mean(gheight.ft))

max.cat <- floods %>% 
  group_by(names) %>% 
  filter(gheight.ft >= major.ft) %>% 
  summarize(n.major = n())

# earliest date that each river reached flood stage
early.flood <- floods %>% 
  group_by(names) %>% 
  filter(gheight.ft >= flood.ft) %>% 
  summarise(earliest.flood.date = min(dateF))

# homework prompts
# prompt 1
# get stream stage data for each stream
peaceRiver <- floods %>% 
  filter(siteID == 2256500)
fisheatingCreek <- floods %>% 
  filter(siteID == 2295637)
santaFeRiver <- floods %>% 
  filter(siteID == 2322500)
withlacoocheeRiver <- floods %>% 
  filter(siteID == 2312000)

