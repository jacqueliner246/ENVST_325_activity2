## ENVST 325 Activity 1
## Author: Jacqueline Reynaga
## Date Created: 2-05-26
## Date Last Updated: 2-06-26

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
min.flood.date <- floods %>% 
  filter(gheight.ft >= flood.ft) %>% 
  group_by(names) %>%
  summarise(earliest.flood.date = min(dateF))

# homework prompts
# prompt 1
# plot stream stage data for each stream
peaceRiver <- streamH %>% 
  filter(siteID == 2295637)
plot(peaceRiver$dateF, peaceRiver$gheight.ft, 
     type = "b", 
     pch = 19, 
     xlab = "Date", 
     ylab = "Stage Height of Peace River (ft)")

fisheatingCreek <- streamH %>% 
  filter(siteID == 2256500)
plot(fisheatingCreek$dateF, fisheatingCreek$gheight.ft, 
     type = "b", 
     pch = 19, 
     xlab = "Date", 
     ylab = "Stage Height of Fisheating Creek (ft)")

santaFeRiver <- streamH %>% 
  filter(siteID == 2322500)
plot(santaFeRiver$dateF, santaFeRiver$gheight.ft, 
     type = "b", 
     pch = 19, 
     xlab = "Date", 
     ylab = "Stage Height of Santa Fe River (ft)")

withlacoocheeRiver <- streamH %>% 
  filter(siteID == 2312000)
plot(withlacoocheeRiver$dateF, withlacoocheeRiver$gheight.ft, 
     type = "b", 
     pch = 19, 
     xlab = "Date", 
     ylab = "Stage Height of Withlacoochee River (ft)")


# prompt 2
# earliest date of occurance for each flood category for each river
min.action.date <- floods %>% 
  group_by(names) %>%
  filter(gheight.ft >= action.ft) %>% 
  summarise(earliest.action.date = min(dateF))

min.moderate.date <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= moderate.ft) %>% 
  summarise(earliest.moderate.date = min(dateF))

min.major.date <- floods %>%
  group_by(names) %>%
  filter(gheight.ft >= major.ft) %>% 
  summarise(earliest.major.date = min(dateF))

full.flood.cat <- left_join(min.action.date, min.flood.date, by = "names")
full.flood.cat <- left_join(full.flood.cat, min.moderate.date, by = "names")
full.flood.cat <- left_join(full.flood.cat, min.major.date, by = "names")
  
# prompt 3
# river that has the highest stage height above major flood category
max.above.major <- floods %>%  
  group_by(names, major.ft) %>% 
  summarize(max.height = max(gheight.ft))
max.above.major$height.above.major = max.above.major$max.height - max.above.major$major.ft

# prompt 4
# ifelse and hist r documentation and examples
?ifelse()
flooded <- floods %>% 
  group_by(names) %>% 
  mutate(flooded = ifelse(gheight.ft > major.ft, TRUE, FALSE)) %>% 
  ungroup()

?hist()
hist(peaceRiver$gheight.ft, xlab = "Stage Height (ft)", main = "Histogram of Peace River Stage Heights")
