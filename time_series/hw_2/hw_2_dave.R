library(readxl)
library(lubridate)
library(tidyverse)
setwd("time_series/hw_2")
excel_sheets("../F-179.xlsx")
# rain <- read_excel("F-179.xlsx", sheet = "Rain")
# tide <- read_excel("F-179.xlsx", sheet = "Tide")

# read only the well sheet from excel
well <- read_excel("../F-179.xlsx", sheet = "Well")

#well <- read_excel("F-179.xlsx", sheet = "Well", col_types = c("date", "date", "skip", "skip", "skip", "numeric"))
# summary(rain)
# summary(tide)

# look at 5 number summary of dataframe
summary(well)

# histogram plot of well
hist(well$Corrected)


# Question 1 ####
well %>% 
  group_by(year(date), month(date)) %>% 
  summarise(avg = mean(Corrected)) %>% 
  unite(date, 1, 2, sep = "-") %>% 
  mutate(date = zoo::as.yearmon(date))

+well_2 <- well %>% 
  # create a new variable which is an integer for the hour of each time
  mutate(time_2 = month(time)) %>% 
  # merge the data and newly created datetime variables into a variable called datetime
  unite(datetime, date, time_2, sep = " ", remove = FALSE) %>%
  # convert the character datetime variable to an R recognized datetime format
  mutate(datetime = ym(datetime)) %>%
  # select only the new datetime variable and rename the Corrected variable to depth
  select(datetime, depth = Corrected) 

well_3 <- well_2 %>% 
  # group by will find the smallest increment which is hour in datetime and prep for grouping functions
  group_by(month()) %>%
  # you can create new variable which aggregate the values of datetime using the mean and stdev functions
  summarise(avg = mean(depth))

mean(well_3$avg)
sd(well_3$avg)

# Question 2 ####
# create a sequnce of dates in the year/month/day hour format incremented by each hour
i <- seq(ymd_h("2007-10-01 01"), ymd_h("2018-06-04 10"), by = '1 hour')
# find the difference between the new date sequence and the original unique hour values for our data
diff <- length(i) - length(well_3$datetime)

# Question 3 ####
# create a gg object using the well_3 dataframe with aesthetics: x = datetime and y = avg
ggplot(well_3, aes(datetime, avg)) +
  # draw a line plot using the above defined aesthetics
  geom_line(color = "blue") +
  # drop some of the ugly R thematic elements for a simple look
  theme_bw() +
  # label the axes and add a title
  labs(x = "Date And Time (in hours)", y = "Avg Depth of Well (in feet)", title = "Avg Depth of Well From 2007-2018")


# faceted time plot by year
ggplot(well_3, aes(datetime, avg)) +
  # draw a line plot using the above defined aesthetics
  geom_line(color = "blue") +
  # drop some of the ugly R thematic elements for a simple look
  theme_bw() +
  # label the axes and add a title
  labs(x = "Date And Time (in hours)", y = "Avg Depth of Well (in feet)", title = "Avg Depth of Well From 2007-2018") +
  facet_wrap(~ year(datetime), scales = "free_x")


# messing around with timeseries objects and functions
well_ts <- ts(well_3$avg, start = c(1,1), frequency = 24*365)         decomp_stl <- stl(well_ts, s.window = 7)
plot(decomp_stl)
