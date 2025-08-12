# this file will answer some of the more "basic" questions for this dataset
library(tidyverse)
library(lubridate)
library(dplyr)

swipes <- read.csv("/home/amcpoyle192/stats/final_project/swipes_updated1.csv")
students <- read.csv("/home/amcpoyle192/stats/final_project/students_updated1.csv")

# 1) Do upperclassmen prefer Thorne over Moulton? Or is it just a function of distance from dorm?
# ... and are these ultimately the same thing?
# Actually we don't know the real dorms...

first_years <- subset(swipes, class_year == 2015)
sophomores <- subset(swipes, class_year == 2014)
juniors <- subset(swipes, class_year == 2013)
seniors <- subset(swipes, class_year == 2012)
superseniors <- subset(swipes, class_year == 2011)

# Upperclassmen info
all_upperclassmen <- subset(swipes, class_year < 2015)

# Dinner
upper_dinner <- subset(all_upperclassmen, meal == "Dinner")
upper_thorne_dinner <- subset(upper_dinner, location == "THAero1")
upper_moulton_dinner <- subset(upper_dinner, location == "MUAero01")

prob_upper_thorne_dinner <- nrow(upper_thorne_dinner)/nrow(upper_dinner)
prob_upper_moulton_dinner <- nrow(upper_moulton_dinner)/nrow(upper_dinner)

# Lunch
upper_lunch <- subset(all_upperclassmen, meal == "Lunch")
upper_thorne_lunch <- subset(upper_lunch, location == "THAero1")
upper_moulton_lunch <- subset(upper_lunch, location == "MUAero01")
upper_fastrack <- subset(upper_lunch, location == "SUAero01") # did they have fast track back in the day?
upper_muexpress_lunch <- subset(upper_lunch, location == "MUAero02")


prob_upper_thorne_lunch <- nrow(upper_thorne_lunch)/nrow(upper_lunch)
prob_upper_moulton_lunch <- nrow(upper_moulton_lunch)/nrow(upper_lunch)
prob_upper_fastrack <- nrow(upper_fastrack)/nrow(upper_lunch)
prob_upper_muexpress_lunch <- nrow(upper_muexpress_lunch)/nrow(upper_lunch)

# Breakfast
upper_breakfast <- subset(all_upperclassmen, meal == "Breakfast")
upper_thorne_breakfast <- subset(upper_breakfast, location == "THAero1")
upper_moulton_breakfast <- subset(upper_breakfast, location == "MUAero01")

prob_upper_thorne_breakfast <- nrow(upper_thorne_breakfast)/nrow(upper_breakfast)
prob_upper_moulton_breakfast <- nrow(upper_moulton_breakfast)/nrow(upper_breakfast)



# The Flinner Effect - it does not exist?
swipes_sorted <- swipes %>% arrange(date_time)

dinner_sorted <- subset(swipes_sorted, meal == "Dinner")
thorne_dinner <- subset(dinner_sorted, location == "THAero1")


thorne_dinner$new_time <- strptime(thorne_dinner$time, format="%H:%M:%S")

# a histogram of data swipes
# TODO: had to convert time to numeric somehow... kind of horrible to intepret
# time_hist <- hist(thorne_dinner$new_time, breaks=25)

# just get non sunday dinner info
find_day <- function(date) {
  # get the year of the date
  y <- as.numeric(year(date))
  m <- as.numeric(month(date))
  d <- as.numeric(day(date))
  
  # get last 2 digits of year
  # it is 11 in our case
  y_two <- y %% 100
  no_remainder <- floor(y_two/4)
  add_back <- y_two + no_remainder
  
  year_code <- add_back %% 2
  
  month_codes = list()
  month_codes[1] = 0
  month_codes[2] = 3
  month_codes[3] = 3
  month_codes[4] = 6
  month_codes[5] = 1
  month_codes[6] = 4
  month_codes[7] = 6
  month_codes[8] = 2
  month_codes[9] = 5
  month_codes[10] = 0
  month_codes[11] = 3
  month_codes[12] = 5
  
  if (m == 1) {
    month_code <- 0
    
  } else if (m == 2) {
    month_code <- 3
    
  } else if (m == 3) {
    month_code <- 3
    
  } else if (m == 4) {
    month_code <- 6
    
  } else if (m == 5) {
    month_code <- 1
    
  } else if (m == 6) {
    month_code <- 4
    
  } else if (m == 7) {
    month_code <- 6
    
  } else if (m == 8) {
    month_code <- 2
    
  } else if (m == 9) {
    month_code <- 5
    
  } else if (m == 10) {
    month_code <- 0
    
  } else if (m == 11) {
    month_code <- 3
    
  } else if (m == 12) {
    month_code <- 5
  } else {
    month_code <- "error"
  }
  
  century_code <- 6 # for the 2000s
  
  date_num <- d
  
  # not a leap year so do not need
  leap_year <- FALSE
  if (y %% 4 == 0) {
    leap_year <- TRUE
  }
  
  # (Year Code + Month Code + Century Code + Date Number – Leap Year modifier) mod 7
  if (leap_year == FALSE) {
    value <- (year_code + month_code + century_code + d) %% 7
  }
  else {
    if ((m == 1) | (m == 2)) {
      value <- (year_code + month_code + century_code + d - 1) %% 7
    }
    else {
      value <- (year_code + month_code + century_code + d) %% 7
    }
  }
  
  if (value == 0) {
    return("Sunday")
  } else if (value == 1) {
    return("Monday")
  } else if (value == 2) {
    return("Tuesday")
  } else if (value == 3) {
    return("Wednesday")
  } else if (value == 4) {
    return("Thursday")
  } else if (value == 5) {
    return("Friday")
  } else if (value == 6) {
    return("Saturday")
  } else {
    return('error')
  }
}

swipes_sorted$day <- lapply(swipes_sorted$date_time, find_day)
swipes_sorted$day <- as.character(swipes_sorted$day)
write.csv(swipes_sorted, "swipes_w_day.csv")

thorne_dinner$day <- lapply(thorne_dinner$date_time, find_day)

no_flinner <- subset(thorne_dinner, day != "Sunday")
flinner <- subset(thorne_dinner, day == "Sunday")

# no_flinner_dinner_hist <- hist(no_flinner$new_time, breaks=25)
# flinner_dinner_hist <- hist(flinner$new_time, breaks=25)

# TODO: do for Moulton, maybe different


# mapping <- compact(mapping)


# thorne_dinner$new_format_mins <- as.numeric(format(time, format='%M'))

# hist(thorne_dinner$time_combined, breaks=25)

moulton_dinner <- subset(dinner_sorted, location == "MUAero01")
moulton_dinner$new_time <- strptime(moulton_dinner$time, format="%H:%M:%S")
moulton_dinner$day <- lapply(moulton_dinner$date_time, find_day)

table(which(moulton_dinner$day))

# IS THERE A DIFFERENCE BETWEEN DINNER MOVEMENTS ON THE WEEKEND?

moulton_dinner_nums <- moulton_dinner %>% count(day, sort = TRUE, name= "day_frequency")

# ARE FIRST-YEARS MORE LIKELY TO GO TO THORNE OR MOULTON ON SUNDAYS
# FIRST-YEAR DINNER PROBS

swipes_sorted$day <- lapply(swipes_sorted$date_time, find_day)

fy_all <- subset(swipes_sorted, first_dorm == current_dorm)
fy_dinner <- subset(fy_all, meal == "Dinner")

days_of_week <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday',
                  'Saturday', 'Sunday')

for (d in days_of_week) {
  # find the probability first-years go to thorne
  t_total <- nrow(subset(fy_dinner, (location == "THAero1") & (day == d)))
  day_total <- nrow(subset(fy_dinner, (meal == 'Dinner') & (day == d)))
  prob_t <- t_total/day_total
  
  m_total <- nrow(subset(fy_dinner, (location == "MUAero01") & (day == d)))
  prob_m <- m_total/day_total

  print(paste('Probability first-years go to Thorne on ', d, ': ', prob_t))
  print(paste('Probability first-years go to Moulton on ', d, ': ', prob_m))
}