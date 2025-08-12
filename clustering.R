library(tidyverse)
library(plyr)
library(lubridate)


swipes <- read.csv("swipes_w_day.csv")

unique_students <- unique(swipes$student_id)

new_ids <- rep(NA, length=length(unique_students))

# remap student ids to be 1-1620 for easy storage in a matrix

for (i in 1:length(unique_students)) {
  new_ids[i] <- i
}

swipes$new_id <- mapvalues(swipes$student_id, from=unique_students, to=new_ids)

swipes$date_time <- as_datetime(swipes$date_time)

# head(swipes,1)['date_time'] + second(25)

adj_matrix <- matrix(0, nrow=length(unique_students), ncol=length(unique_students))

for (i in 1:nrow(swipes)) {
  if (i %% 1000 == 0) {
    print(i)
  }
  row <- swipes[i,]
  current_student <- row$new_id
  current_time <- row$date_time
  prev <- current_time - second(25)
  forward <- current_time + second(25)
  
  interval <- subset(swipes, date_time >= prev)
  interval <- subset(interval, date_time <= forward)
  
  for (j in 1:nrow(interval)) {
    interval_row <- interval[j, ]
    new_student <- interval_row$new_id
    if (current_student != new_student) {
      adj_matrix[current_student][new_student] = adj_matrix[current_student][new_student] + 1
    }
  }
}