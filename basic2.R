library(tidyverse)


swipes <- read.csv("/home/amcpoyle192/stats/final_project/swipes_w_day_time.csv")
dinner <- subset(swipes, meal == 'Dinner')

fy <- subset(dinner, class_year == 2015)
sophomore <- subset(dinner, class_year == 2014)
junior <- subset(dinner, class_year == 2013)
senior <- subset(dinner, class_year == 2012)

fy.thorne.prob <- nrow(subset(fy, location == 'THAero1'))/nrow(fy)
sophomore.thorne.prob <- nrow(subset(sophomore, location == 'THAero1'))/nrow(sophomore)
junior.thorne.prob <- nrow(subset(junior, location == 'THAero1'))/nrow(junior)
senior.thorne.prob <- nrow(subset(senior, location == 'THAero1'))/nrow(senior)

classes <- c("First Year", 'Sophomore', 'Junior', 'Senior')
probs <- c(fy.thorne.prob, sophomore.thorne.prob, junior.thorne.prob, senior.thorne.prob)

data <- data.frame(
  class_year = classes,
  probability_thorne = probs
)

data$class_year <- factor(data$class_year, levels = data$class_year)

dinner.thorne.barplot <- ggplot(data, aes(x=class_year, y=probability_thorne, fill=probability_thorne)) +
  geom_bar(stat='identity', width=0.5) +
  xlab("Class") +
  ylab("Probability") +
  scale_fill_gradientn(colors=cm.colors(10))