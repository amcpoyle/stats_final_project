library(tidyverse)
library(ggridges)
library(gridExtra)

swipes <- read.csv("/home/amcpoyle192/stats/final_project/swipes_w_day_time.csv")

swipes$new_time <- strptime(swipes$new_time, format="%H:%M:%S")
swipes$new_time <- as.POSIXct(swipes$new_time, format="%I:%M")


# format(strptime(times, "%I:%M %p"), format="%H:%M:%S")

# swipes$new_time <- as.POSIXct(swipes$new_time, format="%I:%M")

dinner <- subset(swipes, meal == 'Dinner')

thorne <- subset(swipes, location == 'THAero1')
moulton <- subset(swipes, location == 'MUAero01')

dinner_thorne <- subset(dinner, location == 'THAero1')
dinner_moulton <- subset(dinner, location == 'MUAero01')


dinner_density <- ggplot(dinner, aes(x=new_time)) +
  geom_density() +
  ggtitle("Overall Dinner Swipe Density")

dinner_thorne_density <- ggplot(dinner_thorne, aes(x=new_time)) +
  geom_density() +
  ggtitle('Thorne Dinner Swipe Density')

dinner_moulton_density <- ggplot(dinner_moulton, aes(x=new_time)) +
  geom_density() +
  ggtitle("Moulton Dinner Swipe Density")

dinner_refined <- subset(dinner, (location == 'THAero1') | (location == 'MUAero01'))

dinner_density2 <- ggplot(dinner_refined, aes(x=new_time, group=location, fill=location)) +
  geom_density(adjust=1.5, alpha=0.4) +
  xlab("Time") +
  ylab("Density") +
  ggtitle("Overlay of Dinner Swipe Time Density")


dinner_ridge <- ggplot(dinner_refined, aes(x=new_time, y=location, fill=location)) +
  geom_density_ridges() +
  theme_ridges() +
  theme(legend.position = "None") +
  xlab("Time") +
  ylab("Location")

lunch <- subset(swipes, meal=='Lunch')
lunch_refined <- subset(lunch, (location == 'THAero1') | (location == 'MUAero01'))

lunch_density <- ggplot(lunch, aes(x=new_time)) +
  geom_density() +
  ggtitle("Overall Lunch Swipe Density") +
  xlab("Time") +
  ylab("Density")

lunch_density2 <- ggplot(lunch, aes(x=new_time, group=location, fill=location)) +
  geom_density(adjust=1.5, alpha=0.4) +
  ggtitle("Overlay of Lunch Swipe Time Density") +
  xlab("Time") +
  ylab("Density")

lunch_ridge <- ggplot(lunch, aes(x=new_time, y=location, fill=location)) +
  geom_density_ridges() +
  theme_ridges() +
  theme(legend.position = "None") +
  xlab("Time")

supers <- subset(swipes, meal == 'Late Night Snack')

supers_density <- ggplot(supers, aes(x=new_time)) +
  geom_histogram(bins=100) +
  ggtitle("Supers Swipe Distribution")

lunch_plots <- grid.arrange(lunch_density, lunch_ridge)
dinner_plots <- grid.arrange(dinner_density, dinner_ridge)
