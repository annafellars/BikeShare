library(tidyverse)
library(tidymodels)
library(vroom)
library(DataExplorer)
library(ggplot2)
library(patchwork)

### read in data
test_data <- vroom("test.csv")
train_data <- vroom("train.csv")


## EDA
glimpse(train_data)
plot_intro(train_data)
plot_correlation(train_data)
plot_bar(train_data)
plot_histogram(train_data)


## graphs

#barplot of weather
graph1 <- ggplot(train_data, aes(x = factor(weather), y = count, fill = factor(weather))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("1" = "orange", "2" = "lightblue", "3" = "navy", "4" = "gray"),
                    labels = c("1" = "Sunny", "2" = "Cloudy", "3" = "Stormy", "4" = "Bad Storm")) +
  labs(x = "Weather", y = "Number of Total Rentals")

#scatterplot of count v temp + season
graph2 <- ggplot(train_data, aes(x = temp, y = count, color = factor(season))) +
  geom_point()+
  scale_color_manual(values = c("1" = "pink", "2" = "yellow", "3" = "darkorange", "4" = "lightblue"),
                     labels = c("1" = "Spring", "2" = "Summer", "3" = "Fall", "4" = "Winter")) +
  labs(x = "Tempurature (Celsius)", y = "Number of Total Rentals")

#barplot of working day
graph3 <- ggplot(train_data, aes(x = factor(workingday), y = count, fill = factor(weather))) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("1" = "orange", "2" = "lightblue", "3" = "navy", "4" = "gray"),
                    labels = c("1" = "Sunny", "2" = "Cloudy", "3" = "Stormy", "4" = "Bad Storm")) +
  scale_x_discrete(labels = c("0" = "Weekend", "1" = "Workday")) + 
  labs(x = "Day Type", y = "Number of Total Rentals", fill = "Weather")
  

#scatterplot of wind v count
graph4 <- ggplot(train_data, aes(x = windspeed, y = count, color = humidity)) +
  geom_point()+
  labs(x = "Wind Speed", y = "Number of Total Rentals")


(graph1 + graph2) / (graph3 + graph4)


     


