library(tidyverse)
library(tidymodels)
library(vroom)
library(DataExplorer)
library(ggplot2)
library(patchwork)
library(poissonreg)
library(bestglm)

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


## clean data
train_data$weather <- as.numeric(as.character(train_data$weather))
train_data$weather[train_data$weather == 4] <- 3
train_data <- train_data |>
  mutate(weather)


## Setup and Fit the Linear Regression Model
linmod <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression") |>
  fit(formula = count~weather+windspeed, data=train_data)

## Generate Predictions Using Linear Model
bike_predict <- predict(linmod, new_data = test_data)
bike_predict

     
## Format predictions for Kaggle
kaggle <- bike_predict |>
  bind_cols(test_data) |>
  select(datetime, .pred) |>
  rename(count = .pred) |>
  mutate(count = pmax(0,count)) |>
  mutate(datetime = as.character(format(datetime)))

##write out file
vroom_write(x = kaggle, file = "./BikeSharePreds.csv", delim=",")


##Poisson Regression Model
pois_model <- poisson_reg() |>
  set_engine("glm") |>
  set_mode("regression") |> 
  fit(formula= count~windspeed + as.factor(weather), data=train_data)

##Generate Predictions Using Linear Model
bike_predict2 <- predict(pois_model, new_data = train_data)
bike_predict2

## Format Pois Predictions for Kaggle
pois_kaggle <- bike_predict2 |>
  bind_cols(test_data) |>
  select(datetime, .pred) |>
  rename(count = .pred) |>
  mutate(count = pmax(0,count)) |>
  mutate(datetime = as.character(format(datetime)))

##write out file
vroom_write(x = pois_kaggle, file = "./BikePoisPreds.csv", delim=",")

