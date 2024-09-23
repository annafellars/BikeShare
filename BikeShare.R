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
train_data <- train_data |>
  select(-casual, -registered) |>
  mutate(count = log(count))
  

cleaning_recipe <- recipe(count~., data = train_data) |>
  step_mutate(weather = ifelse(weather == 4,3,weather)) |>
  step_mutate(weather = factor(weather, labels = c("sunny", "cloudy", "stormy"))) |>
  step_date(datetime, features = "month") |>
  step_time(datetime, features = "hour") |>
  step_mutate(season = factor(season, labels = c("spring", "summer", "fall", "winter")))
prepped_recipe <- prep(cleaning_recipe)
bake(prepped_recipe, new_data = train_data)



## Define Linear Regression Model
linmod <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression")

## Combine into a Workflow and fit
bike_workflow <- workflow() |>
  add_recipe(cleaning_recipe) |>
  add_model(linmod) |>
  fit(data=train_data)

## Run all the steps on test data
log_preds <- predict(bike_workflow, new_data = test_data)
linear_preds <- log_preds |>
  mutate(.pred = exp(.pred))
linear_preds
     
## Format predictions for Kaggle
kaggle <- linear_preds|>
  bind_cols(test_data) |>
  select(datetime, .pred) |>
  rename(count = .pred) |>
  mutate(count = pmax(0,count)) |>
  mutate(datetime = as.character(format(datetime)))

##write out file
vroom_write(x = kaggle, file = "./BikeSharePreds3.csv", delim=",")


##Poisson Regression Model 
pois_model <- poisson_reg() |>
  set_engine("glm") |>
  set_mode("regression")

##Combine into a workflow and fit
pois_workflow <- workflow() |>
  add_recipe(cleaning_recipe) |>
  add_model(pois_model) |>
  fit(data = train_data)

##Run all the steps on test data
pois_log_preds <- predict(pois_workflow, new_data = test_data)
pois_preds <- pois_log_preds |>
  mutate(.pred = exp(.pred))

## Format Pois Predictions for Kaggle
pois_kaggle <- pois_preds |>
  bind_cols(test_data) |>
  select(datetime, .pred) |>
  rename(count = .pred) |>
  mutate(count = pmax(0,count)) |>
  mutate(datetime = as.character(format(datetime)))

##write out file
vroom_write(x = pois_kaggle, file = "./BikePoisPreds2.csv", delim=",")

