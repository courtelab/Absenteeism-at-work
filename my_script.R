library(tidyverse)
library(data.table)
library(lubridate)
library(stats)
library(RColorBrewer)
library(caret)

#### RMSE CALCULATION ####
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

dl <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00445/Absenteeism_at_work_AAA.zip", dl)


my_data <- str_split_fixed(readLines(unzip(dl, "Absenteeism_at_work.csv"),
                                     encoding = "latin1"),
                           "\\;",
                           21)

colnames(my_data) <- my_data[1,]
my_data <- my_data[-1,]

my_data <- data.frame(my_data) %>%
  filter(Month.of.absence != 0)

my_data[,1:length(my_data)] <- lapply(my_data[,1:length(my_data)], FUN = function(y) {as.numeric(y)})

#### Naive model

mu <- mean(my_data$Absenteeism.time.in.hours)

my_data <- my_data %>% 
  mutate(pred_rmse = mu)
RMSE(my_data$pred_rmse, my_data$Absenteeism.time.in.hours)


#### Distance, expense, age and Body mass index effects with knn

fit_lm <- train(Absenteeism.time.in.hours ~ Transportation.expense +
                   Distance.from.Residence.to.Work +
                   Age +
                   Body.mass.index +
                   Work.load.Average.day.,
                method = "lm",
                data = my_data)


pred_lm <- predict(fit_lm, my_data)
RMSE(pred_lm, my_data$Absenteeism.time.in.hours)

my_data <- cbind(my_data, pred_lm)
my_data <- my_data %>% 
  mutate(res = Absenteeism.time.in.hours - pred_lm)

#### random forest with factors


fit_rf <- train(Absenteeism.time.in.hours ~ as.factor(Reason.for.absence) +
                  as.factor(ID),
                method = "rf",
                data = my_data)
pred_rf <- predict(fit_rf, my_data)
RMSE(pred_rf, my_data$Absenteeism.time.in.hours)

plot(fit_rf)
fit_rf$bestTune

