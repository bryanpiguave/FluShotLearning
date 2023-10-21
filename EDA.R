library(tidyverse)
library(randomForest)
library(caret)

setwd("C:/Users/USER/Documents/FluShotLearning")

training_features <- read_csv(file="training_set_features.csv")
training_labels <- read_csv(file="training_set_labels.csv")
test_data <- read_csv(file="test_set_features.csv")
dataset_for_training<-merge(x=training_features,y=training_labels, by="respondent_id")

dataset_for_training$h1n1_vaccine = as.factor(dataset_for_training$h1n1_vaccine)
dataset_for_training$seasonal_vaccine <- as.factor(dataset_for_training$seasonal_vaccine)

# Exploratory Data Analysis 


ggplot(data=dataset_for_training) + 
  geom_bar(mapping=aes(h1n1_vaccine),fill='red') +
  xlab("Received the H1N1 flu vaccine")+ scale_x_discrete(breaks=c("0","1"),
                                                          labels=c("No", "Yes"))


dataset_for_training <- dataset_for_training %>%
  mutate(missing_count = rowSums(is.na(.)))

ggplot(dataset_for_training, aes(x = factor(missing_count))) +
  geom_bar(aes(fill = factor(missing_count, levels = 0:max(missing_count)))) +
  labs(
    title = "Complete vs. Incomplete Rows",
    x = "Missing Values Count",
    y = "Count"
  ) +
  scale_fill_discrete(name = "Status", labels = c("Complete", "Incomplete")) +
  theme_minimal()

