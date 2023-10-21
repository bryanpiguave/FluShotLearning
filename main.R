library(tidyverse)
library(lm.beta)
library(olsrr)
library(randomForest)
library(caret)

setwd("C:/Users/USER/Documents/FluShotLearning")

training_features <- read_csv(file="training_set_features.csv")
training_labels <- read_csv(file="training_set_labels.csv")
test_data <- read_csv(file="test_set_features.csv")
dataset_for_training<-merge(x=training_features,y=training_labels, by="respondent_id")

#Saving space 
#rm(training_features,training_labels)

dataset_for_training$h1n1_vaccine = as.factor(dataset_for_training$h1n1_vaccine)
dataset_for_training$seasonal_vaccine <- as.factor(dataset_for_training$seasonal_vaccine)

dataset_for_training <- dataset_for_training |> select(h1n1_vaccine,h1n1_knowledge, seasonal_vaccine,
                                                       income_poverty, race,
                                                       behavioral_wash_hands) |> na.omit() 


print("Number of variables")
length(names(dataset_for_training))

sample <- sample(c(TRUE, FALSE), nrow(dataset_for_training), replace=TRUE, prob=c(0.8,0.2))
train <- dataset_for_training[sample,]
validation <- dataset_for_training[!sample,]


# Training generalized linear model 
THRESHOLD <- 0.2


model_h1n1<- glm(h1n1_vaccine ~ h1n1_knowledge + behavioral_wash_hands+income_poverty+
                                      race,data=train,family="binomial")
val_features <- validation |> 
              select(h1n1_knowledge,behavioral_wash_hands,income_poverty,race)
pred_labels<- predict(model_h1n1,val_features,type="response")
                             
pred_labels <- if_else(pred_labels>THRESHOLD,1,0) |> as.factor()
target_labels<- validation$h1n1_vaccine

comparison <-if_else(target_labels==pred_labels,1,0)
cm <- caret::confusionMatrix(data=pred_labels,reference=target_labels)
cm

saveRDS(model_h1n1, file = "modelh1n1.rds")




# Using test_features for H1N1 vaccine 
test_features <- test_data |> select(h1n1_knowledge,
                                         income_poverty, race,
                                         behavioral_wash_hands)
test_pred <- predict(model_h1n1,test_features,type="response")
test_pred_labels_for_h1n1 <- test_pred




model_seasonal<- glm(seasonal_vaccine ~ h1n1_knowledge + behavioral_wash_hands+income_poverty+
                   race,data=train,family="binomial")
test_pred <- predict(model_h1n1,test_features,type="response")
test_pred_labels_for_seasonal <-test_pred




df <- data.frame(respondent_id = test_data$respondent_id, 
                 h1n1_vaccine = test_pred_labels_for_h1n1,
                 seasonal_vaccine = test_pred_labels_for_seasonal )

df <- df |> mutate(h1n1_vaccine = if_else(is.na(h1n1_vaccine), 0.5, h1n1_vaccine),
                     seasonal_vaccine = if_else(is.na(seasonal_vaccine), 0, seasonal_vaccine))

write_csv(df,file="submission.csv")




