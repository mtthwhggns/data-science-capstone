## Let's start with our libraries

library(tidyverse)
library(caret)

## Now we bring in our data

columnNames <- c('age', # Age of the patient 
                 'sex', # Sex of the patient 
                 'tb', # Total Bilirubin
                 'db', # Direct Bilirubin 
                 'alkphos', # Alkaline Phosphotase
                 'sgpt', # Alamine Aminotransferase
                 'sgot', # Aspartate Aminotransferase
                 'tp', # Total Protiens
                 'alb', # Albumin
                 'ag', # Ratio	Albumin and Globulin Ratio 
                 'outcome') # Selector field used to split the data into two sets

fullData <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(ILPD).csv",
                       sep=',',
                       header=FALSE,
                       col.names=columnNames)

## Remove ayny incomplete rows

fullData <- subset(fullData, complete.cases(fullData))

## Format it in a more human-understandable way

fullData <- fullData %>% 
  mutate(outcome = as.character(outcome)) %>% 
  mutate(outcome = replace(outcome, outcome == '1', 'Care')) %>%
  mutate(outcome = replace(outcome, outcome == '2', 'Control')) %>%
  mutate(outcome = as.factor(outcome))

## Now we remove the columns we decided weren't too useful

fullData <- fullData %>% subset(select = -c(sgpt, db, alb))

## And create some train/test sets

set.seed(1)
trainIndex <- createDataPartition(fullData$outcome, p=.7, list=FALSE)
train <- fullData[trainIndex,]
test <- fullData[-trainIndex,]
rm(trainIndex)

## Train the model
nb_model = train(outcome ~ ., data = train, method = "nb")

## Now create some predictions with the test set to validate if it's working

predictions = predict(nb_model, newdata = test)

## The predictions can be rendered in the confusion matrix format with any 
## summary statistics

confusionMatrix(predictions, test$outcome)