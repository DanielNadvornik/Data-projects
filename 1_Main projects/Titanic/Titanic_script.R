library(tidyverse)
library(ggplot2)
library(Hmisc) 
library(Amelia)
library(rpart)
library(pROC)

getwd()

train <- read.csv("C:/Users/Daniel/Desktop/Rko/1_Dánsko/Titanic/train.csv")
test <- read.csv("C:/Users/Daniel/Desktop/Rko/1_Dánsko/Titanic/test.csv")

########## Process

#Exploring the data 
   #1) Plotting the distributions of numerical variables
   #2) Exploring the missing values of all variables

#Data manipulation
   #1) Using the Name variable to define titles  
   #1) Imputing the missing values in Age variable with rpart
 

#Model development
   #1)Using logistic regression model to predict binary outcome (Survived)
   #2)Evaluating the model

#Using the model for test dataset



########## Script

#Exploring the data

str(train)
head(train)

#1
train_n <- train %>% select(where(is.numeric))

hist.data.frame(train_n)

#2
missmap(train)
missmap(test)

#Data manipulation

#1
train$title <- gsub('(.*, )|(\\..*)', '', train$Name)

table(train$Sex, train$title)

rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

train$title[train$title == 'Mlle']        <- 'Miss' 
train$title[train$title == 'Ms']          <- 'Miss'
train$title[train$title == 'Mme']         <- 'Mrs' 
train$title[train$title %in% rare_title]  <- 'Rare Title'

table(train$Sex, train$title)

#2
anova_mod <- rpart(Age ~ Pclass + Fare + Sex + Embarked + title, train[!is.na(train$Age), ], method = "anova")

pred <- predict(anova_mod, train[is.na(train$Age), ])

pred <- round(pred)

summary(anova_mod)

train$Age <- ifelse(is.na(train$Age), pred, train$Age)


#Model development

#1
sur_mod <- glm(Survived ~ Pclass + Fare + Sex + Age + title, train, family = "binomial")

summary(sur_mod)

train$pred <- predict(sur_mod, type = "response")

mean(train$pred)

train$pred <- ifelse(train$pred > 0.4, 1, 0)


#2
mean(train$Survived == train$pred)

ROC <- roc(train$Survived, train$pred)

plot(ROC, col = "green")

auc(ROC)


############## test

#Data manipulation

#1
test$title <- gsub('(.*, )|(\\..*)', '', test$Name)

table(test$Sex, test$title)

rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

test$title[test$title == 'Mlle']        <- 'Miss' 
test$title[test$title == 'Ms']          <- 'Miss'
test$title[test$title == 'Mme']         <- 'Mrs' 
test$title[test$title %in% rare_title]  <- 'Rare Title'

table(test$Sex, test$title)

#2
anova_mod_t <- rpart(Age ~ Pclass + Fare + Sex + Embarked + title, test[!is.na(test$Age), ], method = "anova")

pred_t <- predict(anova_mod_t, test[is.na(test$Age), ])

pred_t <- round(pred_t)

summary(anova_mod_t)

test$Age <- ifelse(is.na(test$Age), pred_t, test$Age)


#Using the model for test dataset
test$pred <- predict(sur_mod, test, type = "response")

sum(is.na(test$pred))

test <- test[-153, ]

mean(test$pred)

test$Survived <- ifelse(test$pred > 0.4, 1, 0)

table(test$Sex, test$Survived)

#Visualisations
train$ide <- "Train"
test$ide <- "Test"

full <- rbind(train, test)

ggplot(full, aes(Age, fill = factor(Survived))) + geom_histogram() + 
   facet_wrap(~ ide)

ggplot(full, aes(Sex, fill = factor(Survived))) + stat_count() +
   facet_wrap(~ ide)











