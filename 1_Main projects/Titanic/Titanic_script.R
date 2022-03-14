library(tidyverse)
library(ggplot2)
library(Hmisc) 
library(Amelia)
library(rpart)
library(pROC)
library(tidymodels)
library(easystats)

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

roc1 <- plot.roc(roc(train$Survived, train$pred, plot = T))

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



#3 - ML approach - boosted_tree algorithm with hyperparameter tuning

useless_f <- c("pred", "ide", "PassengerId", "Name", "Ticket", "Cabin", "Parch")
train_ml <- train %>% select(-useless_f)

factors <- c("Survived", "Pclass", "Sex", "SibSp", "Embarked", "title")
train_ml[factors] <- lapply(train_ml[factors], factor)

tr_spec <- boost_tree(trees = 20,
   learn_rate = tune(),
   tree_depth = tune()) %>%
   set_mode("classification") %>%
   set_engine("xgboost")

grid_tune <- grid_regular(learn_rate(),
             tree_depth(), 
             levels = 3)

folds <- vfold_cv(train_ml, 4)


#Calculation ~ 20sec
set.seed(123)
results <- tune_grid(tr_spec, 
                     Survived ~., 
                     resamples = folds,
                     grid = grid_tune,
                     metrics = metric_set(roc_auc))


autoplot(results)

#Final model 
best_params <- select_best(results)
final_spec <- finalize_model(tr_spec, best_params)
final_model <- final_spec %>% fit(Survived ~ ., train_ml)

train_ml <- predict(final_model, train_ml, type = "class") %>% bind_cols(train_ml)
train_ml <- as.data.frame(train_ml)

#Evaluation of the ML model
mean(train_ml$Survived == train_ml$.pred_class)
roc2 <- roc(response = train_ml$Survived, predictor= factor(train_ml$.pred_class, 
                                                        ordered = TRUE), plot = T)


#predicting the survival
useless_test_f <- c("pred", "ide", "Survived", "PassengerId", "Name", "Ticket", "Cabin", "Parch")
test_ml <- test %>% select(-useless_test_f)

factors_test <- c("Pclass", "Sex", "SibSp", "Embarked", "title")
test_ml[factors_test] <- lapply(test_ml[factors_test], factor)

test_ml <- predict(final_model, test_ml) %>% 
   bind_cols(test_ml)

train_ml$ide <- "Train"
test_ml$ide <- "Test"

train_ml <- train_ml %>% select(-Survived)

full2 <- rbind(train_ml, test_ml)

ggplot(full2, aes(Age, fill = factor(.pred_class))) + geom_histogram() + 
   facet_wrap(~ ide)

ggplot(full2, aes(Sex, fill = factor(.pred_class))) + stat_count() +
   facet_wrap(~ ide)



#Comparison
AUC_logit_mod <- as.vector(roc1$auc)
AUC_boostedTree_mod <- as.vector(roc2$auc)

AUC_boostedTree_mod > AUC_logit_mod










