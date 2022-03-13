library(ggplot2)
library(vtreat)
library(Metrics)
library(tidymodels)
library(baguette)


#CROSS-VALIDATION-----------------------------
#TO SEE HOW MODEL BUILT FROM ALL DATA WILL PERFORM ON THE NEW DATA

#Cross-validation predicts how well a model built from all the data will perform on new data. As with the test/train split, 
#for a good modeling procedure, cross-validation performance and training performance should be close


# Initialize a column of the appropriate length
mpg$pred.cv <- 0
splitPlan <- kWayCrossValidation(nrow(mpg), 3)

# k is the number of folds
# splitPlan is the cross validation plan

# Run the 3-fold cross validation plan from splitPlan
k <- 3 # Number of folds
mpg$pred.cv <- 0 
for(i in 1:k) {
   split <- splitPlan[[i]]
   model <- lm(cty ~ hwy, data = mpg[split$train, ])
   mpg$pred.cv[split$app] <- predict(model, newdata = mpg[split$app, ])
}

# Predict from a full model
mpg$pred <- predict(lm(cty ~ hwy, data = mpg))

mpg$pred

# Get the rmse of the full model's predictions
Metrics::rmse(mpg$pred, mpg$cty)


# Get the rmse of the cross-validation predictions
Metrics::rmse(mpg$pred.cv, mpg$cty)






#CROSS-VALIDATION BUT EASY------------

#Make k(number of) folds
mt_folds <- vfold_cv(mtcars, k = 10)

# Create a specification
tree_mod <- decision_tree() %>% 
   set_engine("rpart") %>% 
   set_mode("regression")

# Fit all folds to the specification
fits_cv <- fit_resamples(#specification
                        tree_mod, 
                         #formula
                         qsec ~ ., 
                        #k_folds
                        mt_folds,
                        metrics = metric_set(mae, rmse))

collect_metrics(fits_cv)


#CATEGORICAL INPUTS----------------------------------
#funkce model matrix udela z kategorialni promenne 1-0 dummy variable na vice sloupcu, jedna z novych dummy promennych je pak referencni hodnota interceptu,
#kdyz jsou ostatni prediktory (krome tech z dummy) na nule

mpg$manufacturer <- as.factor(mpg$manufacturer)

m1 <- as.data.frame(model.matrix(cty ~ manufacturer + hwy, data = mpg))


lm1 <- lm(hwy ~ ., data = m1)

summary(lm1)





#LOGNORMALLY DISTRIBUTED-----------------------------------------------
# fmla.abs is in the workspace
fmla.abs

# model.abs is in the workspace
summary(model.abs)

# Add predictions to the test set
income_test <- income_test %>%
   mutate(pred.absmodel = predict(model.abs, income_test),      # predictions from model.abs
          pred.logmodel = exp(predict(model.log, income_test))) # predictions from model.log

# Gather the predictions and calculate residuals and relative error
income_long <- income_test %>% 
   gather(key = modeltype, value = pred, pred.absmodel, pred.logmodel) %>%
   mutate(residual = pred - Income2005,     # residuals
          relerr   = residual / Income2005) # relative error

# Calculate RMSE and relative RMSE and compare
income_long %>% 
   group_by(modeltype) %>%                       # group by modeltype
   summarize(rmse     = sqrt(mean(residual^2)),  # RMSE
             rmse.rel = sqrt(mean(relerr^2)))    # Root mean squared relative error
#RESIDUALS FROM DIFFERENT MODELS----------------------
(fmla_lin <- as.formula(cty ~ hwy))
(fmla_sqr <- as.formula(cty ~ I(hwy^2)))

model_lin <- lm(fmla_lin, data = mpg)
model_sqr <- lm(fmla_sqr, data = mpg)

mpg_bm <- mpg %>% mutate(pred_lin = predict(model_lin),
                         pred_sqr = predict(model_sqr))

modelc <- mpg_bm %>% pivot_longer(cols = c("pred_lin", "pred_sqr"), names_to = "modeltype", values_to = "pred_v")

head(modelc)

perfm <- modelc %>% group_by(modeltype) %>% 
   mutate(residual = cty - pred_v, 
          reller = residual/cty) %>% 
   summarize(rmse = sqrt(mean(residual^2)),
             rmsrel = sqrt(mean(reller^2)))
   

#
#HOW TO GET ALL VARIABLE NAMES + FORMULA--------------------------
spamVars <- setdiff(
   #JAKÉ VSECHNY NÁZVY Z DF CHCI
   colnames(spamD),
   #NÁZVY DVOU PROMENNÝCH
   list('rgroup','spam'))

#JAK NAPSAT VZOREC MODELU
spamFormula <- as.formula(paste
                          #ZÁVISLÁ PROMENNÁ
                          ('spam=="spam"', #Create a formula
                                paste(spamVars,collapse=' + '), sep=' ~ '))
#CONFUSION MATRIX--------------

#Spam confusion matrix
cM <- table(truth=spamTest$spam,prediction=spamTest$pred>0.5) #Threshold of 0.5
print(cM)

accuracy_class <- function(x, y) {
   tab <- table(x, y)
   acc <- (tab[1,1] + tab[2,2])/(tab[1,1] + tab[2,2] + tab[1,2] + tab[2,1])
   print(acc)
}

#IDENTIFYING CATEGORICAL AND NUMERIC VARIABLES IN DATASET----------------
#Identify which features are categorical variables. 
catVars<-vars[sapply(dTrainAll[,vars],class) %in% c("factor","character")] #sapply() forces a vector to be returned
#Identify which features are numeric variables. 
numericVars<-vars[sapply(dTrainAll[,vars],class) %in% c("numeric","integer")]








#SPLITTING DATASETS + CONSTANT OUTCOME DISTRIBUTION------------------------
# Create a split with a constant outcome distribution
mtcars_split <- initial_split(mtcars, prop = 0.75, strata = am)

# Proportion of 'yes' outcomes in the training data
counts_train <- table(training(mtcars_split)$am)
prop_yes_train <- counts_train["1"] / sum(counts_train)

# Proportion of 'yes' outcomes in the test data
counts_test <- table(testing(mtcars_split)$am)
prop_yes_test <- counts_test["1"] / sum(counts_test)

paste("Proportion of positive outcomes in training set:", round(prop_yes_train, 2))
paste("Proportion of positive outcomes in test set:", round(prop_yes_test, 2))




#TUNING/TIDYMODELS/DECISION CLASS TREE------------------
mtcars$am <- as.factor(mtcars$am)

# Create a specification with tuning placeholders
tune_spec <- decision_tree(tree_depth = tune(),
                           cost_complexity = tune(),
                           min_n = tune()) %>% 
   # Specify mode
   set_mode("classification") %>%
   # Specify engine
   set_engine("rpart") 

# Create a regular grid
tree_grid <- grid_regular(parameters(tune_spec),
                          #number of possible combination of paramaters,
                          levels = 3)

tree_grid

#cross_validation
folds <- vfold_cv(mtcars, 4)

# Tune along the grid
tune_results <- tune_grid(tune_spec, 
                          am ~ .,
                          resamples = folds,
                          grid = tree_grid,
                          metrics = metric_set(accuracy))

# Plot the tuning results
autoplot(tune_results)

# Select the parameters that perform best
final_params <- select_best(tune_results)

# Finalize the specification
best_spec <- finalize_model(tune_spec, final_params)

# Build the final model
final_model <- fit(best_spec,
                   am ~ .,
                   mtcars)

final_model








#BAGGED_TREES_CLASS/TIDYMODELS/VARIABLE IMPORTANCE + AUC_ROC CURVE-------------
# Create the specification
spec_bagged <- baguette::bag_tree() %>%
   set_mode("classification") %>%
   set_engine("rpart", times = 20)

# Fit to the training data
model_bagged <- fit(spec_bagged,
                    am ~ ., 
                    mtcars)

# Print the model
model_bagged

#add the predictions for new df
predictions <- predict(model_bagged, mtcars, type = "class") %>% 
   bind_cols(mtcars)



