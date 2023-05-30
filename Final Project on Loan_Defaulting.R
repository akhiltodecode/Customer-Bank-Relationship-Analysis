# Suppress dplyr summarise grouping warning messages
options(dplyr.summarise.inform = FALSE)

## Add R libraries here
library(tidyverse)
library(tidymodels)
library(dplyr)
library(rlang)
library(ggplot2)
library(skimr)
library(vip)
library(rpart.plot)
library(parsnip)

# Load data
loans_df <- read_rds("C:/Users/sun/Desktop/shareit/loan_data.rds")

#-------------------------------------------------------------------------------
# Data Analysis [30 Points]

#Question 1
# Are there any dependencies between loan takers and loan defaulters seen in their credit lines?

#Ans:
# Using visualizations to compare the credit lines taken by loan defaulters and non-defaulters in order to determine whether the credit lines variable has an impact on loan defaulters.
# This plot demonstrates how skewed the data are, and step YeoJohnson is used to balance the distribution. The number of credit lines taken is lower for loan defaulters, with the data being skewed to the left.

#For Loan-Defaulters as No, the number of credit lines are more in number
# with the data being skewed to the right side
#Code starts
loan_default_yes <- loans_df %>% filter(loan_default == 'yes')
#print(loan_default_yes)
#Code starts
loan_default_no <- loans_df %>% filter(loan_default == 'no')
#print(loan_default_no)


ggplot(data = loan_default_yes, mapping = aes(x = total_credit_lines)) +
  geom_histogram(fill = '#006EA1', color = 'white', bins = 15) +
  labs(title = 'Distribution of Total Credit Lines for Loan-Defaulters',
       x = 'Total Credit Lines',
       y = 'Number of Loan-Defaulters')

ggplot(data = loan_default_no, mapping = aes(x = total_credit_lines)) +
  geom_histogram(fill = '#006EA1', color = 'white', bins = 15) +
  labs(title = 'Distribution of Total Credit Lines for Loan-Non-Defaulters',
       x = 'Total credit Lines',
       y = 'Number of Loan-Non-Defaulters')

# Splitting dataset into Training and Test Data
set.seed(1)
loan_split <- initial_split(loans_df, prop = 0.75, strata = loan_default)

# Generate a training data frame
loan_df_training <- loan_split %>% training()

# View results
loan_df_training

# Generate a training data frame
loan_df_test <- loan_split %>% testing()

# View results
loan_df_test
summary(loan_df_training)


#Comparing the number of Credit lines to the loan
loaners_recipe <- recipe(loan_default ~ .,
                          data = loan_df_training)
print(loaners_recipe)

summary(loaners_recipe)

loaners_recipe %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  prep() %>%
  bake(new_data = loan_df_test)
print(loaners_recipe)

#b.step_corr() Highly Correlated Predictors cause problem in model fitting,
loaners_recipe %>%
  step_corr(all_numeric(), -all_outcomes()) %>%
  prep() %>%
  bake(new_data = loan_df_test)
print(loaners_recipe)

summary(loaners_recipe)

#To increase the recall for the regression analysis with the threshold value of 0.3
# Recall measures the proportion of events occurring in the domain that are “captured” by the models.
# After implementing the correlation with threshold of 0.3 to increase the recall for the model
# 2 variables of are removed from the new data
loaners_recipe %>%
  step_corr(all_numeric(), -all_outcomes(), -has_role('id variable'), threshold = 0.3) %>%
  prep() %>%
  bake(new_data = loan_df_test)
print(loaners_recipe)

loaners_recipe %>%
  step_YeoJohnson(total_credit_lines) %>%
  prep() %>%
  bake(new_data = loan_default_yes) %>%

  ggplot(mapping = aes(x = total_credit_lines)) +
  geom_histogram(fill = '#006EA1', color = 'white', bins = 8) +
  labs(title = 'Distribution of Total Credit Lines for Loan-Defaulters',
       x = 'Total Credit Lines',
       y = 'Number of Loan-Defaulters')

loaners_recipe %>%
  step_YeoJohnson(total_credit_lines) %>%
  prep() %>%
  bake(new_data = loan_default_no) %>%

  ggplot(mapping = aes(x = total_credit_lines)) +
  geom_histogram(fill = '#006EA1', color = 'white', bins = 15) +
  labs(title = 'Distribution of Total Credit Lines for Loan-Non-Defaulters',
       x = 'Total Credit Lines',
       y = 'Number of Loan-Non-Defaulters')


#Question 2
#a.Is there a relationship between the years credit history, total credit history, duration, loan amount, and loan default?

#Ans: 
#   There are fewer people taking out loans for medical purposes than defaulters.
#   And those who have medical debt default more frequently on their loans.
#   If the borrowers are in the medical industry, the bank must take further measures to ensure that they won't default on their loans.
ggplot(loan_default_no, aes(x=loan_purpose, y=loan_amount)) + geom_point()
ggplot(loan_default_yes, aes(x=loan_purpose, y=loan_amount)) + geom_point()

ggplot(loans_df, aes(x=years_credit_history, y=loan_amount, color= loan_default)) + geom_point() + scale_x_log10() + facet_wrap(~ term)
#Ans: As we can see that the Loan defaulting are more in between 10-30 years of credit history
#     The top default is 40000 with more than 30 years of credit history
ggplot(loans_df, aes(x=total_credit_lines, y=loan_amount, color= loan_default)) + geom_point() + scale_x_log10() + facet_wrap(~ term)
#Ans: As we can observe that the total credit lines are more in number between 10-30 creditlines
#     for both loan_defaulting and loan-non-defaulting
#     The top loan defaulting is 40000 for five year term, with 4 loan-defaulters


#b.Does the correlation seen between years of credit history, total credit history, homeownership, loan amount, and loan default exist?
ggplot(loans_df, aes(x=years_credit_history, y=loan_amount, color= loan_default))+ geom_point() + scale_x_log10() + facet_wrap(~ homeownership)
#Ans: There is few loan defaulters in homeownership of type own and more in mortagage
#     Loan amount of around 0-20000 are higher to default loan in the ownership type rent
ggplot(loans_df, aes(x=total_credit_lines, y=loan_amount, color= loan_default)) + geom_point() + scale_x_log10() + facet_wrap(~ homeownership)
#Ans : The data is pointed more in the center of mortgage of total credit lines at 10-30 with more number of loan defaulters in rent with loan
#      amount ranging from 0-20000 with more loan-defaulters

#Question 3
# Are the dependencies between current job years and loan purpose & Interest rate for the loan-defaulters
current_jy_10 <- loan_default_yes %>%
  filter(current_job_years == 10)

ggplot(current_jy_10, aes(x = loan_purpose, y = interest_rate)) +
  geom_boxplot() 

#Ans: For different loan_purposes like credit_card, medical the median is 15% interest_rate
#     For small_business and improvements the median is just below 15 i.e 14.8% interest_rate min/ max are also similar
#     The first and third quartiles for each of the purpose is dissimilar

#-------------------------------------------------------------------------------
# Predictive Modelling 70 points

#2-Modelling techniques, 
#Using loan_df with train and test data set to set the seed, while considering variables for model use recipe

#Logistic Regressions



#Training and running model for all the dependent variables
# Training model
logistic_model <- glm(loan_default ~ loan_amount+installment+interest_rate+annual_income+current_job_years+debt_to_income+total_credit_lines+ years_credit_history,
                      data = loan_df_training,
                      family = "binomial")
logistic_model

# Summary
summary(logistic_model)

newdata = data.frame(loan_amount = 4200, installment= 900, interest_rate = 15, annual_income = 90000, current_job_years = 6, debt_to_income= 21, total_credit_lines = 9, years_credit_history = 9)

# Predict test data based on model
predict_reg <- predict(logistic_model, 
                       newdata, type = "response")
predict_reg 

#Ans: By using the logistic regression, the predictions based on the variables,
#     show that the final prediction outcome is genuine and helps the bank to evaluate their customers.
#-------------------------------------------------------------------

#Decision Trees
set.seed(1)
loan_split <- initial_split(loans_df, prop = 0.75, strata = loan_default)

# Generate a training data frame
loan_df_training <- loan_split %>% training()

# View results
loan_df_training

# Generate a training data frame
loan_df_test <- loan_split %>% testing()

# View results
loan_df_test

set.seed(1)
churn_folds <- vfold_cv(loan_df_training, v = 5)

loan_transformations <- recipe(loan_default ~ .,
                               data = loan_df_training) %>%
  # Transformation steps
  step_YeoJohnson(all_numeric(), -all_outcomes()) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%

  prep()

skim(loans_df)

tree_model <- decision_tree(cost_complexity = tune(),
tree_depth = tune(), min_n = tune()) %>%
set_engine('rpart') %>%
set_mode('classification')

#Model Creation
tree_workflow <- workflow() %>%
  add_model(tree_model) %>%
add_recipe(loan_transformations)

#Hyperparameter Tuning
#hyperparameter values to test
tree_grid<- grid_regular(cost_complexity(), tree_depth(), min_n(), levels = 2)

#View Grid
tree_grid

#Tuning with tune_grid()
#Tune decision
set.seed(1)
tree_tuning <- tree_workflow %>%
tune_grid(resamples = churn_folds, grid=tree_grid)

#Show top 5 best models
tree_tuning %>% show_best('roc_auc')

#Select best model
best_tree <- tree_tuning %>% select_best(metric = 'roc_auc')

#View best tree parametrs
best_tree

#Finalizing Workflow
final_tree_workflow <- tree_workflow %>%
  finalize_workflow(best_tree)

#Fit the Model
tree_wf_fit <- final_tree_workflow %>%
  fit(data = loan_df_training)

#Explore trained model
tree_fit <- tree_wf_fit %>% pull_workflow_fit()

#Variable_Importance
vip(tree_fit)

#Tree Plot
rpart.plot(tree_fit$fit, roundint = FALSE)

#Train and Evaluate the last_fit()
tree_last_fit <- final_tree_workflow %>%
  last_fit(loan_split)

#Accuracy and Area
tree_last_fit %>%collect_metrics()

#collect Predictions
#Estimated Probabilites
tree_predictions <- tree_last_fit %>% collect_predictions()

#ROC Curve
tree_predictions %>% roc_curve(truth = loan_default, estimate = .pred_yes) %>% autoplot()

#confusion Matrix
conf_mat(tree_predictions, truth = loan_default, estimate = .pred_class)

#Conclusion:
# Decision Tree
# After implementing the decision tree algorithm for tuning the decision to make based on
# the variables that are available in the project, we can observe that there are 5 best models based on
# the roc_auc method.
# The best model has the cost_complexity of 0.000000001 with the tree_depth of 15.
# The tree's last fit's accuracy's estimate is 0.897 and roc_auc's estimate is 0.964

# The Predicition shows that
# Model correctly classified 306 True Positive class data points.
# Model correctly classified 616 True Negative class data points.
# Model incorrectly classified 29 False Positive class data points.
# Model incorrectly classified 77 False Negative class data points.
# The predictions show that Given the comparatively higher proportion of true positive and true negative values in our dataset, this proved to be a rather good classifier

#Random Forest
# Model Specification
rf_model <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>% 
  set_engine('ranger', importance = "impurity") %>%
  set_mode('classification')

# Specify the workflow 
rf_workflow <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(loan_transformations)

rf_workflow

## Create a grid of hyperparameter values to test
set.seed(12)
rf_grid <- grid_random(mtry() %>% 
                         range_set(c(4, 12)),
                       trees(),
                       min_n(), 
                       size = 10)

# Tune random forest workflow 
set.seed(314)
rf_tuning <- rf_workflow %>%
  tune_grid(resamples = churn_folds, grid = rf_grid)

## Show the top 5 best models based on roc_auc metric 
rf_tuning %>%show_best('roc_auc')

# Select best model based on  
best_rf <- rf_tuning %>% select_best(metric = 'roc_auc')

# View the best parameters
best_rf

# Finalize workflow 
final_rf_workflow <- rf_workflow %>% 
finalize_workflow(best_rf) 
final_rf_workflow

# Fit the model
rf_wf_fit <- final_rf_workflow %>% 
fit(data = loan_df_training)

# Extract trained model
rf_fit <- rf_wf_fit %>%
pull_workflow_fit()

# Variable Importance 
vip(rf_fit)

# Train and evaluate with last_fit()
rf_last_fit <- final_rf_workflow %>%
  last_fit(loan_split)

# Accuracy and Area under the ROC curve 
print('printing accuracy rate and roc curve for the random forests')
rf_last_fit %>%
  collect_metrics()

# collect_predictions() 
rf_predictions <- rf_last_fit %>% collect_predictions()
rf_predictions

# ROC Curve 
rf_predictions %>%
  roc_curve(truth = loan_default, estimate = .pred_yes) %>% 
  autoplot()

# Confusion matrix
conf_mat(rf_predictions, truth = loan_default, estimate = .pred_class)


#Theory for the project's outputs is as below:-

# 1. The data talks about the bank's loan defaulting issue from their customers
# 
# - The Company is trying to understand the dependent reasons for the loan-defaulting
#   By knowing the main dependencies that cause this problem, the company can solve the loan-defaulting problem
#   
#   - The Goal of my analysis is to predict the future out-comings of loan-defaulting 
#     To predict this implementing logistic regression, Decision Trees, Random Forests
#     The Main Questions that help to understand the dependencies for this is final analysis are as below

# 1. Are there any dependencies between loan takers and loan defaulters seen in their credit lines?
# 2. a.Is there a relationship between the years credit history, total credit history, duration, loan amount, and loan default?
#    b.Does the correlation seen between years of credit history, total credit history, home-ownership, loan amount, and loan default exist?
# 3. Are the dependencies between current job years and loan purpose & Interest rate for the loan-defaulters
# 4. Using Logistic Regression to analysis and predict the outcome for a loantaker in future
# 5. Using Decision Tree to find out the best tree and options.
# 6. Using Random Forest to fine tune the best tree that analysises the dependencies of the variables
  
#   2. Highlights and key findings from your Exploratory Data Analysis section 
# 
# - What were the interesting findings from your analysis and **why are they important for the business**?
#   The Interesting findings from the analysis are that the Those who take out loans for medical 
#   expenses are less likely to default than those who do, and those with medical debt do so more 
#   frequently. The bank must take further steps to ensure compliance if the borrowers are in the 
#   medical sector.

# - The median interest rate for various loan reasons, such as credit cards and medical, is 15%. 
#   The minimum and maximum interest rates for small businesses and upgrades are likewise similar. 
#   The first and third quartiles for each purpose are different.  

# - The results of the logistic regression demonstrate that the predictions for future events are correct, assisting the bank organization in properly deciding whether to extend credit to consumers and ultimately influencing loan default or non-default rates.

# 3. Your “best” classification model and an analysis of its performance 
#    The three models of logistic regression, Decision Tree and Random Forest have
#    all proved to be worthy and valuable for the predictions making.
#    But the best of three can either be Decision Tree or Random Forest as they provide the 
#    True Positives and True Negatives greater than the False Positives and False Negatives as shown in the outputs
#    
# - The expected error for the model of Decision tree is around 0.103 and for Random Forests is 0.99 which is better. 
# 
# - After performing the predictions on the train and test split data it shows that random forest derives accurate outcomes as shown the output of rf_predicitons
#   
#   - The Performance metric of ROC Curve for the Random Forest is 0.966 and Decision Tree is 0.964.
#     
#     Here's the information that is valuable to the executives of the bank, the Random Forest can be 96.6% times dependent and accurate for all the predicitons
#     that has to be made.'
#     The Decision Tree makes a ROC CUrve of 0.964 that means it is almost 97% times reliable for the predictions and accuracy of the model.

# 4. Your recommendations to the company on how to reduce loan default rates 
# 
# - Recommending the company to take preventive measures by providing loan in more number to homeownership purpose than other if possible
#   and less to those for the purpose of mortgage
# - Less homeowners who own their homes default on their loans than those who mortgage them. # Loan amounts of between 0 and 20,000 are more likely to default than those who own their homes but rent them.

# Potential Impact and benefits 
# - The potential impact of this might be that you might miss some valuable customers that might not loan-default but pay back in time, this is conuter-feited by the matter of fact that the bank organisation has to lessen the amount of loan that can be awared to them to lesser than 5000, as it will save money to the organisation and would not risk the time & money into such scenarios.\


# 5. Conclusion

# Using visualizations to compare the credit lines taken by loan defaulters and non-defaulters in order to determine whether the credit lines variable has an impact on loan defaulters.
# This plot demonstrates how skewed the data are, and step YeoJohnson is used to balance the distribution. The number of credit lines taken is lower for loan defaulters, with the data being skewed to the left.
# For Loan-Defaulters as No, the number of credit lines are more in number
# with the data being skewed to the right side

# To increase the recall for the regression analysis with the threshold value of 0.3
# Recall measures the proportion of events occurring in the domain that are “captured” by the models.
# After implementing the correlation with threshold of 0.3 to increase the recall for the model
# 2 variables of are removed from the new data

# There are fewer people taking out loans for medical purposes than defaulters.
# And those who have medical debt default more frequently on their loans.
# If the borrowers are in the medical industry, the bank must take further measures to ensure that they won't default on their loans.

# As we can see that the Loan defaulting are more in between 10-30 years of credit history
# The top default is 40000 with more than 30 years of credit history

# As we can observe that the total credit lines are more in number between 10-30 creditlines
# for both loan_defaulting and loan-non-defaulting
# The top loan defaulting is 40000 for five year term, with 4 loan-defaulters

# There is few loan defaulters in homeownership of type own and more in mortagage
# Loan amount of around 0-20000 are higher to default loan in the ownership type rent

# The data is pointed more in the center of mortgage of total credit lines at 10-30 with more number of loan defaulters in rent with loan
# amount ranging from 0-20000 with more loan-defaulters

# For different loan_purposes like credit_card, medical the median is 15% interest_rate
# For small_business and improvements the median is just below 15 i.e 14.8% interest_rate min/ max are also similar
# The first and third quartiles for each of the purpose is dissimilar

# By using the logistic regression, the predictions based on the variables,
# show that the final prediction outcome is genuine and helps the bank to evaluate their customers.

# Decision Tree
# After implementing the decision tree algorithm for tuning the decision to make based on
# the variables that are available in the project, we can observe that there are 5 best models based on
# the roc_auc method.
# The best model has the cost_complexity of 0.000000001 with the tree_depth of 15.
# The tree's last fit's accuracy's estimate is 0.897 and roc_auc's estimate is 0.964

# The Prediction shows that
# Model correctly classified 306 True Positive class data points.
# Model correctly classified 616 True Negative class data points.
# Model incorrectly classified 29 False Positive class data points.
# Model incorrectly classified 77 False Negative class data points.
# The predictions show that Given the comparatively higher proportion of true positive and true negative values in our dataset, this proved to be a rather good classifier

#Random Forest
# On implementing the Random Forest Algorithm for fine-tuning, there is one best tree with the right combinations for analysis.
# The Prediction shows that
# Model correctly classified 306 True Positive class data points.
# Model correctly classified 620 True Negative class data points.
# Model incorrectly classified 29 False Positive class data points.
# Model incorrectly classified 77 False Negative class data points.
# The predictions show that Given the comparatively higher proportion of true positive and true negative values in our dataset, this proved to be a rather good classifier

#After working on both Decision Tree and Random Forest, it is clear that these can be 
# trusted for the purpose of decision making in this case.

# The predictions from the Random Forest fitted model shows that almost all the predictions 
# display great value of efficiency by making the output genuine(i.e.matching exactly the final prediction with the loan-default's value for that instance)