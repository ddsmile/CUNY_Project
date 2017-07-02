# Load packages
library(ggplot2)
library(lubridate)
library(leaps)
library(car)
library(caret)
library(randomForest)
library(gbm)

#read in the csv file and clean the data
loan <- read.csv("LoanStats3a_finalVersion.csv", header = T, stringsAsFactors = FALSE)
choice <- c(3, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 20, 22, 23, 24, 25, 26, 27, 30, 31, 32, 33, 34)
loan1 <- loan[ ,choice]

######data clean#####

# term
loan1$term <- as.factor(loan1$term)
# int_rate
loan1$int_rate <- as.numeric(sub("%", "", loan1$int_rate))/100
# grade
loan1$grade <- as.factor(loan1$grade)
# subgrade
loan1$sub_grade <- as.factor(loan1$sub_grade)
# emp_length
loan1[loan1$emp_length == "< 1 year", "emp_length"] <- 0
loan1[loan1$emp_length == "1 year", "emp_length"] <- 1
loan1[loan1$emp_length == "2 years", "emp_length"] <- 2
loan1[loan1$emp_length == "3 years", "emp_length"] <- 3
loan1[loan1$emp_length == "4 years", "emp_length"] <- 4
loan1[loan1$emp_length == "5 years", "emp_length"] <- 5
loan1[loan1$emp_length == "6 years", "emp_length"] <- 6
loan1[loan1$emp_length == "7 years", "emp_length"] <- 7
loan1[loan1$emp_length == "8 years", "emp_length"] <- 8
loan1[loan1$emp_length == "9 years", "emp_length"] <- 9
loan1[loan1$emp_length == "10+ years", "emp_length"] <- 10
loan1[loan1$emp_length == "n/a", "emp_length"] <- NA
loan1$emp_length <- as.integer(loan1$emp_length)
# home_ownership
loan1$home_ownership <- as.factor(loan1$home_ownership)
# is_inc_v
loan1$is_inc_v <- as.factor(loan1$is_inc_v)
# issue_d
loan1$issue_d<-as.Date(mdy(loan1$issue_d))
# purpose
loan1$purpose <- as.factor(loan1$purpose)
# zip code
loan1$zip_code <- as.integer(substr(loan1$zip_code, 1, 1))
# addr_state
loan1$addr_state <- as.factor(loan1$addr_state)
# earliest_cr_line
loan1$earliest_cr_line<-as.Date(mdy(loan1$earliest_cr_line))
# revol_util 
loan1$revol_util <- as.numeric(sub("%", "", loan1$revol_util ))/100
# create  cr_line_age variable
loan1$cr_line_age <- as.numeric(loan1$issue_d-loan1$earliest_cr_line)

#save the data frame into an rdata file in the local directory
save(loan1, file="loan.rdata")

#load the rdata file from the local directory
loan <-load("loan.rdata")

#remove the rows with NAs
#goes from 39,786 to 38,661 rows (1,125 rows removed)
loan1 <- loan1[complete.cases(loan1),]

#print out the 25 variable names
names(loan1)
# [1] "loan_amnt"        "funded_amnt_inv"  "term"             "int_rate"         "installment" 
# [6] "grade"            "sub_grade"        "emp_length"       "home_ownership"   "annual_inc"  
#[11] "is_inc_v"         "issue_d"          "purpose"          "zip_code"         "addr_state"  
#[16] "dti"              "delinq_2yrs"      "earliest_cr_line" "inq_last_6mths"   "open_acc"    #[21] "pub_rec"          "revol_bal"        "revol_util"       "total_acc"        "cr_line_age" 

#plot the distribution of the variables
ggplot(loan1,
       aes(x=loan_amnt))+geom_density()+ggtitle("loan_amnt Distribution")
ggplot(loan1,
       aes(x=funded_amnt_inv))+geom_density()+ggtitle("funded_amnt_inv Distribution")
ggplot(loan1,
       aes(x=term))+geom_density()+ggtitle("term Distribution")
ggplot(loan1,
       aes(x=int_rate))+geom_density()+ggtitle("int_rate Distribution")
ggplot(loan1,
       aes(x=installment))+geom_density()+ggtitle("installment Distribution")
ggplot(loan1,
       aes(x=grade))+geom_density()+ggtitle("grade Distribution")
ggplot(loan1,
       aes(x=sub_grade))+geom_density()+ggtitle("sub_grade Distribution")
ggplot(loan1,
       aes(x=emp_length))+geom_density()+ggtitle("emp_length Distribution")
ggplot(loan1,
       aes(x=home_ownership))+geom_density()+ggtitle("home_ownership Distribution")
ggplot(loan1,
       aes(x=annual_inc))+geom_density()+ggtitle("annual_inc Distribution")
ggplot(loan1,
       aes(x=is_inc_v))+geom_density()+ggtitle("is_inc_v Distribution")
ggplot(loan1,
       aes(x=issue_d))+geom_density()+ggtitle("issue_d Distribution")
ggplot(loan1,
       aes(x=purpose))+geom_density()+ggtitle("purpose Distribution")
ggplot(loan1,
       aes(x=zip_code))+geom_density()+ggtitle("zip_code Distribution")
ggplot(loan1,
       aes(x=addr_state))+geom_density()+ggtitle("addr_state Distribution")
ggplot(loan1,
       aes(x=dti))+geom_density()+ggtitle("dti Distribution")
ggplot(loan1,
       aes(x=delinq_2yrs))+geom_density()+ggtitle("delinq_2yrs Distribution")
ggplot(loan1,
       aes(x=earliest_cr_line))+geom_density()+ggtitle("earliest_cr_line Distribution")
ggplot(loan1,
       aes(x=inq_last_6mths))+geom_density()+ggtitle("inq_last_6mths Distribution")
ggplot(loan1,
       aes(x=open_acc))+geom_density()+ggtitle("open_acc Distribution")
ggplot(loan1,
       aes(x=pub_rec))+geom_density()+ggtitle("pub_rec Distribution")
ggplot(loan1,
       aes(x=revol_bal))+geom_density()+ggtitle("revol_bal Distribution")
ggplot(loan1,
       aes(x=revol_util))+geom_density()+ggtitle("revol_util Distribution")
ggplot(loan1,
       aes(x=total_acc))+geom_density()+ggtitle("total_acc Distribution")
ggplot(loan1,
       aes(x=cr_line_age))+geom_density()+ggtitle("cr_line_age Distribution")

#correlation plots
ggplot(loan1,
       aes(x=loan_amnt,
           y=int_rate))+geom_point()+geom_smooth(method='lm')+ggtitle("loan_amnt")
ggplot(loan1,
       aes(x=funded_amnt_inv,
           y=int_rate))+geom_point()+geom_smooth(method='lm')+ggtitle("funded_amnt_inv")
ggplot(loan1,
       aes(x=term,
           y=int_rate,
           group=1))+geom_point()+geom_smooth(method='lm')+ggtitle("term")
ggplot(loan1,
       aes(x=installment,
           y=int_rate))+geom_point()+geom_smooth(method='lm')+ggtitle("installment")
ggplot(loan1,
       aes(x=grade,
           y=int_rate,
           group=1))+geom_point()+geom_smooth(method='lm')+ggtitle("grade")
ggplot(loan1,
       aes(x=sub_grade,
           y=int_rate,
           group=1))+geom_point()+geom_smooth(method='lm')+ggtitle("sub_grade")
ggplot(loan1,
       aes(x=emp_length,
           y=int_rate))+geom_point()+geom_smooth(method='lm')+ggtitle("emp_length")
ggplot(loan1,
       aes(x=home_ownership,
           y=int_rate,
           group=1))+geom_point()+geom_smooth(method='lm')+ggtitle("home_ownership")
ggplot(loan1,
       aes(x=annual_inc,
           y=int_rate))+geom_point()+geom_smooth(method='lm')+ggtitle("annual_inc")
ggplot(loan1,
       aes(x=is_inc_v,
           y=int_rate,
           group=1))+geom_point()+geom_smooth(method='lm')+ggtitle("is_inc_v")
ggplot(loan1,
       aes(x=issue_d,
           y=int_rate))+geom_point()+geom_smooth(method='lm')+ggtitle("issue_d")
ggplot(loan1,
       aes(x=purpose,
           y=int_rate,
           group=1))+geom_point()+geom_smooth(method='lm')+ggtitle("purpose")
ggplot(loan1,
       aes(x=zip_code,
           y=int_rate))+geom_point()+geom_smooth(method='lm')+ggtitle("zip_code")
ggplot(loan1,
       aes(x=addr_state,
           y=int_rate,
           group=1))+geom_point()+geom_smooth(method='lm')+ggtitle("addr_state")
ggplot(loan1,
       aes(x=dti,
           y=int_rate))+geom_point()+geom_smooth(method='lm')+ggtitle("dti")
ggplot(loan1,
       aes(x=delinq_2yrs,
           y=int_rate))+geom_point()+geom_smooth(method='lm')+ggtitle("delinq_2yrs")
ggplot(loan1,
       aes(x=earliest_cr_line,
           y=int_rate))+geom_point()+geom_smooth(method='lm')+ggtitle("earliest_cr_line")
ggplot(loan1,
       aes(x=inq_last_6mths,
           y=int_rate))+geom_point()+geom_smooth(method='lm')+ggtitle("inq_last_6mths")
ggplot(loan1,
       aes(x=open_acc,
           y=int_rate))+geom_point()+geom_smooth(method='lm')+ggtitle("open_acc")
ggplot(loan1,
       aes(x=pub_rec,
           y=int_rate))+geom_point()+geom_smooth(method='lm')+ggtitle("pub_rec")
ggplot(loan1,
       aes(x=revol_bal,
           y=int_rate))+geom_point()+geom_smooth(method='lm')+ggtitle("revol_bal")
ggplot(loan1,
       aes(x=revol_util,
           y=int_rate))+geom_point()+geom_smooth(method='lm')+ggtitle("revol_util")
ggplot(loan1,
       aes(x=total_acc,
           y=int_rate))+geom_point()+geom_smooth(method='lm')+ggtitle("total_acc")
ggplot(loan1,
       aes(x=cr_line_age,
           y=int_rate))+geom_point()+geom_smooth(method='lm')+ggtitle("cr_line_age")

#correlation matrix
#create a data frame without the factors & dates for the correlation matrix
loan1_cor <- loan1[ , -which(names(loan1) %in% c("term","grade","sub_grade","home_ownership","is_inc_v","purpose","addr_state","issue_d","earliest_cr_line"))]
#view the correlation matrix
View(round(cor(loan1_cor),2))
cor(loan1_cor)

##### Model building #####

#load the rdata file from the local directory
loan <-load("loan.rdata")
loan1$zip_code <- as.factor(loan1$zip_code) # convert zipcode to factor

#remove the rows with NAs
#goes from 39,786 to 38,661 rows (1,125 rows removed)
loan1 <- loan1[complete.cases(loan1),]

# Remove grade, subgrade, issue_d, earliest_cr_line
ex <- c(6, 7, 12, 18)
loan <- loan1[, -ex]


#####dataset#####
#partition the data into Training (60%), Test (20%) & Validation (20%) groups

# sample size = 38,661
n <- dim(loan)[1]
# set random number generator seed
set.seed(1125)
# randomly sample 20% test
test <- sample(n, round(n/5))
# define the test dataset (7,732 rows)
loan_test <- loan[test,]
loan_left <- loan[-test,]
#sample size = 30,929
nn <- dim(loan_left)[1]
# set random number generator seed
set.seed(1125)
# randomly sample 20% validation (25% of the remaining data)
valid <- sample(nn, round(nn/4))
# define the validation dataset (7,732 rows)
loan_valid <- loan_left[valid,]
# define the training dataset (23,197 rows)
loan_train <- loan_left[-valid,]

##### Linear regression and Multicollinearity #####
loan_full_back <- lm(int_rate~., data=loan_train)
# check vif
vif(loan_full_back) #loan_amnt, funded_amnt_inv, and installment higher than 2.50

# Create correlation matrix between predictors (numeric variable only)
loan.num <- loan[ , -which(names(loan) %in% 
                             c("int_rate", "term", "home_ownership", "is_inc_v", "purpose", "zip_code", "addr_state"))]
cortable <- cor(loan.num) 

(sum(cortable["loan_amnt",])-1)/15 # average correlation for loan_amnt 0.221002
(sum(cortable[c("funded_amnt_inv"),])-1)/15 # average correlation for funded_amnt_inv 0.21452
(sum(cortable[c("installment"),])-1)/15 # average correlation for installment 0.21432

# Remove the higher average correlation, loan_amnt

loan_full_back1 <- lm(int_rate~.-loan_amnt, data=loan_train)
vif(loan_full_back1) # funded_amnt_inv and installment still higher than 2.50

# Remove funded_amnt_inv

loan_full_back2 <- lm(int_rate~.-loan_amnt-funded_amnt_inv, data=loan_train) # Final full model 
vif(loan_full_back2) # OK
summary(loan_full_back2)

##### Stepwise #####
#Stepwise Backward Selection
loan_back <- step(loan_full_back2, data=loan_train, direction="backward")
summary(loan_back)
formula(loan_back)
# int_rate ~ term + installment + emp_length + home_ownership + 
#   is_inc_v + purpose + zip_code + dti + delinq_2yrs + inq_last_6mths + 
#   open_acc + pub_rec + revol_bal + revol_util + total_acc + 
#   cr_line_age

#Stepwise Forward Selection
loan_empty_for <- lm(int_rate~1,data=loan_train) 
loan_for <- step(loan_empty_for,   
                 scope=list(lower=formula(loan_empty_for),
                            upper=formula(loan_full_back2)),
                 data=loan_train, direction="forward")
formula(loan_for)
# int_rate ~ revol_util + term + installment + delinq_2yrs + cr_line_age + 
#   inq_last_6mths + purpose + pub_rec + home_ownership + revol_bal + 
#   open_acc + total_acc + dti + zip_code + is_inc_v + emp_length

summary(loan_for)

# Residual plot
par(mfrow = c(2, 2))
plot(loan_back)

##### Regularization#####
# Regularized regression elasticnet 

set.seed(1125)
control <- trainControl(method="cv", number=5)
fit.glmnet <- train(int_rate~., data=loan_train, method="glmnet", metric = "RMSE",trControl=control)
fit.glmnet$bestTune
#   alpha       lambda
# 4  0.55 3.466128e-05

# With center and scale
fit.glmnet1 <- train(int_rate~., data=loan_train, method="glmnet", metric = "RMSE", preProc=c("center","scale"), trControl=control)
fit.glmnet1 # no differece from the model without pre-process

# Plot variable importance
plot(varImp(fit.glmnet, lambda = fit.glmnet$bestTune$lambda), main = "Variable Importance")

##### Random Forest #####
fit.rf <- randomForest(int_rate~., data=loan_train, importance = TRUE, ntrees = 1000) 
# mtry equals to 6.
# default mtry (number of predictors) is the number of predictors divided by 3. 

# Plot variable importance
varImpPlot(fit.rf)

##### Gradient Boosting Machine #####
# Tune gbm model
gbmGrid <- expand.grid(.interaction.depth = seq(1, 7, by = 2),
                       .n.trees = seq(100, 1000, by = 50),
                       .shrinkage = c(0.01, 0.1),
                       .n.minobsinnode = 10)
set.seed(1125)
gbmTune <- train(int_rate~., data=loan_train, method = "gbm", tuneGrid = gbmGrid, verbose = FALSE)
gbmTune

# Tuning parameter 'n.minobsinnode' was held constant at a value of 10
# RMSE was used to select the optimal model using  the smallest value.
# The final values used for the model were n.trees = 1000, interaction.depth = 7, shrinkage = 0.1
# and n.minobsinnode = 10. 

# gbm model with tuned parameters
fit.gbm <- gbm(int_rate~., data=loan_train, 
               distribution = "gaussian", 
               n.trees = 1000, 
               interaction.depth = 7, 
               shrinkage = 0.1)

# plot the relative influence
summary(fit.gbm)

##### Compare MSE between models #####

# Full stack 2
#MSE
mean((loan_test$int_rate - predict(loan_full_back2, loan_test))^2)
# 0.0006209339
sd((loan_test$int_rate - predict(loan_full_back2, loan_test))^2)/sqrt(nrow(loan_test))
# 1.06945e-05

# Loan_back
#MSE
mean((loan_test$int_rate - predict(loan_back, loan_test))^2)
# 0.0006189477
sd((loan_test$int_rate - predict(loan_back, loan_test))^2)/sqrt(nrow(loan_test))
# 1.0655e-05

# Loan_for
#MSE
mean((loan_test$int_rate - predict(loan_for, loan_test))^2)
# 0.0006189477
sd((loan_test$int_rate - predict(loan_for, loan_test))^2)/sqrt(nrow(loan_test))
# 1.0655e-05

# fit.glmnet
#MSE
mean((loan_test$int_rate - predict(fit.glmnet, loan_test))^2)
# 0.0005826064
sd((loan_test$int_rate - predict(fit.glmnet, loan_test))^2)/sqrt(nrow(loan_test))
# 1.007136e-05

# fit.glmnet1
#MSE
mean((loan_test$int_rate - predict(fit.glmnet1, loan_test))^2)
# 0.0005826064
sd((loan_test$int_rate - predict(fit.glmnet1, loan_test))^2)/sqrt(nrow(loan_test))
# 1.007136e-05

# fit.rf 
#MSE
mean((loan_test$int_rate - predict(fit.rf, loan_test))^2)
# 0.000453902
sd((loan_test$int_rate - predict(fit.rf, loan_test))^2)/sqrt(nrow(loan_test))
# 7.635157e-06

# fit.gbm
#MSE
mean((loan_test$int_rate - predict(fit.gbm, loan_test, n.trees = 1000))^2)
# 0.0001929875
sd((loan_test$int_rate - predict(fit.gbm, loan_test, n.trees = 1000))^2)/sqrt(nrow(loan_test))
# 3.903445e-06


##### Prediction #####

int_rate <- predict(fit.gbm, loan_valid, n.trees = 1000)