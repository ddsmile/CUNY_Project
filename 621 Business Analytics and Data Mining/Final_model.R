library(car)
library(caret)
library(randomForest)
library(gbm)


# setwd("C:\\Users\\xiwu\\Documents\\course\\CUNY_MSDA\\621 Business Analysis and Data Mining\\Final")
setwd("C:\\Users\\ddsmile\\Documents\\CUNY_MSDA\\621 Business Analytics\\Final")

#load the rdata file from the local directory
loan <-load("loan.rdata")
loan1$zip_code <- as.factor(loan1$zip_code) # convert zipcode to factor

#remove the rows with NAs
#goes from 39,786 to 38,661 rows (1,125 rows removed)
loan1 <- loan1[complete.cases(loan1),]


#####Linear regression and check multicollinearity#####
# Full model
# loan1_full_back <- lm(int_rate~., data=loan1_train)
# summary(loan1_full_back)
# 
# # multicollinearity
# vif(loan1_full_back)
# # Error in vif.default(loan1_full_back) : there are aliased coefficients in the model
# # http://stats.stackexchange.com/questions/112442/what-are-aliased-coefficients
# alias(loan1_full_back)
# # The problem is from grade, subgrade and cr_line_age

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

##### Linear regression#####
loan_full_back <- lm(int_rate~., data=loan_train)
vif(loan_full_back)
# GVIF Df GVIF^(1/(2*Df))
# loan_amnt       1.878707e+01  1        4.334406
# funded_amnt_inv 1.087491e+01  1        3.297713
# term            2.614533e+00  1        1.616952
# installment     1.773823e+01  1        4.211677
# emp_length      1.185055e+00  1        1.088603
# home_ownership  1.437893e+00  4        1.046444
# annual_inc      1.207246e+00  1        1.098747
# is_inc_v        1.321287e+00  2        1.072135
# purpose         1.431934e+00 13        1.013904
# zip_code        5.005692e+21  9       16.051876
# addr_state      5.673809e+21 49        1.667164
# dti             1.352236e+00  1        1.162857
# delinq_2yrs     1.035770e+00  1        1.017728
# inq_last_6mths  1.063476e+00  1        1.031250
# open_acc        2.110338e+00  1        1.452700
# pub_rec         1.035658e+00  1        1.017673
# revol_bal       1.519959e+00  1        1.232866
# revol_util      1.388986e+00  1        1.178552
# total_acc       2.372691e+00  1        1.540354
# cr_line_age     1.355130e+00  1        1.164101

# http://statisticalhorizons.com/multicollinearity
summary(loan_full_back)

# loan_back <- step(loan_full_back, data=loan_train, direction="backward")
# summary(loan_back)
# formula(loan_back)
# # int_rate ~ loan_amnt + funded_amnt_inv + term + installment + 
# #     emp_length + home_ownership + annual_inc + is_inc_v + purpose + 
# #     zip_code + dti + delinq_2yrs + inq_last_6mths + open_acc + 
# #     pub_rec + revol_bal + revol_util + total_acc + cr_line_age
# 
# vif(loan_back)


###### Multicollinearity problem #####

# Create correlation matrix between predictors (numeric variable only)
loan.num <- loan[ , -which(names(loan) %in% 
                             c("int_rate", "term", "home_ownership", "is_inc_v", "purpose", "zip_code", "addr_state"))]
cortable <- cor(loan.num) 

(sum(cortable["loan_amnt",])-1)/15 # average correlation for loan_amnt
# [1] 0.221002

(sum(cortable[c("funded_amnt_inv"),])-1)/15 # average correlation for funded_amnt_inv
# [1] 0.21452

(sum(cortable[c("installment"),])-1)/15 # average correlation for installment 
# [1] 0.21432

# remove the higher average correlation, therefore loan_amnt

# Remove loan_amnt
loan_full_back1 <- lm(int_rate~.-loan_amnt, data=loan_train)
vif(loan_full_back1)
# GVIF Df GVIF^(1/(2*Df))
# funded_amnt_inv 9.963256e+00  1        3.156463
# term            1.795742e+00  1        1.340053
# installment     8.814094e+00  1        2.968854
# emp_length      1.184577e+00  1        1.088383
# home_ownership  1.436304e+00  4        1.046299
# annual_inc      1.206348e+00  1        1.098339
# is_inc_v        1.311658e+00  2        1.070176
# purpose         1.422263e+00 13        1.013640
# zip_code        5.005079e+21  9       16.051766
# addr_state      5.650635e+21 49        1.667094
# dti             1.351910e+00  1        1.162717
# delinq_2yrs     1.032170e+00  1        1.015958
# inq_last_6mths  1.061571e+00  1        1.030326
# open_acc        2.106787e+00  1        1.451478
# pub_rec         1.035366e+00  1        1.017530
# revol_bal       1.511119e+00  1        1.229276
# revol_util      1.358034e+00  1        1.165347
# total_acc       2.368338e+00  1        1.538941
# cr_line_age     1.352345e+00  1        1.162904

# Remove funded_amnt_inv

##### Final full model #####
loan_full_back2 <- lm(int_rate~.-loan_amnt-funded_amnt_inv, data=loan_train)
vif(loan_full_back2) 
# OK
# GVIF Df GVIF^(1/(2*Df))
# term           1.117800e+00  1        1.057260
# installment    1.436932e+00  1        1.198721
# emp_length     1.182164e+00  1        1.087274
# home_ownership 1.434797e+00  4        1.046162
# annual_inc     1.206348e+00  1        1.098339
# is_inc_v       1.291288e+00  2        1.065996
# purpose        1.413044e+00 13        1.013387
# zip_code       5.003862e+21  9       16.051549
# addr_state     5.609612e+21 49        1.666970
# dti            1.350202e+00  1        1.161982
# delinq_2yrs    1.024727e+00  1        1.012288
# inq_last_6mths 1.056561e+00  1        1.027892
# open_acc       2.098385e+00  1        1.448580
# pub_rec        1.033854e+00  1        1.016786
# revol_bal      1.509757e+00  1        1.228722
# revol_util     1.346621e+00  1        1.160440
# total_acc      2.364335e+00  1        1.537639
# cr_line_age    1.347405e+00  1        1.160778
summary(loan_full_back2)

# Residual standard error: 0.02489 on 23106 degrees of freedom
# Multiple R-squared:  0.5534,	Adjusted R-squared:  0.5516 
# F-statistic: 318.1 on 90 and 23106 DF,  p-value: < 2.2e-16



##### Stepwise #####
#Stepwise Backward Selection
loan_back <- step(loan_full_back2, data=loan_train, direction="backward")
summary(loan_back)
# Residual standard error: 0.02489 on 23156 degrees of freedom
# Multiple R-squared:  0.5524,	Adjusted R-squared:  0.5517 
# F-statistic: 714.5 on 40 and 23156 DF,  p-value: < 2.2e-16
formula(loan_back)
# int_rate ~ term + installment + emp_length + home_ownership + 
#   is_inc_v + purpose + zip_code + dti + delinq_2yrs + inq_last_6mths + 
#   open_acc + pub_rec + revol_bal + revol_util + total_acc + 
#   cr_line_age

#Stepwise Forward Selection
loan_empty_for <- lm(int_rate~1,data=loan_train)
# summary(loan_empty_for)
loan_for <- step(loan_empty_for,   
                 scope=list(lower=formula(loan_empty_for),
                            upper=formula(loan_full_back2)),
                 data=loan_train, direction="forward")
formula(loan_for)
# int_rate ~ revol_util + term + installment + delinq_2yrs + cr_line_age + 
#   inq_last_6mths + purpose + pub_rec + home_ownership + revol_bal + 
#   open_acc + total_acc + dti + zip_code + is_inc_v + emp_length

summary(loan_for)
# Residual standard error: 0.02489 on 23156 degrees of freedom
# Multiple R-squared:  0.5524,	Adjusted R-squared:  0.5517 
# F-statistic: 714.5 on 40 and 23156 DF,  p-value: < 2.2e-16

AIC(loan_full_back2) #-105423.7
AIC(loan_back) #-105475.1
AIC(loan_for) #-105475.1

BIC(loan_full_back2) #-104683
BIC(loan_back) #-105136.9
BIC(loan_for) #-105136.9

par(mfrow = c(2, 2))
plot(loan_full_back)

plot(loan_back)

plot(loan_for)

##### Regularization#####
# Regularized regression elasticnet 
# glmnet: https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
set.seed(1125)
control <- trainControl(method="cv", number=5)
fit.glmnet <- train(int_rate~., data=loan_train, method="glmnet", metric = "RMSE",trControl=control)
fit.glmnet
# alpha  lambda        RMSE        Rsquared   RMSE SD       Rsquared SD
# 0.10   3.466128e-05  0.02411562  0.5792248  0.0001421774  0.004311486
# 0.10   3.466128e-04  0.02419245  0.5768573  0.0001656868  0.005224735
# 0.10   3.466128e-03  0.02522099  0.5460452  0.0001719979  0.006267574
# 0.55   3.466128e-05  0.02411393  0.5792870  0.0001474217  0.004522287
# 0.55   3.466128e-04  0.02427824  0.5743003  0.0001831668  0.005982884
# 0.55   3.466128e-03  0.02621653  0.5202503  0.0001855797  0.007873125
# 1.00   3.466128e-05  0.02411396  0.5792924  0.0001506785  0.004673805
# 1.00   3.466128e-04  0.02440318  0.5704411  0.0001934615  0.006431524
# 1.00   3.466128e-03  0.02725012  0.4930518  0.0001899227  0.008902073
# 
# RMSE was used to select the optimal model using  the smallest value.
# The final values used for the model were alpha = 0.55 and lambda = 3.466128e-05. 

fit.glmnet$bestTune
#   alpha       lambda
# 4  0.55 3.466128e-05


fit.glmnet$
# plot(fit.glmnet)

# http://stats.stackexchange.com/questions/67827/caret-and-coefficients-glmnet
coef(fit.glmnet$finalModel, 3.466128e-05)
fit.glmnet$bestTune$lambda

# With center and scale
fit.glmnet1 <- train(int_rate~., data=loan_train, method="glmnet", metric = "RMSE", preProc=c("center","scale"), trControl=control)
fit.glmnet1

fit.glmnet1
# glmnet 
# 
# 23197 samples
# 20 predictor
# 
# Pre-processing: centered (92), scaled (92) 
# Resampling: Cross-Validated (5 fold) 
# Summary of sample sizes: 18558, 18557, 18558, 18557, 18558 
# Resampling results across tuning parameters:
#   
#   alpha  lambda        RMSE        Rsquared   RMSE SD       Rsquared SD
# 0.10   3.466128e-05  0.02410047  0.5795984  0.0003267441  0.010319809
# 0.10   3.466128e-04  0.02417731  0.5772376  0.0003231526  0.010095288
# 0.10   3.466128e-03  0.02521668  0.5460928  0.0002942557  0.009748254
# 0.55   3.466128e-05  0.02409852  0.5796700  0.0003251920  0.010336099
# 0.55   3.466128e-04  0.02426479  0.5746315  0.0003150664  0.009949867
# 0.55   3.466128e-03  0.02621743  0.5201006  0.0002746243  0.010699143
# 1.00   3.466128e-05  0.02409951  0.5796417  0.0003239711  0.010366699
# 1.00   3.466128e-04  0.02439557  0.5705656  0.0003084614  0.009905443
# 1.00   3.466128e-03  0.02725073  0.4929534  0.0002912328  0.011338623
# 
# RMSE was used to select the optimal model using  the smallest value.
# The final values used for the model were alpha = 0.55 and lambda = 3.466128e-05. 


# http://www.inside-r.org/packages/cran/caret/docs/varimp
plot(varImp(fit.glmnet, lambda = fit.glmnet$bestTune$lambda), main = "Variable Importance")


# http://www.rmdk.ca/boosting_forests_bagging.html
##### Random Forest #####
fit.rf <- randomForest(int_rate~., data=loan_train, importance = TRUE, ntrees = 1000) 
# mtry equals to 6.
# default mtry (number of predictors) is the number of predictors divided by 3. 

varImpPlot(fit.rf)

# Use caret package
fit.rf.caret <- train(int_rate~., data = loan_train,
                  method='rf',
                  importance=TRUE)

varImpPlot(fit.rf.caret$finalModel)

##### Gradient Boosting Machine #####

gbmGrid <- expand.grid(.interaction.depth = seq(1, 7, by = 2),
                       .n.trees = seq(100, 1000, by = 50),
                       .shrinkage = c(0.01, 0.1),
                       .n.minobsinnode = 10)
set.seed(1125)
gbmTune <- train(int_rate~., data=loan_train, method = "gbm", tuneGrid = gbmGrid, verbose = FALSE)
gbmTune

# > gbmTune
# Stochastic Gradient Boosting 
# 
# 23197 samples
# 20 predictor
# 
# No pre-processing
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 23197, 23197, 23197, 23197, 23197, 23197, ... 
# Resampling results across tuning parameters:
#   
#   shrinkage  interaction.depth  n.trees  RMSE        Rsquared   RMSE SD       Rsquared SD
# 0.01       1                   100     0.03264101  0.3789593  0.0001711914  0.007288695
# 0.01       1                   150     0.03140598  0.3986688  0.0001653298  0.008069017
# 0.01       1                   200     0.03048883  0.4186262  0.0001624823  0.007143298
# 0.01       1                   250     0.02978867  0.4352228  0.0001595149  0.007475263
# 0.01       1                   300     0.02920492  0.4565503  0.0001574548  0.008135369
# 0.01       1                   350     0.02868882  0.4747959  0.0001564397  0.008323290
# 0.01       1                   400     0.02823566  0.4892681  0.0001543114  0.007553008
# 0.01       1                   450     0.02783788  0.5008384  0.0001510999  0.007361104
# 0.01       1                   500     0.02748677  0.5112688  0.0001484179  0.007028438
# 0.01       1                   550     0.02716936  0.5206310  0.0001456165  0.006408616
# 0.01       1                   600     0.02687890  0.5294656  0.0001457568  0.006349188
# 0.01       1                   650     0.02661313  0.5371508  0.0001448745  0.006249203
# 0.01       1                   700     0.02636718  0.5445429  0.0001425061  0.006105294
# 0.01       1                   750     0.02614136  0.5506750  0.0001400600  0.006013033
# 0.01       1                   800     0.02593242  0.5560928  0.0001393664  0.005638825
# 0.01       1                   850     0.02574017  0.5610881  0.0001384680  0.005529104
# 0.01       1                   900     0.02556153  0.5657162  0.0001370357  0.005558532
# 0.01       1                   950     0.02539559  0.5700465  0.0001350317  0.005382100
# 0.01       1                  1000     0.02524154  0.5738709  0.0001337033  0.005305532
# 0.01       3                   100     0.03051125  0.4333155  0.0001654253  0.008298968
# 0.01       3                   150     0.02895215  0.4789647  0.0001648329  0.007995029
# 0.01       3                   200     0.02781263  0.5104763  0.0001619368  0.007162483
# 0.01       3                   250     0.02694972  0.5326626  0.0001574211  0.006556950
# 0.01       3                   300     0.02625994  0.5506033  0.0001524391  0.006412389
# 0.01       3                   350     0.02569669  0.5647337  0.0001487133  0.005774112
# 0.01       3                   400     0.02522634  0.5765086  0.0001454671  0.005549651
# 0.01       3                   450     0.02482839  0.5863781  0.0001418860  0.005324306
# 0.01       3                   500     0.02449033  0.5944356  0.0001384522  0.005276314
# 0.01       3                   550     0.02420215  0.6011495  0.0001345203  0.005096359
# 0.01       3                   600     0.02395426  0.6069925  0.0001323865  0.004998881
# 0.01       3                   650     0.02373648  0.6121777  0.0001309581  0.004829010
# 0.01       3                   700     0.02354005  0.6170446  0.0001286030  0.004777414
# 0.01       3                   750     0.02336204  0.6216609  0.0001279654  0.004741136
# 0.01       3                   800     0.02319777  0.6260155  0.0001291433  0.004772081
# 0.01       3                   850     0.02304914  0.6299679  0.0001274145  0.004688982
# 0.01       3                   900     0.02291020  0.6336228  0.0001275072  0.004570587
# 0.01       3                   950     0.02278213  0.6370154  0.0001260878  0.004480072
# 0.01       3                  1000     0.02266285  0.6401970  0.0001249894  0.004447237
# 0.01       5                   100     0.02963406  0.4705014  0.0001629727  0.007562276
# 0.01       5                   150     0.02794544  0.5118704  0.0001663341  0.008021573
# 0.01       5                   200     0.02674627  0.5415716  0.0001628591  0.007297244
# 0.01       5                   250     0.02584797  0.5635209  0.0001530839  0.006291787
# 0.01       5                   300     0.02514364  0.5806800  0.0001483388  0.005924878
# 0.01       5                   350     0.02458051  0.5943554  0.0001441673  0.005429187
# 0.01       5                   400     0.02412629  0.6052249  0.0001415755  0.005320238
# 0.01       5                   450     0.02374887  0.6143685  0.0001373268  0.005076337
# 0.01       5                   500     0.02342885  0.6223413  0.0001341567  0.004856460
# 0.01       5                   550     0.02315021  0.6294202  0.0001342837  0.004832849
# 0.01       5                   600     0.02290474  0.6358068  0.0001345190  0.004811951
# 0.01       5                   650     0.02268426  0.6416884  0.0001347394  0.004715328
# 0.01       5                   700     0.02248510  0.6469872  0.0001331112  0.004596346
# 0.01       5                   750     0.02230468  0.6517950  0.0001298937  0.004526910
# 0.01       5                   800     0.02213944  0.6562540  0.0001291480  0.004490569
# 0.01       5                   850     0.02198561  0.6604150  0.0001282715  0.004515082
# 0.01       5                   900     0.02184312  0.6643020  0.0001269008  0.004403709
# 0.01       5                   950     0.02171305  0.6678221  0.0001285776  0.004439003
# 0.01       5                  1000     0.02159286  0.6710729  0.0001311992  0.004445283
# 0.01       7                   100     0.02905746  0.4952707  0.0001615490  0.007391550
# 0.01       7                   150     0.02730984  0.5322341  0.0001646490  0.007446335
# 0.01       7                   200     0.02607264  0.5612842  0.0001603498  0.006650120
# 0.01       7                   250     0.02516014  0.5828492  0.0001530113  0.005958695
# 0.01       7                   300     0.02445997  0.5992842  0.0001460933  0.005696490
# 0.01       7                   350     0.02390784  0.6125391  0.0001437379  0.005627314
# 0.01       7                   400     0.02345448  0.6237590  0.0001447562  0.005723107
# 0.01       7                   450     0.02307487  0.6333686  0.0001436088  0.005459019
# 0.01       7                   500     0.02274643  0.6417943  0.0001407627  0.005237344
# 0.01       7                   550     0.02245994  0.6493406  0.0001363778  0.004996187
# 0.01       7                   600     0.02220476  0.6561180  0.0001370036  0.005067645
# 0.01       7                   650     0.02197364  0.6622874  0.0001391000  0.005037980
# 0.01       7                   700     0.02176758  0.6677630  0.0001368792  0.004930081
# 0.01       7                   750     0.02157820  0.6728872  0.0001378128  0.004796841
# 0.01       7                   800     0.02140201  0.6777000  0.0001370774  0.004704315
# 0.01       7                   850     0.02123752  0.6821810  0.0001362870  0.004625617
# 0.01       7                   900     0.02108549  0.6862952  0.0001370798  0.004610470
# 0.01       7                   950     0.02093819  0.6903319  0.0001369843  0.004545242
# 0.01       7                  1000     0.02079975  0.6941399  0.0001355820  0.004419958
# 0.10       1                   100     0.02516407  0.5752345  0.0001368300  0.005324174
# 0.10       1                   150     0.02410147  0.5994282  0.0001230165  0.004736568
# 0.10       1                   200     0.02352626  0.6130799  0.0001173832  0.004514919
# 0.10       1                   250     0.02315376  0.6222133  0.0001176510  0.004284986
# 0.10       1                   300     0.02290296  0.6280856  0.0001135616  0.004279434
# 0.10       1                   350     0.02272498  0.6326973  0.0001139266  0.004247739
# 0.10       1                   400     0.02258480  0.6365070  0.0001129630  0.004132322
# 0.10       1                   450     0.02246432  0.6399400  0.0001126875  0.004001035
# 0.10       1                   500     0.02235699  0.6430751  0.0001126178  0.004020014
# 0.10       1                   550     0.02225804  0.6460137  0.0001140038  0.004071857
# 0.10       1                   600     0.02216766  0.6487171  0.0001126975  0.003993784
# 0.10       1                   650     0.02208224  0.6512692  0.0001131468  0.004058866
# 0.10       1                   700     0.02200342  0.6536483  0.0001140803  0.004059880
# 0.10       1                   750     0.02192868  0.6558800  0.0001152324  0.004058093
# 0.10       1                   800     0.02185542  0.6580622  0.0001168887  0.004085236
# 0.10       1                   850     0.02178736  0.6601389  0.0001174376  0.004095109
# 0.10       1                   900     0.02172215  0.6620755  0.0001191857  0.004159132
# 0.10       1                   950     0.02165893  0.6639603  0.0001189398  0.004105008
# 0.10       1                  1000     0.02159684  0.6658417  0.0001203459  0.004118630
# 0.10       3                   100     0.02267268  0.6387208  0.0001348877  0.004639837
# 0.10       3                   150     0.02183028  0.6618652  0.0001347711  0.004503118
# 0.10       3                   200     0.02117998  0.6806730  0.0001298718  0.004343775
# 0.10       3                   250     0.02060785  0.6971380  0.0001350711  0.004526606
# 0.10       3                   300     0.02011882  0.7110642  0.0001532578  0.004877864
# 0.10       3                   350     0.01968556  0.7233282  0.0001625169  0.004979700
# 0.10       3                   400     0.01930375  0.7339806  0.0001637022  0.004730680
# 0.10       3                   450     0.01895853  0.7435436  0.0001620945  0.004634687
# 0.10       3                   500     0.01863451  0.7523884  0.0001558057  0.004397481
# 0.10       3                   550     0.01832519  0.7607454  0.0001620437  0.004362526
# 0.10       3                   600     0.01803784  0.7683376  0.0001638327  0.004271479
# 0.10       3                   650     0.01777542  0.7752397  0.0001604012  0.004146656
# 0.10       3                   700     0.01751709  0.7818996  0.0001666829  0.004071296
# 0.10       3                   750     0.01727577  0.7880388  0.0001643675  0.004112775
# 0.10       3                   800     0.01703955  0.7939247  0.0001594565  0.003974467
# 0.10       3                   850     0.01681747  0.7993648  0.0001546687  0.003749428
# 0.10       3                   900     0.01660023  0.8046917  0.0001502140  0.003643030
# 0.10       3                   950     0.01639411  0.8096876  0.0001580834  0.003809258
# 0.10       3                  1000     0.01619432  0.8144278  0.0001562112  0.003776585
# 0.10       5                   100     0.02163167  0.6688592  0.0001489648  0.005494773
# 0.10       5                   150     0.02062522  0.6972758  0.0001432226  0.005167176
# 0.10       5                   200     0.01973955  0.7220455  0.0001578206  0.005327677
# 0.10       5                   250     0.01904431  0.7414194  0.0001612959  0.004738263
# 0.10       5                   300     0.01842139  0.7582106  0.0001564206  0.004596996
# 0.10       5                   350     0.01788862  0.7723236  0.0001512142  0.004174188
# 0.10       5                   400     0.01742417  0.7842979  0.0001573390  0.004133875
# 0.10       5                   450     0.01698889  0.7951611  0.0001632195  0.004406879
# 0.10       5                   500     0.01658176  0.8051243  0.0001681178  0.004283003
# 0.10       5                   550     0.01621479  0.8138925  0.0001637305  0.004162486
# 0.10       5                   600     0.01588047  0.8217453  0.0001724075  0.004214021
# 0.10       5                   650     0.01554714  0.8293463  0.0001784659  0.004231174
# 0.10       5                   700     0.01524490  0.8361096  0.0001782177  0.004168076
# 0.10       5                   750     0.01496054  0.8423358  0.0001770949  0.003997576
# 0.10       5                   800     0.01469590  0.8480375  0.0001664768  0.003619617
# 0.10       5                   850     0.01444657  0.8532930  0.0001677908  0.003500572
# 0.10       5                   900     0.01420918  0.8581750  0.0001565531  0.003138973
# 0.10       5                   950     0.01398791  0.8626809  0.0001582278  0.003149871
# 0.10       5                  1000     0.01377833  0.8668653  0.0001487032  0.002953018
# 0.10       7                   100     0.02084966  0.6917273  0.0001861666  0.005500959
# 0.10       7                   150     0.01961008  0.7259287  0.0001654826  0.004758524
# 0.10       7                   200     0.01863721  0.7525543  0.0001540832  0.004597367
# 0.10       7                   250     0.01784299  0.7735498  0.0001627958  0.004406263
# 0.10       7                   300     0.01718273  0.7903753  0.0001696510  0.004287401
# 0.10       7                   350     0.01660157  0.8046666  0.0001766779  0.004220306
# 0.10       7                   400     0.01607110  0.8173081  0.0001733159  0.003987188
# 0.10       7                   450     0.01559218  0.8283046  0.0001686365  0.003787695
# 0.10       7                   500     0.01516844  0.8377944  0.0001750994  0.003708441
# 0.10       7                   550     0.01477494  0.8462849  0.0001743755  0.003625953
# 0.10       7                   600     0.01441700  0.8538293  0.0001739891  0.003620891
# 0.10       7                   650     0.01409913  0.8604370  0.0001593337  0.003169481
# 0.10       7                   700     0.01379745  0.8664178  0.0001572687  0.003033673
# 0.10       7                   750     0.01351466  0.8719940  0.0001635146  0.003033346
# 0.10       7                   800     0.01325151  0.8769945  0.0001638453  0.003063032
# 0.10       7                   850     0.01301668  0.8814103  0.0001495919  0.002668648
# 0.10       7                   900     0.01279060  0.8855774  0.0001453310  0.002574729
# 0.10       7                   950     0.01258671  0.8892242  0.0001410643  0.002542343
# 0.10       7                  1000     0.01239209  0.8926579  0.0001444812  0.002513359
# 
# Tuning parameter 'n.minobsinnode' was held constant at a value of 10
# RMSE was used to select the optimal model using  the smallest value.
# The final values used for the model were n.trees = 1000, interaction.depth = 7, shrinkage = 0.1
# and n.minobsinnode = 10. 

fit.gbm <- gbm(int_rate~., data=loan_train, 
               distribution = "gaussian", 
               n.trees = 1000, 
               interaction.depth = 7, 
               shrinkage = 0.1)
summary(fit.gbm)
plot.gbm(fit.gbm)

varImp(fit.gbm, n.trees = 1000)
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


##### caret package
(RMSE(obs = loan_test$int_rate, pred = predict(loan_full_back2, loan_test)))^2
(RMSE(obs = loan_test$int_rate, pred = predict(loan_for, loan_test)))^2
(RMSE(obs = loan_test$int_rate, pred = predict(fit.glmnet, loan_test)))^2


##### Prediction #####

int_rate <- predict(fit.gbm, loan_valid, n.trees = 1000)
