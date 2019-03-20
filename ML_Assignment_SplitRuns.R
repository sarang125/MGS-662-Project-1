#***Predicting the no. of feedbacks a blog document is going to receive****#

# STEP 1 : Importing the files (with no headers)

# TRAIN DATASET
base_train_set <- read.csv("C:/Users/SARANG/Desktop/DOCS/Study/Spring/ML/Assignment 1/BlogFeedback/blogData_train.csv", header = FALSE)
dim(base_train_set) # Validating the dimensions to be same as given data
head(base_train_set[,51]) # Random manual check for some column value

# TEST DATASETS
test_set_feb <- read.csv("C:/Users/SARANG/Desktop/DOCS/Study/Spring/ML/Assignment 1/BlogFeedback/blogData_test-2012.02.21.00_00.csv", header = FALSE)
dim(test_set_feb)
test_set_march <- read.csv("C:/Users/SARANG/Desktop/DOCS/Study/Spring/ML/Assignment 1/BlogFeedback/blogData_test-2012.03.21.00_00.csv", header = FALSE)
dim(test_set_march)

# For Experiment 1
test_set_feb_basic <- test_set_feb[,c(51:60,281)] 
test_set_march_basic <- test_set_march[,c(51:60,281)]

# Optional to give header names
colnames(test_set_feb_basic) <- c("F1","F2","F3","F4","F5","F6","F7","F8","F9","F10","Target")
colnames(test_set_march_basic) <- c("F1","F2","F3","F4","F5","F6","F7","F8","F9","F10","Target")


# STEP 2 : Extracting only the relavant columns for Experiment 1
basic_feature <- base_train_set[,c(51:60,281)]
basic_feature <- as.data.frame(basic_feature)
dim(basic_feature)
head(basic_feature)
colnames(basic_feature) <- c("F1","F2","F3","F4","F5","F6","F7","F8","F9","F10","Target")
head(basic_feature)

# Checking for any "NA" entries
a <- basic_feature[is.na(basic_feature)]
dim(a)

# STEP 3 : Sample the trainset for ease of handling data volume
library(dplyr)

#**************EXPERIMENT 1 STARTS*********

# Trial 1 
set.seed(1)
shuffled_data <- sample_frac(basic_feature,size = 1, replace = FALSE)

basic_feature_train_trial_1 <- shuffled_data[1:5000,]
dim(basic_feature_train_trial_1)


basic_feature_model_1 <- lm(Target ~ ., data = basic_feature_train_trial_1)
basic_feature_model_1
summary(basic_feature_model_1)
residuals <- residuals(basic_feature_model_1)
(mse_1 <- mean(residuals^2)) # Mean Squared Error
(sd_1 <- sd(residuals)) # Std. Deviation

pred <- predict(basic_feature_model_1,test_set_feb_basic)
(mse_1_test_feb <- mean((test_set_feb_basic$Target - pred)^2))

pred <- predict(basic_feature_model_1,test_set_march_basic)
(mse_1_test_march <- mean((test_set_march_basic$Target - pred)^2))

# Trial 2
set.seed(2)
shuffled_data <- sample_frac(basic_feature,size = 1, replace = FALSE)

basic_feature_train_trial_2 <- shuffled_data[1:5000,]
dim(basic_feature_train_trial_2)

basic_feature_model_2 <- lm(Target ~ ., data = basic_feature_train_trial_2)
basic_feature_model_2
summary(basic_feature_model_2)
residuals <- residuals(basic_feature_model_2)
(mse_2 <- mean(residuals^2))
(sd_2 <- sd(residuals))

pred <- predict(basic_feature_model_2,test_set_feb_basic)
(mse_2_test_feb <- mean((test_set_feb_basic$Target - pred)^2))

pred <- predict(basic_feature_model_2,test_set_march_basic)
(mse_2_test_march <- mean((test_set_march_basic$Target - pred)^2))

# Trial 3
set.seed(3)
shuffled_data <- sample_frac(basic_feature,size = 1, replace = FALSE)

basic_feature_train_trial_3 <- shuffled_data[1:5000,]
dim(basic_feature_train_trial_3)

basic_feature_model_3 <- lm(Target ~ ., data = basic_feature_train_trial_3)
basic_feature_model_3
summary(basic_feature_model_3)
residuals <- residuals(basic_feature_model_3)
(mse_3 <- mean(residuals^2))
(sd_3 <- sd(residuals))

pred <- predict(basic_feature_model_3,test_set_feb_basic)
(mse_3_test_feb <- mean((test_set_feb_basic$Target - pred)^2))

pred <- predict(basic_feature_model_3,test_set_march_basic)
(mse_3_test_march <- mean((test_set_march_basic$Target - pred)^2))

# Trial 4 
set.seed(4)
shuffled_data <- sample_frac(basic_feature,size = 1, replace = FALSE)

basic_feature_train_trial_4 <- shuffled_data[1:5000,]
dim(basic_feature_train_trial_4)

basic_feature_model_4 <- lm(Target ~ ., data = basic_feature_train_trial_4)
basic_feature_model_4
summary(basic_feature_model_4)
residuals <- residuals(basic_feature_model_4)
(mse_4 <- mean(residuals^2))
(sd_4 <- sd(residuals))

pred <- predict(basic_feature_model_4,test_set_feb_basic)
(mse_4_test_feb <- mean((test_set_feb_basic$Target - pred)^2))

pred <- predict(basic_feature_model_4,test_set_march_basic)
(mse_4_test_march <- mean((test_set_march_basic$Target - pred)^2))

# Trial 5 
set.seed(5)
shuffled_data <- sample_frac(basic_feature,size = 1, replace = FALSE)

basic_feature_train_trial_5 <- shuffled_data[1:5000,]
dim(basic_feature_train_trial_5)

basic_feature_model_5 <- lm(Target ~ ., data = basic_feature_train_trial_5)
basic_feature_model_5
summary(basic_feature_model_5)
residuals <- residuals(basic_feature_model_5)
(mse_5 <- mean(residuals^2))
(sd_5 <- sd(residuals))

pred <- predict(basic_feature_model_5,test_set_feb_basic)
(mse_5_test_feb <- mean((test_set_feb_basic$Target - pred)^2))

pred <- predict(basic_feature_model_5,test_set_march_basic)
(mse_5_test_march <- mean((test_set_march_basic$Target - pred)^2))

# Average of the 5 trials just to get the sense of overall performance

(avg_mse_train_basic_5_trials <- mean(mse_1,mse_2,mse_3,mse_4,mse_5))
(avg_mse_test_basic_5_trials_feb <- mean(mse_1_test_feb,mse_2_test_feb,mse_3_test_feb,mse_4_test_feb,mse_5_test_feb))
(avg_mse_test_basic_5_trials_march <- mean(mse_1_test_march,mse_2_test_march,mse_3_test_march,mse_4_test_march,mse_5_test_march))


(result_basic_features <- data.frame(Trial_No = c("1","2","3","4","5"),
                     Std_Dev_Train = c(sd_1,sd_2,sd_3,sd_4,sd_5),
                     MSE_Train = c(mse_1,mse_2,mse_3,mse_4,mse_5),
                     MSE_Test_Feb = c(mse_1_test_feb,mse_2_test_feb,mse_3_test_feb,mse_4_test_feb,mse_5_test_feb),
                     MSE_Test_March = c(mse_1_test_march,mse_2_test_march,mse_3_test_march,mse_4_test_march,mse_5_test_march)))

"
Summary of the SD,MSE on TRAIN,TEST datasets

Trial_No Std_Dev_Train MSE_Train MSE_Test_Feb MSE_Test_March
      1      32.93693  1084.624     2290.892       203.2740
      2      32.41406  1050.461     2231.797       190.9155
      3      38.55493  1486.185     2271.272       196.0413
      4      39.29995  1544.177     2352.733       234.0599
      5      33.63287  1130.944     2101.678       213.1533

"

#***************EXPERIMENT 1 ENDS*********

#**************EXPERIMENT 2 STARTS*********

textual_feature <- base_train_set[,c(63:262,281)] # Picking only the relavent features
textual_feature <- as.data.frame(textual_feature)
dim(textual_feature)
head(textual_feature)

test_set_feb_textual <- test_set_feb[,c(63:262,281)]
head(test_set_feb_textual)
test_set_march_textual <- test_set_march[,c(63:262,281)]
head(test_set_march_textual)

# Trial 1 
set.seed(1)
shuffled_data <- sample_frac(textual_feature,size = 1, replace = FALSE)

textual_feature_train_trial_1 <- shuffled_data[1:5000,]

textual_feature_model_1 <- lm(V281 ~ ., data = textual_feature_train_trial_1)
textual_feature_model_1
summary(textual_feature_model_1)
residuals <- residuals(textual_feature_model_1)
(mse_1 <- mean(residuals^2))
(sd_1 <- sd(residuals))

pred <- predict(textual_feature_model_1,test_set_feb_textual)
(mse_1_test_feb <- mean((test_set_feb_textual$V281 - pred)^2))

pred <- predict(textual_feature_model_1,test_set_march_textual)
(mse_1_test_march <- mean((test_set_march_textual$V281 - pred)^2))

# Trial 2
textual_feature_train_trial_2 <- shuffled_data[5001:10000,]
dim(textual_feature_train_trial_2)

textual_feature_model_2 <- lm(V281 ~ ., data = textual_feature_train_trial_2)
textual_feature_model_2
summary(textual_feature_model_2)
residuals <- residuals(textual_feature_model_2)
(mse_2 <- mean(residuals^2))
(sd_2 <- sd(residuals))

pred <- predict(textual_feature_model_2,test_set_feb_textual)
(mse_2_test_feb <- mean((test_set_feb_textual$V281 - pred)^2))

pred <- predict(textual_feature_model_2,test_set_march_textual)
(mse_2_test_march <- mean((test_set_march_textual$V281 - pred)^2))

# Trial 3
textual_feature_train_trial_3 <- shuffled_data[10001:15000,]
dim(textual_feature_train_trial_3)

textual_feature_model_3 <- lm(V281 ~ ., data = textual_feature_train_trial_3)
textual_feature_model_3
summary(textual_feature_model_3)
residuals <- residuals(textual_feature_model_3)
(mse_3 <- mean(residuals^2))
(sd_3 <- sd(residuals))

pred <- predict(textual_feature_model_3,test_set_feb_textual)
(mse_3_test_feb <- mean((test_set_feb_textual$V281 - pred)^2))

pred <- predict(textual_feature_model_3,test_set_march_textual)
(mse_3_test_march <- mean((test_set_march_textual$V281 - pred)^2))

# Trial 4 
textual_feature_train_trial_4 <- shuffled_data[15001:20000,]
dim(textual_feature_train_trial_4)

textual_feature_model_4 <- lm(V281 ~ ., data = textual_feature_train_trial_4)
textual_feature_model_4
summary(textual_feature_model_4)
residuals <- residuals(textual_feature_model_4)
(mse_4 <- mean(residuals^2))
(sd_4 <- sd(residuals))

pred <- predict(textual_feature_model_4,test_set_feb_textual)
(mse_4_test_feb <- mean((test_set_feb_textual$V281 - pred)^2))

pred <- predict(textual_feature_model_4,test_set_march_textual)
(mse_4_test_march <- mean((test_set_march_textual$V281 - pred)^2))

# Trial 5 
textual_feature_train_trial_5 <- shuffled_data[20001:25000,]
dim(textual_feature_train_trial_5)

textual_feature_model_5 <- lm(V281 ~ ., data = textual_feature_train_trial_5)
textual_feature_model_5
summary(textual_feature_model_5)
residuals <- residuals(textual_feature_model_5)
(mse_5 <- mean(residuals^2))
(sd_5 <- sd(residuals))

pred <- predict(textual_feature_model_5,test_set_feb_textual)
(mse_5_test_feb <- mean((test_set_feb_textual$V281 - pred)^2))

pred <- predict(textual_feature_model_5,test_set_march_textual)
(mse_5_test_march <- mean((test_set_march_textual$V281 - pred)^2))

(avg_mse_train_textual_5_trials <- mean(mse_1,mse_2,mse_3,mse_4,mse_5))
(avg_mse_test_textual_5_trials_feb <- mean(mse_1_test_feb,mse_2_test_feb,mse_3_test_feb,mse_4_test_feb,mse_5_test_feb))
(avg_mse_test_textual_5_trials_march <- mean(mse_1_test_march,mse_2_test_march,mse_3_test_march,mse_4_test_march,mse_5_test_march))

(result_textual_features <- data.frame(Trial_No = c("1","2","3","4","5"),
                                     Std_Deviation_Train = c(sd_1,sd_2,sd_3,sd_4,sd_5),
                                     MSE_Train = c(mse_1,mse_2,mse_3,mse_4,mse_5),
                                     MSE_Test_Feb = c(mse_1_test_feb,mse_2_test_feb,mse_3_test_feb,mse_4_test_feb,mse_5_test_feb),
                                     MSE_Test_March = c(mse_1_test_march,mse_2_test_march,mse_3_test_march,mse_4_test_march,mse_5_test_march)))

"
Summary of SD,MSE for Train,Test Datasets

Trial_No Std_Deviation_Train MSE_Train MSE_Test_Feb MSE_Test_March
      1            32.93693  1084.624     2290.892       203.2740
      2            32.41406  1050.461     2231.797       190.9155
      3            38.55493  1486.185     2271.272       196.0413
      4            39.29995  1544.177     2352.733       234.0599
      5            33.63287  1130.944     2101.678       213.1533
 

"



library(glmnet)
x <- as.matrix(basic_feature_train_trial_1[,1:10])
head(x)
y <-  as.matrix(basic_feature_train_trial_1[11])
head(y)

basic_feature_glmnet_1 <- glmnet(x, y, family = "gaussian", alpha = 0)
basic_feature_glmnet_1
summary(basic_feature_glmnet_1)
test <- predict(basic_feature_glmnet_1,as.matrix(test_set_feb_basic[,1:10]), type = "link", s = 1.69)
test
(diff <- mean((test_set_feb_basic$Target - test)^2))

#March
test <- predict(basic_feature_glmnet_1,as.matrix(test_set_march_basic[,1:10]), type = "link", s = 1.69)
(diff <- mean((test_set_march_basic$Target - test)^2))
