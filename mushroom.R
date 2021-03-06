if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(DataExplorer)) install.packages("DataExplorer", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(DataExplorer)
library(rpart.plot)

##########################################################
# Download Data set, create train set & test test
##########################################################

# UCI Machine Learning Repository Mushroom dataset:

ifelse(file.exists(".\\data\\agaricus-lepiota.data"),
       { 
         dl <- ".\\data\\agaricus-lepiota.data"
       },
       {
         dl <- tempfile()
         download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data", dl)
       })

mushroom <- fread(text = readLines(dl), header = FALSE, stringsAsFactors = TRUE,
                  col.names = c(
                    "class","cap_shape",
                    "cap_surface","cap_color",
                    "bruises","odor","gill_attachment",
                    "gill_spacing","gill_size","gill_color",
                    "stalk_shape","stalk_root","stalk_surface_above_ring",
                    "stalk_surface_below_ring","stalk_color_above_ring",
                    "stalk_color_below_ring","veil_type","veil_color",
                    "ring_number","ring_type","spore_print_color",
                    "population","habitat"))


mushroom <- as.data.frame(mushroom)
rm(dl)

# quick check on data

str(mushroom)
summary(mushroom)

# remove missing & constant atribute

mushroom <- mushroom %>% select(-veil_type, -stalk_root)


# using data.explorer package to do data exploration

plot_intro(mushroom)

plot_missing(mushroom)

plot_str(mushroom)

plot_bar(mushroom, nrow = 2L, ncol = 4L)

plot_bar(mushroom)

plot_bar(mushroom, by = "class")

plot_correlation(mushroom, maxcat = 5L)

# splite test set & train set

set.seed(12345, sample.kind="Rounding")
test_index <- createDataPartition(y = mushroom$class, times = 1,
                                  p = 0.2, list = FALSE)
test_set <- mushroom[test_index,]
train_set <- mushroom[-test_index,]

rm(test_index)


###########
### glm
###########

start_time <- Sys.time()
fit_glm <- train(class ~ ., method = "glm", data = train_set)
time_diff <- Sys.time() - start_time

s <- summary(fit_glm)

s

# look for NA attribute

s$aliased[which(s$aliased == TRUE)]

# remove NA attribute

train_set <- train_set %>% select(-stalk_color_above_ring, -stalk_color_below_ring, 
                                  -veil_color, -ring_number, -ring_type, -spore_print_color)
test_set <- test_set %>% select(-stalk_color_above_ring, -stalk_color_below_ring, 
                                -veil_color, -ring_number, -ring_type, -spore_print_color)

# redo training 

start_time <- Sys.time()
fit_glm <- train(class ~ ., method = "glm", data = train_set)# maxit = 200)
time_diff <- Sys.time() - start_time

s <- summary(fit_glm)

s

s$aliased[which(s$aliased == TRUE)]

# record results

y_hat_glm <- predict(fit_glm, test_set)

cm <- confusionMatrix(y_hat_glm, test_set$class)

model_results <- tibble(Method = "glm",
                        Accuracy = cm$overall["Accuracy"],
                        Kappa = cm$overall["Kappa"],
                        Sensitivity = cm$byClass["Sensitivity"],
                        Specificity = cm$byClass["Specificity"],
                        Train_Time = time_diff)

model_results %>% knitr::kable()
                            

### data recovery

set.seed(12345, sample.kind="Rounding")
test_index <- createDataPartition(y = mushroom$class, times = 1,
                                  p = 0.2, list = FALSE)
test_set <- mushroom[test_index,]
train_set <- mushroom[-test_index,]

rm(test_index)

### end data recovery


###########
### lda
###########

# training
start_time <- Sys.time()
fit_lda <- train(class ~ ., method = "lda", data = train_set)
time_diff <- Sys.time() - start_time

s <- summary(fit_lda)

s

# record results
y_hat_lda <- predict(fit_lda, test_set)

cm <- confusionMatrix(y_hat_lda, test_set$class)

model_results <- rbind(model_results,
                       tibble(Method = "lda",
                              Accuracy = cm$overall["Accuracy"],
                              Kappa = cm$overall["Kappa"],
                              Sensitivity = cm$byClass["Sensitivity"],
                              Specificity = cm$byClass["Specificity"],
                              Train_Time = time_diff))
                       
model_results %>% knitr::kable()


###########
### naive_bayes
###########

# training
start_time <- Sys.time()
fit_nb <- train(class ~ ., method = "naive_bayes", data = train_set)
time_diff <- Sys.time() - start_time

summary(fit_nb)

# record results

y_hat_nb <- predict(fit_nb, test_set)

cm <- confusionMatrix(y_hat_nb, test_set$class)

model_results <- rbind(model_results,
                       tibble(Method = "navie bayes",
                        Accuracy = cm$overall["Accuracy"],
                        Kappa = cm$overall["Kappa"],
                        Sensitivity = cm$byClass["Sensitivity"],
                        Specificity = cm$byClass["Specificity"],
                        Train_Time = time_diff))

model_results %>% knitr::kable()

###########
### svmLinear
###########

# training
start_time <- Sys.time()
fit_svmLinear <- train(class ~ ., method = "svmLinear", data = train_set)
time_diff <- Sys.time() - start_time

fit_svmLinear["finalModel"]

# record results

y_hat_svmLinear <- predict(fit_svmLinear, test_set)

cm <- confusionMatrix(y_hat_svmLinear, test_set$class)

model_results <- rbind(model_results,
                       tibble(Method = "svmLinear",
                              Accuracy = cm$overall["Accuracy"],
                              Kappa = cm$overall["Kappa"],
                              Sensitivity = cm$byClass["Sensitivity"],
                              Specificity = cm$byClass["Specificity"],
                              Train_Time = time_diff))

model_results %>% knitr::kable()


###########
#### knn
###########

# training
start_time <- Sys.time()
fit_knn <- train(class ~ ., method = "knn", data = train_set)
time_diff <- Sys.time() - start_time

fit_knn["finalModel"]
fit_knn$bestTune

# record results

y_hat_knn <- predict(fit_knn, test_set)

cm <- confusionMatrix(y_hat_knn, test_set$class)

model_results <- rbind(model_results,
                       tibble(Method = "knn",
                              Accuracy = cm$overall["Accuracy"],
                              Kappa = cm$overall["Kappa"],
                              Sensitivity = cm$byClass["Sensitivity"],
                              Specificity = cm$byClass["Specificity"],
                              Train_Time = time_diff))

model_results %>% knitr::kable()


###########
#### gamLoess
###########

# training
start_time <- Sys.time()
fit_gamLoess <- train(class ~ ., method = "gamLoess", data = train_set)
time_diff <- Sys.time() - start_time

fit_gamLoess$bestTune

# record results
y_hat_gamLoess <- predict(fit_gamLoess, test_set)

cm <- confusionMatrix(y_hat_gamLoess, test_set$class)

model_results <- rbind(model_results,
                       tibble(Method = "gamLoess",
                              Accuracy = cm$overall["Accuracy"],
                              Kappa = cm$overall["Kappa"],
                              Sensitivity = cm$byClass["Sensitivity"],
                              Specificity = cm$byClass["Specificity"],
                              Train_Time = time_diff))

model_results %>% knitr::kable()

###########
#### multinom
###########

# training

start_time <- Sys.time()
fit_multinom <- train(class ~ ., method = "multinom", data = train_set)
time_diff <- Sys.time() - start_time

fit_multinom["finalModel"]

# record results

y_hat_multinom <- predict(fit_multinom, test_set)

cm <- confusionMatrix(y_hat_multinom, test_set$class)

model_results <- rbind(model_results,
                       tibble(Method = "multinom",
                              Accuracy = cm$overall["Accuracy"],
                              Kappa = cm$overall["Kappa"],
                              Sensitivity = cm$byClass["Sensitivity"],
                              Specificity = cm$byClass["Specificity"],
                              Train_Time = time_diff))

model_results %>% knitr::kable()



###########
#### Classification Model
###########

# training
start_time <- Sys.time()
fit_rpart <- train(class ~ ., method = "rpart", data = train_set)
time_diff <- Sys.time() - start_time

fit_rpart["finalModel"]

rpart.plot(fit_rpart$finalModel)

# record results

y_hat_rpart <- predict(fit_rpart, test_set)

cm <- confusionMatrix(y_hat_rpart, test_set$class)

model_results <- rbind(model_results,
                       tibble(Method = "rpart",
                              Accuracy = cm$overall["Accuracy"],
                              Kappa = cm$overall["Kappa"],
                              Sensitivity = cm$byClass["Sensitivity"],
                              Specificity = cm$byClass["Specificity"],
                              Train_Time = time_diff))

model_results %>% knitr::kable()


###########
#### random forest
###########

# training
start_time <- Sys.time()
fit_rf <- train(class ~ ., method = "Rborist", data = train_set)
time_diff <- Sys.time() - start_time

fit_rf$bestTune

varImp(fit_rf)

# record results

y_hat_rf <- predict(fit_rf, test_set)

cm <- confusionMatrix(y_hat_rf, test_set$class)

model_results <- rbind(model_results,
                       tibble(Method = "random forest",
                              Accuracy = cm$overall["Accuracy"],
                              Kappa = cm$overall["Kappa"],
                              Sensitivity = cm$byClass["Sensitivity"],
                              Specificity = cm$byClass["Specificity"],
                              Train_Time = time_diff))

model_results %>% knitr::kable()

###########
#### adaboost
###########

# training
start_time <- Sys.time()
fit_adaboost <- train(class ~ ., method = "adaboost", data = train_set)
time_diff <- Sys.time() - start_time

fit_adaboost["finalModel"]

varImp(fit_adaboost)

# record results

y_hat_adaboost <- predict(fit_adaboost, test_set)

cm <- confusionMatrix(y_hat_adaboost, test_set$class)

model_results <- rbind(model_results,
                       tibble(Method = "adaboost",
                              Accuracy = cm$overall["Accuracy"],
                              Kappa = cm$overall["Kappa"],
                              Sensitivity = cm$byClass["Sensitivity"],
                              Specificity = cm$byClass["Specificity"],
                              Train_Time = time_diff))

model_results %>% knitr::kable()


###########
#### ensemble
###########

# training
start_time <- Sys.time()
y_hat_results <- bind_cols(y_hat_glm, y_hat_lda, y_hat_nb, 
                           y_hat_svmLinear, y_hat_rpart,
                           y_hat_knn, y_hat_gamLoess, y_hat_multinom, 
                           y_hat_rf, y_hat_adaboost)
time_diff <- Sys.time() - start_time

# record results

y_hat_ensemble <- ifelse(rowMeans(y_hat_results == "e") >0.5, "e", "p")

cm <- confusionMatrix(as.factor(y_hat_ensemble), test_set$class)

model_results <- rbind(model_results,
                       tibble(Method = "ensemble",
                              Accuracy = cm$overall["Accuracy"],
                              Kappa = cm$overall["Kappa"],
                              Sensitivity = cm$byClass["Sensitivity"],
                              Specificity = cm$byClass["Specificity"],
                              Train_Time = time_diff))

model_results %>% knitr::kable()
