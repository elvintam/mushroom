library(tidyverse)
library(caret)

####need to add pca ####

summary(train_set)
str(train_set)


datarecovery <- function(){
  set.seed(12345, sample.kind="Rounding")
  test_index <- createDataPartition(y = mushroom$class, times = 1,
                                    p = 0.2, list = FALSE)
  test_set <- mushroom[test_index,]
  train_set <- mushroom[-test_index,]
  
  rm(test_index)
}

###########
### glm
###########

datarecovery()

fit_glm <- train(class ~ ., method = "glm", data = train_set)

warnings()

s <- summary(fit_glm)

s

s$aliased[which(s$aliased == TRUE)]

train_set %>% filter(stalk_color_above_ring == "c") %>% nrow()
train_set %>% filter(stalk_color_below_ring == "c") %>% nrow()
train_set %>% filter(stalk_color_below_ring == "o") %>% nrow()
train_set %>% filter(veil_color == "w") %>% nrow()
train_set %>% filter(veil_color == "y") %>% nrow()
train_set %>% filter(ring_number == "t") %>% nrow()
train_set %>% filter(ring_type == "n") %>% nrow()
train_set %>% filter(spore_print_color == "h") %>% nrow()


train_set <- train_set %>% select(-stalk_color_above_ring, -stalk_color_below_ring, 
                                  -veil_color, -ring_number, -ring_type, -spore_print_color)
test_set <- test_set %>% select(-stalk_color_above_ring, -stalk_color_below_ring, 
                                -veil_color, -ring_number, -ring_type, -spore_print_color)

fit_glm <- train(class ~ ., method = "glm", data = train_set)# maxit = 200)

warnings()

s <- summary(fit_glm)

s

s$aliased[which(s$aliased == TRUE)]

y_hat_glm <- predict(fit_glm, test_set)

cm <- confusionMatrix(y_hat_glm, test_set$class)

model_results <- tibble(Method = "glm",
                        Accuracy = cm$overall["Accuracy"],
                        Kappa = cm$overall["Kappa"],
                        Sensitivity = cm$byClass["Sensitivity"],
                        Specificity = cm$byClass["Specificity"])

model_results %>% knitr::kable()
                            

###########
### lda
###########


datarecovery()

fit_lda <- train(class ~ ., method = "lda", data = train_set)

s <- summary(fit_lda)

s

y_hat_lda <- predict(fit_lda, test_set)

cm <- confusionMatrix(y_hat_lda, test_set$class)

model_results <- rbind(model_results,
                       tibble(Method = "lda",
                              Accuracy = cm$overall["Accuracy"],
                              Kappa = cm$overall["Kappa"],
                              Sensitivity = cm$byClass["Sensitivity"],
                              Specificity = cm$byClass["Specificity"]))

model_results %>% knitr::kable()


###########
### naive_bayes
###########


datarecovery()

fit_nb <- train(class ~ ., method = "naive_bayes", data = train_set)

s <- summary(fit_nb)

s

y_hat_nb <- predict(fit_nb, test_set)

cm <- confusionMatrix(y_hat_nb, test_set$class)

model_results <- rbind(model_results,
                       tibble(Method = "navie bayes",
                        Accuracy = cm$overall["Accuracy"],
                        Kappa = cm$overall["Kappa"],
                        Sensitivity = cm$byClass["Sensitivity"],
                        Specificity = cm$byClass["Specificity"]))

model_results %>% knitr::kable()

###########
### svmLinear
###########

datarecovery()

fit_svmLinear <- train(class ~ ., method = "svmLinear", data = train_set)
fit_svmLinear["finalModel"]

s <- summary(fit_svmLinear)

s

y_hat_svmLinear <- predict(fit_svmLinear, test_set)

cm <- confusionMatrix(y_hat_svmLinear, test_set$class)

model_results <- rbind(model_results,
                       tibble(Method = "svmLinear",
                              Accuracy = cm$overall["Accuracy"],
                              Kappa = cm$overall["Kappa"],
                              Sensitivity = cm$byClass["Sensitivity"],
                              Specificity = cm$byClass["Specificity"]))

model_results %>% knitr::kable()


###########
#### Classification Model
###########

datarecovery()

fit_rpart <- train(class ~ ., method = "rpart", data = train_set)
fit_rpart["finalModel"]

s <- summary(fit_rpart)

y_hat_rpart <- predict(fit_rpart, test_set)

cm <- confusionMatrix(y_hat_rpart, test_set$class)

model_results <- rbind(model_results,
                       tibble(Method = "rpart",
                              Accuracy = cm$overall["Accuracy"],
                              Kappa = cm$overall["Kappa"],
                              Sensitivity = cm$byClass["Sensitivity"],
                              Specificity = cm$byClass["Specificity"]))

model_results %>% knitr::kable()


###########
#### knn
###########

datarecovery()

fit_knn <- train(class ~ ., method = "knn", data = train_set)
fit_knn["finalModel"]

s <- summary(fit_knn)

y_hat_knn <- predict(fit_knn, test_set)

cm <- confusionMatrix(y_hat_knn, test_set$class)

model_results <- rbind(model_results,
                       tibble(Method = "knn",
                              Accuracy = cm$overall["Accuracy"],
                              Kappa = cm$overall["Kappa"],
                              Sensitivity = cm$byClass["Sensitivity"],
                              Specificity = cm$byClass["Specificity"]))

model_results %>% knitr::kable()


###########
#### gamLoess
###########

datarecovery()

fit_gamLoess <- train(class ~ ., method = "gamLoess", data = train_set)
fit_gamLoess["finalModel"]

s <- summary(fit_gamLoess)

y_hat_gamLoess <- predict(fit_gamLoess, test_set)

cm <- confusionMatrix(y_hat_gamLoess, test_set$class)

model_results <- rbind(model_results,
                       tibble(Method = "gamLoess",
                              Accuracy = cm$overall["Accuracy"],
                              Kappa = cm$overall["Kappa"],
                              Sensitivity = cm$byClass["Sensitivity"],
                              Specificity = cm$byClass["Specificity"]))

model_results %>% knitr::kable()


###########
#### multinom
###########

datarecovery()

fit_multinom <- train(class ~ ., method = "multinom", data = train_set)
fit_multinom["finalModel"]

s <- summary(fit_multinom)

y_hat_multinom <- predict(fit_multinom, test_set)

cm <- confusionMatrix(y_hat_multinom, test_set$class)

model_results <- rbind(model_results,
                       tibble(Method = "multinom",
                              Accuracy = cm$overall["Accuracy"],
                              Kappa = cm$overall["Kappa"],
                              Sensitivity = cm$byClass["Sensitivity"],
                              Specificity = cm$byClass["Specificity"]))

model_results %>% knitr::kable()


###########
#### rf
###########

datarecovery()

fit_rf <- train(class ~ ., method = "rf", data = train_set)
fit_rf["finalModel"]

s <- summary(fit_rf)

y_hat_rf <- predict(fit_rf, test_set)

cm <- confusionMatrix(y_hat_rf, test_set$class)

model_results <- rbind(model_results,
                       tibble(Method = "rf",
                              Accuracy = cm$overall["Accuracy"],
                              Kappa = cm$overall["Kappa"],
                              Sensitivity = cm$byClass["Sensitivity"],
                              Specificity = cm$byClass["Specificity"]))

model_results %>% knitr::kable()

###########
#### adaboost
###########

datarecovery()

fit_adaboost <- train(class ~ ., method = "adaboost", data = train_set)
fit_adaboost["finalModel"]

s <- summary(fit_adaboost)

y_hat_adaboost <- predict(fit_adaboost, test_set)

cm <- confusionMatrix(y_hat_adaboost, test_set$class)

model_results <- rbind(model_results,
                       tibble(Method = "adaboost",
                              Accuracy = cm$overall["Accuracy"],
                              Kappa = cm$overall["Kappa"],
                              Sensitivity = cm$byClass["Sensitivity"],
                              Specificity = cm$byClass["Specificity"]))

model_results %>% knitr::kable()


###########
#### ensemble
###########

y_hat_results <- bind_cols(y_hat_glm, y_hat_lda, y_hat_nb, 
                           y_hat_svmLinear, y_hat_rpart,
                           y_hat_knn, y_hat_gamLoess, y_hat_multinom, 
                           y_hat_rf, y_hat_adaboost)

y_hat_ensemble <- ifelse(rowMeans(y_hat_results == "e") >0.5, "e", "p")

cm <- confusionMatrix(as.factor(y_hat_ensemble), test_set$class)

model_results <- rbind(model_results,
                       tibble(Method = "ensemble",
                              Accuracy = cm$overall["Accuracy"],
                              Kappa = cm$overall["Kappa"],
                              Sensitivity = cm$byClass["Sensitivity"],
                              Specificity = cm$byClass["Specificity"]))

model_results %>% knitr::kable()