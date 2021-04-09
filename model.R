library(tidyverse)
library(caret)



summary(train_set)
str(train_set)


### glm

train_set <- train_set %>% select(-veil_type, -stalk_root)
test_set <- test_set %>% select(-veil_type, -stalk_root)

fit_glm <- train(class ~ ., method = "glm", data = train_set)

fit_glm["finalModel"]

warnings()

s <- summary(fit_glm)

s$aliased[which(s$aliased == TRUE)]

s
  
train_set %>% filter(stalk_color_above_ring == "c") %>% nrow()
train_set %>% filter(stalk_color_below_ring == "c") %>% nrow()
train_set %>% filter(stalk_color_below_ring == "o") %>% nrow()
train_set %>% filter(veil_color == "w") %>% nrow()
train_set %>% filter(veil_color == "y") %>% nrow()
train_set %>% filter(ring_number == "t") %>% nrow()
train_set %>% filter(ring_type == "n") %>% nrow()
train_set %>% filter(spore_print_color == "h") %>% nrow()


train_set <- train_set %>% select(-stalk_color_above_ring, -stalk_color_below_ring, -veil_color, -ring_number, -ring_type, -spore_print_color, -habitat)
test_set <- test_set %>% select(-stalk_color_above_ring, -stalk_color_below_ring, -veil_color, -ring_number, -ring_type, -spore_print_color, -habitat)

fit_glm <- train(class ~ ., method = "glm", data = train_set)# maxit = 200)
fit_glm["finalModel"]

warnings()

s <- summary(fit_glm)

s$aliased[which(s$aliased == TRUE)]


y_hat_glm <- predict(fit_glm, test_set)

result <- mean(y_hat_glm == test_set$class)

result

s <- confusionMatrix(y_hat_glm, test_set$class)

s

model_results <- tibble(Method = "glm",
                        Accuracy = s$overall["Accuracy"],
                        Kappa = s$overall["Kappa"],
                        Sensitivity = s$byClass["Sensitivity"],
                        Specificity = s$byClass["Specificity"])

model_results %>% knitr::kable()
                            

#### Classification Model, rpart


### data recovery
set.seed(12345, sample.kind="Rounding")
test_index <- createDataPartition(y = mushroom$class, times = 1,
                                  p = 0.2, list = FALSE)
test_set <- mushroom[test_index,]
train_set <- mushroom[-test_index,]

rm(test_index)
### data recovery


train_set <- train_set %>% select(-veil_type, -stalk_root)
test_set <- test_set %>% select(-veil_type, -stalk_root)

fit_nb<- train(class ~ ., method = "naive_bayes", data = train_set)
fit_nb["finalModel"]

summary(fit_nb)

y_hat_nb <- predict(fit_nb, test_set)

result <- mean(y_hat_nb == test_set$class)

result

s <- confusionMatrix(y_hat_nb, test_set$class)

model_results <- rbind(model_results,
                       tibble(Method = "navie bayes",
                        Accuracy = s$overall["Accuracy"],
                        Kappa = s$overall["Kappa"],
                        Sensitivity = s$byClass["Sensitivity"],
                        Specificity = s$byClass["Specificity"]))

model_results %>% knitr::kable()


### svmLinear

### data recovery
set.seed(12345, sample.kind="Rounding")
test_index <- createDataPartition(y = mushroom$class, times = 1,
                                  p = 0.2, list = FALSE)
test_set <- mushroom[test_index,]
train_set <- mushroom[-test_index,]

rm(test_index)
### data recovery


train_set <- train_set %>% select(-veil_type, -stalk_root)
test_set <- test_set %>% select(-veil_type, -stalk_root)

fit_svmLinear<- train(class ~ ., method = "svmLinear", data = train_set)
fit_svmLinear["finalModel"]

summary(fit_svmLinear)

y_hat_svmLinear <- predict(fit_svmLinear, test_set)

result <- mean(y_hat_svmLinear == test_set$class)

result

s <- confusionMatrix(y_hat_svmLinear, test_set$class)

model_results <- rbind(model_results,
                       tibble(Method = "svmLinear",
                              Accuracy = s$overall["Accuracy"],
                              Kappa = s$overall["Kappa"],
                              Sensitivity = s$byClass["Sensitivity"],
                              Specificity = s$byClass["Specificity"]))

model_results %>% knitr::kable()
