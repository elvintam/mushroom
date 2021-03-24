library(tidyverse)
library(caret)
library(data.table)



####reference
# https://www.kaggle.com/sanchitakarmakar/mushroom-classification-99-75-ac-rf-pca
# https://www.kaggle.com/andreshg/mushroom-s-h2o-automl-and-clasic-models-auc-1-0
# https://www.kaggle.com/tosinabase/mushroom-classification-tree-methods-comparison
#####

##########################################################
# Download Data set, create train set & test test
##########################################################

# UCI Machine Learning Repository Mushroom dataset:
# https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data


dl <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data", dl)

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

set.seed(12345, sample.kind="Rounding")
test_index <- createDataPartition(y = mushroom$class, times = 1,
                                  p = 0.2, list = FALSE)
test_set <- mushroom[test_index,]
train_set <- mushroom[-test_index,]

rm(test_index)

summary(train_set$class)

str(train_set)

models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn")


#models <- "glm"

train_set <- train_set %>% select(-veil_type)

test_set <- test_set %>% select(-veil_type)

str(train_set)

fits <- lapply(models, function(model){
  print(model)
  train(class ~ ., method = model, data = train_set)
})

names(fits) <- models

result <- sapply(fits, function(model){
  y_hat <- predict(model, test_set)
})

colindex <- seq(1, 5, 1)

result_accuracy <- sapply(colindex, function(x){
  confusionMatrix(as.factor(result[,x]), test_set$class)$overall["Accuracy"]  
})


result_accuracy

fitglm <- train(class ~ ., method = "glm", data = train_set)

fitglm["finalModel"]

y_hat <- predict(fitglm, test_set)

mean(y_hat == test_set$class)

fitglm

library(corrplot)
library(Hmisc)


train_set_cor <- cor(train_set)

corrplot(train_set)

head(mtcars)
M <- cor(mtcars)
head(round(M, 2))

M <- cor(as.matrix(train_set))


help(cor)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", 
         # Add coefficient of correlation
         tl.col="black", tl.srt=45, 
         #Text label color and rotation
         # Combine with significance
         sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE )

help("corrplot")

##### Question 6.1 for reference

library(tidyverse)

models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

library(caret)
library(dslabs)

# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
data("mnist_27")

fits <- lapply(models, function(model){
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
})

names(fits) <- models

result <- sapply(fits, function(model){
  y_hat <- predict(model, mnist_27$test)
})

###Q2
nrow(result)
ncol(result)

length(mnist_27$test$y)
length(models)

colindex <- seq(1, 10, 1)

length(as.factor(result[,1]))
length(mnist_27$test$y)

###Q3
result_accuracy <- sapply(colindex, function(x){
  confusionMatrix(as.factor(result[,x]), mnist_27$test$y)$overall["Accuracy"]  
})

mean(result_accuracy)


###Q4

result_ensemble <- ifelse(rowMeans(result[] == "7") >= 0.5, "7", "2")

result_ensemble

mean(result_ensemble == mnist_27$test$y)


###Q5

names(result_accuracy) <- models

result_enemble_accuracy <- mean(result_ensemble == mnist_27$test$y)

sum(result_accuracy >= result_enemble_accuracy)

which(result_accuracy >= result_enemble_accuracy)

###Q6

min_accuracy <- sapply(seq(1,10,1), function(x){
  min(fits[[x]]$results$Accuracy)
})

mean(min_accuracy)

###Q7

#names(min_accuracy) <- models

min_accuracy


model_select <- which(min_accuracy >= 0.8)

length(model_select)

result_ensemble_q7 <- sapply(seq(1,200,1), function(x){
  ifelse(sum(result[x,model_select] == "7")/length(model_select) > 0.5, "7", "2")
})

result_ensemble_q7 <- ifelse(rowMeans(result[,model_select] == "7") >0.5, "7", "2")

result_ensemble_q7

mean(result_ensemble_q7 == mnist_27$test$y)

