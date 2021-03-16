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
                   "class","cap-shape",
                   "cap-surface","cap-color",
                   "bruises","odor","gill-attachment",
                   "gill-spacing","gill-size","gill-color",
                   "stalk-shape","stalk-root","stalk-surface-above-ring",
                   "stalk-surface-below-ring","stalk-color-above-ring",
                   "stalk-color-below-ring","veil-type","veil-color",
                   "ring-number","ring-type","spore-print-color",
                   "population","habitat"))

mushroom <- as.data.frame(mushroom)
rm(dl)

class(mushroom)
summary(mushroom)

set.seed(12345, sample.kind="Rounding")
test_index <- createDataPartition(y = mushroom$class, times = 1,
                                  p = 0.2, list = FALSE)
test_set <- mushroom[test_index,]
train_set <- mushroom[-test_index,]

rm(test_index)
