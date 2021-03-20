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

class(mushroom)
summary(mushroom)

set.seed(12345, sample.kind="Rounding")
test_index <- createDataPartition(y = mushroom$class, times = 1,
                                  p = 0.2, list = FALSE)
test_set <- mushroom[test_index,]
train_set <- mushroom[-test_index,]

rm(test_index)
