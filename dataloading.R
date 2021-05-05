if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(DataExplorer)) install.packages("DataExplorer", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(DataExplorer)

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

### check if is null

is.null(mushroom)
str(mushroom)
summary(mushroom)

plot_intro(mushroom)

plot_missing(mushroom)

plot_str(mushroom)

plot_bar(mushroom, nrow = 2L, ncol = 4L)

mushroom <- mushroom %>% select(-veil_type, -stalk_root)

plot_bar(mushroom)

plot_bar(mushroom, by = "class")

plot_correlation(mushroom, maxcat = 5L)

plot_prcomp(mushroom)

# $page_0 to control page

#
#create_report(mushroom)

mushroom <- mushroom %>% select(-veil_type, -stalk_root)


set.seed(12345, sample.kind="Rounding")
test_index <- createDataPartition(y = mushroom$class, times = 1,
                                  p = 0.2, list = FALSE)
test_set <- mushroom[test_index,]
train_set <- mushroom[-test_index,]

rm(test_index)

