library(tidyverse)
library(caret)

class(mushroom)
summary(mushroom)

head(train_set)

predicted_class <- 
  ifelse(train_set$habitat == 'l' & train_set$`cap-color` == 'w', 'p', 'e')

predicted_class

sum(predicted_class == 'p')

mean(predicted_class == train_set$class)

train_set %>% filter(habitat == 'l' & `cap-color` == 'w') %>% nrow()
