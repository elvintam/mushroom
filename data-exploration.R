library(tidyverse)
library(caret)

summary(mushroom) 
str(mushroom) 

sapply(mushroom, function(x) {length(unique(x))} )

mushroom <- mushroom %>% select(-veil_type, -stalk_root)

set.seed(12345, sample.kind="Rounding")
test_index <- createDataPartition(y = mushroom$class, times = 1,
                                  p = 0.2, list = FALSE)
test_set <- mushroom[test_index,]
train_set <- mushroom[-test_index,]

rm(test_index)


mushroom %>% ggplot(aes(class)) + geom_bar(aes(fill = class), stat = "count") +
             stat_count(geom = "text", colour = "black", size = 4,
                        aes(label = ..count..), position= position_stack(vjust=0.5))

mushroom %>% ggplot(aes(cap_shape)) + geom_bar(aes(fill = cap_shape), stat = "count") +
  stat_count(geom = "text", colour = "black", size = 4,
             aes(label = ..count..), position= position_stack(vjust=0.5))

sapply(mushroom, function(df, response_variable, indep_variable){

  response_variable <- enquo(response_variable)
  indep_variable <- enquo(indep_variable)
  
  resp_plot <- 
    df %>%
    ggplot(aes()) +
    geom_bar(aes(fill = ), stat = "count") +
    stat_count(geom = "text", colour = "black", 
               size = 4, aes(label = ..count..), 
               position= position_stack(vjust=0.5))
  
  return(resp_plot)
    
})

