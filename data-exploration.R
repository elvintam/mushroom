library(tidyverse)
library(caret)


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

