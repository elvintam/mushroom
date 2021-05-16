library(tidyverse)
library(caret)


mushroom %>% ggplot(aes(class, fill = class)) + geom_bar(stat = "count") +
             stat_count(geom = "text", colour = "black", size = 4,
                        aes(label = ..count..), position= position_stack(vjust=0.5))

mushroom %>% ggplot(aes(cap_shape , fill = class)) + 
  geom_bar(position = "dodge") #
  #facet_grid(scales = "free_x")
  # stat_count(geom = "text", colour = "black", size = 4,
  #            aes(label = ..count..), position= position_stack(vjust=0.5))



# geom_bar(aes(fill = class), position = "dodge") +   
   # stat_count(geom = "text", colour = "black", size = 4,
   #          aes(label = ..count..), position= position_stack(vjust=0.5))

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

