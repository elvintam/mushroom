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
             stat_count(geom = "text", colour = "white", size = 4,
                        aes(label = ..count..), position= position_stack(vjust=0.5))


featurePlot(x = train_set[, 1:23],
            y = train_set$class,
            plot = "box",
            #strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))

help("featurePlot")


