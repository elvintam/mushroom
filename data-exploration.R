library(tidyverse)
library(caret)

summary(train_set)
str(train_set)


featurePlot(x = train_set[, 1:23],
            y = train_set$class,
            plot = "box",
            #strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))

help("featurePlot")
