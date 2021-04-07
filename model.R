library(tidyverse)
library(caret)



summary(train_set)
str(train_set)

train_set <- train_set %>% select(-veil_type, -stalk_root)
test_set <- test_set %>% select(-veil_type, -stalk_root)

fitglm <- train(class ~ ., method = "glm", data = train_set)

fitglm["finalModel"]

warnings()

s <- summary(fitglm)

s$aliased[which(s$aliased == TRUE)]

s
  
train_set %>% filter(stalk_color_above_ring == "c") %>% nrow()
train_set %>% filter(stalk_color_below_ring == "c") %>% nrow()
train_set %>% filter(stalk_color_below_ring == "o") %>% nrow()
train_set %>% filter(veil_color == "w") %>% nrow()
train_set %>% filter(veil_color == "y") %>% nrow()
train_set %>% filter(ring_number == "t") %>% nrow()
train_set %>% filter(ring_type == "n") %>% nrow()
train_set %>% filter(spore_print_color == "h") %>% nrow()


train_set <- train_set %>% select(-stalk_color_above_ring, -stalk_color_below_ring, -veil_color, -ring_number, -ring_type, -spore_print_color, -habitat)
test_set <- test_set %>% select(-stalk_color_above_ring, -stalk_color_below_ring, -veil_color, -ring_number, -ring_type, -spore_print_color, -habitat)

fitglm <- train(class ~ ., method = "glm", data = train_set)# maxit = 200)
fitglm["finalModel"]

warnings()

s <- summary(fitglm)

s$aliased[which(s$aliased == TRUE)]


y_hat <- predict(fitglm, test_set)

result <- mean(y_hat == test_set$class)

fitglm$coef

help("train")