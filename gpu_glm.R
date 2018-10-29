

library(h2o4gpu)

# Prepare data
x <- iris[1:4]
y <- as.integer(iris$Species) # all columns, including the response, must be numeric

# Initialize and train the classifier
model <- h2o4gpu.random_forest_classifier() %>% fit(x, y)

# Make predictions
pred <- model %>% predict(x)

