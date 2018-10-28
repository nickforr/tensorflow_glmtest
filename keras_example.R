
library(keras)
library(dplyr)
library(tidyr)



use_session_with_seed(1,disable_parallel_cpu = FALSE)

data <- readRDS("vita_mock.RDS")

x <- data[, 1:5]
y <- data[, "DeadInYOE"]

# one hot encode classes
y = to_categorical(as.integer(y) - 1 , num_classes = 2)

# create sequential model
model = keras_model_sequential()

# add layers, first layer needs input dimension
model %>%
  layer_dense(units = ncol(y), input_shape = ncol(x), activation = "softmax")

# add a loss function and optimizer
model %>%
  compile(
    loss = "categorical_crossentropy",
    optimizer = "adagrad",
    metrics = "accuracy"
  )

# fit model with our training data set, training will be done for 200 times data set
fit = model %>%
  fit(
    x = x,
    y = y,
    shuffle = T,
    batch_size = 5,
    validation_split = 0.3,
    epochs = 200
  )
plot(fit)


# https://medium.com/@the1ju/simple-logistic-regression-using-keras-249e0cc9a970


data = iris[sample(nrow(iris)),]

y = data[, "Species"]
x = data[,1:4]

# scale to [0,1]
x = as.matrix(apply(x, 2, function(x) (x-min(x))/(max(x) - min(x))))

# one hot encode classes / create DummyFeatures
levels(y) = 1:length(y)
y = to_categorical(as.integer(y) - 1 , num_classes = 3)

# create sequential model
model = keras_model_sequential()

# add layers, first layer needs input dimension
model %>%
  layer_dense(units = 3, activation = "softmax")

# add a loss function and optimizer
model %>%
  compile(
    loss = "categorical_crossentropy",
    optimizer = "adagrad",
    metrics = "accuracy"
  )

# fit model with our training data set, training will be done for 200 times data set
fit = model %>%
  fit(
    x = x,
    y = y,
    shuffle = T,
    batch_size = 5,
    validation_split = 0.3,
    epochs = 200
  )
plot(fit)
