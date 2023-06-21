# https://www.r-tutor.com/
# https://developer.nvidia.com/blog/gpu-accelerated-r-cloud-teraproc-cluster-service/


# install.packages("C:/Users/Windows/Desktop/R/rpux_0.5.0/rpud_0.5.0.zip", 
#                  repos = NULL)

# install.packages("jsonlite", type = "source")

install.packages("C:/Users/donbo/Downloads/rpux_0.7.2_win/rpux_0.7.2_win/rpud_0.7.2.zip", 
                 repos = NULL)

install.packages("C:/Users/donbo/Downloads/rpux_0.7.2_win/rpux_0.7.2_win/rpudplus_0.7.2.zip", 
                 repos = NULL)


# install.packages("C:/Users/donbo/Downloads/rpux_0.7.2_win/rpux_0.7.2_win/rpud_0.7.2_src.zip", 
#                  type="source",
#                  repos = NULL)
# 
# install.packages("C:/Users/donbo/Downloads/rpux_0.7.2_win/rpux_0.7.2_win/rpud_0.7.2_src/rpud_0.7.2/", 
#                  type="source",
#                  repos = NULL)
# 
# install.packages("C:/Users/donbo/Downloads/rpux_0.7.2_win/rpux_0.7.2_win/rpud_0.7.2_src/", 
#                  type="source",
#                  repos = NULL)

# C:\Users\donbo\Downloads\rpux_0.7.2_win\rpux_0.7.2_win\rpud_0.7.2_src\rpud_0.7.2

install.packages("C:/Users/donbo/Downloads/rpux_0.7.2_win/rpux_0.7.2_win/rpudplus_0.7.2.zip", 
                 type="source",
                 repos = NULL)


library(rpud) 
library(rpudplus)

# https://www.cs.toronto.edu/~kriz/cifar-10-binary.tar.gz

ds <- rpudlCreateDataSource(
  data.format="cifar10", 
  data.dir="data/cifar")

rpudlFindTrainingDataMean(ds, filename="cifar10-data.mean")


test.data <- function(dim, num, seed=17) { 
     set.seed(seed) 
     matrix(rnorm(dim * num), nrow=num) 
 } 
m <- test.data(120, 4500) 
m <- test.data(1200, 10000) 
system.time(dist(m)) 
library(rpud)                  # load rpud with rpudplus 
system.time(rpuDist(m)) 


# djb start here
# install.packages("reticulate")
# install.packages("tensorflow")
# install.packages("keras")
# library(reticulate)
# path_to_python <- install_python()
# install_python(version = "3.9:latest", list = FALSE, force = FALSE)
# install_python(version = "3.9.16", list = FALSE, force = FALSE)
# # pyenv-install --list
# 
# virtualenv_create("r-reticulate", python = path_to_python)
# 
# library(tensorflow)
# # install_tensorflow(envname = "r-reticulate")
# # install.packages("keras")
# library(keras)
# # install_keras(envname = "r-reticulate")
# 
# library(tensorflow)
# tf$constant("Hello Tensorflow!")
# 
# 
# library(tensorflow)
# library(keras)
# tensorflow::tf$config$list_physical_devices("GPU")
# tensorflow::install_tensorflow()


# https://stackoverflow.com/questions/64774459/error-python-module-tensorflow-was-not-found-rstudio-windows10-path-problem
# I have met the same problem of intalling tensorflow in Rstudio, but I found a way to avoid it by installing tensorflow in a new anaconda environment, then activate the envirnoment inside Rstudio. It works in my computer (win 11, R4.2.1).
# install anaconda
# create a new tensorflow environment named tf-gpu, open anaconda prompt and run:
#   conda create -n tf-gpu tensorflow-gpu
#   conda activate tf-gpu
# You can check the environment in anaconda prompt by:
#   conda env list
# open Rstudio and run:
# library(reticulate)
# # install_tensorflow(envname = "tf-gpu")
# library(tensorflow)
# # use_condaenv("tf-gpu", required = TRUE)
# tf$constant("Hello Tensorflow!")
# 
# install_python("3.9.16")
# install_python(version = "3.9:latest", list = TRUE, force = FALSE)

# https://tensorflow.rstudio.com/install/local_gpu

install.packages("keras")
library(keras)
install_keras(envname = "tf-gpu")
use_condaenv("tf-gpu", required = TRUE)
library(tensorflow)
library(keras)

tf$constant("Hello Tensorflow!")

# tutorials ----
library(tensorflow)
library(keras)
use_condaenv("tf-gpu", required = TRUE)

# https://tensorflow.rstudio.com/tutorials/quickstart/beginner

c(c(x_train, y_train), c(x_test, y_test)) %<-% keras::dataset_mnist()
x_train <- x_train / 255
x_test <-  x_test / 255

model <- keras_model_sequential(input_shape = c(28, 28)) %>%
  layer_flatten() %>%
  layer_dense(128, activation = "relu") %>%
  layer_dropout(0.2) %>%
  layer_dense(10)

predictions <- predict(model, x_train[1:2, , ])
predictions

tf$nn$softmax(predictions)

loss_fn <- loss_sparse_categorical_crossentropy(from_logits = TRUE)
loss_fn(y_train[1:2], predictions)

model %>% compile(
  optimizer = "adam",
  loss = loss_fn,
  metrics = "accuracy"
)
model %>% fit(x_train, y_train, epochs = 5)
model %>% evaluate(x_test,  y_test, verbose = 2)

probability_model <- keras_model_sequential() %>%
  model() %>%
  layer_activation_softmax() %>%
  layer_lambda(tf$argmax)

probability_model(x_test[1:5, , ])

# more ----
# https://www.manning.com/books/deep-learning-with-r-second-edition?utm_source=kalinowski&utm_medium=affiliate&utm_campaign=book_allaire2_deep_5_24_22&a_aid=kalinowski&a_bid=c7cc060f#toc
# https://tensorflow.rstudio.com/
# https://tensorflow.rstudio.com/tutorials/
# https://www.tensorflow.org/tutorials/keras/
# https://www.tensorflow.org/guide/keras
# https://subscription.packtpub.com/book/big-data-and-business-intelligence/9781787121089/1/ch01lvl1sec14/installing-tensorflow-in-r

# advanced ----
library(tensorflow)
library(tfdatasets)
library(keras)
use_condaenv("tf-gpu", required = TRUE)

c(c(x_train, y_train), c(x_test, y_test)) %<-% keras::dataset_mnist()
x_train %<>% { . / 255 }
x_test  %<>% { . / 255 }

train_ds <- list(x_train, y_train) %>%
  tensor_slices_dataset() %>%
  dataset_shuffle(10000) %>%
  dataset_batch(32)

test_ds <- list(x_test, y_test) %>%
  tensor_slices_dataset() %>%
  dataset_batch(32)

my_model <- new_model_class(
  classname = "MyModel",
  initialize = function(...) {
    super$initialize()
    self$conv1 <- layer_conv_2d(filters = 32, kernel_size = 3,
                                activation = 'relu')
    self$flatten <- layer_flatten()
    self$d1 <- layer_dense(units = 128, activation = 'relu')
    self$d2 <- layer_dense(units = 10)
  },
  call = function(inputs) {
    inputs %>%
      tf$expand_dims(3L) %>%
      self$conv1() %>%
      self$flatten() %>%
      self$d1() %>%
      self$d2()
  }
)

# Create an instance of the model
model <- my_model()

loss_object <- loss_sparse_categorical_crossentropy(from_logits = TRUE)
optimizer <- optimizer_adam()

train_loss <- metric_mean(name = "train_loss")
train_accuracy <- metric_sparse_categorical_accuracy(name = "train_accuracy")

test_loss <- metric_mean(name = "test_loss")
test_accuracy <- metric_sparse_categorical_accuracy(name = "test_accuracy")

# setup train
train_step <- function(images, labels) {
  with(tf$GradientTape() %as% tape, {
    # training = TRUE is only needed if there are layers with different
    # behavior during training versus inference (e.g. Dropout).
    predictions <- model(images, training = TRUE)
    loss <- loss_object(labels, predictions)
  })
  gradients <- tape$gradient(loss, model$trainable_variables)
  optimizer$apply_gradients(zip_lists(gradients, model$trainable_variables))
  train_loss(loss)
  train_accuracy(labels, predictions)
}

train <- tf_function(function(train_ds) {
  for (batch in train_ds) {
    c(images, labels) %<-% batch
    train_step(images, labels)
  }
})


# setup test
test_step <- function(images, labels) {
  # training = FALSE is only needed if there are layers with different
  # behavior during training versus inference (e.g. Dropout).
  predictions <- model(images, training = FALSE)
  t_loss <- loss_object(labels, predictions)
  test_loss(t_loss)
  test_accuracy(labels, predictions)
}

test <- tf_function(function(test_ds) {
  for (batch in test_ds) {
    c(images, labels) %<-% batch
    test_step(images, labels)
  }
})

reset_metrics <- function() {
  for (metric in list(train_loss, train_accuracy,
                      test_loss, test_accuracy))
    metric$reset_states()
}

a <- proc.time()
EPOCHS <- 10
for (epoch in seq_len(EPOCHS)) {
  # Reset the metrics at the start of the next epoch
  reset_metrics()
  train(train_ds)
  test(test_ds)
  cat(sprintf('Epoch %d', epoch), "\n")
  cat(sprintf('Loss: %f', train_loss$result()), "\n")
  cat(sprintf('Accuracy: %f', train_accuracy$result() * 100), "\n")
  cat(sprintf('Test Loss: %f', test_loss$result()), "\n")
  cat(sprintf('Test Accuracy: %f', test_accuracy$result() * 100), "\n")
}
b <- proc.time()
b - a



