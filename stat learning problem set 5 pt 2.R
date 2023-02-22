library(keras)

###question one

cifar10 <- dataset_cifar10()
c(c(train_images, train_labels), c(test_images, test_labels)) %<-% cifar10
#loading data and keras
train_images <- array_reshape(train_images, c(50000, 32, 32, 3))
#if you open cifar and look at train x its integer 50000,32,32,3 so lifted those numbers. why?
#looked its 50000 images of 32x32 pixels with 3 for the color filter RBG NOT greyscale which is just 1
train_images <- train_images / 255
#why divide by 255 to get values between 0 and 1 !
test_images <- array_reshape(test_images, c(10000, 32, 32, 3))
test_images <- test_images / 255
train_labels <- to_categorical(train_labels)
test_labels <- to_categorical(test_labels)
#turning into normal data

##printing the first picture
library(plotly)
fig <- plot_ly(type="image", z=train_images[1, , , ]*255)
fig
#awful blurry picture of a frog! blurry as only 32

#question 2
model <- keras_model_sequential() 
  model %>% layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                input_shape = c(32, 32, 3)) %>%
    #input shape must fit the size of the pictures!!1!
    #convulational layer with 32 fitlers of size 3,3, maybe could increase the size as images larger?
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    #pool down then increase on wards
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu")
  summary(model)
  #numbers lifted from steven as he didnt know when its approproaite to change them so I dont
  
  model <- model %>% 
    layer_flatten() %>% 
    layer_dense(units = 64, activation = "relu") %>% 
    layer_dense(units = 10, activation = "softmax")
  #numbers lifted from steven as he didnt know when its approproaite to change them so I dont
  #Apart from the soft max layer which returns a probability for each class, therefore units must equal
  #number of classes, 10 in this case
summary(model)
#to look at whats going on in the model

model %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)
#from this point down it keeps adding to the model, so if you want to compare one part in isolation, IE
#the callback you will need to run and  recompile the model as above
model %>% fit(
  x=train_images, y=train_labels,
  epochs = 5, batch_size = 64, validation_data =list(test_images,test_labels)
)
#hey actually runs!! set epochs to 5and batch size=64 as steven did
#Was getting very bad accuracy so then increased this (<4% increase in accuracy each epoch...)
#started at 55% got 73%
#set to 20 and started way higher (75%...) ended with 95%
#All the above TRAINING accuracy
#adding validation_data to add test set in (must be a list therefore list(x,y)) reports val accuracys 0.6942


##question 3 
model %>% fit(
  x=train_images, y=train_labels,
  epochs = 5, batch_size = 64, validation_data =list(test_images,test_labels), callbacks = callback_early_stopping(patience = 2)
)
#fits the model but has a callback called earlystopping (which stops when valset is not decreasing)
#(type ?callback_early_stopping to see further)
#i added patcience=2 which just means it will run another 2 epochs to check the validation isnt decreasing,
#otherwise I found it stopped very quickly