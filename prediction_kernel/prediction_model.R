#training model
library(keras)

options(digits=10)
#setwd("C:\\Users\\CCYoTzom\\Desktop\\house_data")
#data <- read.csv("house_transaction_record.csv", header = T, sep = ",", stringsAsFactors = F)

data <- subset(data,select = -c(transaction_year, transaction_month, transaction_day, 
                                finished_month, finished_day, ParkPrice,
                                CityLandClass, MainUse, Wall, ParkMount, Dist))

#split train&test data
#set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = floor(0.8 * nrow(data)))

train <- data[train_ind, ]
test <- data[-train_ind, ]

x_train <- as.matrix(subset(train,select = -c(Price)))#feature
y_train <- as.matrix(train$Price)#target

x_test <- as.matrix(subset(test,select = -c(Price)))#feature
y_test <- as.matrix(test$Price)#target

model <- keras_model_sequential() 

model %>% 
  layer_dense(units = 676, input_shape = c(length(data)-1), kernel_initializer = 'uniform') %>% 
  #layer_dropout(rate = 0.2) %>% #隨機忽略神經元
  #layer_dense(units = 128, activation = "relu") %>%
  layer_activation_leaky_relu(input_shape = c(length(data)-1),trainable = T) %>%
  layer_dropout(rate = 0.15) %>%
  layer_dense(units = 338) %>%
  layer_activation_leaky_relu(trainable = T) %>%
  layer_dropout(rate = 0.15) %>%
  layer_dense(units = 169) %>%
  layer_activation_leaky_relu(trainable = T) %>%
  layer_dropout(rate = 0.15) %>%
  layer_dense(units = 84) %>%
  layer_activation_leaky_relu(trainable = T) %>%
  layer_dropout(rate = 0.15) %>%
  layer_dense(units = 42) %>%
  layer_activation_leaky_relu(trainable = T) %>%
  layer_dropout(rate = 0.15) %>%
  layer_dense(units = 21) %>%
  layer_activation_leaky_relu(trainable = T) %>%
  layer_dropout(rate = 0.15) %>%
  layer_dense(units = 10) %>%
  layer_activation_leaky_relu(trainable = T) %>%
  layer_dropout(rate = 0.15) %>%
  layer_dense(units = 5) %>%
  layer_activation_leaky_relu(trainable = T) %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 1) %>%
summary(model)

model %>% compile(
  loss = 'mean_absolute_error',
  #optimizer = optimizer_rmsprop(),
  #optimizer = optimizer_sgd(),
  optimizer = 'adam',
  metrics = c('mse')
)

history <- model %>% fit(x_train, 
                         y_train,
                         epochs = 300, #遞迴次數
                         batch_size = 128, #每次使用樣本數
                         validation_split = 0.3 #驗證數據集
)

pred <- model %>% predict(x = x_test)

reduction_pred <- (pred * (max_list[12] - min_list[12])) + min_list[12]
reduction_y_test <- (y_test * (max_list[12] - min_list[12])) + min_list[12]
rate <- ((reduction_pred - reduction_y_test) / reduction_pred) * 100

result <- data.frame(reduction_y_test, reduction_pred, reduction_y_test - reduction_pred, rate)
names(result) <- c("正確答案", "預測結果", "正確-預測", "差距(%)")

cat(" 差距小於0 :", sum(length(rate[rate < 0])), "個\n",
    "差距等於0 :", sum(length(rate[rate == 0])),"個\n",
    "差距大於0 :", sum(length(rate[rate > 0])), "個\n\n",
    #"差距大於10且小於20 :", sum(length(rate[rate > 10 && rate <= 20])), "個\n",
    #"差距大於20 :", sum(length(rate[rate > 20])), "個\n\n",
    "總預測個數", length(pred),  "個\n",
    "總預測金額", prettyNum(sum(result[,1]), big.mark = ","),  "元\n\n",
    "總預測誤差", prettyNum(sum(result[,1]) - sum(result[,2]), big.mark = ","), "元\n",
    "平均預測誤差", prettyNum((sum(result[,1]) - sum(result[,2])) / length(pred), big.mark = ","), "元\n\n",
    "總預測誤差(%)", sum(result[,4]), "%\n",
    "平均預測誤差(%)", sum(result[,4]) / length(pred), "%\n")

head(result)

#model %>% save_model_hdf5("pred_model.h5")
#model %>% save_model_weights_hdf5("pred_model_weights.h5")
#model1 <- load_model_hdf5("pred_model.h5")
