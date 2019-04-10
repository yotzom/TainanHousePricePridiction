#library(lintr)
library(plumber)
# library(jsonlite)
# library(tensorflow)
# library(keras)

setwd("/home/ai05/workspace/prediction_model/newmodel")
r <- plumb("house_basic.R")

r$registerHook("exit", function(){
  print("Exit Handle!")
})

#r$run(port = 8000)
r$run(host = "120.114.51.149", port = 8700)
