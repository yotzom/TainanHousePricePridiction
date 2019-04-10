library(stringr)
library(keras)

setwd("/home/ai05/workspace/prediction_model/newmodel")
model <- load_model_hdf5("pred_model_25.h5")
load("normalization_25.rda")
load("list.rda")
load("Pricedetail.rda")

#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}

#load model 
#model <- load_model_hdf5("pred_model.h5")
#load(file = "pred_model.rds")
#load(file = "pred_model_history.rds")
#load("normalization.rda")

#* House prediction model
#* @param address
#* @param finished_year
#* @param BuildingMount
#* @param LandMount
#* @param ParkArea_m2
#* @param ParkType
#* @param ManagementUnit
#* @param ToiletMount
#* @param HallMount
#* @param roomMount
#* @param BuildingArea_m2
#* @param BuildingMaterial
#* @param AllFloor
#* @param Type
#* @param LocateFloor
#* @param LandArea_m2
#* @param TransactionContent 
#* @get /prediction
prediction <- function(TransactionContent, LandArea_m2, LocateFloor, AllFloor, Type, 
                         BuildingMaterial, BuildingArea_m2, roomMount, HallMount, 
                         ToiletMount, ManagementUnit, ParkType, ParkArea_m2, LandMount, 
                         BuildingMount, finished_year, address){
  cat(address)

  data_list <- c(TransactionContent, LandArea_m2, LocateFloor, AllFloor, 
                 Type, BuildingMaterial, BuildingArea_m2, roomMount, HallMount, 
                 ToiletMount, ManagementUnit, ParkType, ParkArea_m2, LandMount, 
                 BuildingMount, finished_year)
  data <- as.data.frame(data_list)
  data_t <- t(data)
  data <- as.data.frame(data_t)
  #split address
  #data[1,data$Type == Type_list[i]] <- i
  tmp1 <- ''
  tmp <- ''
  
  # if(str_detect(address, "市")){
  #   tmp <- str_split(address, "市")
  # }else{
  #   tmp[1] <- ""
  #   tmp[2] <- address
  # }
  # tmp1[1] <- tmp[[1]][1]
  
  if(str_detect(address, "區")){
    tmp <- str_split(address, "區")
  }else{
    tmp[1] <- ""
    tmp[2] <- address
  }
  tmp1[1] <- tmp[[1]][1]
  
  if(str_detect(tmp[2], "里")){
    tmp <- str_split(tmp[2], "里")
  }else{
    tmp[1] <- ""
    tmp[2] <- tmp[2]
  }
  tmp1[2] <- tmp[[1]][1]
  
  if(str_detect(tmp[2], "路")){
    tmp <- str_split(tmp[2], "路")
  }else{
    tmp[1] <- ""
    tmp[2] <- tmp[2]
  }
  tmp1[3] <- tmp[[1]][1]
  
  if(str_detect(tmp[2], "鄰")){
    tmp <- str_split(tmp[2], "鄰")
  }else{
    tmp[1] <- ""
    tmp[2] <- tmp[2]
  }
  tmp1[4] <- tmp[[1]][1]
  
  if(str_detect(tmp[2], "街")){
    tmp <- str_split(tmp[2], "街")
  }else{
    tmp[1] <- ""
    tmp[2] <- tmp[2]
  }
  tmp1[5] <- tmp[[1]][1]
  
  if(str_detect(tmp[2], "段")){
    tmp <- str_split(tmp[2], "段")
  }else{
    tmp[1] <- ""
    tmp[2] <- tmp[2]
  }
  tmp1[6] <- tmp[[1]][1]
  
  if(str_detect(tmp[2], "巷")){
    tmp <- str_split(tmp[2], "巷")
  }else{
    tmp[1] <- ""
    tmp[2] <- tmp[2]
  }
  tmp1[7] <- tmp[[1]][1]
  
  if(str_detect(tmp[2], "弄")){
    tmp <- str_split(tmp[2], "弄")
  }else{
    tmp[1] <- ""
    tmp[2] <- tmp[2]
  }
  tmp1[8] <- tmp[[1]][1]
  
  if(str_detect(tmp[2], "號")){
    tmp <- str_split(tmp[2], "號")
  }else{
    tmp[1] <- ""
    tmp[2] <- tmp[2]
  }
  tmp1[9] <- tmp[[1]][1]
  
  address_df <- as.data.frame(as.character(tmp1))
  address_df <- as.data.frame(t(address_df))
  address_df <- data.frame(lapply(address_df, as.character), stringsAsFactors=FALSE)
  
  for (i in 1:length(V1_list)) {
    address_df$V1[address_df$V1 == V1_list[i]] <- i
  }
  
  for (i in 1:length(V2_list)) {
    address_df$V2[address_df$V2 == V2_list[i]] <- i
  }

  for (i in 1:length(V3_list)) {
    address_df$V3[address_df$V3 == V3_list[i]] <- i
  }

  for (i in 1:length(V4_list)) {
    address_df$V4[address_df$V4 == V4_list[i]] <- i
  }

  for (i in 1:length(V5_list)) {
    address_df$V5[address_df$V5 == V5_list[i]] <- i
  }

  for (i in 1:length(V6_list)) {
    address_df$V6[address_df$V6 == V6_list[i]] <- i
  }

  for (i in 1:length(V7_list)) {
    address_df$V7[address_df$V7 == V7_list[i]] <- i
  }

  for (i in 1:length(V8_list)) {
    address_df$V8[address_df$V8 == V8_list[i]] <- i
  }

  for (i in 1:length(V9_list)) {
    address_df$V9[address_df$V9 == V9_list[i]] <- i
  }
  
  data[1,17] <- address_df$V1
  data[1,18] <- address_df$V2
  data[1,19] <- address_df$V3
  data[1,20] <- address_df$V4
  data[1,21] <- address_df$V5
  data[1,22] <- address_df$V6
  data[1,23] <- address_df$V7
  data[1,24] <- address_df$V8
  data[1,25] <- address_df$V9
  
  for (i in 17:25) {
    if(is.na(data[i])){
      data[i] <- round(runif(1,3,6))
    }
    if(!is.numeric(data[i])){
      data[i] <- round(runif(1,3,6))
    }
  }
	
  housePrice <- Pricedetail[data[1,19], 1]
  houseAreaPrice <- Pricedetail[data[1,19], 2]
  
  #predict
  data <- as.matrix(data)
  data <- as.numeric(data)
  for (i in 1:length(data)) {
    data[i] <- (data[i] - min_list[i]) / (max_list[i] - min_list[i])
  }
  data <- t(data)
  data <- as.matrix(data)
  
  pred <- model %>% predict(x = data)
  
  reduction_pred <- (pred * (price_max - price_min)) + price_min
  reduction_pred <- round(reduction_pred)
  cat("Price : ", reduction_pred)
  return_array <- c(reduction_pred, housePrice, houseAreaPrice)
  return(return_array)
 }

#* test
#* @param num
#* @get /testfunc
testfunc <- function(){
  return("ok")
}

#step1 load model V
#step2 normalization data
#step3 prediction
#step4 reduction data
#step5 return result

# prediction <- function(Dist, TransactionContent, LandArea_m2, CityLandClass, LocateFloor, AllFloor, 
#                         Type, MainUse, BuildingMaterial, BuildingArea_m2, roomMount, HallMount, 
#                         ToiletMount, Wall, ManagementUnit, ParkType, ParkArea_m2, LandMount, 
#                         BuildingMount, ParkMount, V1, V2, V3, V4, V5, V6, V7, V8, V9){
