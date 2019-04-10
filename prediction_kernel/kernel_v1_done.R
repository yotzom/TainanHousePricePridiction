library(stringr)
#library(xlsx)

#reference
#https://blog.csdn.net/definedone/article/details/79140709

setwd("C:\\Users\\CcYoTzom\\Desktop\\house_prediction_kernel\\output")
#data <- read.csv("houseTransactionRecord_max_min_normal.csv", header = T, sep = ",", stringsAsFactors = F)
data <- data.frame()
filename_list <- list.files(pattern = ".csv")

for (i in 1:length(filename_list)) {
  data <- rbind(data, read.csv(filename_list[i], header = F, sep = ",", stringsAsFactors = F))
}

names(data) <- c("Dist", "TransactionContent", "Address", "LandArea_m2", "CityLandClass", "NotCityLandClass", 
                 "NotCityLandClassNote", "TransactionDate", "TransactionMount", "LocateFloor", "AllFloor",
                 "Type", "MainUse", "BuildingMaterial", "FinishedDate", "BuildingArea_m2", "roomMount",
                 "HallMount", "ToiletMount", "Wall", "ManagementUnit", "Price", "PricePerM2", "ParkType", 
                 "ParkArea_m2", "ParkPrice", "Note", "Number") 
#preprocessing

#data <- read.csv("row_data.csv", header = T, sep = ",", stringsAsFactors = F)

#NA處理
data <- data[complete.cases(data),]

#step.1 刪除含有note的資料
data <- data[which(data$Note == ""),]

#step.2 刪除number
data <- subset(data,select = -c(Number, Note))

#step.3 刪除只交易車位或土地的資料
data <- data[which(data$TransactionContent != "土地"),]
data <- data[which(data$TransactionContent != "車位"),]

#step.4 刪除非都市用地
data <- data[which(data$NotCityLandClass == ""),]

#step.5 刪除NotCityLandClass.NotCityLandClassNote
data <- subset(data,select = -c(NotCityLandClass, NotCityLandClassNote))

#step.6 交易總比數拆分成土地、建物、車位
#strsplit(data$TransactionMount, split = "建物")
tmp <- as.data.frame(t(as.data.frame(strsplit(data$TransactionMount, split = "土地"))), stringsAsFactors = F)
tmp <- cbind(tmp, as.data.frame(t(as.data.frame(strsplit(tmp$V2, split = "建物"))), stringsAsFactors = F))
colnames(tmp) <- c("V1", "V2", "V3", "tmp")
tmp <- cbind(tmp, as.data.frame(t(as.data.frame(strsplit(tmp$tmp, split = "車位"))), stringsAsFactors = F))
colnames(tmp) <- c("delete", "delete1", "LandMount", "delete2", "BuildingMount", "ParkMount")
data <- cbind(data, tmp$LandMount, tmp$BuildingMount, tmp$ParkMount)
remove(tmp)
names(data)[25:27] <- c("LandMount", "BuildingMount", "ParkMount")
data <- subset(data,select = -c(TransactionMount))

#step.7 刪除建物類型工廠、倉庫、廠辦、其他
data <- data[which(data$Type != "工廠"),]
data <- data[which(data$Type != "倉庫"),]
data <- data[which(data$Type != "廠辦"),]
data <- data[which(data$Type != "其他"),]

#step.8 刪除主要用途工業用、農業用、住工用、見其他登記事項(公設)
data <- data[which(data$MainUse != "工業用"),]
data <- data[which(data$MainUse != "農業用"),]
data <- data[which(data$MainUse != "住工用"),]
data <- data[which(data$MainUse != "見其他登記事項"),]

#step.9 刪除都市用地類型農、工
data <- data[which(data$CityLandClass != "農"),]
data <- data[which(data$CityLandClass != "工"),]

#step.10 刪除PricePerM2
data <- subset(data,select = -c(PricePerM2))

#step.11 刪除價格低於1100000
data <- data[which(data$Price >= 1100000),]
data <- data[which(data$Price <= 11000000),]

#step.12 remove contain NA row
data <- data[complete.cases(data),]
#setwd("C:\\Users\\CCYoTzom\\Desktop\\house_data")
#data <- read.csv("house_transaction_record.csv", header = T, sep = ",", stringsAsFactors = F)
nrow(data)
ncol(data)
summary(data)

#split FinishedDate
tmp <- sapply(data$FinishedDate, function(x){
  str_pad(string = x, side = "left", width = 7)
})

finished_year <- sapply(tmp, function(x){
  substring(x, 1, 3)
})

finished_year <- sapply(finished_year, function(x){
  str_trim(x, side='left')
})

finished_month <- sapply(tmp, function(x){
  substring(x, 4, 5)
})

finished_day <- sapply(tmp, function(x){
  substring(x, 6, 7)
})

finished_year <- as.data.frame(as.integer(finished_year))
finished_month <- as.data.frame(as.integer(finished_month))
finished_day <- as.data.frame(as.integer(finished_day))

names(finished_year) <- c("finished_year")
names(finished_month) <- c("finished_month")
names(finished_day) <- c("finished_day")

data <- cbind(data, finished_day)
data <- cbind(data, finished_month)
data <- cbind(data, finished_year)
data <- subset(data,select = -c(FinishedDate))

remove(tmp, finished_year, finished_month, finished_day)

#split TransactionDate
tmp <- sapply(data$TransactionDate, function(x){
  str_pad(string = x, side = "left", width = 7)
})

transaction_year <- sapply(tmp, function(x){
  substring(x, 1, 3)
})

transaction_year <- sapply(transaction_year, function(x){
  str_trim(x, side='left')
})

transaction_month <- sapply(tmp, function(x){
  substring(x, 4, 5)
})

transaction_day <- sapply(tmp, function(x){
  substring(x, 6, 7)
})

transaction_year <- as.vector(transaction_year)
transaction_month <- as.vector(transaction_month)
transaction_day <- as.vector(transaction_day)

tmp <- cbind(transaction_year, transaction_month, transaction_day)
tmp <- data.frame(tmp)
data <- cbind(data, tmp)
data <- subset(data,select = -c(TransactionDate))

remove(tmp, transaction_year, transaction_month, transaction_day)

#write.table(x = data, file = "house_transaction_record.csv", row.names = FALSE, sep = ",")
#data <- read.csv("house_transaction_record_v2.csv", header = T, sep = ",", stringsAsFactors = F)

Dist_list <- unique(data$Dist)
for (i in 1:length(Dist_list)) {
  data$Dist[data$Dist == Dist_list[i]] <- i
}

CityLandClass_list <- unique(data$CityLandClass)
for (i in 1:length(CityLandClass_list)) {
  data$CityLandClass[data$CityLandClass == CityLandClass_list[i]] <- i
}

Type_list <- unique(data$Type)
for (i in 1:length(Type_list)) {
  data$Type[data$Type == Type_list[i]] <- i
}

MainUse_list <- unique(data$MainUse)
for (i in 1:length(MainUse_list)) {
  data$MainUse[data$MainUse == MainUse_list[i]] <- i
}

BuildingMaterial_list <- unique(data$BuildingMaterial)
for (i in 1:length(BuildingMaterial_list)) {
  data$BuildingMaterial[data$BuildingMaterial == BuildingMaterial_list[i]] <- i
}

Wall_list <- unique(data$Wall)
for (i in 1:length(Wall_list)) {
  data$Wall[data$Wall == Wall_list[i]] <- i
}

ManagementUnit_list <- unique(data$ManagementUnit)
for (i in 1:length(ManagementUnit_list)) {
  data$ManagementUnit[data$ManagementUnit == ManagementUnit_list[i]] <- i
}

ParkType_list <- unique(data$ParkType)
for (i in 1:length(ParkType_list)) {
  data$ParkType[data$ParkType == ParkType_list[i]] <- i
}

LocateFloor_list <- unique(data$LocateFloor)
for (i in 1:length(LocateFloor_list)) {
  data$LocateFloor[data$LocateFloor == LocateFloor_list[i]] <- i
}

AllFloor_list <- unique(data$AllFloor)
for (i in 1:length(AllFloor_list)) {
  data$AllFloor[data$AllFloor == AllFloor_list[i]] <- i
}

TransactionContent_list <- unique(data$TransactionContent)
for (i in 1:length(TransactionContent_list)) {
  data$TransactionContent[data$TransactionContent == TransactionContent_list[i]] <- i
}

tmp <- c()
tmp1 <- data.frame()

for (i in 1:length(data$Address)) {
  tmp[i] <- str_split(data$Address[i], "區")
}

for (i in 1:length(data$Address)) {
  if(str_detect(data$Address[i], "區")){
    tmp[i] <- str_split(data$Address[i], "區")
  }else{
    tmp[[i]][1] <- ""
    tmp[[i]][2] <- data$Address[i]
  }
  tmp1[i, 1] <- tmp[[i]][1]
}

for (i in 1:length(data$Address)) {
  if(str_detect(tmp[[i]][2], "里")){
    tmp[i] <- str_split(tmp[[i]][2], "里")
  }else{
    tmp[[i]][1] <- ""
    tmp[[i]][2] <- tmp[[i]][2]
  }
  tmp1[i, 2] <- tmp[[i]][1]
}

for (i in 1:length(data$Address)) {
  if(str_detect(tmp[[i]][2], "路")){
    tmp[i] <- str_split(tmp[[i]][2], "路")
  }else{
    tmp[[i]][1] <- ""
    tmp[[i]][2] <- tmp[[i]][2]
  }
  tmp1[i, 3] <- tmp[[i]][1]
}

for (i in 1:length(data$Address)) {
  if(str_detect(tmp[[i]][2], "鄰")){
    tmp[i] <- str_split(tmp[[i]][2], "鄰")
  }else{
    tmp[[i]][1] <- ""
    tmp[[i]][2] <- tmp[[i]][2]
  }
  tmp1[i, 4] <- tmp[[i]][1]
}

for (i in 1:length(data$Address)) {
  if(str_detect(tmp[[i]][2], "街")){
    tmp[i] <- str_split(tmp[[i]][2], "街")
  }else{
    tmp[[i]][1] <- ""
    tmp[[i]][2] <- tmp[[i]][2]
  }
  tmp1[i, 5] <- tmp[[i]][1]
}

for (i in 1:length(data$Address)) {
  if(str_detect(tmp[[i]][2], "段")){
    tmp[i] <- str_split(tmp[[i]][2], "段")
  }else{
    tmp[[i]][1] <- ""
    tmp[[i]][2] <- tmp[[i]][2]
  }
  tmp1[i, 6] <- tmp[[i]][1]
}

for (i in 1:length(data$Address)) {
  if(str_detect(tmp[[i]][2], "巷")){
    tmp[i] <- str_split(tmp[[i]][2], "巷")
  }else{
    tmp[[i]][1] <- ""
    tmp[[i]][2] <- tmp[[i]][2]
  }
  tmp1[i, 7] <- tmp[[i]][1]
}

for (i in 1:length(data$Address)) {
  if(str_detect(tmp[[i]][2], "弄")){
    tmp[i] <- str_split(tmp[[i]][2], "弄")
  }else{
    tmp[[i]][1] <- ""
    tmp[[i]][2] <- tmp[[i]][2]
  }
  tmp1[i, 8] <- tmp[[i]][1]
}

for (i in 1:length(data$Address)) {
  if(str_detect(tmp[[i]][2], "號")){
    tmp[i] <- str_split(tmp[[i]][2], "號")
  }else{
    tmp[[i]][1] <- ""
    tmp[[i]][2] <- tmp[[i]][2]
  }
  tmp1[i, 9] <- tmp[[i]][1]
}

data <- cbind(data, tmp1)
remove(tmp, tmp1, i)

#write.table(x = data, file = "house_transaction_record.csv", row.names = FALSE, sep = ",")
#data <- read.csv("house_transaction_record_v3.csv", header = T, sep = ",", stringsAsFactors = F)

V1_list <- unique(data$V1)
for (i in 1:length(V1_list)) {
  data$V1[data$V1 == V1_list[i]] <- i
}

V2_list <- unique(data$V2)
for (i in 1:length(V2_list)) {
  data$V2[data$V2 == V2_list[i]] <- i
}

V3_list <- unique(data$V3)
for (i in 1:length(V3_list)) {
  data$V3[data$V3 == V3_list[i]] <- i
}

V4_list <- unique(data$V4)
for (i in 1:length(V4_list)) {
  data$V4[data$V4 == V4_list[i]] <- i
}

V5_list <- unique(data$V5)
for (i in 1:length(V5_list)) {
  data$V5[data$V5 == V5_list[i]] <- i
}

V6_list <- unique(data$V6)
for (i in 1:length(V6_list)) {
  data$V6[data$V6 == V6_list[i]] <- i
}

V7_list <- unique(data$V7)
for (i in 1:length(V7_list)) {
  data$V7[data$V7 == V7_list[i]] <- i
}

V8_list <- unique(data$V8)
for (i in 1:length(V8_list)) {
  data$V8[data$V8 == V8_list[i]] <- i
}

V9_list <- unique(data$V9)
for (i in 1:length(V9_list)) {
  data$V9[data$V9 == V9_list[i]] <- i
}

data <- subset(data,select = -c(Address))

#old outliers process
# data <- data[data$Price>1490000,]
# data <- data[data$Price<14900000,]

#outliers process

#1.Standard Deviation 標準差  
price_mean <- sum(data$Price)/length(data$Price)
price_sd <- sd(data$Price)
x <- seq(-1.5,1.5,length=length(x))
y <- dnorm(x, mean = price_mean, sd = price_sd)
plot(x, y, type = "l", lty= 1, ylab = "房屋總價")

count = 0
sigma = 3
for (i in 1:nrow(data)) {
  if((data$Price[i]-price_mean)/price_sd > sigma || (data$Price[i]-price_mean)/price_sd < -sigma){
    count <- count + 1
  }
}
cat("共有", count, "筆資料大於", sigma, "個標準差")

#2.Hampel identifier
price_median <- median(data$Price)
price_median_list <- abs(price_median - data$Price)
median_absolute_deviation <- median(price_median_list)

count = 0
for (i in 1:length(price_median_list)) {
  if(price_median_list[i]/(median_absolute_deviation/0.6745) > 2.24){
    count <- count + 1
  }
}
cat("共有", count, "筆資料為離群值")

#3.boxplot
# interquartile range (IQR) = Q3 − Q1
# 
# 盒子兩端延伸出去的虛線是Q1 − 1.5 x IQR 或 Q3 + 1.5 x IQR
price_Q1 <- quantile(data$Price, 0.25)
price_median <- median(data$Price)
price_Q3 <- quantile(data$Price, 0.75)
IQR <- price_Q3 - price_Q1

count = 0
for (i in 1:nrow(data)) {
  if(data$Price[i] > (price_Q3 + 1.5 * IQR) || data$Price[i] < (price_Q1 - 1.5 * IQR)){
    count <- count + 1
  }
}
cat("共有", count, "筆資料為離群值")

#cast to numeric
names_list <- names(data)
data <- apply(data, 1, as.numeric)
data <- as.data.frame(t(data))
names(data) <- c(names_list)

#max-min normalization
#max_list
max_list <- round(apply(data, 2, max), digits = 10)
#min_list
min_list <- round(apply(data, 2, min), digits = 10)

save(max_list, min_list, file = "normalization.rda")

# save(AllFloor_list, BuildingMaterial_list, CityLandClass_list, LocateFloor_list, ManagementUnit_list,
#      ParkType_list, TransactionContent_list, Type_list, V1_list, V2_list, V3_list, V4_list,
#      V5_list, V6_list, V7_list, V8_list, V9_list, Wall_list, file = "list.rda")

for (i in 1:length(names(data))) {
  for (j in 1:length(data[,i])) {
    data[j, i] <- (data[j, i] - min_list[i]) / (max_list[i] - min_list[i])
  }
}

#write.table(x = data, file = "house_transaction_record.csv", row.names = FALSE, sep = ",")
#data <- read.csv("house_transaction_record.csv", header = T, sep = ",", stringsAsFactors = F)