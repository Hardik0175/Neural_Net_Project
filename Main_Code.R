library(sp)
library(rgdal)
library(raster)
library(maptools)
library(ggmap)
library(dplyr)
library(scales)
library(spatialEco)
library(rgeos)
library(geosphere)


## --------- Loading Data --------- ##

## Path need to be change
accidents <- read.csv("E:/Ryerson_Spring_20/Neural_Network/Project/Data/accidents6.csv")
string_crs <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
accidents <- accidents[!is.na(accidents$Lat),]


## --------- Initialization --------- ##
upperLat <- max(accidents$Lat)
lowerLat <- min(accidents$Lat)
upperLong <- max(accidents$Long)
lowerLong <- min(accidents$Long)
time <- accidents$YEAR*10000 + accidents$MONTH*100
time <- data.frame(time)
accidents <- cbind(time,accidents)


## --------- Loading L.A. Map ---------##

## Path need to be change
county <- readOGR(dsn = 'E:/Ryerson_Spring_20/Neural_Network/Project/Map')
TAO <- as(county, "SpatialPolygons")
TAO <- TAO[1:318]
coords_data <- county@data[1:318,]
angelesn_area <-  SpatialPolygonsDataFrame(TAO,coords_data)
angelesn_area_full <-  spTransform(angelesn_area,CRS(string_crs))
plot(angelesn_area_full)

## --------- Loading Grid ---------##
x = y = base =100
xy <- matrix(rnorm(x*y),x,y)
mesh <- raster(xy)
extent(mesh) <- c(lowerLong, upperLong, lowerLat, upperLat)
projection(mesh) <- CRS(string_crs)
mesh2 <- rasterToPolygons(mesh,dissolve=TRUE)
mesh2@data$ID <- c(1:nrow(mesh2@data))
plot(mesh2)


## --------- Intersection of Map and Grid --------- ##
intersection <- intersect(mesh2,angelesn_area_full)
plot(intersection)


## --------- Pre Processing --------- ##
accidents$time <- substr(accidents$time,1,6)
accidents <- accidents[,c("time","Lat","Long")]
accidents_stats <- summary(factor(accidents$time))
accidents_stats <- data.frame(time = names(accidents_stats),count = accidents_stats,stringsAsFactors = F)
area_data <- intersection@data


## --------- Adding accidents according to areas --------- ##
add_values <- function(area_data,coords_data,fname,polygon){
  point <- SpatialPointsDataFrame(coords = coords_data[,c("Long","Lat")], data = coords_data,proj4string = CRS(string_crs))
  pdata <-  point.in.poly(point,polygon)
  inpoly_ID <- pdata@data$ID
  temp <- table(inpoly_ID)
  area_data[,fname] <- sapply(area_data$ID,function(x){sum(temp[names(temp)==x])})
  return(area_data)
}

add_value_static <- function(area_data,coords_data,fname,polygon){
  point <- SpatialPointsDataFrame(coords = coords_data[,c("Long","Lat")], data = coords_data,proj4string = CRS(string_crs))
  pdata <-  point.in.poly(point,polygon)
  inpoly_ID <- pdata@data$ID
  temp <- table(inpoly_ID)
  area_data[,toString(fname)] <- sapply(area_data$ID,function(x){sum(temp[names(temp)==x])})
  return(area_data)
}

area_data2 <- add_values(area_data,accidents,"accidents",intersection)

## --------- Filtering Values --------- ##
exist <- !area_data2$accidents == 0
intersection_re <- intersection[exist,]
area_data3 <- area_data2[exist,]
cbind(data = nrow(area_data3),sp = length(intersection_re))
exist_index <- as.integer(row.names(area_data3))
plot(intersection_re)


## --------- is Hotspot --------- ##
time_sequence <- sort(unique(accidents$time))
summary(factor(accidents$time))


## --------- Creating Time data --------- ## 
time_data <- list()
time_data2 <- list()

for(i in 1:length(time_sequence)){
  time_data2[[i]] <- add_values(area_data3,accidents[accidents$time == time_sequence[i],],"accidents",intersection_re)
  time_data2[[i]]$time <- time_sequence[i]
  time_data[[i]] <- select(time_data2[[i]],accidents,time) 
}

for(i in 1:length(time_sequence)){
  time_data[[i]][time_data[[i]][,"accidents"] >= 1,"hotspot"]  <- 1
  time_data[[i]][time_data[[i]][,"accidents"] == 0,"hotspot"]  <- 0
}

for(i in  13 : length(time_sequence)){
  time_data[[i]][,"last_1"]  <- time_data[[i-1]][,"hotspot"]
  time_data[[i]][,"last_3"]  <- time_data[[i-1]][,"hotspot"] + time_data[[i-2]][,"hotspot"] + time_data[[i-3]][,"hotspot"]
  time_data[[i]][,"last_6"]  <- time_data[[i-1]][,"hotspot"] + time_data[[i-2]][,"hotspot"] + time_data[[i-3]][,"hotspot"] + time_data[[i-4]][,"hotspot"] + time_data[[i-5]][,"hotspot"] + time_data[[i-6]][,"hotspot"]
  time_data[[i]][,"last_9"]  <- time_data[[i-1]][,"hotspot"] + time_data[[i-2]][,"hotspot"] + time_data[[i-3]][,"hotspot"] + time_data[[i-4]][,"hotspot"] + time_data[[i-5]][,"hotspot"] + time_data[[i-6]][,"hotspot"] + time_data[[i-7]][,"hotspot"] + time_data[[i-8]][,"hotspot"] + time_data[[i-9]][,"hotspot"]
  time_data[[i]][,"last_12"]  <- time_data[[i-1]][,"hotspot"] + time_data[[i-2]][,"hotspot"] + time_data[[i-3]][,"hotspot"] + time_data[[i-4]][,"hotspot"] + time_data[[i-5]][,"hotspot"] + time_data[[i-6]][,"hotspot"] + time_data[[i-7]][,"hotspot"] + time_data[[i-8]][,"hotspot"] + time_data[[i-9]][,"hotspot"] + time_data[[i-10]][,"hotspot"] + time_data[[i-11]][,"hotspot"] + time_data[[i-12]][,"hotspot"]
  time_data[[i]][,"before_12"]  <- time_data[[i-12]][,"hotspot"]
}


## --------- Splitting Values for 8 models from 2:1 to 9:1 --------- ## 
train_list <- list()
test_list <- list()

for(i in 1 : 8){
  tmp <- time_data[[i+43]]
  test_list[[i]] <- tmp
}

for(i in 1 : 8){
  train_frame <- test_list[[1]][0,]
  for(j in 34 : (i + 34)){
    tmp <- time_data[[j]]
    train_frame <- rbind(train_frame,tmp)
  }
  train_list[[i]] <- train_frame
}


## --------- Normalization --------- ## 
maxmin <- function(x){(x-min(x))/(max(x)-min(x))}

for(i in 1 : 8){
  for(j in 4:ncol(train_list[[1]])){
    train_list[[i]][,j] <- maxmin(train_list[[i]][,j])
    test_list[[i]][,j] <- maxmin(test_list[[i]][,j])
  }
}

for(i in 1 : 8){
  train_list[[i]]$hotspot <- factor(train_list[[i]]$hotspot)
  test_list[[i]]$hotspot <- factor(test_list[[i]]$hotspot)
}

## --------- Training of model --------- ## 
library(h2o)
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jdk-11.0.7')
localH2O <- h2o.init(ip = 'localhost', port = 54321, max_mem_size = '6g')
dependent_variable <- colnames(train_list[[1]])[3]
independent_variable <- colnames(train_list[[1]])[c(4:9)]
dependent_variable
independent_variable

model_performance <- function(matrix){
  if(ncol(matrix) == 2){
    PPV <- matrix[2,2] / sum(matrix[,2])
    TPR <- matrix[2,2] / sum(matrix[2,])
    NPV <- matrix[1,1] / sum(matrix[,1])
    TNR <- matrix[1,1] / sum(matrix[1,])
    accuracy <- (matrix[1,1] + matrix[2,2]) / sum(matrix)
    F1 <- 2 * (PPV * TPR)/(PPV + TPR)
    if(is.na(F1)){F1 = 0}
    performance <- cbind(PPV,TPR,NPV,TNR,accuracy,F1)
    return(performance)
  }else{
    return(data.frame(PPV = 0,TPR = 0,NPV = 0,TNR = 0,accuracy = 0,F1 = 0))
  }
}

## --------- Initializing variable of storing the performance of each model --------- ## 
model_dnn_list <- list()
model_random_forest_list <- list()
matrix_dnn_list <- list()
matrix_random_forest_list <- list()
random_forest_prediction_list <- list()
dnn_prediction_list <- list()

random_forest_perfromane <- data.frame(PPV = 0,TPR = 0,NPV = 0,TNR = 0,accuracy = 0,F1 = 0)[0,]
dnn_performance <- data.frame(PPV = 0,TPR = 0,NPV = 0,TNR = 0,accuracy = 0,F1 = 0)[0,]


## --------- Using Random Forest and DNN  --------- ## 
for(i in 1 : 8){
  train_frame <- train_list[[i]]
  test_frame <- test_list[[i]]
  
  dtrain <- as.h2o(train_frame)
  dtest <- as.h2o(test_frame)
  
  model_random_forest <- h2o.randomForest("randomForest",
                                          x = independent_variable,
                                          y = dependent_variable,
                                          training_frame = dtrain)
  
  model_dnn <- h2o.deeplearning(x = independent_variable,
                                y = dependent_variable,
                                training_frame = dtrain,
                                hidden = c(100,100,100,100,100,100,100),
                                activation = "RectifierWithDropout",
                                adaptive_rate = F,
                                rate = 0.001,
                                epochs = 30)
  
  rf_prediction <- as.data.frame(h2o.predict(model_random_forest, newdata = dtest))
  dp_prediction <- as.data.frame(h2o.predict(model_dnn, newdata = dtest))
  
  rf_matrix <- table(test_frame$hotspot,rf_prediction$predict)
  dp_matrix <- table(test_frame$hotspot,dp_prediction$predict)
  
  model_dnn_list[[i]] <- model_dnn
  model_random_forest_list[[i]] <- model_random_forest
  matrix_dnn_list[[i]] <- dp_matrix
  matrix_random_forest_list[[i]] <- rf_matrix
  random_forest_prediction_list[[i]] <- rf_prediction
  dnn_prediction_list[[i]] <- dp_prediction
  
  random_forest_perfromane[i,] <- model_performance(rf_matrix)
  dnn_performance[i,] <- model_performance(dp_matrix)
}


#####################################
# Add the Google api in this command and run it before plotting-
#      register_google(key = "[your key]", write = TRUE)
# Enable the API and then Add the key in the box
#####################################


show_ggmap <- function(plot){
  polygon <- data.frame(lon = 0,lat = 0,group = 0)[0,]
  for(i in 1 : length(plot)){
    tmp <- data.frame(plot@polygons[[i]]@Polygons[[1]]@coords)
    names(tmp) <- c("lon","lat")
    tmp$group = i
    polygon <- rbind(polygon,tmp)
  }
  
  
  map <- get_map(maptype = "roadmap",location=c(lon=-118.2437, lat=34.0522), zoom = 8)
  map_preicted <-ggmap(map, extent = "panel") + 
    geom_polygon(data = polygon,
                 aes(lon,lat, group = group),
                 fill = "darkred",alpha = 0.6)
  plot(map_preicted)
}


show_ggmap(intersection_re[as.logical(as.integer(as.character(dnn_prediction_list[[7]]$predict))),])

show_ggmap(intersection_re[as.logical(as.integer(as.character(test_list[[7]]$hotspot))),])
