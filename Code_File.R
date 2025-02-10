library("sp")
library("sf")
library("raster")
library("dplyr") 
library("randomForest")
library("caret")
library("e1071")


dem <- raster("dem.tif")
twi <- raster("twi.tif")
precip.A <- raster("precip_April.tif")
precip.S <- raster("Precip_September.tif")
soil.moist.A <- raster("Soilmoist_April.tif")
soil.moist.S <- raster("soilmoist_September.tif")


Slope <- terrain(dem, opt="slope", unit="degrees")
rst <- writeRaster(Slope, "slope.tif")


rasStack <- stack(dem, twi, precip.A, precip.S, soil.moist.A, soil.moist.S, Slope)

plot(rasStack)

landslides <- shapefile("landslides_training.shp")

rasValue <-extract(rasStack, landslides)

data <- cbind(rasValue, landslides$landslide)

data<- as.data.frame(data)
colnames(data)[8] <- 'landslide'


str(data)
summary(data)

#as you can see numeric data are converted to character which we have to change

data[] <- lapply(data, function(x) as.numeric(as.character(x)))
str(data)


set.seed(1234)

sum(is.na(data))

data[is.na(data)] <- 0


data$landslide <- as.factor(data$landslide)

str(data)

trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")



train <- sample(nrow(data), 0.7*nrow(data), replace = FALSE)
TrainSet <- data[train,]
ValidSet <- data[-train,]
dim(TrainSet)
dim((ValidSet))


rf_default <- train(landslide~.,
                    data = TrainSet,
                    method = "rf",
                    metric = "Accuracy",
                    trControl = trControl)



print(rf_default)


prediction <-predict(rf_default, ValidSet)


confusionMatrix(prediction, ValidSet$landslide)


imp <- varImp(rf_default)

plot(imp)


prediction <-predict(rasStack, rf_default)

newcol <- c("beige", "blue")

plot(prediction, col=newcol)


rst <- writeRaster(prediction, ("RF_prediction.tif"), 
                   format="GTiff", overwrite=TRUE)


















#some code chunks
newlist <- list(twi, precip.A, precip.S, soil.moist.A, soil.moist.S, Ls.08.B5, LS.08.B4)

newextent <- extent(dem)

for (i in newlist){
  extent(i) <- newextent
  print(extent(i))
} 

rasStack <- stack(newlist)










