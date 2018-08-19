
########Libraries required for this assignment######

install.packages("stringr")
install.packages("tidyr")
install.packages("dplyr")
install.packages("MASS")
install.packages("car")
library(stringr)
library(tidyr)
library(dplyr)
library(MASS)
library(car)

cars <- read.csv("CarPrice_Assignment.csv")

#structure of the dataset
str(cars)
cars$symboling<-as.character(cars$symboling)
cars$symboling<-as.factor(cars$symboling)


#checking for NAs in the cars data frame
sapply(cars, function(x) sum(is.na(x)))

#checking for duplicate values in unique variable
sum(duplicated(cars$car_ID))

#rounding off the below columns to two places
cars$boreratio <- round(cars$boreratio, 2)
cars$stroke <- round(cars$stroke, 2)
cars$compressionratio <- round(cars$compressionratio, 2)

#Split columns for more data -- Carname 
cars$CarName<- str_replace_all(cars$CarName," ","-")
cars<-separate(cars,CarName,into = c("CarName","model"),sep = "-")
cars<-cars[,-4]
cars$CarName<-as.factor(cars$CarName)

#cleaning CarName variable
cars$CarName[cars$CarName=="vw"]<-"volkswagen"
cars$CarName[cars$CarName=="vokswagen"]<-"volkswagen"
cars$CarName[cars$CarName=="maxda"]<-"mazda"
cars$CarName[cars$CarName=="Nissan"]<-"nissan"
cars$CarName[cars$CarName=="porcshce"]<-"porsche"
cars$CarName[cars$CarName=="toyouta"]<-"toyota"

#removing first column
cars <- cars[,-1]
cars <- cars[,-8] #removing enginelocation variable - as 99% of the data has only "front" value

#removing outliers

quantile(cars$enginesize,seq(0,1,0.01))
boxplot(cars$enginesize)
cars$enginesize[cars$enginesize>201.20]<-201.20

quantile(cars$compressionratio,seq(0,1,0.01))
boxplot(cars$compressionratio)
cars$compressionratio[cars$compressionratio>10.9400]<-10.9400

quantile(cars$citympg,seq(0,1,0.01))
boxplot(cars$citympg)
cars$citympg[cars$citympg>44.72]<-44.72

#converting categorical variables into numerical levels
#fueltype- gas=0 ;diesel=1
levels(cars$fueltype)<-c(1,0)
cars$fueltype <- as.numeric(levels(cars$fueltype))[cars$fueltype]
#aspiration- std=1;turbo=0
levels(cars$aspiration)<-c(1,0)
cars$aspiration <- as.numeric(levels(cars$aspiration))[cars$aspiration]
#doornumber-two=0;four=1
levels(cars$doornumber)<-c(1,0)
cars$doornumber <- as.numeric(levels(cars$doornumber))[cars$doornumber]


#creating dummy variables for CarName
dummy_1 <- data.frame(model.matrix( ~CarName, data = cars))
dummy_1 <- dummy_1[,-1]
cars <- cbind(cars[,-2], dummy_1)


#creating dummy variables for carbody
dummy_2 <- data.frame(model.matrix( ~carbody, data = cars))
dummy_2 <- dummy_2[,-1]
cars <- cbind(cars[,-5], dummy_2)

#creating dummy variables for drivewheel
dummy_3 <- data.frame(model.matrix( ~drivewheel, data = cars))
dummy_3 <- dummy_3[,-1]
cars <- cbind(cars[,-5], dummy_3)

#creating dummy variables for enginetype
dummy_4 <- data.frame(model.matrix( ~enginetype, data = cars))
dummy_4 <- dummy_4[,-1]
cars <- cbind(cars[,-10], dummy_4)

#creating dummy variables for cylindernumber
dummy_5 <- data.frame(model.matrix( ~cylindernumber, data = cars))
dummy_5 <- dummy_5[,-1]
cars <- cbind(cars[,-10], dummy_5)

#creating dummy variables for fuelsystem
dummy_6 <- data.frame(model.matrix( ~fuelsystem, data = cars))
dummy_6 <- dummy_6[,-1]
cars <- cbind(cars[,-11], dummy_6)

#creating dummy variables for symboling
dummy_7 <- data.frame(model.matrix( ~symboling, data = cars))
dummy_7 <- dummy_7[,-1]
cars <- cbind(cars[,-1], dummy_7)


### Model Building

# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(cars), 0.7*nrow(cars))
train = cars[trainindices,]
test = cars[-trainindices,]

model_1<-lm(price~.,data = train)

summary(model_1)

#using stepAIC to remove insignificant variables
step <- stepAIC(model_1, direction="both")
step

#after stepAIC checking for VIF & p values
model_2<-lm(formula = price ~ aspiration + carlength + carwidth + carheight + 
              curbweight + boreratio + stroke + horsepower + CarNamebmw + 
              CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
              CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemercury + 
              CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
              CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
              CarNametoyota + CarNamevolkswagen + CarNamevolvo + carbodyhardtop + 
              carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
              enginetypedohcv + enginetyperotor + cylindernumberfive + 
              cylindernumberfour + cylindernumbersix + symboling1, data = train)

summary(model_2)
vif(model_2)

#removing carlength which has high VIF and p-value
model_3 <- lm(formula = price ~ aspiration + carwidth + carheight + 
                curbweight + boreratio + stroke + horsepower + CarNamebmw + 
                CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemercury + 
                CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
                CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
                CarNametoyota + CarNamevolkswagen + CarNamevolvo + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypedohcv + enginetyperotor + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + symboling1, data = train)

summary(model_3) # 0.9684
vif(model_3)

#removing carbodysedan which has high VIF and p-value
model_4 <- lm(formula = price ~ aspiration + carwidth + carheight + 
                curbweight + boreratio + stroke + horsepower + CarNamebmw + 
                CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemercury + 
                CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
                CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
                CarNametoyota + CarNamevolkswagen + CarNamevolvo + carbodyhardtop + 
                carbodyhatchback + carbodywagon + drivewheelrwd + 
                enginetypedohcv + enginetyperotor + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + symboling1, data = train)

summary(model_4) # 0.9676
vif(model_4)

#removing drivewheelrwd which has high VIF and p-value
model_5 <- lm(formula = price ~ aspiration + carwidth + carheight + 
                curbweight + boreratio + stroke + horsepower + CarNamebmw + 
                CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemercury + 
                CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
                CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
                CarNametoyota + CarNamevolkswagen + CarNamevolvo + carbodyhardtop + 
                carbodyhatchback + carbodywagon +
                enginetypedohcv + enginetyperotor + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + symboling1, data = train)

summary(model_5) # 0.9671
vif(model_5)

#removing horsepower which has high VIF and p-value
model_6 <- lm(formula = price ~ aspiration + carwidth + carheight + 
                curbweight + boreratio + stroke +CarNamebmw + 
                CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemercury + 
                CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
                CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
                CarNametoyota + CarNamevolkswagen + CarNamevolvo + carbodyhardtop + 
                carbodyhatchback + carbodywagon +
                enginetypedohcv + enginetyperotor + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + symboling1, data = train)

summary(model_6) # 0.9667 
vif(model_6)

#removing carheight which has high VIF and p-value
model_7 <- lm(formula = price ~ aspiration + carwidth + 
                curbweight + boreratio + stroke +CarNamebmw + 
                CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemercury + 
                CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
                CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
                CarNametoyota + CarNamevolkswagen + CarNamevolvo + carbodyhardtop + 
                carbodyhatchback + carbodywagon +
                enginetypedohcv + enginetyperotor + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + symboling1, data = train)

summary(model_7) # 0.9665
vif(model_7)

#since all the varaibles which have high VIF , are also signicant due to P-value
#will remove the variables which have high p-value and check r-squared

#removing carbodyhardtop has high P value
model_8 <- lm(formula = price ~ aspiration + carwidth + 
                curbweight + boreratio + stroke +CarNamebmw + 
                CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemercury + 
                CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
                CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
                CarNametoyota + CarNamevolkswagen + CarNamevolvo +
                carbodyhatchback + carbodywagon +
                enginetypedohcv + enginetyperotor + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + symboling1, data = train)

summary(model_8) # 0.9668
vif(model_8)



#removing carbodyhatchback which has high p-value
model_9 <- lm(formula = price ~ aspiration + carwidth + 
                curbweight + boreratio + stroke +CarNamebmw + 
                CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemercury + 
                CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
                CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
                CarNametoyota + CarNamevolkswagen + CarNamevolvo +carbodywagon +
                enginetypedohcv + enginetyperotor + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + symboling1, data = train)

summary(model_9) # 0.967
vif(model_9)

#removing symboling1 which has high p-value
model_10 <- lm(formula = price ~ aspiration + carwidth + 
                 curbweight + boreratio + stroke +CarNamebmw + 
                 CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
                 CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemercury + 
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
                 CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
                 CarNametoyota + CarNamevolkswagen + CarNamevolvo +carbodywagon +
                 enginetypedohcv + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix, data = train)

summary(model_10) # 0.967
vif(model_10)

#removing carbodywagon which has high p-value
model_11 <- lm(formula = price ~ aspiration + carwidth + 
                 curbweight + boreratio + stroke +CarNamebmw + 
                 CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
                 CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemercury + 
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
                 CarNameporsche + CarNamerenault + CarNamesaab + CarNamesubaru + 
                 CarNametoyota + CarNamevolkswagen + CarNamevolvo +
                 enginetypedohcv + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix, data = train)

summary(model_11) # 0.9666
vif(model_11)

#removing CarNamesaab which has high p-value
model_12 <- lm(formula = price ~ aspiration + carwidth + 
                 curbweight + boreratio + stroke +CarNamebmw + 
                 CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
                 CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemercury + 
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
                 CarNameporsche + CarNamerenault + CarNamesubaru + 
                 CarNametoyota + CarNamevolkswagen + CarNamevolvo +
                 enginetypedohcv + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix, data = train)
 
summary(model_12) #0.9659
vif(model_12)

#removing stroke which has high p-value
model_13 <- lm(formula = price ~ aspiration + carwidth + 
                 curbweight + boreratio +CarNamebmw + 
                 CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
                 CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemercury + 
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
                 CarNameporsche + CarNamerenault + CarNamesubaru + 
                 CarNametoyota + CarNamevolkswagen + CarNamevolvo +
                 enginetypedohcv + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix, data = train)

summary(model_13) #0.9655
vif(model_13)

#removing CarNamevolvo which has high p-value
model_14 <- lm(formula = price ~ aspiration + carwidth + 
                 curbweight + boreratio +CarNamebmw + 
                 CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
                 CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemercury + 
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
                 CarNameporsche + CarNamerenault + CarNamesubaru + 
                 CarNametoyota + CarNamevolkswagen +
                 enginetypedohcv + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix, data = train)


summary(model_14) #0.965 
vif(model_14)

#removing boreratio which has high p-value
model_15 <- lm(formula = price ~ aspiration + carwidth + 
                 curbweight  +CarNamebmw + 
                 CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
                 CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemercury + 
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
                 CarNameporsche + CarNamerenault + CarNamesubaru + 
                 CarNametoyota + CarNamevolkswagen +
                 enginetypedohcv + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix, data = train)

summary(model_15) #0.9649 
vif(model_15)

#removing CarNamemercury which has high p-value
model_16 <- lm(formula = price ~ aspiration + carwidth + 
                 curbweight  +CarNamebmw + 
                 CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
                 CarNameisuzu + CarNamejaguar + CarNamemazda +
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
                 CarNameporsche + CarNamerenault + CarNamesubaru + 
                 CarNametoyota + CarNamevolkswagen +
                 enginetypedohcv + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix, data = train)

summary(model_16) #0.9646
vif(model_16)

#removing CarNameisuzu which has high p-value
model_17 <- lm(formula = price ~ aspiration + carwidth + 
                 curbweight  +CarNamebmw + 
                 CarNamebuick + CarNamechevrolet + CarNamedodge + CarNamehonda + 
                 CarNamejaguar + CarNamemazda +
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
                 CarNameporsche + CarNamerenault + CarNamesubaru + 
                 CarNametoyota + CarNamevolkswagen +
                 enginetypedohcv + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix, data = train)

summary(model_17) #0.9642
vif(model_17)


#removing CarNamehonda 
model_18 <- lm(formula = price ~ aspiration + carwidth + 
                 curbweight  +CarNamebmw + 
                 CarNamebuick + CarNamechevrolet + CarNamedodge +
                 CarNamejaguar + CarNamemazda +
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + CarNameplymouth + 
                 CarNameporsche + CarNamerenault + CarNamesubaru + 
                 CarNametoyota + CarNamevolkswagen +
                 enginetypedohcv + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix, data = train)

summary(model_18) #0.962
vif(model_18)

# removing CarNamenissan 
model_19 <- lm(formula = price ~ aspiration + carwidth + 
                 curbweight  +CarNamebmw + 
                 CarNamebuick + CarNamechevrolet + CarNamedodge +
                 CarNamejaguar + CarNamemazda +
                 CarNamemitsubishi  + CarNamepeugeot + CarNameplymouth + 
                 CarNameporsche + CarNamerenault + CarNamesubaru + 
                 CarNametoyota + CarNamevolkswagen +
                 enginetypedohcv + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix, data = train)

summary(model_19) #0.9609
vif(model_19)

#removing CarNamedodge 
model_20 <- lm(formula = price ~ aspiration + carwidth + 
                 curbweight  +CarNamebmw + 
                 CarNamebuick + CarNamechevrolet +
                 CarNamejaguar + CarNamemazda +
                 CarNamemitsubishi  + CarNamepeugeot + CarNameplymouth + 
                 CarNameporsche + CarNamerenault + CarNamesubaru + 
                 CarNametoyota + CarNamevolkswagen +
                 enginetypedohcv + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix, data = train)

summary(model_20) #0.9592
vif(model_20)

#removing CarNameplymouth
model_21 <- lm(formula = price ~ aspiration + carwidth + 
                 curbweight  +CarNamebmw + 
                 CarNamebuick + CarNamechevrolet +
                 CarNamejaguar + CarNamemazda +
                 CarNamemitsubishi  + CarNamepeugeot +
                 CarNameporsche + CarNamerenault + CarNamesubaru + 
                 CarNametoyota + CarNamevolkswagen +
                 enginetypedohcv + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix, data = train)

summary(model_21) #0.9579
vif(model_21)

#removing CarNamevolkswagen
model_22 <- lm(formula = price ~ aspiration + carwidth + 
                 curbweight  +CarNamebmw + 
                 CarNamebuick + CarNamechevrolet +
                 CarNamejaguar + CarNamemazda +
                 CarNamemitsubishi  + CarNamepeugeot +
                 CarNameporsche + CarNamerenault + CarNamesubaru + 
                 CarNametoyota +enginetypedohcv + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix, data = train)

summary(model_22) #0.9566
vif(model_22)

#removing CarNamemazda
model_23 <- lm(formula = price ~ aspiration + carwidth + 
                 curbweight  +CarNamebmw + 
                 CarNamebuick + CarNamechevrolet +
                 CarNamejaguar + CarNamemitsubishi  + CarNamepeugeot +
                 CarNameporsche + CarNamerenault + CarNamesubaru + 
                 CarNametoyota +enginetypedohcv + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix, data = train)

summary(model_23) #0.9558
vif(model_23)

#removing CarNamesubaru
model_24 <- lm(formula = price ~ aspiration + carwidth + 
                 curbweight  +CarNamebmw + 
                 CarNamebuick + CarNamechevrolet +
                 CarNamejaguar + CarNamemitsubishi  + CarNamepeugeot +
                 CarNameporsche + CarNamerenault + 
                 CarNametoyota +enginetypedohcv + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix, data = train)

summary(model_24) #0.9548
vif(model_24)

#removing CarNamerenault
model_25 <- lm(formula = price ~ aspiration + carwidth + 
                 curbweight  +CarNamebmw + 
                 CarNamebuick + CarNamechevrolet +
                 CarNamejaguar + CarNamemitsubishi  + CarNamepeugeot +
                 CarNameporsche +CarNametoyota +enginetypedohcv + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix, data = train)

summary(model_25) #0.9536
vif(model_25)

#removing CarNametoyota
model_26 <- lm(formula = price ~ aspiration + carwidth + 
                 curbweight  +CarNamebmw + 
                 CarNamebuick + CarNamechevrolet +
                 CarNamejaguar + CarNamemitsubishi  + CarNamepeugeot +
                 CarNameporsche +enginetypedohcv + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix, data = train)

summary(model_26) #0.9522
vif(model_26)

#removing CarNamemitsubishi
model_27 <- lm(formula = price ~ aspiration + carwidth + 
                 curbweight  +CarNamebmw + 
                 CarNamebuick + CarNamechevrolet +
                 CarNamejaguar + CarNamepeugeot +
                 CarNameporsche +enginetypedohcv + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix, data = train)

summary(model_27) #0.9506
vif(model_27)

#removing CarNamebuick
model_28 <-lm(formula = price ~ aspiration + carwidth + 
                curbweight  +CarNamebmw + 
                CarNamechevrolet +
                CarNamejaguar + CarNamepeugeot +
                CarNameporsche +enginetypedohcv + enginetyperotor + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix, data = train)

summary(model_28) #0.9484
vif(model_28)

#removing aspiration
model_28 <- lm(formula = price ~ carwidth + 
                 curbweight  +CarNamebmw + 
                 CarNamechevrolet +
                 CarNamejaguar + CarNamepeugeot +
                 CarNameporsche +enginetypedohcv + enginetyperotor + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix, data = train)

summary(model_28) #0.9443
vif(model_28)

#removing cylindernumberfour
model_29 <- lm(formula = price ~ carwidth + 
                 curbweight  +CarNamebmw + 
                 CarNamechevrolet +
                 CarNamejaguar + CarNamepeugeot +
                 CarNameporsche +enginetypedohcv + enginetyperotor + cylindernumberfive + 
                 cylindernumbersix, data = train)
summary(model_29) #0.8954
vif(model_29)

#removing enginetyperotor
model_30 <- lm(formula = price ~ carwidth + 
                 curbweight  +CarNamebmw + 
                 CarNamechevrolet +
                 CarNamejaguar + CarNamepeugeot +
                 CarNameporsche +enginetypedohcv + cylindernumberfive + 
                 cylindernumbersix, data = train)

summary(model_30) #0.896
vif(model_30)

#removing cylindernumberfive
model_31 <- lm(formula = price ~ carwidth + 
                 curbweight  +CarNamebmw + 
                 CarNamechevrolet +
                 CarNamejaguar + CarNamepeugeot +
                 CarNameporsche +enginetypedohcv + cylindernumbersix, data = train)

summary(model_31) #0.8963
vif(model_31)

#removing cylindernumbersix
model_32 <- lm(formula = price ~ carwidth + 
                 curbweight  +CarNamebmw + 
                 CarNamechevrolet +
                 CarNamejaguar + CarNamepeugeot +
                 CarNameporsche +enginetypedohcv, data = train)

summary(model_32) #0.8955
vif(model_32)


#removing CarNamejaguar
model_33 <- lm(formula = price ~ carwidth + 
                 curbweight  +CarNamebmw + 
                 CarNamechevrolet +CarNamepeugeot +
                 CarNameporsche +enginetypedohcv, data = train)

summary(model_33) #0.8933
vif(model_33)

#removing CarNamechevrolet
model_34 <- lm(formula = price ~ carwidth + 
                 curbweight  +CarNamebmw + CarNamepeugeot +
                 CarNameporsche +enginetypedohcv, data = train)

summary(model_34) #0.8861
vif(model_34)

#removing carwidth
model_35 <- lm(formula = price ~ curbweight  +CarNamebmw + CarNamepeugeot +
                 CarNameporsche +enginetypedohcv, data = train)

summary(model_35) #0.8647
vif(model_35)

#removing enginetypedohcv
model_36 <- lm(formula = price ~ curbweight  +CarNamebmw + CarNamepeugeot +
                 CarNameporsche, data = train)

summary(model_36) #0.8561
vif(model_36)


# Predict the car prices in the testing dataset
Predict_1 <- predict(model_36,test[,-18])
test$test_price <- Predict_1
# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2
# check R-squared
rsquared #0.8045673


### Model Summary

##Key Performance Indicators identified which primarily define the prices of the cars in US market.
#1. curbweight
#2. CarNamebmw
#3. CarNamepeugeot
#4. CarNameporsche

# Model R^2     - 0.8561
# Predicted R^2 - 0.8045

## KIP's ##

#1.  Curbweight - weights do matter when buying a car. A car with more weight will be less efficient, difficult to handle, 
#### and will put a lot of strain on the engine and brakes due to inertia. Inorder to increase the efficiency of the car more price is to be paid.
#### Therefore, high curbweight will effect in higher prices. Manufacturer should try to achieve a middle ground value of curbweights for best pricing.

#2.  Car Companies - Brand values of a companies will directly effect the pricing of the car. Inorder to enter the US market by the chinese company,
#### brand's of a car should be considered as one of the major factor for pricing of a car.
