db <- read.csv("C:\\Users\\Darren\\Documents\\GitHub\\R-eview\\houseData.csv")
#start by profiling data
head(db)
summary(db)

db$HousingMedianAge <- impute(db$HousingMedianAge)
hist(log(db$Population))

db$PopulationLog <- log(db$Population)
db$TotalRoomsLog <- log(db$TotalRooms)
db$HouseholdsLog <- log(db$Households)
db$TotalBedroomsLog <- log(db$TotalBedrooms)

#standaradizing the data is essential to understand 
ds <- as.data.frame(scale(db))
summary(ds)

colnames(ds)
l <- lm(MedianHouseValue ~ Population + MedianIncome + HousingMedianAge, ds)
summary(l)

#extracting data from different databases only work when they have same number of rows
l2 <- lm(log(db$MedianHouseValue) ~ Population + MedianIncome + HousingMedianAge, ds)
summary(l2)

l3 <- lm(MedianHouseValue ~.,ds)
summary(l3)
colnames(ds)

#turn column to true or false condition if medianhousevalue is greater than mean then 1, else 0.
db$MedianHouseValueBinary <- ifelse(db$MedianHouseValue > median(db$MedianHouseValue),1,0)
db$MedianHouseValue <- NULL

db$MedianHouseValueBinary <- as.factor(db$MedianHouseValueBinary)
randomDf <- db[sample(nrow(db)),]
head(randomDf)
nrow(randomDf)

trainDf <- randomDf[1:15000,]
testDf <- randomDf[15001:nrow(randomDf),]

#logisitic regression
logit <- glm(MedianHouseValueBinary~.,trainDf, family = "binomial")
probs <- predict(logit,testDf,type="response")
labels <- ifelse(probs > 0.5,1,0)
cm <- table(testDf$MedianHouseValueBinary, labels)
#accuracy, make sure to play around with boundaries (start with 0.5 then change to 0.4, etc.)
(cm[1,1] + cm[2,2])/sum(cm)

#naive bayes
library(e1071)
nb <- naiveBayes(MedianHouseValueBinary~,trainDf)
labels <- predict(nb, testDf)
cm <- table(testDf$MedianHouseValueBinary, labels)

(cm[1,1] + cm[2,2])/sum(cm)







