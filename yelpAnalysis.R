
d<- read.csv("C:\\Users\\Darren\\Documents\\Github\\R-eview\\dataYelpPractice.csv")

head(d)
summary(d)

d$top10percent <- as.factor(d$top10percent)

randomDf <- d[sample(nrow(d)),]
trainDf <- randomDf[1: round(0.65 * nrow(randomDf)),]
fromInd <- (round(0.65 * nrow(randomDf))+1)
testDf <- randomDf[fromInd:nrow(randomDf),]

### Logistic
library(ROCR)
mylogit <- glm(top10percent~., data = trainDf, family ="binomial")
logitProbs <- predict(mylogit,testDf, type = "response")
Logit_PredictedClass <- ifelse(logitProbs > 0.5, 1, 0)
cm <- table(Logit_PredictedClass,testDf$top10percent)
Accuracy <- (cm[1,1]+cm[2,2])/sum(cm)
Accuracy

pLog <- prediction(logitProbs, testDf$top10percent)
perf <- performance (pLog, "acc")
plot(perf)
abline(v = 0.5)
aucLog <- performance (pLog, "auc")
aucLogScore <- unlist(slot(aucLog, "y.values"))
aucLogScore

### NAIVE BAYES
library(e1071)
nb <- naiveBayes(top10percent~., data = trainDf)
NB_PredictedClass <-predict(nb,testDf)
nrow(testDf)
nrow(trainDf)
length(NB_PredictedClass)
cm <- table(NB_PredictedClass,testDf$top10percent)
Accuracy <- (cm[1,1]+cm[2,2])/sum(cm)
Accuracy

predClasses <- predict(nb,testDf, type = "raw")
df <- as.data.frame(predClasses)
colnames(df) <- c('prob_0','prob_1')

pred <- prediction(df$prob_1,testDf$top10percent)
accObj <- performance(pred, "acc")
max(unlist(slot(accObj,"y.values")))
aucObj <- performance(pred, "auc")
unlist(slot(aucObj,"y.values"))

### TREES
library(party)
tr <- ctree(top10percent~., data = trainDf)
plot(tr)
Trees_PredictedClass <- predict(tr,testDf)
Trees_PredictedClass2 <- predict(testTree,testDf, type = "prob")

cm <- table(Trees_PredictedClass,testDf$top10percent)
Accuracy <- (cm[1,1]+cm[2,2])/sum(cm)
cm
Accuracy

df <- do.call(rbind.data.frame, Trees_PredictedClass2)
colnames(df) <- c('prob0','prob1')
p <- prediction(df$prob1,testDf$top10percent)
aucObj <- performance(p,"auc")
unlist(slot(aucObj,"y.values"))

### SVM:
svmRes <- svm(top10percent~., data = trainDf)
svm_PredictedClass <- predict(svmRes,testDf)
cm <- table(svm_PredictedClass,testDf$top10percent)
Accuracy <- (cm[1,1]+cm[2,2])/sum(cm)
cm
Accuracy

labelsNo <- as.numeric(as.character(svm_PredictedClass))
SVMPred <- prediction(labelsNo,testDf$top10percent)
aucObj <- performance(SVMPred, "auc")
unlist(slot(aucObj, "y.values"))
