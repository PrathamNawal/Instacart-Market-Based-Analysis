##lets take order_products_train as training data to build model
trainingdata = order_products_train
trainingdata = left_join(trainingdata,user_prob,by = 'user_id')
trainingdata = left_join(trainingdata,product_prob,by = "product_id")

trainingdata$reordered = as.factor(trainingdata$reordered)
trainingdata$order_id = NULL

gc()

#divide the trainingdata into trainmodel and testmodel
trainmodel =trainingdata[sample(nrow(trainingdata), 900000, replace = F), ]
testmodel = trainingdata[!(1:nrow(trainingdata)) %in% as.numeric(row.names(trainmodel)), ]

###################################################################################
#MODEL-1
#Decision tree using C50 for classification (69.2% accuracy)(F1 SCORE =  0.5647456)
library(C50)
library(caret)
start=Sys.time()
modelDT = C5.0(reordered ~ ., data = trainmodel)
end = Sys.time()
print(end-start)
summary(modelDT)
plot(modelDT)


#extracting variable importance
varimportance = as.data.frame(varImp(modelDT))
varimportance = cbind(rownames(varimportance),varimportance)
colnames(varimportance) = c("Varimportance", "importance")
rownames(varImp) = NULL
varimportance
#visualizing variable importance
ggplot(varimportance,aes(x=Varimportance, y=importance))+
  geom_bar(stat="identity",fill="blue")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+ggtitle("variable importance")

#MODEL-2
#Naive_Bayes
#library(e1071)
#start=Sys.time()
#modelNB = naiveBayes(reordered ~ ., data = trainmodel)
#end = Sys.time()
#print(end-start)
#summary(modelNB)

#predict using test data
predictTest = predict(modelDT, testmodel[,-3], type = "class")

#Visualize the confusion matrix
xtab = table(observed = testmodel[,3], predicted = predictTest)
confusionMatrix(xtab)

#MODEL-2
#start=Sys.time()
#modelLG <- glm(reordered ~ ., data = trainmodel,family = "binomial")
#end = Sys.time()
#print(end-start)
#summary(modelLG)

#predict using test data
#predictTest = predict(modelLG, testmodel[,-3])

test_history = left_join(test_history,user_prob,by = 'user_id')
test_history = left_join(test_history,product_prob,by = "product_id")
test_history = test_history[,c(2,3,5,13,14,15,16,17,18)]
head(test_history)


#Final Prediction-DecisionTree
test_history_pred = predict(modelDT, test_history,type = "class")
#-------------------OR------------------------------#
#Final Prediction- Naive Bayes
#test_history_pred = predict(modelNB, test_history,type = "class")
#Final Prediction- SVM
#est_history_pred = predict(modelLG,test_history)

test_history$reordered = test_history_pred
test_history_new = test_history[,c(1,3,10)]
test = test[,c(2,1)]
finalprediction = left_join(test_history_new,test,by = "user_id")
head(finalprediction)
table(finalprediction$reordered)
finalprediction = finalprediction[which(finalprediction$reordered == 1),]
table(finalprediction$reordered)
finalprediction = finalprediction[,c(4,1)]
finalprediction = split(finalprediction$product_id,finalprediction$order_id)
finalprediction = as.vector(finalprediction)
finalprediction = as.matrix(finalprediction)
finalprediction = as.data.frame(finalprediction)
finalprediction = cbind(rownames(finalprediction), finalprediction)
colnames(finalprediction)[1] = "order_id"
colnames(finalprediction)[2] = "product_id"
rownames(finalprediction) = NULL
colnames(finalprediction)[1] = "order_id"
colnames(finalprediction)[2] = "products"

outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

#Orders that weren't reordered
NoneReorders <- as.data.frame(outersect(test$order_id,finalprediction$order_id))
NoneReorders$newcol <- rep("None",nrow(NoneReorders))
names(NoneReorders) <- c("order_id","products")
finalprediction = rbind(finalprediction,NoneReorders)
names(finalprediction) <- c("order_id","products")
finalprediction = as.matrix(finalprediction)
finalprediction = as.data.frame(finalprediction)

#Encoding final prediction
finalprediction$order_id = as.integer(finalprediction$order_id)
finalprediction$products = as.character(finalprediction$products)

#Saving the output to  csv
write.csv(finalprediction,"submissionDT_Final.csv")
