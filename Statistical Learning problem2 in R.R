
####  RSS functoin

RSS <- function(ytrue,ypred)
{
	sum( (ytrue-ypred) * (ytrue-ypred) )
}

#############  (d)
train_data=ozone[trainset,]
test_data=ozone[testset,]

####### fitting the train data with linear  model 

lm.model <- lm(ozone~radiation+temperature+wind, data=train_data)

 
######## predicting of the model on the test set

predict <- predict(lm.model,test_data[2:4])

df_true_pred =data.frame(test_data$ozone,predict)
names(df_true_pred)=c("TrueValues","predValues")

########  RSS and correlation
RSS(df_true_pred$TrueValues,df_true_pred$predValues)

########  scatter plot
plot(df_true_pred, main = " True VS Predicted ",pch=21)

##############  (e)   ##########
library(FNN)

trainRSS=c()
testRSS=c()

for(i in 3:30 )
{
knnRegTrain=knn.reg(as.matrix(train_data[2:4]),test = NULL,as.matrix(train_data[1]),k=i,algorithm=c("kd_tree"))
trainRSS=c(trainRSS,RSS(train_data[1], knnRegTrain$pred))
 
knnRegTest=knn.reg(as.matrix(train_data[2:4]),as.matrix(test_data[2:4]),as.matrix(train_data[1]),k=i, 
        algorithm=c("kd_tree"))
testRSS=c(testRSS,RSS(test_data[1], knnRegTest$pred))
}

matplot(c(3:30),cbind(testRSS,trainRSS),pch=19,main="Test and Training Eroor",xlab="K" ,ylab="Test and Train Error")



