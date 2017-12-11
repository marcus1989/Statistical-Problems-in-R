###### Reading the Data #########
data=read.csv("phoneme.csv")

######### Splittig into train and test set #############

train=subset(data,!regexpr("test", data$speaker) > 0)
test=subset(data,!regexpr("train", data$speaker) > 0)

########## Excluding columns not required #######

train=train[-c(1,259)]
test=test[-c(1,259)]


####### Fitting LDA #######################

require(MASS)
clf=lda(g~.,train)

############# Calculating train and test error ##################

predtrainld=predict(clf,train[-c(258)])$class
predtestld=predict(clf,test[-c(258)])$class
erldtr=mean(predtrainld!=train$g)
erldts=mean(predtestld!=test$g)
erldtr
erldts

################ Plotting LDA in full Rank and in reduced ranks ###############

plot(clf)
plot(clf,dimen=1)
plot(clf,dimen=2)
plot(clf,dimen=3)


######## Calculating Train and Test Errors for different Dimentions #####################

error=function(model,traindata,testdata)
{
totalpredtr=c()
totalpredts=c()
totalerrortr=c()
totalerrorts=c()
for(i in 1:4)
{
predtrain=predict(model,traindata[-c(258)],dimen=i)$class
predtest=predict(model,testdata[-c(258)],dimen=i)$class
errortr=mean(predtrain!=traindata$g)
errorts=mean(predtest!=testdata$g)
totalpredtr=as.data.frame(cbind(totalpredtr,predtrain))
totalpredts=as.data.frame(cbind(totalpredts,predtest))
totalerrortr=as.data.frame(cbind(totalerrortr,errortr))
totalerrorts=as.data.frame(cbind(totalerrorts,errorts))
}
colnames(totalerrortr)=c(1:4)
colnames(totalerrorts)=c(1:4)
error=as.data.frame(rbind(totalerrortr,totalerrorts))
rownames(error)=c("Train","Test")
return(error)
}

e=error(clf,train,test)

############ Plotting Train and Test Errors against Dimentions #############################

leg=c(rownames(e))
matplot(c(1:4),t(e), xlab="Dimentions",ylab="Error",main= "Train and Test Error for Different Dimentions", type=c("b"),pch=17,col=1:2)
legend('topright',legend=leg,pch=17,col=1:2)

############## Calculating new train and test data with only aa and ao ######################

newtrain=subset(train,!regexpr("dcl", train$g) > 0& !regexpr("iy", train$g)>0& !regexpr("sh", train$g)>0)
newtest=subset(test,!regexpr("dcl", test$g) > 0& !regexpr("iy", test$g)>0& !regexpr("sh", test$g)>0)

############# Fitting LDA using New train and test data ##########################

newclf=lda(g~.,newtrain)

############ Ploting new Model ####################

plot(newclf)

############ Predicting using this model and calculating train and test error ##########

prednewtrain=predict(newclf,newtrain[-c(258)])$class
prednewtest=predict(newclf,newtest[-c(258)])$class
ernewtr=mean(prednewtrain!=newtrain$g)
ernewts=mean(prednewtest!=newtest$g)
ernewtr
ernewts
############# Fitting QDA ##########################

clfqd=qda(g~.,train)

################### Predicting using fitted QDA and Calculating Errors #######

predtrainqd=predict(clfqd,train[-c(258)])$class
predtestqd=predict(clfqd,test[-c(258)])$class
erqdtr=mean(predtrainqd!=train$g)
erqdts=mean(predtestqd!=test$g)
erqdtr
erqdts

############## Prediction using only aa and ao ########################

predqdnwtr=predict(clfqd,newtrain[-c(258)])$class
predqdnwts=predict(clfqd,newtest[-c(258)])$class
erqdnwtr=mean(predqdnwtr!=newtrain$g)
erqdnwts=mean(predqdnwts!=newtest$g)
erqdnwtr
erqdnwts

###############################################################
