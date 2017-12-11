##(a)
#transform prostate.train and prostate.test as matrix
load("ps02p3.Rdata")
pro.test<-as.matrix(prostate.test)
pro.train<-as.matrix(prostate.train)

##(b)
#a function ridge.df(X, lambda) that computes the effective degrees of freedom for ridge regression.
lambda<-c(seq(0,1000,1))
ridge.df <- function(X,lambda)
{
I<-diag(ncol(X))
df<-diag(X%*%ginv(t(X)%*%X+lambda*I)%*%t(X))
sum(df)
}

##(c)
#Compute ridge regression models
ridge_train<-lm.ridge(lpsa~.,prostate.train,lambda=c(1:1000))$coef
ridge_test<-lm.ridge(lpsa~.,prostate.test,lambda=c(1:1000))$coef



#Compute the degrees of freedom
df_train<-c(df_train)
for(i in 0:1000)
{
df_train[i]<-ridge.df(pro.train[,1:8],lambda=i)
}
df_test<-c()
for(k in 0:1000)
{
df_test[k]<-ridge.df(pro.test[,1:8],lambda=k)
}
matplot(as.matrix(df_train),t(ridge_train),lty = 1:8,col = 1:8,type = "b",lwd = 1, pch = 21,bg = 1:8,main="train-ridge coef",xlab = "df(lambda)", ylab = "coef")
matplot(as.matrix(df_test),t(ridge_test),lty = 1:8,col = 1:8,type = "b",lwd = 1, pch = 21,bg = 1:8,main="test-ridge coef",xlab = "df(lambda)", ylab = "coef")

#Compute the train and test error for ridge regression

Rtrain<- c()
for(j in 0:1000)
{
btrain<-lm.ridge(lpsa~.,prostate.train,lambda=j)$coef
Rtrain[j]<-t(pro.train[,9]-pro.train[,1:8]%*%btrain)%*%(pro.train[,9]-pro.train[,1:8]%*%btrain)+j*(t(btrain)%*%btrain)
}
Rtest<-c()
for(h in 0:1000)
{
btest<-lm.ridge(lpsa~.,prostate.test,lambda=j)$coef
Rtest[h]<-t(pro.test[,9]-pro.test[,1:8]%*%btest)%*%(pro.test[,9]-pro.test[,1:8]%*%btest)+h*(t(btest)%*%btest)
}
#Plot both test and training error against the degrees of freedom
plot(df_train,Rtrain,,main="train-rss",xlab = "df(lambda)", ylab = "rss")
plot(df_test,Rtest,main="test-rss",xlab = "df(lambda)", ylab = "rss")

##(d)
#Compute lasso regression models
train<-lars(pro.train[,1:8],pro.train[,9],type="lasso")

test<-lars(pro.test[,1:8],pro.test[,9],type="lasso")
#Plot the resulting coefficient curves
plot(train)
plot(test)

#Compute train and test error for different standardized parameters s
a<-predict(lars(pro.train[,1:8],pro.train[,9],type="lasso"),pro.train[,1:8])
b<-predict(lars(pro.test[,1:8],pro.test[,9],type="lasso"),pro.test[,1:8])

#Plot the train and test error against s.
plot(a$fraction,train$RSS)
plot(b$fraction,test$RSS)

##(e)
pcr(pro.train[,9]~., prostate.train, ncomp = 1)
pcr(formula = pro.train[, 9] ~ pro.train[, 1], ncomp = 9, data = prostate.train)



