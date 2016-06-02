setwd("C:/Users/jh3561/Desktop/hw4")
library(MASS)
data=Boston
fit<-lm(medv~crim+zn+indus+nox+rm+age+tax,data=data)
summary(fit)

x_data<-data[,c("crim","zn","indus","nox","rm","age","tax")]
x_data<-as.matrix(x_data)
y_data<-data[,"medv"]
correlation<-cor(x_data)
library(usdm)
vif(x_data)

fit1<-lm.ridge(medv~crim+zn+indus+nox+rm+age+tax,data=data,lambda=2)
library(parcor)
ridge.opt<-ridge.cv(x_data,y_data,lambda=c(1,5,10,15,20,25,30),plot.it=TRUE)

library(leaps)
data_new<-data[,c("medv","crim","zn","indus","nox","rm","age","tax")]
data_new<-data.frame(data_new)
regfit.full=regsubsets(medv???.,data_new )
summary(regfit.full)
reg.summary=summary (regfit.full)
plot(reg.summary$adjr2 ,xlab =" Number of Variables ",ylab=" Adjusted RSq",type="l")
which.max (reg.summary$adjr2)
fit2<-lm(medv~crim+zn+rm+age+tax,data=data)



full=lm(medv~crim+zn+indus+nox+rm+age+tax,data=data_new)
null=lm(medv~1,data=data_new)
step(null, scope=list(upper=full, lower=null), direction='forward', trace=TRUE)
step(null, scope=list(upper=full, lower=null), direction='both', trace=TRUE)
step(full, scope=list(upper=full, lower=null), direction='backward', trace=TRUE)

library(glmnet)
grid=10^seq(10,-2,length=100)
x_train=x_data[1:400,]
y_train=y_data[1:400]
x_test=x_data[401:506,]
y_test=y_data[401:506]

lasso.mod=glmnet(x_train,y_train,alpha =1,lambda=grid)
plot(lasso.mod)


set.seed(1)
cv.out=cv.glmnet(x_train,y_train,alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
out=glmnet(x_data,y_data,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)

lasso.opt=mylars(x_data,y_data,k=10)
lasso.opt$lambda.opt
lasso.opt$coefficients
lasso.opt$intercept


