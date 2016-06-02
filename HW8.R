library("Sleuth3")
data<-ex2224
ltime=log(data$Time)
data$System=as.factor(data$System)
data$Operator=as.factor(data$Operator)
data$Valve=as.factor(data$Valve)
data$Size=as.factor(data$Size)
data$Mode=as.factor(data$Mode)

fit1<-glm(Failures~System+Operator+Valve+Size+Mode,offset=ltime,family=poisson(link=log),data=data)
summary(fit1)
anova(fit1,test="Chisq")

pchisq(fit1$deviance, df=fit1$df.residual, lower.tail=FALSE)
null<-glm(Failures~1,offset=ltime,family=poisson(link=log),data=data)
step(fit1, direction="backward", trace=TRUE)

fit2<-glm(Failures~System+Operator+Valve+Size,offset=ltime,family=poisson(link=log),data=data)
fit3<-glm(Failures~System+Valve+Size,offset=ltime,family=poisson(link=log),data=data)

x=model.matrix(data$Failures~data$System+data$Operator+data$Valve+data$Size+data$Mode)[,-1]
y=as.vector(data$Failures)
set.seed=1
cv<-cv.glmnet(x,y,family="poisson",offset=ltime)
cv$lambda.min
fit4<-glmnet(x,y,family="poisson",offset=ltime,lambda=0.4477339)
fit4$a0
fit4$beta


